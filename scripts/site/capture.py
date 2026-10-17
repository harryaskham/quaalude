#!/usr/bin/env python3
"""Record what the site quotes from quaalude by compiling it and asking GHCi.

    python3 scripts/site/capture.py

1. Builds GHC with every dependency (scripts/site/ghc-env.nix, a pinned nixpkgs).
2. Builds the library offline with cabal against exactly those packages.
3. Runs one GHCi session that prints `:browse` for every module (the reference) and evaluates
   each example in EXAMPLES (the snippets on the page), separated by markers.

Writes scripts/site/data/{browse.json, examples.json}. Runs in a clean environment with a
scratch HOME and a UTF-8 locale (without one, GHCi prints every Unicode name as "?").
"""
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
DATA = HERE / "data"

# (id, GHCi input). Inputs are typed exactly as on the page; `:t` lines show types.
EXAMPLES = [
    ("alias-int", ":kind! ℤ"),
    ("alias-int64", ":kind! ℤ₆₄"),
    ("alias-pair", ":kind! ℤ²"),
    ("alias-maybe", ":kind! Ѓ ℤ (?)"),
    ("mk-set", "mk [3, 1, 2, 1] :: Set ℤ"),
    ("un-set", "un (mk [3, 1, 2, 1] :: Set ℤ)"),
    ("co-map", "co [(1, 'a'), (2, 'b')] :: Map ℤ Char"),
    ("sort-up", "(🎜) [3, 1, 2]"),
    ("sort-down", "(🎝) [3, 1, 2]"),
    ("cond", "(2 > 1) ??? \"yes\" $ \"no\""),
    ("parse-numbers", "\"move 10 to -3, then 42\" |- numbers :: [ℤ]"),
    ("both", "both (+ 1) (1, 2)"),
    ("apply-pair", "(+) $@ (2, 3)"),
    ("dup", "dup 'x'"),
    ("concat", "(<>!) [\"ab\", \"cd\"]"),
    ("member", "3 ∈ (mk [1, 2, 3] :: Set ℤ)"),
    ("insert", "3 |-> (mk [1] :: Set ℤ)"),
    ("size", "size (mk \"hello\" :: Set Char) :: ℤ"),
    ("lookup", "(mkMap [(1, 'a')] :: Map ℤ Char) |? 2"),
    ("index", "(mkMap [(1, 'a')] :: Map ℤ Char) |! 1"),
    ("set-key", "(mkMap [(1, 'a')] :: Map ℤ Char) |. (2, 'b')"),
    ("as-bool", "as @ℤ True"),
    ("grid-let", "let g = readGrid \"..#\\n#..\\n\" :: Grid Char"),
    ("grid-get", "g ||? (2, 0)"),
    ("grid-find", "gridFindOne '#' g"),
    ("grid-max", "maxXY g"),
    ("turn", "turnCW DirUp"),
    ("move", "move DirUp (2 :: Int) (5, 5 :: Int)"),
    ("manhattan", "manhattan (1, 2) (4, 6 :: Int)"),
]


def sh(args, env, cwd=ROOT, stdin=None, timeout=3600, merge=False):
    """Run a command; with merge, stderr is interleaved into stdout (GHCi reports errors there)."""
    r = subprocess.run(args, cwd=cwd, env=env, input=stdin, text=True, timeout=timeout, stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT if merge else subprocess.PIPE)
    if r.returncode:
        sys.exit(f"{' '.join(args[:3])} failed:\n{(r.stderr or r.stdout)[-3000:]}")
    return r.stdout


def modules():
    mods = []
    for f in sorted((ROOT / "src").rglob("*.hs")):
        mods.append(".".join(f.relative_to(ROOT / "src").with_suffix("").parts))
    return mods


def main():
    DATA.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"), "HOME": os.path.join(tmp, "home"),
               "LANG": "C.UTF-8", "LC_ALL": "C.UTF-8"}
        out = os.path.join(tmp, "ghc")
        sh(["nix", "build", "--impure", "-f", str(HERE / "ghc-env.nix"), "-o", out], env)
        ghc = os.path.join(out, "bin", "ghc")
        env["PATH"] = os.path.join(out, "bin") + ":" + env["PATH"]
        sh(["cabal", "build", "--offline", "-w", ghc, "lib:quaalude"], env)

        mods = modules()
        script = [":set -XOverloadedStrings -XUnicodeSyntax -XTypeApplications -XDataKinds", "import Quaalude"]
        for m in mods:
            script += [f'putStrLn "@@browse {m}"', f":browse {m}"]
        for key, expr in EXAMPLES:
            script += [f'putStrLn "@@example {key}"', expr]
        script.append('putStrLn "@@end"')
        text = sh(["cabal", "repl", "--offline", "-w", ghc, "lib:quaalude", "-v0"], env,
                  stdin="\n".join(script) + "\n", merge=True)

    parts = re.split(r"^@@(browse|example|end) ?(.*)$", text, flags=re.M)
    browse, examples = {}, {}
    for i in range(1, len(parts) - 1, 3):
        kind, name, body = parts[i], parts[i + 1].strip(), parts[i + 2].strip("\n")
        if kind == "browse":
            browse[name] = body
        elif kind == "example":
            examples[name] = {"input": dict(EXAMPLES)[name], "output": body}
    missing = [m for m in mods if m not in browse] + [k for k, _ in EXAMPLES if k not in examples]
    errors = {k: v["output"][:300] for k, v in examples.items() if "error" in v["output"]}
    if errors:
        sys.exit(f"examples failed: {json.dumps(errors, ensure_ascii=False, indent=1)}")
    if missing:
        sys.exit(f"no output for: {missing}")
    (DATA / "browse.json").write_text(json.dumps(browse, indent=1, ensure_ascii=False) + "\n")
    (DATA / "examples.json").write_text(json.dumps(examples, indent=1, ensure_ascii=False) + "\n")
    print(f"browse: {len(browse)} modules; examples: {len(examples)}")


if __name__ == "__main__":
    main()
