"""Generated regions for the quaalude site, all from data captured by compiling the library.

data/browse.json    GHCi :browse of every module (capture.py)
data/examples.json  GHCi inputs and their real output (capture.py)
data/usage.json     how often real solutions use each name (usage.py)
data/showcase.hs    one real solution (advent 2024, day 6)
"""
import html
import json
import re
from pathlib import Path

from hs import highlight

DATA = Path(__file__).resolve().parent / "data"
FIGS = {}  # no drawings on this page


def load(name):
    return json.loads((DATA / name).read_text())


# What each module is for; checked against its :browse output.
MODULES = {
    "Quaalude.Alias": "Notation: number-set types (ℤ, ℕ, ℚ, ℝ, 𝔹, 𝕊, 𝕋), logic and function operators (∘ ≡ ≤ ∧ ∨ ⋅), ⊥, sorting with 🎜 and 🎝, and the ??? conditional.",
    "Quaalude.Bits": "Bit helpers: bits to and from integers, most and least common bit, hex to binary.",
    "Quaalude.BoundedSet": "Sets that track their bounds and an origin.",
    "Quaalude.Braket": "An experiment with bra-ket notation. Not re-exported by Quaalude.",
    "Quaalude.Collection": "One vocabulary for lists, sets, maps, sequences, vectors and queues: mk, un and co for conversion, and operators for lookup, insertion, deletion, membership and size.",
    "Quaalude.Compose": "Composition of functions of any arity, through a type family.",
    "Quaalude.Coord": "Directions in two and three dimensions, turning, coordinates, Manhattan and squared distances, neighbourhoods.",
    "Quaalude.Described": "Functions and arguments tagged with type-level descriptions.",
    "Quaalude.Encoding.JSON": "Aeson deriving helpers: prefix stripping, omitted Nothing fields.",
    "Quaalude.Encoding.JSON.Instances": "JSON instances for the library's types.",
    "Quaalude.Encoding.JSON.TH": "Template Haskell for JSON instances.",
    "Quaalude.Geometry": "Rectangles and line segments: intersection, subtraction, outlines.",
    "Quaalude.Grid": "The Griddable class over ten storage backends, pure and mutable; typed cells; reading, iterating, cropping and rotating grids.",
    "Quaalude.Lists": "Heterogeneous lists and length-indexed homogeneous lists.",
    "Quaalude.LossQ": "Priority queues ordered by a loss function, for best-first search.",
    "Quaalude.Math": "Digits, differences, sign, triangular numbers, mean and median.",
    "Quaalude.MaxSet": "A set that keeps track of its maximum.",
    "Quaalude.Megaparsec": "A Megaparsec parser type and helpers such as parseMatches.",
    "Quaalude.QuasiSet": "A wrapper that treats a container as a set.",
    "Quaalude.Records": "HList record helpers: prefix stripping and record conversion.",
    "Quaalude.Reducible": "Type-level machinery that reduces curried functions to a terminal value.",
    "Quaalude.Shape": "Shapes as sets of cells with their bounds, for falling-block and tiling puzzles.",
    "Quaalude.Show": "Show instances, such as arrows for directions.",
    "Quaalude.Solve": "Z3 from Haskell: solve and minimize, with results extracted by type.",
    "Quaalude.TH": "Compile-time embedding of puzzle input and examples (aoc, grid, …) and runners for every day.",
    "Quaalude.Tracers": "Debug.Trace variants: conditional, prefixed, animated, and tracing inside parsers.",
    "Quaalude.Tuple": "Tuple construction, access and uncurried application for large arities ($@, dup, …).",
    "Quaalude.Type": "Type-level experiments, work in progress. Not re-exported by Quaalude.",
    "Quaalude.Unary": "Operators taking () for prefix forms, set union and intersection, and Ѓ, the Maybe alias.",
    "Quaalude.Util": "Parsing helpers (|-, number, numbers), conversion with as, memoisation, iteration and more.",
    "Quaalude.Variadic": "Variadic functions through type classes.",
}


def _ghci(keys):
    ex = load("examples.json")
    rows = []
    for k in keys:
        e = ex[k]
        rows.append(f'<span class="p">λ</span> {html.escape(e["input"])}')
        if e["output"].strip():
            rows.append(f'<span class="o">{html.escape(e["output"].rstrip())}</span>')
    return '<pre class="ghci" tabindex="0" aria-label="GHCi session"><code>' + "\n".join(rows) + "</code></pre>"


QUALIFIER = re.compile(r"\b(?:[A-Z][\w']*\.)+(?=[A-Za-z(\u0370-\u03ff])")


def _type_of(signature):
    """The type part of a signature, for display: default-method signatures and module qualifiers removed."""
    sig = signature.split(" default ", 1)[0]
    sig = sig.split("::", 1)[1].strip() if "::" in sig else sig
    return QUALIFIER.sub("", sig)


def _sig_index():
    return {n["name"]: f'{n["name"]} :: {_type_of(n["signature"])}   ({n["module"].removeprefix("Quaalude.")})' for n in load("usage.json")["names"]}


def showcase():
    src = (DATA / "showcase.hs").read_text()
    return '<pre class="code" tabindex="0"><code>' + highlight(src.rstrip(), _sig_index()) + "</code></pre>"


# Names this solution uses only at the type level, where they mean something else: in
# `Ѓ ℤ (?)`, (?) is the class from Quaalude.Unary, not the value-level ? from Quaalude.Alias.
TYPE_ONLY = {"?"}


def glossary():
    src = (DATA / "showcase.hs").read_text()
    names = load("usage.json")["names"]
    rows = []
    for n in names:
        name = n["name"]
        if name in TYPE_ONLY:
            continue
        pat = re.escape(name)
        if re.search(rf"(?<![\w'#|.?<>!$&*+/=@^~-]){pat}(?![\w'#|.?<>!$&*+/=@^~-])", src):
            sig = n["signature"]
            rows.append(f'<tr><td><code class="sym">{html.escape(name)}</code></td>'
                        f'<td><code>{html.escape(_type_of(sig))}</code></td>'
                        f'<td class="mod">{html.escape(n["module"].removeprefix("Quaalude."))}</td></tr>')
    return ('<table class="gloss"><caption>Every quaalude name in this solution, with its type</caption>\n'
            '<thead><tr><th scope="col">Name</th><th scope="col">Type</th><th scope="col">Module</th></tr></thead>\n<tbody>\n'
            + "\n".join(rows) + "\n</tbody></table>")


def usage():
    u = load("usage.json")
    rows = []
    for n in u["names"][:30]:
        sig = _type_of(n["signature"])
        rows.append(f'<tr><td><code class="sym">{html.escape(n["name"])}</code></td>'
                    f'<td class="num">{n["solutions"]}</td>'
                    f'<td><code>{html.escape(sig)}</code></td>'
                    f'<td class="mod">{html.escape(n["module"].removeprefix("Quaalude."))}</td></tr>')
    return (f'<table class="usage"><caption>The 30 most widely used of the {u["distinct"]} quaalude names '
            f'in the {u["solutions"]} solutions</caption>\n'
            '<thead><tr><th scope="col">Name</th><th scope="col" class="num">Solutions</th><th scope="col">Type</th>'
            '<th scope="col">Module</th></tr></thead>\n<tbody>\n' + "\n".join(rows) + "\n</tbody></table>")


def _entries(text):
    """Split :browse output into declarations: a line at column 0 starts one."""
    out, cur = [], []
    for line in text.splitlines():
        if line.strip().startswith("{-#"):
            continue
        if line and not line[0].isspace() and cur:
            out.append("\n".join(cur))
            cur = []
        cur.append(line)
    if cur:
        out.append("\n".join(cur))
    return out


def reference():
    browse = load("browse.json")
    parts = []
    for mod in sorted(m for m in browse if m != "Quaalude"):
        entries = _entries(browse[mod])
        desc = MODULES.get(mod, "")
        # Modules Quaalude does not re-export are printed fully qualified by GHCi; show short names.
        body = "\n".join(
            f'<div class="e" data-k="{html.escape(e.lower(), quote=True)}"><code>{html.escape(e.replace(mod + ".", ""))}</code></div>'
            for e in entries)
        slug = mod.lower().replace(".", "-")
        parts.append(f'<details class="mod" id="{slug}"><summary><span class="name">{html.escape(mod)}</span>'
                     f'<span class="count">{len(entries)}</span><span class="desc">{html.escape(desc)}</span></summary>'
                     f'<div class="entries" tabindex="0">{body or "<p class=note>Instances only.</p>"}</div></details>')
    return "\n".join(parts)


GEN = {
    "showcase": showcase,
    "glossary": glossary,
    "usage": usage,
    "reference": reference,
    "ghci-notation": lambda: _ghci(["alias-int", "alias-int64", "alias-pair", "alias-maybe"]),
    "ghci-collections": lambda: _ghci(["mk-set", "un-set", "co-map", "member", "insert", "size", "lookup", "index", "set-key"]),
    "ghci-grid": lambda: _ghci(["grid-let", "grid-get", "grid-find", "grid-max", "turn", "move", "manhattan"]),
    "ghci-small": lambda: _ghci(["parse-numbers", "both", "apply-pair", "dup", "concat", "as-bool", "sort-up", "sort-down", "cond"]),
}
