#!/usr/bin/env python3
"""Count which quaalude names real code uses: the Advent of Code solutions written with it.

    python3 scripts/site/usage.py PATH/TO/advent

Reads the 2024 and 2025 solutions (the years that use quaalude as their Prelude) and
data/browse.json, and writes data/usage.json: for each quaalude name, how many solutions use
it and how often, with its signature and module.
"""
import collections
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
TOKEN = re.compile(r"[A-Za-z_\u0370-\u03ff\u0400-\u04ff\u2100-\u214f][\w'\u2080-\u209f\u00b2\u00b3\u00b9\u2070-\u207f]*"
                   r"|[^\s\w()\[\],;`\"']+")


def index():
    """name -> (signature, module) from :browse output; class methods included."""
    idx = {}
    for mod, text in json.loads((HERE / "data" / "browse.json").read_text()).items():
        if mod == "Quaalude":
            continue
        cur = None
        for line in text.splitlines():
            m = re.match(r"^\s{0,2}(\([^)]+\)|[^\s:()]+)\s+::", line)
            if m and not line.startswith(("type ", "class ", "data ", "newtype ")):
                cur = m.group(1).strip("()")
                idx.setdefault(cur, [line.strip(), mod])
            elif cur and line[:1].isspace() and not line.strip().startswith("{-#"):
                if len(idx[cur][0]) < 400:
                    idx[cur][0] += " " + line.strip()
            else:
                cur = None
    return idx


def main():
    advent = Path(sys.argv[1])
    idx = index()
    files, uses = collections.Counter(), collections.Counter()
    sols = sorted([*advent.glob("2024/src/Day*.hs"), *advent.glob("2025/src/Day*.hs")])
    sols = [s for s in sols if s.stem != "Day666"]  # the year template, not a solution
    for f in sols:
        toks = [t for t in TOKEN.findall(f.read_text()) if t in idx]
        uses.update(toks)
        files.update(set(toks))
    out = {"solutions": len(sols), "distinct": len(files),
           "names": [{"name": n, "solutions": k, "uses": uses[n], "signature": idx[n][0], "module": idx[n][1]}
                     for n, k in files.most_common()]}
    (HERE / "data" / "usage.json").write_text(json.dumps(out, indent=1, ensure_ascii=False) + "\n")
    print(f"{len(sols)} solutions, {len(files)} distinct quaalude names")


if __name__ == "__main__":
    main()
