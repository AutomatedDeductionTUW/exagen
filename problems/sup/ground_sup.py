# Usage example:
#  python3.7 ground_sup.py -n 29 -o generated/ --run_vampire --vampire_bin ./vampire_rel_custom-kbo_4482
#
# Generates smt files (i.smt2) with a set of clauses following the pattern:
#   F(X) = a | F(Y) = a
#   X = a [ | Z != Z]
#   Y = a [ | Z != Z ]
#   F(a) != a [ | Z != Z]
# where:
#  - F is either f or g,
#  - X, Y, Z are from { f(a), f(b), g(a), g(b) }, they are different, and at least one of them contains b,
#  - F is not in both X and Y,
#  - if F=f, then Z=g(b), if F=g, then Z=f(b), and
#  - the " | Z != Z" is contained in exactly one clause.
#
# For each combination of F, X, Y, Z the script generates three instances of the same input, each of these
# instances with different weights+precedence (in files i_weights.tex and i_precedence.tex).

import argparse
import itertools
import os
import subprocess
import sys

header_dict = {"f": "(declare-fun f (A) A)\n",
               "g": "(declare-fun g (A) A)\n",
               "a": "(declare-const a A)\n",
               "b": "(declare-const b A)\n",
               "A": "(declare-sort A 0)\n"}
header = "".join(header_dict[x] for x in ("A", "a", "b", "f", "g"))

consts = ["a", "b"]
fns = ["f", "g"]

body = ["(or (= (F X) C) (= (F Y) C))",
        "(= X C)",
        "(= Y C)",
        "(not (= (F C) C))"]

def replace(strings, replacements):
    res = "\n".join(strings)
    for r in replacements:
        res = res.replace(r[0], r[1])
    return res

def get_weight_and_precedence(x1, x2, f, j):
    f1, f2 = x1[1], x2[1]
    c1, c2 = x1[3], x2[3]
    w = []
    p = ()
    if j == 0:
        f3 = (f1, f2)[f1 == f]
        f4 = f
        if f1 != f2 and c1 != c2: f3, f4 = f4, f3
        w = {1 : ["b", f3], 2: ["a"], 3: [f4]}
        p = ("a", "b", f3, f4)
    elif j == 1:
        f3, f4 = f1, f
        if f1 != f2:
            f3 = f
            f4 = (f1, f2)[f1 == f]
        w = {0: [f3], 1 : ["b"], 2: ["a"], 3: [f4]}
        p = (f3, "a", f4, "b")
    elif j == 2:
        f3 = f
        f4 = (f1, f2)[f1 == f]
        if f1 == f2 or c1 != c2:
            w = {0: [f3], 1 : ["b", f4], 3: ["a"]}
            p = (f3, "a", "b", f4)
        else:
            w = {1 : ["b", f4], 2: [f3], 3: ["a"]}
            p = (f3, f4, "a", "b")
    return w, p

def run_vampire(vbin, fname, weights):
    wstr = ",".join(symbol+"="+str(weight) for weight in weights for symbol in weights[weight])
    to_run = vbin + " --input_syntax smtlib2 -sp occurrence -av off -fsr off -fs off -fd off -bd off --custom_kbo_weights " + wstr + " " + fname
    print(to_run)
    process = subprocess.run(to_run, shell=True, capture_output=True, text=True)
    print(process.stdout)


composites = ["(" + f + " " + c + ")" for f, c in itertools.product(fns, consts)]

parser = argparse.ArgumentParser(description="Generate up to 36 ground superposition problems.")
parser.add_argument("-n", default=36, type=int, help="Number of problems to generate")
parser.add_argument("-o", help="Output directory")
parser.add_argument("--run_vampire", default=False, action="store_true", help="Run Vampire on generated problems")
parser.add_argument("--vampire_bin", default="vampire", help="Path to Vampire binary")
args = parser.parse_args()

i = 1
for x1, x2, x3, f in itertools.product(composites, composites, composites, fns):
    c = "a"
    if c in x1 and c in x2: continue
    if f in x1 and f in x2: continue
    if x1 == x2: continue
    if f in x3 or c in x3: continue
    if f in x1 and c in x1: continue
    if f in x2 and c in x2: continue

    for j in range(3):
        if i > args.n: break
        w, p = get_weight_and_precedence(x1, x2, f, j)
        fprefix = os.path.join(args.o, "exam-"+str(i).zfill(2), "sup")
        # Generate files with weights and precedence
        wout = open(fprefix + "-weights.tex", "w")
        wstrs = ["w(" + symbol + ") = " + str(key) for key in w for symbol in w[key]]
        wout.write("$" + ", ".join(wstrs) + "$")
        wout.close()
        pout = open(fprefix + "-precedence.tex", "w")
        pout.write("$" + r""" \gg """.join(p) + "$")
        pout.close()
        # Create the clauses
        strings = ["(assert " + s + ")" for s in body]
        idx = 1 + ((i+j) % 3)
        strings[idx] = "(assert (or (not (= Z Z)) " + body[idx] + "))"
        fout = open(fprefix + ".smt2", "w")
        # Declare symbols in the reverse order of precedence
        fout.write(header_dict["A"])
        for symbol in reversed(p): fout.write(header_dict[symbol])
        # Write the clauses
        fout.write(replace(strings, [("F", f), ("C", c), ("X", x1), ("Y", x2), ("Z", x3)]))
        fout.close()
        # Optionally run Vampire on the generated file
        if args.run_vampire:
            print("Running vampire for input #" + str(i) + ":")
            run_vampire(args.vampire_bin, fprefix + ".smt2", w)
        i += 1

    if i > args.n: break
