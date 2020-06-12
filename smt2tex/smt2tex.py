#!/usr/bin/env python3
# Usage example:
#  smt2tex.py < formula.smt2 > formatted.tex

import argparse

import formula_parser
import lisp_parser


parser = argparse.ArgumentParser(description="Convert *.smt2 to LaTeX")
parser.add_argument("--conjunction", default=False,  action="store_true",
        help="Conjoin formulas into a single conjunction")
args = parser.parse_args()


if __name__ == "__main__":
    print("\\begin{align*}\n%")

    clause_number = 1
    linebreaks = set()
    for stmt in lisp_parser.parse_file("/dev/stdin"):
        # Specify linebreaks in input file with a command like the following:
        #   (set-info :latex-linebreak-after-clause "3 7 5")
        if len(stmt) == 3 and stmt[0] == "set-info" and stmt[1] == ":latex-linebreak-after-clause":
            linebreaks_str = stmt[2][1:-1]  # note: remove quotes around value
            linebreaks = set(int(w) for w in linebreaks_str.split())

        if len(stmt) > 0 and stmt[0] == "assert":
            formula = stmt[1]

            if clause_number > 1:
                if args.conjunction:
                    print("%\n\\land\n%")
                else:
                    print("~\\\\~")

            if clause_number == 1 and args.conjunction:
                print("& %")

            print(f"% {formula}")
            latex = formula_parser.parse(formula)[0]

            if args.conjunction:
                if formula[0] == 'or':
                    print(f"({latex})")
                else:
                    print(latex)

                if clause_number in linebreaks:
                    print("~\\\\~\n& %")
                    # print("~\\land \\\\~")
            else:
                print(f"& {latex}")

            clause_number += 1

    print("%\n\\end{align*}")
