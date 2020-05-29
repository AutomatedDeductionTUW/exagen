#!/usr/bin/env python3

import formula_parser
import lisp_parser

if __name__ == "__main__":
    print("\\[\n%")

    first = True
    for stmt in lisp_parser.parse_file("/dev/stdin"):
        if len(stmt) > 0 and stmt[0] == "assert":
            formula = stmt[1]

            if first:
                first = False
            else:
                print("%\n\\land\n%")

            print(f"% {formula}")
            latex = formula_parser.parse(formula)[0]

            if formula[0] == 'or':
                print(f"({latex})")
            else:
                print(latex)
    print("%\n\\]")
