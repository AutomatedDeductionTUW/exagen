#!/usr/bin/env python3

import formula_parser
import lisp_parser

if __name__ == "__main__":
    for stmt in lisp_parser.parse_file("/dev/stdin"):
        if len(stmt) > 0 and stmt[0] == "assert":
            formula = stmt[1]
            # print(formula)
            latex = formula_parser.parse(formula)[0]
            print(rf"\[ {latex} \]")
            print()
