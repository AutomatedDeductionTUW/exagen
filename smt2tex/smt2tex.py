#!/usr/bin/env python3

import formula_parser
import lisp_parser

if __name__ == "__main__":
    import sys
    print(f"Args: {sys.argv}")
    for stmt in lisp_parser.parse_file("/dev/stdin"):
        if len(stmt) > 0 and stmt[0] == "assert":
            formula = stmt[1]
            print(stmt[1])
            print(r"\[ {} \]".format(formula_parser.parse(formula)[0]))
            print()
    print("hi")
