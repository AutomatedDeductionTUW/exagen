#!/usr/bin/env zsh

# Abort on errors
set -euo pipefail

SMT2TEX="../smt2tex/smt2tex.py"

trash final/

for exam in exam-*; do
    echo "Compiling $exam"

    # Produce latex version of smt files
    for smtfile in "$exam"/*.smt2; do
        "$SMT2TEX" < "$smtfile" > "${smtfile}.tex"
    done

    # Link the template from parent directory
    ln -sf "../template.tex" "${exam}/${exam}.tex"

    # Compile exam
    ( cd "$exam" && latexmk -quiet -pdf "${exam}.tex" )
done

mkdir -p final
cp exam-*/exam-*.pdf final/

echo
echo "Compiled exams are in directory 'final'"
