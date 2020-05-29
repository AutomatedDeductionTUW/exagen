#!/usr/bin/env zsh

# Abort on errors
set -euo pipefail


for exam in exam-*; do
    echo "Compiling $exam"

    ln -sf "../template.tex" "$exam/$exam.tex"
    (
        cd "$exam"
        echo hi
        latexmk -quiet -pdf "$exam.tex"
    )
done

mkdir -p final
cp exam-*/exam-*.pdf final/

echo
echo "Compiled exams are in directory 'final'"
