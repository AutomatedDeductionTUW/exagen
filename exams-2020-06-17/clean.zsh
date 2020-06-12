#!/usr/bin/env zsh

# Abort on errors
set -euo pipefail

setopt extendedglob

# Absolute path of directory containing the current script
# Modifiers: P -> absolute path; h -> parent directory
exams_dir="${${(%):-%x}:P:h}"

for file in "$exams_dir"/*/**/*.(aux|fdb_latexmk|fls|log|out|pdf|synctex.gz|smt2.tex)(#qN); do
    rm -- "$file"
done
