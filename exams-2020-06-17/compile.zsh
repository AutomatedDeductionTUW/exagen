#!/usr/bin/env zsh

# Abort on errors
set -euo pipefail

setopt extendedglob

# Absolute path of directory containing the current script
# Modifiers: P -> absolute path; h -> parent directory
exams_dir="${${(%):-%x}:P:h}"

# Root directory of repository
root_dir="${exams_dir:h}"

num_exams="3"   # Number of exams to generate
seed="123"      # Pseudo-random number generator seed

SMT2TEX="${root_dir}/smt2tex/smt2tex.py"

for file in "$exams_dir"/final/*.pdf(#qN); do
    rm -- "$file"
done

# Generate SAT problems
( cd "${root_dir}/generator" &&
    stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" sat )

# Generate SMT problems
( cd "${root_dir}/generator" &&
    stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" smt -t "${root_dir}/problems/smt/template1.smt2" )

# Generate ground superposition problems
# TODO
# ( cd "${root_dir}/generator" &&
#     stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" sup )

# Generate redundancy problems
# TODO
# ( cd "${root_dir}/generator" &&
#     stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" red )

for exam in "$exams_dir"/exam-*; do
    echo "Compiling $exam"
    examtex="${exam:t}.tex"

    # Produce latex version of smt files
    for smtfile in "$exam"/*.smt2; do
        "$SMT2TEX" < "$smtfile" > "${smtfile}.tex"
    done

    # Link the template from parent directory
    ln -sf "../template.tex" "${exam}/${examtex}"

    # Compile exam
    ( cd "$exam" && latexmk -quiet -pdf "$examtex" )
done

mkdir -p "${exams_dir}/final"
cp "$exams_dir"/exam-*/exam-*.pdf "${exams_dir}/final/"

echo
echo "Compiled exams are in directory 'final'"
