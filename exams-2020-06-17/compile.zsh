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
echo "Generate SAT problems..."
( cd "${root_dir}/generator" &&
    stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" sat )

# Generate SMT problems
echo "\nGenerate SMT problems..."
( cd "${root_dir}/generator" &&
    stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" smt -t "${root_dir}/problems/smt/template1.smt2" )

# Generate ground superposition problems
echo "\nGenerate ground superposition problems..."
python3 "${root_dir}/problems/sup/ground_sup.py" -n "$num_exams" -o "$exams_dir"

# Generate redundancy problems
echo "\nGenerate non-ground superposition / redundancy problems..."
( cd "${root_dir}/generator" &&
    stack run -- -n "$num_exams" -s "$seed" -o "$exams_dir" red )

for exam in "$exams_dir"/exam-*; do
    echo "\nCompiling $exam"
    examtex="${exam:t}.tex"

    # Produce latex version of smt files
    "$SMT2TEX" --conjunction < "${exam}/smt.smt2" > "${exam}/smt.smt2.tex"
    "$SMT2TEX" < "${exam}/sup.smt2" > "${exam}/sup.smt2.tex"

    # Link the template from parent directory
    ln -sf "../template.tex" "${exam}/${examtex}"

    # Compile exam
    ( cd "$exam" && latexmk -quiet -pdf "$examtex" )
done

mkdir -p "${exams_dir}/final"
cp "$exams_dir"/exam-*/exam-*.pdf "${exams_dir}/final/"

echo
echo "Compiled exams are in directory 'final'"
