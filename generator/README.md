# exagen

## How to compile and run

1. Install the Haskell build tool `stack`, either from system packages or from https://haskellstack.org

2. Run `stack run` in this directory (this also downloads the compiler and all dependencies)


## Usage

- Specify number of exams with `-n 30`.
- Specify RNG seed with `-s 123`.
- Specify output directory with `-o /path/to/exams`.


## Generate the SAT problems

Subcommand `sat`.

    $ stack run -- -s 123 -n 40 -o ../exams-2020-06-17 sat


## Generate the SMT problems

Subcommand `smt`.
Specify template with `-t /path/to/template.smt2`.

    $ stack run -- -s 123 -n 40 -o ../exams-2020-06-17 smt -t ../problems/smt/template1.smt2
