#!/bin/sh

sources="main.c io.c lexer.c token.c parser.c cgen.c typechecker.c error.c"

set -xe

gcc -O0 -g -o pcp $sources -Wall -Wextra -Wno-switch -Wno-sign-compare
