#!/bin/sh

sources="main.c io.c lexer.c token.c parser.c cgen.c typechecker.c"

set -xe

gcc -ggdb -o pcp $sources -Wall -Wextra -Wno-switch