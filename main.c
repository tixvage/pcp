#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "io.h"
#include "lexer.h"
#include "parser.h"
#include "typechecker.h"
#include "cgen.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <filename>\n", argv[0]);
        exit(1);
    }
    const char *filename = argv[1];

    char* content = read_entire_file(filename);

    Lexer lexer = {0};
    init_lexer(&lexer, content, filename);

    Parser parser = {0};
    init_parser(&parser, lexer);

    parser_parse(&parser);
    Checked_File cf =  typechecker_check(&parser.res);
    cgen_generate(&cf, "tests/out.c");
}
