#ifndef _TYPECHECKER_H
#define _TYPECHECKER_H

#include "parser.h"

typedef struct Checker_Info {
    struct {
        char **data;
        int len;
    } funcs;
} Checker_Info;

void typechecker_check(Parsed_File *decls);

#endif
