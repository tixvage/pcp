#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

Checker_Info info = {0};

void check_functions(Parsed_File *decls);
bool function_exist(char *name);
void check_function(Fn_Decl *fn);

void check_functions(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        if (function_exist(decls->fn_decls.data[i]->name)) {
            fprintf(stderr, "Function `%s` already exists\n", decls->fn_decls.data[i]->name);
            exit(1);
        }
        check_function(decls->fn_decls.data[i]);
        array_push(info.funcs, decls->fn_decls.data[i]->name);
    }
}

bool function_exist(char *name) {
    for (int i = 0; i < info.funcs.len; i++) {
        if (strcmp(info.funcs.data[i], name) == 0) {
            return true;
        }
    }
    return false;
}

void check_function(Fn_Decl *fn) {
    (void) fn;
}

void typechecker_check(Parsed_File *decls) {
    check_functions(decls);
}
