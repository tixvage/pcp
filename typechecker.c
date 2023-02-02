#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"
#include "error.h"

Checker_Info info = {0};

void check_functions(Parsed_File *decls);
Fn_Decl *function_exist(char *name);
void check_function(Fn_Decl *fn);

void check_functions(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        Fn_Decl *possible_decl = function_exist(decls->fn_decls.data[i]->name.value);
        if (possible_decl) {
            error_msg(decls->fn_decls.data[i]->name.loc, ERROR_FATAL, "function `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        check_function(decls->fn_decls.data[i]);
        array_push(info.funcs, decls->fn_decls.data[i]);
    }
}

Fn_Decl *function_exist(char *name) {
    for (int i = 0; i < info.funcs.len; i++) {
        if (strcmp(info.funcs.data[i]->name.value, name) == 0) {
            return info.funcs.data[i];
        }
    }
    return NULL;
}

void check_function(Fn_Decl *fn) {
    (void) fn;
}

void typechecker_check(Parsed_File *decls) {
    check_functions(decls);
}
