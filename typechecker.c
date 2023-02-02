#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "utils.h"
#include "error.h"

Checker_Info info = {0};

typedef struct Var_Array Var_Array;

void check_function_decls(Parsed_File *decls);
void check_function_statements(Parsed_File *decls);
Fn_Decl *function_exist(char *name);
void check_function(Fn_Decl *fn);
void check_scope(Var_Array vars_copy, Scope scope, Fn_Decl *fn);
Var var_exist(Var_Array vars, char *name);
char *check_expr(Var_Array vars, Expr *expr);

void check_function_decls(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        Fn_Decl *possible_decl = function_exist(decls->fn_decls.data[i]->name.value);
        if (possible_decl) {
            error_msg(decls->fn_decls.data[i]->name.loc, ERROR_FATAL, "function `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        array_push(info.funcs, decls->fn_decls.data[i]);
    }
}

void check_function_statements(Parsed_File *decls) {
    for (int i = 0; i < info.funcs.len; i++) {
        check_function(info.funcs.data[i]);
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

struct Var_Array {
    Var *data;
    int len;
};

void check_function(Fn_Decl *fn) {
    Var_Array vars = {0};
    for (int i = 0; i < fn->args.len; i++) {
        array_push(vars, fn->args.data[i]);
    }

    check_scope(vars, fn->body, fn);
}

void check_scope(Var_Array vars_copy, Scope scope, Fn_Decl *fn) {
    Var_Array vars = {0};
    array_copy(vars, vars_copy);

    for (int i = 0; i < scope.len; i++) {
        Stmt stmt = scope.data[i];
        switch (stmt.kind) {
            case STMT_IF_STMT: {
                char *type = check_expr(vars, stmt.as.if_stmt->expr);
                if (strcmp(type, "bool") != 0) {
                    error_msg(stmt.as.if_stmt->expr->loc, ERROR_FATAL, "expected type `bool` for if statement but got `%s`", type);
                    exit(1);
                }
                check_scope(vars, stmt.as.if_stmt->body, fn);
            } break;
            case STMT_RETURN_STMT: {
                char *type = check_expr(vars, stmt.as.return_stmt->expr);
                if (type == NULL || strcmp(type, fn->return_type) != 0) {
                    error_msg(stmt.as.return_stmt->expr->loc, ERROR_FATAL, "expected type `%s` for the return type of `%s` function but got `%s`", fn->return_type, fn->name.value, type);
                    error_msg(fn->name.loc, ERROR_NOTE, "`%s` defined here", fn->name.value);
                    exit(1);
                }
            } break;
            case STMT_VAR_DECL: {
                Var possible_var = var_exist(vars, stmt.as.var_decl->name.value);
                if (possible_var.type) {
                    error_msg(stmt.as.var_decl->name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
                    error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
                    exit(1);
                }

                char *type = check_expr(vars, stmt.as.var_decl->value);
                if (strcmp(type, stmt.as.var_decl->type) != 0) {
                    error_msg(stmt.as.var_decl->value->loc, ERROR_FATAL, "expected type `%s` but got `%s`", stmt.as.var_decl->type, type);
                    exit(1);
                }
                Var var = (Var){stmt.as.var_decl->type, stmt.as.var_decl->name};
                array_push(vars, var);
            } break;
            case STMT_VAR_ASSIGN: {
                assert(0 && "todo");
            } break;
            case STMT_EXPR: {
                check_expr(vars, stmt.as.expr);
            } break;
            case STMT_EMPTY:  {
            } break;
            default: {
                assert(0 && "unreacheable");
            } break;
        }
    }
}

Var var_exist(Var_Array vars, char *name) {
    for (int i = 0; i < vars.len; i++) {
        if (strcmp(vars.data[i].name.value, name) == 0) {
            return vars.data[i];
        }
    }
    return (Var){0};
}

char *check_expr(Var_Array vars, Expr *expr) {
    switch (expr->kind) {
        case EXPR_NUMBER: {
            return "i32";
        } break;
        case EXPR_IDENTIFIER: {
            Var possible_var = var_exist(vars, expr->as.identifier->name);
            if (possible_var.type) {
                return possible_var.type;
            } else {
                error_msg(expr->loc, ERROR_FATAL, "variable `%s` could not found in scope", expr->as.identifier->name);
                exit(1);
            }
        } break;
        case EXPR_BIN_OP: {
            char *lhs = check_expr(vars, expr->as.bin_op->left);
            char *rhs = check_expr(vars, expr->as.bin_op->right);

            if (strcmp(lhs, rhs) != 0) {
                error_msg(expr->loc, ERROR_FATAL, "expected type `%s` but got `%s`", lhs, rhs);
                exit(1);
            }
            if (expr->as.bin_op->op.type == TOKEN_EQUAL_EQUAL) {
                return "bool";
            }
            return lhs;
        } break;
        case EXPR_UN_OP: {
            return check_expr(vars, expr->as.un_op->expr);
        } break;
        case EXPR_FUNC_CALL: {
            Fn_Decl *possible_fn = function_exist(expr->as.func_call->name);
            if (!possible_fn) {
                error_msg(expr->loc, ERROR_FATAL, "function `%s` could not found", expr->as.func_call->name);
                exit(1);
            }
            if (possible_fn->args.len != expr->as.func_call->args.len) {
                error_msg(expr->loc, ERROR_FATAL, "function `%s` accepts %d arguments but %d given", expr->as.func_call->name, possible_fn->args.len, expr->as.func_call->args.len);
                error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                exit(1);
            }
            for (int i = 0; i < possible_fn->args.len; i++) {
                char *expected_type = possible_fn->args.data[i].type;
                char *given_type = check_expr(vars, expr->as.func_call->args.data[i]);
                if (strcmp(expected_type, given_type) != 0) {
                    error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                    error_msg(expr->as.func_call->args.data[i]->loc, ERROR_FATAL, "expected type `%s` but got `%s`", expected_type, given_type);
                    error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                    exit(1);
                }
            }
            return possible_fn->return_type;
        } break;
        case EXPR_CAST: {
            //TODO: check if cast is possible
            check_expr(vars, expr->as.cast->expr);
            return expr->as.cast->type.value;
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
    return NULL;
}

void typechecker_check(Parsed_File *decls) {
    check_function_decls(decls);
    check_function_statements(decls);
}
