#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "error.h"

Checker_Info info = {0};

const Type builtin_types[] = {
    {"i8", TYPE_NUMBER},
    {"i16", TYPE_NUMBER},
    {"i32", TYPE_NUMBER},
    {"i64", TYPE_NUMBER},
    {"u8", TYPE_NUMBER},
    {"u16", TYPE_NUMBER},
    {"u32", TYPE_NUMBER},
    {"u64", TYPE_NUMBER},
    {"cstr", TYPE_STRING},
    {"bool", TYPE_BOOLEAN},
    {"auto", TYPE_NEEDS_INFERRING},
};

void check_struct_decls(Parsed_File *decls) {
    for (int i = 0; i < decls->struct_decls.len; i++) {
        Struct_Decl *possible_decl = struct_exist(decls->struct_decls.data[i]->name.value);
        if (possible_decl) {
            error_msg(decls->struct_decls.data[i]->name.loc, ERROR_FATAL, "function `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        array_push(info.structs, decls->struct_decls.data[i]);
        Type type = {decls->struct_decls.data[i]->name.value, TYPE_STRUCT};
        array_push(info.types, type);
    }
}

void check_struct_vars(void) {
    for (int i = 0; i < info.structs.len; i++) {
        check_struct(info.structs.data[i]);
    }
}

void check_struct(Struct_Decl *sc) {
    //TODO: check types of variables and variable names
    (void) sc;
}

void check_function_decls(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        Fn_Decl *possible_decl = function_exist(decls->fn_decls.data[i]->name.value);
        if (possible_decl) {
            error_msg(decls->fn_decls.data[i]->name.loc, ERROR_FATAL, "function `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        Type possible_type = type_exist(decls->fn_decls.data[i]->return_type);
        if (!possible_type.str) {
            error_msg(decls->fn_decls.data[i]->name.loc, ERROR_FATAL, "function `%s` has invalid return type", decls->fn_decls.data[i]->name.value);
            exit(1);
        }
        array_push(info.funcs, decls->fn_decls.data[i]);
    }
}

void check_function_statements(void) {
    for (int i = 0; i < info.funcs.len; i++) {
        if (!info.funcs.data[i]->eextern) check_function(info.funcs.data[i]);
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

Struct_Decl *struct_exist(char *name) {
    for (int i = 0; i < info.structs.len; i++) {
        if (strcmp(info.structs.data[i]->name.value, name) == 0) {
            return info.structs.data[i];
        }
    }
    return NULL;
}

void check_function(Fn_Decl *fn) {
    Var_Array vars = {0};
    for (int i = 0; i < fn->args.len; i++) {
        array_push(vars, check_var(fn->args.data[i]));
    }

    check_scope(vars, fn->body, fn, 0);
}

void check_scope(Var_Array vars_copy, Scope scope, Fn_Decl *fn, int deep) {
    Var_Array vars = {0};
    array_copy(vars, vars_copy);

    bool return_found = false;

    for (int i = 0; i < scope.len; i++) {
        Stmt stmt = scope.data[i];
        switch (stmt.kind) {
            case STMT_IF_STMT: {
                Type type = check_expr(vars, stmt.as.if_stmt->expr, type_exist("bool"));
                if (strcmp(type.str, "bool") != 0) {
                    error_msg(stmt.as.if_stmt->expr->loc, ERROR_FATAL, "expected type `bool` for if statement but got `%s`", type.str);
                    exit(1);
                }
                check_scope(vars, stmt.as.if_stmt->body, fn, deep + 1);
            } break;
            case STMT_FOR_STMT: {
                Checked_Var possible_var = var_exist(vars, stmt.as.for_stmt->var.name.value);
                if (possible_var.type.str) {
                    error_msg(stmt.as.for_stmt->var.name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
                    error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
                    exit(1);
                }
                Type lhs = check_expr(vars, stmt.as.for_stmt->range.start, type_exist("i32"));
                Type rhs = check_expr(vars, stmt.as.for_stmt->range.end, type_exist("i32"));

                if ((lhs.flags & TYPE_NUMBER) == 0 || (rhs.flags & TYPE_NUMBER) == 0) {
                    error_msg(stmt.as.for_stmt->range.start->loc, ERROR_FATAL, "range type must be `i32`");
                    exit(1);
                }

                Var_Array for_vars = {0};
                array_copy(for_vars, vars);
                array_push(for_vars, check_var(stmt.as.for_stmt->var));
                check_scope(for_vars, stmt.as.for_stmt->body, fn, deep + 1);
            } break;
            case STMT_RETURN_STMT: {
                Type type = check_expr(vars, stmt.as.return_stmt->expr, type_exist(fn->return_type));
                if (type.str == NULL || strcmp(type.str, fn->return_type) != 0) {
                    error_msg(stmt.as.return_stmt->expr->loc, ERROR_FATAL, "expected type `%s` for the return type of `%s` function but got `%s`", fn->return_type, fn->name.value, type.str);
                    error_msg(fn->name.loc, ERROR_NOTE, "`%s` defined here", fn->name.value);
                    exit(1);
                }
                return_found = true;
            } break;
            case STMT_VAR_DECL: {
                Checked_Var possible_var = var_exist(vars, stmt.as.var_decl->name.value);
                if (possible_var.type.str) {
                    error_msg(stmt.as.var_decl->name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
                    error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
                    exit(1);
                }

                if (!stmt.as.var_decl->zero_init) {
                    Type possible_var_type = type_exist(stmt.as.var_decl->type);
                    if (!possible_var_type.str) {
                        error_msg(stmt.as.var_decl->name.loc, ERROR_FATAL, "variable `%s` has invalid type", stmt.as.var_decl->name.value);
                        exit(1);
                    }
                    Type type = check_expr(vars, stmt.as.var_decl->value, possible_var_type);
                    if (strcmp(stmt.as.var_decl->type, "auto") == 0) {
                        if (strcmp(type.str, "auto") == 0) {
                            error_msg(stmt.as.var_decl->name.loc, ERROR_FATAL, "type of `%s` could not be inferred", stmt.as.var_decl->name.value);
                            exit(1);
                        }
                        stmt.as.var_decl->type = type.str;
                    }
                    if (strcmp(stmt.as.var_decl->type, "auto") != 0 && strcmp(type.str, stmt.as.var_decl->type) != 0) {
                        error_msg(stmt.as.var_decl->value->loc, ERROR_FATAL, "expected type `%s` but got `%s`", stmt.as.var_decl->type, type.str);
                        exit(1);
                    }
                }
                Var var = (Var){stmt.as.var_decl->type, stmt.as.var_decl->name};
                array_push(vars, check_var(var));
            } break;
            case STMT_VAR_ASSIGN: {
                assert(0 && "todo");
            } break;
            case STMT_EXPR: {
                check_expr(vars, stmt.as.expr, type_exist("auto"));
            } break;
            case STMT_EMPTY:  {
            } break;
            default: {
                assert(0 && "unreacheable");
            } break;
        }
    }

    if (deep == 0 && !return_found && strcmp(fn->return_type, "void") != 0) {
        error_msg(fn->name.loc, ERROR_FATAL, "`%s` reaches end of non-void function", fn->name.value);
        exit(1);
    }
}

Checked_Var var_exist(Var_Array vars, char *name) {
    for (int i = 0; i < vars.len; i++) {
        if (strcmp(vars.data[i].name.value, name) == 0) {
            return vars.data[i];
        }
    }
    return (Checked_Var){0};
}

Type check_expr(Var_Array vars, Expr *expr, Type wanted_type) {
    switch (expr->kind) {
        case EXPR_NUMBER: {
            /*if ((wanted_type.flags & TYPE_NUMBER) != 0) {
                return wanted_type;
            }*/
            return type_exist("i32");
        } break;
        case EXPR_STRING: {
            return type_exist("cstr");
        } break;
        case EXPR_IDENTIFIER: {
            Checked_Var possible_var = var_exist(vars, expr->as.identifier->name);
            if (possible_var.type.str) {
                return possible_var.type;
            } else {
                error_msg(expr->loc, ERROR_FATAL, "variable `%s` could not found in scope", expr->as.identifier->name);
                exit(1);
            }
        } break;
        case EXPR_BIN_OP: {
            Type lhs = check_expr(vars, expr->as.bin_op->left, wanted_type);
            Type rhs = check_expr(vars, expr->as.bin_op->right, wanted_type);

            if (strcmp(lhs.str, rhs.str) != 0) {
                error_msg(expr->loc, ERROR_FATAL, "expected type `%s` but got `%s`", lhs.str, rhs.str);
                exit(1);
            }
            if (expr->as.bin_op->op.type == TOKEN_EQUAL_EQUAL) {
                return type_exist("bool");
            }
            return lhs;
        } break;
        case EXPR_UN_OP: {
            return check_expr(vars, expr->as.un_op->expr, wanted_type);
        } break;
        case EXPR_FUNC_CALL: {
            Fn_Decl *possible_fn = function_exist(expr->as.func_call->name);
            if (!possible_fn) {
                error_msg(expr->loc, ERROR_FATAL, "function `%s` could not found", expr->as.func_call->name);
                exit(1);
            }
            if (possible_fn->has_va_arg) return type_exist(possible_fn->return_type);
            if (possible_fn->args.len != expr->as.func_call->args.len) {
                error_msg(expr->loc, ERROR_FATAL, "function `%s` accepts %d arguments but %d given", expr->as.func_call->name, possible_fn->args.len, expr->as.func_call->args.len);
                error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                exit(1);
            }
            for (int i = 0; i < possible_fn->args.len; i++) {
                Type expected_type = check_var(possible_fn->args.data[i]).type;
                Type given_type = check_expr(vars, expr->as.func_call->args.data[i], expected_type);
                if (strcmp(expected_type.str, given_type.str) != 0) {
                    error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                    error_msg(expr->as.func_call->args.data[i]->loc, ERROR_FATAL, "expected type `%s` but got `%s`", expected_type.str, given_type.str);
                    error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                    exit(1);
                }
            }
            return type_exist(possible_fn->return_type);
        } break;
        case EXPR_CAST: {
            //TODO: check if cast is possible
            check_expr(vars, expr->as.cast->expr, type_exist("auto"));
            return type_exist(expr->as.cast->type.value);
        } break;
        case EXPR_STRUCT_CONSTRUCT: {
            //TODO: check if construction is like Person{name = "sussy", age = 69};
            Struct_Decl *sd = struct_exist(wanted_type.str);
            Struct_Construct *sc = expr->as.struct_construct;

            if (!sd && (wanted_type.flags & TYPE_NEEDS_INFERRING) == 0) {
                error_msg(expr->loc, ERROR_FATAL, "struct `%s` could not found", wanted_type.str);
                exit(1);
            }

            if (sd->vars.len != sc->args.len) {
                error_msg(expr->loc, ERROR_FATAL, "struct `%s` has %d fields but %d given", sd->name.value, sd->vars.len, sc->args.len);
                error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                exit(1);
            }

            for (int i = 0; i < sc->args.len; i++) {
                bool found = false;
                for (int j = 0; j < sd->vars.len; j++) {
                    if (strcmp(sd->vars.data[j]->name.value, sc->args.data[i].name.value) == 0) {
                        Type expected_type = type_exist(sd->vars.data[j]->type);
                        Type given_type = check_expr(vars, sc->args.data[i].expr, expected_type);
                        if (strcmp(expected_type.str, given_type.str) != 0) {
                            error_msg(expr->loc, ERROR_FATAL, "fields of `%s` struct are incorrect", sd->name.value);
                            error_msg(sc->args.data[i].name.loc, ERROR_FATAL, "expected type `%s` but got `%s`", expected_type.str, given_type.str);
                            error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                            exit(1);
                        }
                        found = true;
                    }
                }
                if (!found) {
                    error_msg(sc->args.data[i].name.loc, ERROR_FATAL, "struct `%s` does not have a field named `%s`", sd->name.value, sc->args.data[i].name.value);
                    error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                    exit(1);
                }
            }

            return wanted_type;
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
    assert(0 && "unreacheable");
}

Type type_exist(char *str) {
    for (int i = 0; i < info.types.len; i++) {
        if (str && strcmp(info.types.data[i].str, str) == 0) {
            return info.types.data[i];
        }
    }

    return (Type){0};
}

Checked_Var check_var(Var var) {
    Checked_Var res = {0};
    res.name = var.name;
    Type possible_type = type_exist(var.type);
    if (!possible_type.str) {
        error_msg(var.name.loc, ERROR_FATAL, "type `%s` is not declared", var.type);
        exit(1);
    }
    res.type = possible_type;
    return res;
}

void typechecker_check(Parsed_File *decls) {
    array_append(info.types, builtin_types, sizeof(builtin_types)/sizeof(builtin_types[0]));
    check_struct_decls(decls);
    check_struct_vars();
    check_function_decls(decls);
    check_function_statements();
}
