#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "error.h"

Checked_File info = {0};

const Type builtin_types[] = {
    {"void", TYPE_VOID},
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
    {"va_arg", TYPE_NEEDS_INFERRING},
};

void check_structs(Parsed_File *decls) {
    for (int i = 0; i < decls->struct_decls.len; i++) {
        Struct_Decl *decl = decls->struct_decls.data[i];
        Checked_Struct_Decl *possible_decl = struct_exist(decl->name.value);
        if (possible_decl) {
            error_msg(decl->name.loc, ERROR_FATAL, "struct `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        possible_decl = calloc(1, sizeof(Checked_Fn_Decl));
        Var_Array vars = {0};
        for (int j = 0; j < decl->vars.len; j++) {
            Checked_Var_Decl *var_decl = check_var_decl(decl->vars.data[i], &vars, NULL, 0);
            array_push(possible_decl->vars, var_decl);
        }
        Type type = {decl->name.value, TYPE_STRUCT};
        array_push(info.types, type);
    }
}

void check_functions(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        Checked_Fn_Decl *possible_decl = function_exist(decls->fn_decls.data[i]->name.value);
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
        possible_decl = malloc(sizeof(Checked_Fn_Decl));
        possible_decl->name = decls->fn_decls.data[i]->name;
        possible_decl->return_type = possible_type;
        possible_decl->eextern = decls->fn_decls.data[i]->eextern;
        possible_decl->has_va_arg = decls->fn_decls.data[i]->has_va_arg;
        possible_decl->body.data = NULL;
        possible_decl->body.len = 0;
        possible_decl->args.data = NULL;
        possible_decl->args.len = 0;

        Var_Array vars = {0};
        for (int j = 0; j < decls->fn_decls.data[i]->args.len; j++) {
            Var var = decls->fn_decls.data[i]->args.data[j];
            array_push(vars, check_var(var));
        }
        if (!possible_decl->eextern) {
            possible_decl->body = check_scope(vars, decls->fn_decls.data[i]->body, possible_decl, 0);
        }
        possible_decl->args = vars;
        array_push(info.funcs, possible_decl);
    }
}

Checked_Fn_Decl *function_exist(char *name) {
    for (int i = 0; i < info.funcs.len; i++) {
        if (strcmp(info.funcs.data[i]->name.value, name) == 0) {
            return info.funcs.data[i];
        }
    }
    return NULL;
}

Checked_Struct_Decl *struct_exist(char *name) {
    for (int i = 0; i < info.structs.len; i++) {
        if (strcmp(info.structs.data[i]->name.value, name) == 0) {
            return info.structs.data[i];
        }
    }
    return NULL;
}

Checked_Scope check_scope(Var_Array vars_copy, Scope scope, Checked_Fn_Decl *fn, int deep) {
    Checked_Scope res = {0};
    Var_Array vars = {0};
    array_copy(vars, vars_copy);

    bool return_found = false;

    for (int i = 0; i < scope.len; i++) {
        Checked_Stmt checked_stmt = {0};
        Stmt stmt = scope.data[i];
        switch (stmt.kind) {
            case STMT_IF_STMT: {
                Checked_If_Stmt *if_stmt = check_if_stmt(stmt.as.if_stmt, vars, fn, deep + 1);
                checked_stmt.kind = CHECKED_STMT_IF_STMT;
                checked_stmt.as.if_stmt = if_stmt;
            } break;
            case STMT_FOR_STMT: {
                Checked_For_Stmt *for_stmt = check_for_stmt(stmt.as.for_stmt, vars, fn, deep + 1);
                checked_stmt.kind = CHECKED_STMT_FOR_STMT;
                checked_stmt.as.for_stmt = for_stmt;
            } break;
            case STMT_RETURN_STMT: {
                Checked_Return_Stmt *return_stmt = check_return_stmt(stmt.as.return_stmt, vars, fn, deep);
                checked_stmt.kind = CHECKED_STMT_RETURN_STMT;
                checked_stmt.as.return_stmt = return_stmt;

                return_found = true;
            } break;
            case STMT_VAR_DECL: {
                Checked_Var_Decl *var_decl = check_var_decl(stmt.as.var_decl, &vars, fn, deep);
                checked_stmt.kind = CHECKED_STMT_VAR_DECL;
                checked_stmt.as.var_decl = var_decl;
            } break;
            case STMT_VAR_ASSIGN: {
                Checked_Var_Assign *var_assign = check_var_assign(stmt.as.var_assign, vars, fn, deep);
                checked_stmt.kind = CHECKED_STMT_VAR_ASSIGN;
                checked_stmt.as.var_assign = var_assign;
            } break;
            case STMT_EXPR: {
                Checked_Expr *expr = check_expr(stmt.as.expr, vars, fn, deep, type_exist("auto"));
                checked_stmt.kind = CHECKED_STMT_EXPR;
                checked_stmt.as.expr = expr;
            } break;
            case STMT_EMPTY:  {
            } break;
            default: {
                assert(0 && "unreacheable");
            } break;
        }
        array_push(res, checked_stmt);
    }

    if (deep == 0 && !return_found && strcmp(fn->return_type.str, "void") != 0) {
        error_msg(fn->name.loc, ERROR_FATAL, "`%s` reaches end of non-void function", fn->name.value);
        exit(1);
    }

    return res;
}

Checked_If_Stmt *check_if_stmt(If_Stmt *if_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_If_Stmt *res = calloc(1, sizeof(Checked_If_Stmt));

    res->expr = check_expr(if_stmt->expr, vars_copy, fn, deep, type_exist("bool"));
    if ((res->expr->type.flags & TYPE_BOOLEAN) == 0) {
        error_msg(if_stmt->expr->loc, ERROR_FATAL, "expected type `bool` for if statement but got `%s`", res->expr->type.str);
        exit(1);
    }

    res->body = check_scope(vars_copy, if_stmt->body, fn, deep);

    return res;
}

Checked_For_Stmt *check_for_stmt(For_Stmt *for_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_For_Stmt *res = calloc(1, sizeof(Checked_For_Stmt));

    Checked_Var possible_var = var_exist(vars_copy, for_stmt->var.name.value);
    if (possible_var.type.str) {
        error_msg(for_stmt->var.name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
        error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
        exit(1);
    }
    res->var = possible_var;

    res->range.start = check_expr(for_stmt->range.start, vars_copy, fn, deep, type_exist("i32"));
    res->range.end = check_expr(for_stmt->range.end, vars_copy, fn, deep, type_exist("i32"));
    Type lhs = res->range.start->type;
    Type rhs = res->range.end->type;
    if ((lhs.flags & TYPE_NUMBER) == 0 || (rhs.flags & TYPE_NUMBER) == 0) {
        error_msg(for_stmt->range.start->loc, ERROR_FATAL, "range type must be `i32`");
        exit(1);
    }

    Var_Array for_vars = {0};
    array_copy(for_vars, vars_copy);
    array_push(for_vars, possible_var);
    res->body = check_scope(for_vars, for_stmt->body, fn, deep);

    return res;
}

Checked_Return_Stmt *check_return_stmt(Return_Stmt *return_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_Return_Stmt *res = calloc(1, sizeof(Checked_Return_Stmt));

    res->expr = check_expr(return_stmt->expr, vars_copy, fn, deep, fn->return_type);
    if (!type_eq(res->expr->type, fn->return_type)) {
        error_msg(return_stmt->expr->loc, ERROR_FATAL, "expected type `%s` for the return type of `%s` function but got `%s`", fn->return_type.str, fn->name.value, res->expr->type.str);
        error_msg(fn->name.loc, ERROR_NOTE, "`%s` defined here", fn->name.value);
        exit(1);
    }

    return res;
}

Checked_Var_Decl *check_var_decl(Var_Decl *var_decl, Var_Array *vars, Checked_Fn_Decl *fn, int deep) {
    Checked_Var_Decl *res = calloc(1, sizeof(Checked_Var_Decl));

    Checked_Var possible_var = var_exist(*vars, var_decl->name.value);
    if (possible_var.type.str) {
        error_msg(var_decl->name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
        error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
        exit(1);
    }
    res->name = var_decl->name;

    Type possible_var_type = type_exist(var_decl->type);
    if (!possible_var_type.str) {
        error_msg(var_decl->name.loc, ERROR_FATAL, "variable `%s` has invalid type", var_decl->name.value);
        exit(1);
    }
    if (var_decl->zero_init) {
        res->value = NULL;
        res->zero_init = true;
    } else {
        res->value = check_expr(var_decl->value, *vars, fn, deep, possible_var_type);
        if (type_eq(possible_var_type, type_exist("auto"))) {
            if (type_eq(res->value->type, type_exist("auto"))) {
                error_msg(var_decl->name.loc, ERROR_FATAL, "type of `%s` could not be inferred", var_decl->name.value);
                exit(1);
            }
            possible_var_type = res->value->type;
        }
        if (!type_eq(possible_var_type, res->value->type)) {
            error_msg(var_decl->value->loc, ERROR_FATAL, "expected type `%s` but got `%s`", var_decl->type, res->value->type.str);
            exit(1);
        }
    }

    Checked_Var var = {var_decl->name, res->value->type};
    array_push(*vars, var);

    return res;
}

Checked_Var_Assign *check_var_assign(Var_Assign *var_assign, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_Var_Assign *res = calloc(1, sizeof(Checked_Var_Assign));

    Checked_Var possible_var = var_exist(vars_copy, var_assign->var->name);
    if (!possible_var.type.str) {
        error_msg(var_assign->loc, ERROR_FATAL, "variable `%s` is undeclared", var_assign->var->name);
        exit(1);
    }

    res->var = var_assign->var;
    Checked_Expr *expr = check_expr(var_assign->expr, vars_copy, fn, deep, possible_var.type);
    if (!type_eq(expr->type, possible_var.type)) {
        error_msg(var_assign->expr->loc, ERROR_FATAL, "expected type `%s` but got `%s`", possible_var.type.str, expr->type.str);
        exit(1);
    }
    res->expr = expr;

    return res;
}

Checked_Expr *check_expr(Expr *expr, Var_Array vars, Checked_Fn_Decl *fn, int deep, Type wanted_type) {
    Checked_Expr *res = calloc(1, sizeof(Checked_Expr));

    switch (expr->kind) {
        case EXPR_NUMBER: {
            Type type = type_exist("i32");
            if ((wanted_type.flags & TYPE_NUMBER) != 0) {
                type = wanted_type;
            }
            res->type = type;
            res->kind = CHECKED_EXPR_NUMBER;
            res->as.number = expr->as.number;
        } break;
        case EXPR_STRING: {
            res->type = type_exist("cstr");
            res->kind = CHECKED_EXPR_STRING;
            res->as.string = expr->as.string;
        } break;
        case EXPR_IDENTIFIER: {
            Type type = var_exist(vars, expr->as.identifier->name).type;
            if (!type.str) {
                error_msg(expr->loc, ERROR_FATAL, "variable `%s` could not found in scope", expr->as.identifier->name);
                exit(1);
            }
            res->type = type;
            res->kind = CHECKED_EXPR_IDENTIFIER;
            res->as.identifier = expr->as.identifier;
        } break;
        case EXPR_BIN_OP: {
            Checked_Expr *left = check_expr(expr->as.bin_op->left, vars, fn, deep, wanted_type);
            Checked_Expr *right = check_expr(expr->as.bin_op->right, vars, fn, deep, wanted_type);
            Type type = left->type;

            if (!type_eq(left->type, right->type)) {
                error_msg(expr->loc, ERROR_FATAL, "expected type `%s` but got `%s`", left->type.str, right->type.str);
                exit(1);
            }
            if (expr->as.bin_op->op.type == TOKEN_EQUAL_EQUAL) {
                type = type_exist("bool");
            }
            res->type = type;
            res->kind = CHECKED_EXPR_BIN_OP;
            Checked_Bin_Op *bin_op = malloc(sizeof(Checked_Bin_Op));
            bin_op->left = left;
            bin_op->right = right;
            bin_op->op = expr->as.bin_op->op;
            res->as.bin_op = bin_op;
        } break;
        case EXPR_UN_OP: {
            Checked_Expr *un_op_expr = check_expr(expr->as.un_op->expr, vars, fn, deep, wanted_type);
            res->type = un_op_expr->type;
            res->kind = CHECKED_EXPR_UN_OP;
            Checked_Un_Op *un_op = malloc(sizeof(Checked_Un_Op));
            un_op->expr = un_op_expr;
            un_op->op = expr->as.un_op->op;
            res->as.un_op = un_op;
        } break;
        case EXPR_FUNC_CALL: {
            Checked_Fn_Decl *possible_fn = function_exist(expr->as.func_call->name);

            Checked_Func_Call *func_call = calloc(1, sizeof(Checked_Func_Call));
            func_call->name = expr->as.func_call->name;

            if (!possible_fn) {
                error_msg(expr->loc, ERROR_FATAL, "function `%s` could not found", expr->as.func_call->name);
                exit(1);
            }
            if (possible_fn->has_va_arg) {
                for (int i = 0; i < expr->as.func_call->args.len; i++) {
                    Checked_Expr *given_expr = check_expr(expr->as.func_call->args.data[i], vars, fn, deep, type_exist("auto"));
                    array_push(func_call->args, given_expr);
                }
            } else {
                if (possible_fn->args.len != expr->as.func_call->args.len) {
                    error_msg(expr->loc, ERROR_FATAL, "function `%s` accepts %d arguments but %d given", expr->as.func_call->name, possible_fn->args.len, expr->as.func_call->args.len);
                    error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                    exit(1);
                }
                for (int i = 0; i < possible_fn->args.len; i++) {
                    Type expected_type = possible_fn->args.data[i].type;
                    Checked_Expr *given_expr = check_expr(expr->as.func_call->args.data[i], vars, fn, deep, expected_type);
                    if (!type_eq(expected_type, given_expr->type)) {
                        error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                        error_msg(expr->as.func_call->args.data[i]->loc, ERROR_FATAL, "expected type `%s` but got `%s`", expected_type.str, given_expr->type.str);
                        error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                        exit(1);
                    }
                    array_push(func_call->args, given_expr);
                }
            }
            res->type = possible_fn->return_type;
            res->kind = CHECKED_EXPR_FUNC_CALL;
            res->as.func_call = func_call;
        } break;
        case EXPR_CAST: {
            Type type = type_exist(expr->as.cast->type.value);
            if (!type.str) {
                error_msg(expr->loc, ERROR_FATAL, "use of undeclared `%s` type", expr->as.cast->type.value);
                exit(1);
            }
            Type convert = type_exist("auto");
            if (expr->as.cast->expr->kind == EXPR_STRUCT_CONSTRUCT) {
                convert = type;
            }
            Checked_Expr *cast_expr = check_expr(expr->as.cast->expr, vars, fn, deep, convert);
            Checked_Cast *cast = malloc(sizeof(Checked_Cast));
            cast->expr = cast_expr;
            cast->type = type;
            res->type = type;
            res->kind = CHECKED_EXPR_CAST;
            res->as.cast = cast;
        } break;
        /*case EXPR_STRUCT_CONSTRUCT: {
            //TODO: check if construction is like Person{name = "sussy", age = 69};
            Struct_Decl *sd = struct_exist(wanted_type.str);
            Struct_Construct *sc = expr->as.struct_construct;

            if (!sd) {
                if ((wanted_type.flags & TYPE_NEEDS_INFERRING) != 0) {
                    return wanted_type;
                } else {
                    error_msg(expr->loc, ERROR_FATAL, "struct `%s` could not found", wanted_type.str);
                    exit(1);
                }
            }

            int non_default_fields = sd->vars.len;
            for (int i = 0; i < sd->vars.len; i++) {
                if (sd->vars.data[i]->value) non_default_fields -= 1;
            }

            if (non_default_fields > sc->args.len) {
                error_msg(expr->loc, ERROR_FATAL, "struct `%s` needs at least %d fields but %d given", sd->name.value, non_default_fields, sc->args.len);
                error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                exit(1);
            } else if (sc->args.len > sd->vars.len) {
                error_msg(expr->loc, ERROR_FATAL, "struct `%s` has %d fields but %d given", sd->name.value, sd->vars.len, sc->args.len);
                error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                exit(1);
            }

            struct {
                Struct_Construct_Arg *data;
                int len;
            } default_args = {0};

            for (int i = 0; i < sd->vars.len; i++) {
                //if (sd->vars.data[i]->value) continue;
                bool found = false;
                for (int j = 0; j < sc->args.len; j++) {
                    if (strcmp(sd->vars.data[i]->name.value, sc->args.data[j].name.value) == 0) {
                        found = true;
                    }
                }
                if (!found) {
                    if (sd->vars.data[i]->value) {
                        Struct_Construct_Arg default_arg = {0};
                        default_arg.name = sd->vars.data[i]->name;
                        default_arg.expr = sd->vars.data[i]->value;
                        array_push(default_args, default_arg);
                    } else {
                        error_msg(expr->loc, ERROR_FATAL, "struct `%s` needs field named `%s`", sd->name.value, sd->vars.data[i]->name.value);
                        error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                        exit(1);
                    }
                }
            }

            array_append(sc->args, default_args.data, default_args.len);

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

            sc->type = wanted_type.str;
            return wanted_type;
        } break;*/
        default: {
            assert(0 && "unreacheable");
        } break;
    }

    return res;
}

Checked_Var var_exist(Var_Array vars, char *name) {
    for (int i = 0; i < vars.len; i++) {
        if (strcmp(vars.data[i].name.value, name) == 0) {
            return vars.data[i];
        }
    }
    return (Checked_Var){0};
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

bool type_eq(Type a, Type b) {
    return (strcmp(a.str, b.str) == 0) && a.flags == b.flags;
}

Checked_File typechecker_check(Parsed_File *decls) {
    array_append(info.types, builtin_types, sizeof(builtin_types)/sizeof(builtin_types[0]));
    check_structs(decls);
    check_functions(decls);
    return info;
}
