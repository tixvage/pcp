#include "typechecker.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "error.h"

Checked_File info = {0};

Type builtin_types[] = {
    {"void", TYPE_VOID, {NULL}},
    {"i8", TYPE_NUMBER, {NULL}},
    {"i16", TYPE_NUMBER, {NULL}},
    {"i32", TYPE_NUMBER, {NULL}},
    {"i64", TYPE_NUMBER, {NULL}},
    {"u8", TYPE_NUMBER, {NULL}},
    {"u16", TYPE_NUMBER, {NULL}},
    {"u32", TYPE_NUMBER, {NULL}},
    {"u64", TYPE_NUMBER, {NULL}},
    {"cstr", TYPE_STRING, {NULL}},
    {"bool", TYPE_BOOLEAN, {NULL}},
    {"auto", TYPE_NEEDS_INFERRING, {NULL}},
    {"va_arg", TYPE_NEEDS_INFERRING, {NULL}},
};

Var_Array global_vars = {0};

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
            Checked_Var_Decl *var_decl = check_var_decl(decl->vars.data[j], &vars, NULL, 0);
            array_push(possible_decl->vars, var_decl);
        }
        possible_decl->name = decl->name;
        possible_decl->eextern = decl->eextern;
        Type *type = calloc(1, sizeof(Type));
        type->str = decl->name.value;
        type->flag = TYPE_STRUCT;
        array_push(info.types, type);
        array_push(info.structs, possible_decl);
    }
}

void check_top_assignments(Parsed_File *decls) {
    for (int i = 0; i < decls->top_var_decls.len; i++) {
        Checked_Var_Decl *var = check_var_decl(decls->top_var_decls.data[i], &global_vars, NULL, 0);
        array_push(info.top_assignments, var);
    }
}

void check_functions(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        Fn_Decl *decl = decls->fn_decls.data[i];
        Checked_Fn_Decl *possible_decl = function_exist(decl->name.value);
        if (possible_decl) {
            error_msg(decl->name.loc, ERROR_FATAL, "function `%s` already defined", possible_decl->name.value);
            error_msg(possible_decl->name.loc, ERROR_NOTE, "`%s` first defined here", possible_decl->name.value);
            exit(1);
        }
        Type *possible_type = check_type(decl->return_type);
        if (!possible_type) {
            error_msg(decl->name.loc, ERROR_FATAL, "function `%s` has invalid return type", decl->name.value);
            exit(1);
        }
        possible_decl = calloc(1, sizeof(Checked_Fn_Decl));
        possible_decl->name = decl->name;
        possible_decl->return_type = possible_type;
        possible_decl->eextern = decl->eextern;
        possible_decl->has_va_arg = decl->has_va_arg;

        Var_Array vars = {0};
        for (int j = 0; j < decl->args.len; j++) {
            Checked_Var_Decl *var_decl = check_var_decl(decl->args.data[j], &vars, NULL, 0);
            array_push(possible_decl->args, var_decl);
        }

        if (possible_decl->args.len > 0) {
            bool found = possible_decl->args.data[0]->zero_init;
            bool first = found;
            bool old = found;
            bool changed = false;
            for (int j = 1; j < possible_decl->args.len; j++) {
                found = possible_decl->args.data[j]->zero_init;
                if (found != old) {
                    if (changed || !first) {
                        possible_decl->mixed_default_args = true;
                        break;
                    } else {
                        changed = true;
                    }
                }
                old = found;
            }
        }

        if (!possible_decl->eextern) {
            Var_Array vars_copy = {0};
            array_copy(vars_copy, global_vars);
            array_append(vars_copy, vars.data, vars.len);
            possible_decl->body = check_scope(vars_copy, decl->body, possible_decl, 0);
        }
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
            case STMT_WHILE_STMT: {
                Checked_While_Stmt *while_stmt = check_while_stmt(stmt.as.while_stmt, vars, fn, deep + 1);
                checked_stmt.kind = CHECKED_STMT_WHILE_STMT;
                checked_stmt.as.while_stmt = while_stmt;
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

    if (deep == 0 && !return_found && strcmp(fn->return_type->str, "void") != 0) {
        error_msg(fn->name.loc, ERROR_FATAL, "`%s` reaches end of non-void function", fn->name.value);
        exit(1);
    }

    return res;
}

Checked_If_Stmt *check_if_stmt(If_Stmt *if_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_If_Stmt *res = calloc(1, sizeof(Checked_If_Stmt));

    res->expr = check_expr(if_stmt->expr, vars_copy, fn, deep, type_exist("bool"));
    if (res->expr->type->flag != TYPE_BOOLEAN) {
        error_msg(if_stmt->expr->loc, ERROR_FATAL, "expected type `bool` for if statement but got `"Type_Fmt"`", Type_Arg(res->expr->type));
        exit(1);
    }

    res->body = check_scope(vars_copy, if_stmt->body, fn, deep);

    return res;
}

Checked_While_Stmt *check_while_stmt(While_Stmt *while_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_While_Stmt *res = calloc(1, sizeof(Checked_While_Stmt));

    res->expr = check_expr(while_stmt->expr, vars_copy, fn, deep, type_exist("bool"));
    if (res->expr->type->flag != TYPE_BOOLEAN) {
        error_msg(while_stmt->expr->loc, ERROR_FATAL, "expected type `bool` for if statement but got `"Type_Fmt"`", Type_Arg(res->expr->type));
        exit(1);
    }

    res->body = check_scope(vars_copy, while_stmt->body, fn, deep);

    return res;
}

Checked_For_Stmt *check_for_stmt(For_Stmt *for_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_For_Stmt *res = calloc(1, sizeof(Checked_For_Stmt));

    Checked_Var possible_var = var_exist(vars_copy, for_stmt->var.value);
    if (possible_var.name.value) {
        error_msg(for_stmt->var.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
        error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
        exit(1);
    }
    res->var = (Checked_Var){for_stmt->var, type_exist("i32")};

    res->range.start = check_expr(for_stmt->range.start, vars_copy, fn, deep, type_exist("i32"));
    res->range.end = check_expr(for_stmt->range.end, vars_copy, fn, deep, type_exist("i32"));
    Type *lhs = res->range.start->type;
    Type *rhs = res->range.end->type;
    if (lhs->flag != TYPE_NUMBER || rhs->flag != TYPE_NUMBER) {
        error_msg(for_stmt->range.start->loc, ERROR_FATAL, "range type must be `i32`");
        exit(1);
    }

    Var_Array for_vars = {0};
    array_copy(for_vars, vars_copy);
    array_push(for_vars, res->var);
    res->body = check_scope(for_vars, for_stmt->body, fn, deep);

    return res;
}

Checked_Return_Stmt *check_return_stmt(Return_Stmt *return_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_Return_Stmt *res = calloc(1, sizeof(Checked_Return_Stmt));

    res->expr = check_expr(return_stmt->expr, vars_copy, fn, deep, fn->return_type);
    if (!type_eq(res->expr->type, fn->return_type)) {
        error_msg(return_stmt->expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` for the return type of `%s` function but got `"Type_Fmt"`", Type_Arg(fn->return_type), fn->name.value, Type_Arg(res->expr->type));
        error_msg(fn->name.loc, ERROR_NOTE, "`%s` defined here", fn->name.value);
        exit(1);
    }

    return res;
}

Checked_Var_Decl *check_var_decl(Var_Decl *var_decl, Var_Array *vars, Checked_Fn_Decl *fn, int deep) {
    Checked_Var_Decl *res = calloc(1, sizeof(Checked_Var_Decl));

    Checked_Var possible_var = var_exist(*vars, var_decl->name.value);
    if (possible_var.name.value) {
        error_msg(var_decl->name.loc, ERROR_FATAL, "variable `%s` already exists in scope", possible_var.name.value);
        error_msg(possible_var.name.loc, ERROR_NOTE, "`%s` first defined here", possible_var.name.value);
        exit(1);
    }
    res->name = var_decl->name;

    Type *possible_var_type = check_type(var_decl->type);
    if (!possible_var_type) {
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
            error_msg(var_decl->value->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(possible_var_type), Type_Arg(res->value->type));
            exit(1);
        }
    }

    Checked_Var var = {var_decl->name, possible_var_type};
    res->type = possible_var_type;
    array_push(*vars, var);

    return res;
}

Checked_Var_Assign *check_var_assign(Var_Assign *var_assign, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep) {
    Checked_Var_Assign *res = calloc(1, sizeof(Checked_Var_Assign));

    Checked_Expr *var = check_expr(var_assign->var, vars_copy, fn, deep, type_exist("auto"));
    res->var = var;
    Type *type = var->type;
    Checked_Expr *expr = check_expr(var_assign->expr, vars_copy, fn, deep, type);
    if (!type_eq(expr->type, type)) {
        error_msg(var_assign->expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(type), Type_Arg(expr->type));
        exit(1);
    }
    res->expr = expr;

    return res;
}

Checked_Expr *check_expr(Expr *expr, Var_Array vars, Checked_Fn_Decl *fn, int deep, Type *wanted_type) {
    Checked_Expr *res = calloc(1, sizeof(Checked_Expr));

    switch (expr->kind) {
        case EXPR_NULL: {
            Type *type = type_exist("auto");
            if (wanted_type->flag == TYPE_POINTER || wanted_type->flag == TYPE_ARRAY) {
                type = wanted_type;
            }
            res->type = type;
            res->kind = CHECKED_EXPR_NULL;
        } break;
        case EXPR_NUMBER: {
            Type *type = type_exist("i32");
            if (wanted_type->flag == TYPE_NUMBER) {
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
        case EXPR_BOOLEAN: {
            res->type = type_exist("bool");
            res->kind = CHECKED_EXPR_BOOLEAN;
            res->as.boolean = expr->as.boolean;
        } break;
        case EXPR_IDENTIFIER: {
            Checked_Identifier *id = check_identifier(expr->as.identifier, expr->loc, vars);

            res->type = get_actual_id_type(id);
            res->kind = CHECKED_EXPR_IDENTIFIER;
            res->as.identifier = id;
        } break;
        case EXPR_BIN_OP: {
            Checked_Expr *left = check_expr(expr->as.bin_op->left, vars, fn, deep, wanted_type);
            Checked_Expr *right = check_expr(expr->as.bin_op->right, vars, fn, deep, left->type);
            Type *type = left->type;

            if (!type_eq(left->type, right->type)) {
                error_msg(expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(left->type), Type_Arg(right->type));
                exit(1);
            }
            if (expr->as.bin_op->op.type == TOKEN_EQUAL_EQUAL || expr->as.bin_op->op.type == TOKEN_BANG_EQUAL) {
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
            Type *type = un_op_expr->type;
            if (expr->as.un_op->op.type == TOKEN_CARET) {
                Type *get_ptr = calloc(1, sizeof(Type));
                get_ptr->flag = TYPE_POINTER;
                get_ptr->str = type->str;
                get_ptr->base.pointer = type;
                type = get_ptr;
            }
            if (expr->as.un_op->op.type == TOKEN_ASTERISK) {
                if (type->flag != TYPE_POINTER && type->flag != TYPE_ARRAY) {
                    error_msg(expr->loc, ERROR_FATAL, "expected `*%s` but got `%s`", type->str, type->str);
                    exit(1);
                }
                char *id = type->str;
                type = type->base.pointer;
                type->str = id;
            }
            res->type = type;
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
                    Checked_Expr *given_expr = check_expr(expr->as.func_call->args.data[i].expr, vars, fn, deep, type_exist("auto"));
                    Checked_Construct_Arg arg = {0};
                    arg.name = expr->as.func_call->args.data[i].name;
                    arg.expr = given_expr;
                    array_push(func_call->args, arg);
                }
            } else {
                bool with_arg_expr = false;
                if (expr->as.func_call->args.len > 0) {
                    bool found = expr->as.func_call->args.data[0].name.value;
                    bool old = found;
                    for (int i = 1; i < expr->as.func_call->args.len; i++) {
                        found = expr->as.func_call->args.data[i].name.value;
                        if (found != old) {
                            error_msg(expr->loc, ERROR_FATAL, "mixture of `arg = expr` and `expr` elements is not allowed");
                            exit(1);
                        }
                        old = found;
                    }
                    with_arg_expr = found;
                }
                if (with_arg_expr) {
                    for (int i = 0; i < expr->as.func_call->args.len; i++) {
                        bool found = false;
                        for (int j = 0; j < possible_fn->args.len; j++) {
                            if (strcmp(expr->as.func_call->args.data[i].name.value, possible_fn->args.data[j]->name.value) == 0) {
                                found = true;
                            }
                        }
                        if (!found) {
                            error_msg(expr->loc, ERROR_FATAL, "function `%s` does not have a argument named `%s`", possible_fn->name.value, expr->as.func_call->args.data[i].name.value);
                            error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                            exit(1);
                        }
                    }
                    for (int i = 0; i < possible_fn->args.len; i++) {
                        bool found = false;
                        for (int j = 0; j < expr->as.func_call->args.len; j++) {
                            if (strcmp(expr->as.func_call->args.data[j].name.value, possible_fn->args.data[i]->name.value) == 0) {
                                Type *expected_type = possible_fn->args.data[i]->type;
                                Checked_Expr *given_expr = check_expr(expr->as.func_call->args.data[j].expr, vars, fn, deep, expected_type);
                                if (!type_eq(expected_type, given_expr->type)) {
                                    error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                                    error_msg(expr->as.func_call->args.data[j].expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(expected_type), Type_Arg(given_expr->type));
                                    error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                                    exit(1);
                                }
                                found = true;
                                Checked_Construct_Arg arg = {0};
                                arg.name = expr->as.func_call->args.data[j].name;
                                arg.expr = given_expr;
                                array_push(func_call->args, arg);
                            }
                        }
                        if (!found) {
                            Checked_Construct_Arg arg = {0};
                            arg.name = possible_fn->args.data[i]->name;
                            arg.expr = possible_fn->args.data[i]->value;
                            if (!arg.expr) {
                                error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                                error_msg(possible_fn->name.loc, ERROR_FATAL, "argument `%s` does not have any default value", possible_fn->args.data[i]->name.value);
                                exit(1);
                            }
                            array_push(func_call->args, arg);
                        }
                    }
                } else {
                    if (possible_fn->mixed_default_args) {
                        if (possible_fn->args.len != expr->as.func_call->args.len) {
                            error_msg(expr->loc, ERROR_FATAL, "function `%s` accepts %d arguments but %d given", expr->as.func_call->name, possible_fn->args.len, expr->as.func_call->args.len);
                            error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                            exit(1);
                        }
                        for (int i = 0; i < possible_fn->args.len; i++) {
                            Type *expected_type = possible_fn->args.data[i]->type;
                            Checked_Expr *given_expr = check_expr(expr->as.func_call->args.data[i].expr, vars, fn, deep, expected_type);
                            if (!type_eq(expected_type, given_expr->type)) {
                                error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                                error_msg(expr->as.func_call->args.data[i].expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(expected_type), Type_Arg(given_expr->type));
                                error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                                exit(1);
                            }
                            Checked_Construct_Arg arg = {0};
                            arg.name = expr->as.func_call->args.data[i].name;
                            arg.expr = given_expr;
                            array_push(func_call->args, arg);
                        }
                    } else {
                        for (int i = 0; i < possible_fn->args.len; i++) {
                            Type *expected_type = possible_fn->args.data[i]->type;
                            Checked_Expr *given_expr = NULL;
                            if (i >= expr->as.func_call->args.len) {
                                given_expr = possible_fn->args.data[i]->value;
                                if (!given_expr) {
                                    error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                                    error_msg(possible_fn->name.loc, ERROR_FATAL, "argument `%s` does not have any default value", possible_fn->args.data[i]->name.value);
                                    exit(1);
                                }
                            } else {
                                given_expr = check_expr(expr->as.func_call->args.data[i].expr, vars, fn, deep, expected_type);
                            }
                            if (!type_eq(expected_type, given_expr->type)) {
                                error_msg(expr->loc, ERROR_FATAL, "arguments to `%s` function are incorrect", expr->as.func_call->name);
                                error_msg(expr->as.func_call->args.data[i].expr->loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(expected_type), Type_Arg(given_expr->type));
                                error_msg(possible_fn->name.loc, ERROR_NOTE, "`%s` defined here", possible_fn->name.value);
                                exit(1);
                            }
                            Checked_Construct_Arg arg = {0};
                            arg.name = expr->as.func_call->args.data[i].name;
                            arg.expr = given_expr;
                            array_push(func_call->args, arg);
                        }
                    }
                }
            }
            res->type = possible_fn->return_type;
            res->kind = CHECKED_EXPR_FUNC_CALL;
            res->as.func_call = func_call;
        } break;
        case EXPR_CAST: {
            Type *type = check_type(expr->as.cast->type);
            if (!type) {
                error_msg(expr->loc, ERROR_FATAL, "use of undeclared `%s` type", expr->as.cast->type->id);
                exit(1);
            }
            Type *convert = type_exist("auto");
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
        case EXPR_STRUCT_CONSTRUCT: {
            Checked_Struct_Decl *sd = struct_exist(wanted_type->str);
            Struct_Construct *sc = expr->as.struct_construct;
            Checked_Struct_Construct *csc = calloc(1, sizeof(Checked_Struct_Construct));

            if (!sd) {
                if (wanted_type->flag == TYPE_NEEDS_INFERRING) {
                    res->type = wanted_type;
                    res->kind = CHECKED_EXPR_STRUCT_CONSTRUCT;
                    res->as.struct_construct = csc;
                    return res;
                } else {
                    error_msg(expr->loc, ERROR_FATAL, "struct `%s` could not found", wanted_type->str);
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
                Checked_Construct_Arg *data;
                int len;
            } default_args = {0};

            for (int i = 0; i < sd->vars.len; i++) {
                bool found = false;
                for (int j = 0; j < sc->args.len; j++) {
                    if (strcmp(sd->vars.data[i]->name.value, sc->args.data[j].name.value) == 0) {
                        found = true;
                    }
                }
                if (!found) {
                    if (sd->vars.data[i]->value) {
                        Checked_Construct_Arg default_arg = {0};
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

            array_append(csc->args, default_args.data, default_args.len);

            for (int i = 0; i < sc->args.len; i++) {
                bool found = false;
                for (int j = 0; j < sd->vars.len; j++) {
                    if (strcmp(sd->vars.data[j]->name.value, sc->args.data[i].name.value) == 0) {
                        Type *expected_type = sd->vars.data[j]->type;
                        Checked_Expr *given_expr = check_expr(sc->args.data[i].expr, vars, fn, deep, expected_type);
                        if (!type_eq(expected_type, given_expr->type)) {
                            error_msg(expr->loc, ERROR_FATAL, "fields of `%s` struct are incorrect", sd->name.value);
                            error_msg(sc->args.data[i].name.loc, ERROR_FATAL, "expected type `"Type_Fmt"` but got `"Type_Fmt"`", Type_Arg(expected_type), Type_Arg(given_expr->type));
                            error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                            exit(1);
                        }
                        found = true;
                        Checked_Construct_Arg arg = {0};
                        arg.name = sd->vars.data[j]->name;
                        arg.expr = given_expr;
                        array_push(csc->args, arg);
                    }
                }
                if (!found) {
                    error_msg(sc->args.data[i].name.loc, ERROR_FATAL, "struct `%s` does not have a field named `%s`", sd->name.value, sc->args.data[i].name.value);
                    error_msg(sd->name.loc, ERROR_NOTE, "`%s` defined here", sd->name.value);
                    exit(1);
                }
            }

            csc->type = wanted_type;
            res->type = wanted_type;
            res->kind = CHECKED_EXPR_STRUCT_CONSTRUCT;
            res->as.struct_construct = csc;
        } break;
        case EXPR_ARRAY_CONSTRUCT: {
            Type *res_type = type_exist("auto");
            if (wanted_type->flag == TYPE_ARRAY) {
                res_type = wanted_type;
            }
            Checked_Array_Construct *ac = calloc(1, sizeof(Checked_Array_Construct));
            if (expr->as.array_construct->exprs.len > 0) {
                Type *expected = res_type;
                if (res_type->flag == TYPE_ARRAY) {
                    expected = res_type->base.array->base;
                }

                Expr *first_unchecked = expr->as.array_construct->exprs.data[0];
                Checked_Expr *first_expr = check_expr(first_unchecked, vars, fn, deep, expected);
                if (!type_eq(expected, type_exist("auto"))) {
                    if (!type_eq(expected, first_expr->type)) {
                        error_msg(first_unchecked->loc, ERROR_FATAL, "expected `"Type_Fmt"` got `"Type_Fmt"`", Type_Arg(expected), Type_Arg(first_expr->type));
                        exit(1);
                    }
                } else {
                    if (type_eq(first_expr->type, type_exist("auto"))) {
                        error_msg(expr->loc, ERROR_FATAL, "type could not inferred");
                        exit(1);
                    }
                    res_type = calloc(1, sizeof(Type));
                    res_type->str = first_expr->type->str;
                    res_type->flag = TYPE_ARRAY;
                    Array *array = calloc(1, sizeof(Array));
                    array->base = first_expr->type;
                    array->len = expr->as.array_construct->exprs.len;
                    res_type->base.array = array;
                    expected = first_expr->type;
                }
                array_push(ac->exprs, first_expr);
                for (int i = 1; i < expr->as.array_construct->exprs.len; i++) {
                    Expr *unchecked = expr->as.array_construct->exprs.data[i];
                    Checked_Expr *ce = check_expr(unchecked, vars, fn, deep, expected);
                    if (!type_eq(expected, ce->type)) {
                        error_msg(unchecked->loc, ERROR_FATAL, "expected `"Type_Fmt"` got `"Type_Fmt"`", Type_Arg(expected), Type_Arg(ce->type));
                        exit(1);
                    }
                    array_push(ac->exprs, ce);
                }
            } else {
                error_msg(expr->loc, ERROR_FATAL, "array length can not be 0");
                exit(1);
            }
            res->type = res_type;
            res->kind = CHECKED_EXPR_ARRAY_CONSTRUCT;
            res->as.array_construct = ac;
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }

    return res;
}

Checked_Identifier *check_identifier(Identifier *identifier, Loc loc, Var_Array vars) {
    Type *type = var_exist(vars, identifier->name).type;
    if (!type) {
        error_msg(loc, ERROR_FATAL, "variable `%s` could not found in scope", identifier->name);
        exit(1);
    }
    Checked_Identifier *id = calloc(1, sizeof(Checked_Identifier));
    id->type = type;
    id->name = identifier->name;

    Checked_Identifier *checked_root = id;
    Identifier *root = identifier;

    while (root->child) {
        root = root->child;
        Checked_Struct_Decl *possible_struct = struct_exist(type->str);
        if (!possible_struct) {
            error_msg(loc, ERROR_FATAL, "type `%s` does not have any fields", type->str);
            exit(1);
        }
        Checked_Var_Decl *possible_var = struct_var_exist(possible_struct, root->name);
        if (!possible_var) {
            error_msg(loc, ERROR_FATAL, "struct `%s` does not have a field named `%s`", possible_struct->name.value, root->name);
            exit(1);
        }
        type = possible_var->type;

        Checked_Identifier *child = calloc(1, sizeof(Checked_Identifier));
        child->type = possible_var->type;
        child->name = root->name;
        checked_root->child = child;
        checked_root = checked_root->child;
    }

    return id;
}

Checked_Var var_exist(Var_Array vars, char *name) {
    for (int i = 0; i < vars.len; i++) {
        if (strcmp(vars.data[i].name.value, name) == 0) {
            return vars.data[i];
        }
    }
    return (Checked_Var){0};
}

Checked_Var_Decl *struct_var_exist(Checked_Struct_Decl *sd, char *name) {
    for (int i = 0; i < sd->vars.len; i++) {
        if (strcmp(sd->vars.data[i]->name.value, name) == 0) {
            return sd->vars.data[i];
        }
    }
    return NULL;
}

Type *get_actual_id_type(Checked_Identifier *id) {
    Type *type = id->type;
    Checked_Identifier *ptr = id;

    while (ptr->child) {
        type = ptr->child->type;
        ptr = ptr->child;
    }

    return type;
}

Type *type_exist(char *str) {
    for (int i = 0; i < info.types.len; i++) {
        if (str && strcmp(info.types.data[i]->str, str) == 0) {
            return info.types.data[i];
        }
    }

    return NULL;
}

Type *check_type(Parser_Type *t) {
    if (!t) {
        return type_exist("auto");
    }

    Type *possible_type = type_exist(t->id);
    if (!possible_type) {
        error_msg(t->loc, ERROR_FATAL, "invalid type `%s`", t->id);
        exit(1);
    }
    Type *res = calloc(1, sizeof(Type));
    //TODO: this will cause issues related to typedefs
    memcpy(res, possible_type, sizeof(Type));

    if (t->type == BASIC_POINTER) {
        res->base.pointer = check_type(t->base);
        res->flag = TYPE_POINTER;
    } else if (t->type == BASIC_ARRAY) {
        Array *array = calloc(1, sizeof(Array));
        *array = (Array){check_type(t->base), t->len};
        res->base.array = array;
        res->flag = TYPE_ARRAY;
    }

    return res;
}

bool type_eq(Type *a, Type *b) {
    if (a->flag != b->flag) {
        return false;
    }

    if (a->flag == TYPE_STRUCT || a->flag == TYPE_NUMBER) {
        return strcmp(a->str, b->str) == 0;
    }

    if (a->flag == TYPE_STRING || a->flag == TYPE_BOOLEAN || a->flag == TYPE_VOID || a->flag == TYPE_NEEDS_INFERRING) {
        return true;
    }

    if (a->flag == TYPE_POINTER) {
        return type_eq(a->base.pointer, b->base.pointer);
    }

    if (a->flag == TYPE_ARRAY) {
        return a->base.array->len == b->base.array->len && type_eq(a->base.array->base, b->base.array->base);
    }

    return false;
}

Checked_File typechecker_check(Parsed_File *decls) {
    array_append(info.types, &builtin_types, sizeof(builtin_types)/sizeof(builtin_types[0]));
    check_structs(decls);
    check_top_assignments(decls);
    check_functions(decls);
    return info;
}
