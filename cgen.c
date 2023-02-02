#include "cgen.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ast.h"

FILE *f;

void cgen_prepare(void);
void cgen_functions(Parsed_File *decls);
void cgen_function(Fn_Decl *fn);
void cgen_statement(Stmt stmt);
void cgen_if_statment(If_Stmt *if_stmt);
void cgen_return_statment(Return_Stmt *return_stmt);
void cgen_var_declaration(Var_Decl *var_decl);
void cgen_var_assignment(Var_Assign *var_assign);
void cgen_expr(Expr *expr);

void cgen_prepare(void) {
    fprintf(f, "#include <stdbool.h>\n");
    fprintf(f, "#include <stdint.h>\n");
    fprintf(f, "typedef uint8_t u8;\n");
    fprintf(f, "typedef int8_t i8;\n");
    fprintf(f, "typedef uint16_t u16;\n");
    fprintf(f, "typedef int16_t i16;\n");
    fprintf(f, "typedef uint32_t u32;\n");
    fprintf(f, "typedef int32_t i32;\n");
    fprintf(f, "typedef uint64_t u64;\n");
    fprintf(f, "typedef int64_t i64;\n");
    fprintf(f, "typedef const char *cstr;\n");
    fprintf(f, "\n");
}

void cgen_functions(Parsed_File *decls) {
    for (int i = 0; i < decls->fn_decls.len; i++) {
        cgen_function(decls->fn_decls.data[i]);
    }
}

void cgen_function(Fn_Decl *fn) {
    fprintf(f, "%s %s(", fn->return_type, fn->name.value);
    if (fn->args.data == NULL) {
        fprintf(f, "void");
    } else {
        Var first_arg = fn->args.data[0];
        fprintf(f, "%s %s", first_arg.type, first_arg.name.value);
        for (int i = 1; i < fn->args.len; i++) {
            fprintf(f, ", %s %s", fn->args.data[i].type, fn->args.data[i].name.value);
        }
    }
    fprintf(f, ") {\n");
    for (int i = 0; i < fn->body.len; i++) {
        cgen_statement(fn->body.data[i]);
    }
    fprintf(f, "}\n");
}

void cgen_statement(Stmt stmt) {
    switch (stmt.kind) {
        case STMT_IF_STMT: {
            cgen_if_statment(stmt.as.if_stmt);
        } break;
        case STMT_RETURN_STMT: {
            cgen_return_statment(stmt.as.return_stmt);
        } break;
        case STMT_VAR_DECL: {
            cgen_var_declaration(stmt.as.var_decl);
        } break;
        case STMT_VAR_ASSIGN: {
            cgen_var_assignment(stmt.as.var_assign);
        } break;
        case STMT_EXPR: {
            cgen_expr(stmt.as.expr);
            fprintf(f, ";\n");
        } break;
        case STMT_EMPTY:  {
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
}

void cgen_if_statment(If_Stmt *if_stmt) {
    fprintf(f, "if (");
    cgen_expr(if_stmt->expr);
    fprintf(f, ") {\n");
    for (int i = 0; i < if_stmt->body.len; i++) {
        cgen_statement(if_stmt->body.data[i]);
    }
    fprintf(f, "}\n");
}

void cgen_return_statment(Return_Stmt *return_stmt) {
    fprintf(f, "return ");
    cgen_expr(return_stmt->expr);
    fprintf(f, ";\n");
}

void cgen_var_declaration(Var_Decl *var_decl) {
    fprintf(f, "%s %s = ", var_decl->type, var_decl->name.value);
    cgen_expr(var_decl->value);
    fprintf(f, ";\n");
}

void cgen_var_assignment(Var_Assign *var_assign) {
    fprintf(f, "%s = ", var_assign->var.value);
    cgen_expr(var_assign->expr);
    fprintf(f, ";\n");
}

void cgen_expr(Expr *expr) {
    switch (expr->kind) {
        case EXPR_NUMBER: {
            fprintf(f, "%d", expr->as.number->value);
        } break;
        case EXPR_STRING: {
            fprintf(f, "\"%s\"", expr->as.string->value);
        } break;
        case EXPR_IDENTIFIER: {
            fprintf(f, "%s", expr->as.identifier->name);
        } break;
        case EXPR_BIN_OP: {
            cgen_expr(expr->as.bin_op->left);
            fprintf(f, " %s ", expr->as.bin_op->op.value);
            cgen_expr(expr->as.bin_op->right);
        } break;
        case EXPR_UN_OP: {
            fprintf(f, "%s(", expr->as.un_op->op.value);
            cgen_expr(expr->as.un_op->expr);
            fprintf(f, ")");
        } break;
        case EXPR_FUNC_CALL: {
            Func_Call *func_call = expr->as.func_call;
            fprintf(f, "%s(", func_call->name);
            if (func_call->args.data != NULL) {
                Expr *first_arg = func_call->args.data[0];
                cgen_expr(first_arg);
                for (int i = 1; i < func_call->args.len; i++) {
                    Expr *arg = func_call->args.data[i];
                    fprintf(f, ", ");
                    cgen_expr(arg);
                }
            }
            fprintf(f, ")");
        } break;
        case EXPR_CAST: {
            fprintf(f, "(%s)(", expr->as.cast->type.value);
            cgen_expr(expr->as.cast->expr);
            fprintf(f, ")");
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
}

void cgen_generate(Parsed_File *decls, const char *path) {
    f = fopen(path, "w");
    if (f == NULL) {
        fprintf(stderr, "Couldn't write to `%s`\n", path);
        exit(1);
    }
    cgen_prepare();
    cgen_functions(decls);
    fclose(f);
}
