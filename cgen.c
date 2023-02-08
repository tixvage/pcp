#include "cgen.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"

FILE *f;

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

void cgen_structs(Checked_File *decls) {
    for (int i = 0; i < decls->structs.len; i++) {
        cgen_struct(decls->structs.data[i]);
    }
}

void cgen_struct(Checked_Struct_Decl *sc) {
    fprintf(f, "typedef struct %s {\n", sc->name.value);
    for (int i = 0; i < sc->vars.len; i++) {
        fprintf(f, "%s %s;\n", sc->vars.data[i]->type.str, sc->vars.data[i]->name.value);
    }
    fprintf(f, "} %s;\n", sc->name.value);
}

void cgen_functions(Checked_File *decls) {
    for (int i = 0; i < decls->funcs.len; i++) {
        cgen_function(decls->funcs.data[i]);
    }
}

void cgen_function(Checked_Fn_Decl *fn) {
    fprintf(f, "%s %s(", fn->return_type.str, fn->name.value);
    if (fn->args.data == NULL) {
        fprintf(f, "void");
    } else {
        Checked_Var first_arg = fn->args.data[0];
        if (strcmp(first_arg.type.str, "va_arg") != 0) {
            fprintf(f, "%s %s", first_arg.type.str, first_arg.name.value);
        } else {
            fprintf(f, "%s", first_arg.name.value);
        }
        for (int i = 1; i < fn->args.len; i++) {
            if (strcmp(fn->args.data[i].type.str, "va_arg") != 0) {
                fprintf(f, ", %s %s", fn->args.data[i].type.str, fn->args.data[i].name.value);
            } else {
                fprintf(f, ", %s", fn->args.data[i].name.value);
            }
        }
    }
    if (fn->eextern) {
        fprintf(f, ");\n");
        return;
    }
    fprintf(f, ") {\n");
    for (int i = 0; i < fn->body.len; i++) {
        cgen_statement(fn->body.data[i]);
    }
    fprintf(f, "}\n");
}

void cgen_top_assignments(Checked_File *decls) {
    for (int i = 0; i < decls->top_assignments.len; i++) {
        cgen_var_declaration(decls->top_assignments.data[i]);
    }
}

void cgen_statement(Checked_Stmt stmt) {
    switch (stmt.kind) {
        case CHECKED_STMT_IF_STMT: {
            cgen_if_statment(stmt.as.if_stmt);
        } break;
        case CHECKED_STMT_FOR_STMT: {
            cgen_for_statment(stmt.as.for_stmt);
        } break;
        case CHECKED_STMT_RETURN_STMT: {
            cgen_return_statment(stmt.as.return_stmt);
        } break;
        case CHECKED_STMT_VAR_DECL: {
            cgen_var_declaration(stmt.as.var_decl);
        } break;
        case CHECKED_STMT_VAR_ASSIGN: {
            cgen_var_assignment(stmt.as.var_assign);
        } break;
        case CHECKED_STMT_EXPR: {
            cgen_expr(stmt.as.expr);
            fprintf(f, ";\n");
        } break;
        case CHECKED_STMT_EMPTY:  {
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
}

void cgen_if_statment(Checked_If_Stmt *if_stmt) {
    fprintf(f, "if (");
    cgen_expr(if_stmt->expr);
    fprintf(f, ") {\n");
    for (int i = 0; i < if_stmt->body.len; i++) {
        cgen_statement(if_stmt->body.data[i]);
    }
    fprintf(f, "}\n");
}

void cgen_for_statment(Checked_For_Stmt *for_stmt) {
    fprintf(f, "for (i32 %s = ", for_stmt->var.name.value);
    cgen_expr(for_stmt->range.start);
    fprintf(f, "; %s < ", for_stmt->var.name.value);
    cgen_expr(for_stmt->range.end);
    fprintf(f, "; %s++) {\n", for_stmt->var.name.value);
    for (int i = 0; i < for_stmt->body.len; i++) {
        cgen_statement(for_stmt->body.data[i]);
    }
    fprintf(f, "}\n");
}

void cgen_return_statment(Checked_Return_Stmt *return_stmt) {
    fprintf(f, "return ");
    cgen_expr(return_stmt->expr);
    fprintf(f, ";\n");
}

void cgen_var_declaration(Checked_Var_Decl *var_decl) {
    fprintf(f, "%s %s = ", var_decl->value->type.str, var_decl->name.value);
    if (var_decl->zero_init) {
        fprintf(f, "{0}");
    } else {
        cgen_expr(var_decl->value);
    }
    fprintf(f, ";\n");
}

void cgen_var_assignment(Checked_Var_Assign *var_assign) {
    fprintf(f, "%s", var_assign->var->name);
    Identifier *root = var_assign->var;
    while (root->child) {
        fprintf(f, ".%s", root->child->name);
        root = root->child;
    }
    fprintf(f, " = ");
    cgen_expr(var_assign->expr);
    fprintf(f, ";\n");
}

void cgen_expr(Checked_Expr *expr) {
    switch (expr->kind) {
        case EXPR_NUMBER: {
            fprintf(f, "%d", expr->as.number->value);
        } break;
        case EXPR_STRING: {
            fprintf(f, "\"%s\"", expr->as.string->value);
        } break;
        case EXPR_IDENTIFIER: {
            fprintf(f, "%s", expr->as.identifier->name);
            Identifier *root = expr->as.identifier;
            while (root->child) {
                fprintf(f, ".%s", root->child->name);
                root = root->child;
            }
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
            Checked_Func_Call *func_call = expr->as.func_call;
            fprintf(f, "%s(", func_call->name);
            if (func_call->args.data != NULL) {
                Checked_Expr *first_arg = func_call->args.data[0];
                cgen_expr(first_arg);
                for (int i = 1; i < func_call->args.len; i++) {
                    Checked_Expr *arg = func_call->args.data[i];
                    fprintf(f, ", ");
                    cgen_expr(arg);
                }
            }
            fprintf(f, ")");
        } break;
        case EXPR_CAST: {
            fprintf(f, "(%s)(", expr->as.cast->type.str);
            cgen_expr(expr->as.cast->expr);
            fprintf(f, ")");
        } break;
        case EXPR_STRUCT_CONSTRUCT: {
            fprintf(f, "(%s){ ", expr->as.struct_construct->type.str);
            for (int i = 0; i < expr->as.struct_construct->args.len; i++) {
                Checked_Struct_Construct_Arg arg = expr->as.struct_construct->args.data[i];
                fprintf(f, ".%s = ", arg.name.value);
                cgen_expr(arg.expr);
                fprintf(f, ", ");
            }
            fprintf(f, " }");
        } break;
        default: {
            assert(0 && "unreacheable");
        } break;
    }
}

void cgen_generate(Checked_File *decls, const char *path) {
    f = fopen(path, "w");
    if (f == NULL) {
        fprintf(stderr, "Couldn't write to `%s`\n", path);
        exit(1);
    }
    cgen_prepare();
    cgen_structs(decls);
    cgen_top_assignments(decls);
    cgen_functions(decls);
    fclose(f);
}
