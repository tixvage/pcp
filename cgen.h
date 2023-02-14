#ifndef _CGEN_H
#define _CGEN_H

#include "typechecker.h"

void cgen_generate(Checked_File *decls, const char *path);

void cgen_prepare(void);
void cgen_type(Type *type);
void cgen_structs(Checked_File *decls);
void cgen_struct(Checked_Struct_Decl *sc);
void cgen_functions(Checked_File *decls);
void cgen_function(Checked_Fn_Decl *fn);
void cgen_top_assignments(Checked_File *decls);
void cgen_statement(Checked_Stmt stmt);
void cgen_if_statement(Checked_If_Stmt *if_stmt);
void cgen_for_statement(Checked_For_Stmt *for_stmt);
void cgen_while_statement(Checked_While_Stmt *while_stmt);
void cgen_return_statement(Checked_Return_Stmt *return_stmt);
void cgen_var_declaration(Checked_Var_Decl *var_decl);
void cgen_var_assignment(Checked_Var_Assign *var_assign);
void cgen_expr(Checked_Expr *expr);

#endif
