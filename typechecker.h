#ifndef _TYPECHECKER_H
#define _TYPECHECKER_H

#include "parser.h"
#include "utils.h"

typedef struct Type {
    char *str;
    int flags;
} Type;

typedef enum Type_Flag {
    TYPE_NUMBER = BIT(0),
    TYPE_STRING = BIT(1),
    TYPE_BOOLEAN = BIT(2),
    TYPE_USER_DEFINED = BIT(3),
    TYPE_NEEDS_INFERRING = BIT(4),
    TYPE_STRUCT = BIT(5) | TYPE_USER_DEFINED,
} Type_Flag;

typedef struct Checker_Info {
    struct {
        Fn_Decl **data;
        int len;
    } funcs;
    struct {
        Struct_Decl **data;
        int len;
    } structs;
    struct {
        Type *data;
        int len;
    } types;
} Checker_Info;

typedef struct Checked_Var {
    Token name;
    Type type;
} Checked_Var;

typedef struct Var_Array {
    Checked_Var *data;
    int len;
} Var_Array;

void typechecker_check(Parsed_File *decls);

void check_struct_decls(Parsed_File *decls);
void check_struct_vars(void);
void check_function_decls(Parsed_File *decls);
void check_function_statements(void);
Fn_Decl *function_exist(char *name);
Struct_Decl *struct_exist(char *name);
void check_function(Fn_Decl *fn);
void check_struct(Struct_Decl *sc);
void check_scope(Var_Array vars_copy, Scope scope, Fn_Decl *fn, int deep);
Checked_Var var_exist(Var_Array vars, char *name);
Type check_expr(Var_Array vars, Expr *expr, Type wanted_type);
Type type_exist(char *str);
Checked_Var check_var(Var var);

#endif
