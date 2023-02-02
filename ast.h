#ifndef _AST_H
#define _AST_H

#include <stdbool.h>

#include "token.h"

typedef struct Ast Ast;
typedef struct Expr Expr;
typedef struct Stmt Stmt;

typedef struct Func_Arg {
    char *type;
    char *name;
} Func_Arg;

typedef struct Bin_Op {
    Expr *left;
    Token op;
    Expr *right;
} Bin_Op;

typedef struct Un_Op {
    Token op;
    Expr *expr;
} Un_Op;

typedef struct Number {
    int value;
} Number;

typedef struct Identifier {
    char *name;
} Identifier;

typedef struct Func_Call {
    char *name;
    struct {
        Expr **data;
        int len;
    } args;
} Func_Call;

typedef struct Var_Assign {
    Token var;
    Expr *expr;
} Var_Assign;

typedef struct If_Stmt {
    struct {
        Stmt *data;
        int len;
    } body;
    Expr *expr;
} If_Stmt;

typedef struct Return_Stmt {
    Expr *expr;
} Return_Stmt;

typedef struct Var_Decl {
    bool constant;
    char *name;
    char *type;
    Expr *value;
} Var_Decl;

typedef struct Fn_Decl {
    char *name;
    char *return_type;
    struct {
        Stmt *data;
        int len;
    } body;
    struct {
        Func_Arg *data;
        int len;
    } args;
} Fn_Decl;

typedef enum Expr_Kind {
    EXPR_INVALID,
    EXPR_NUMBER,
    EXPR_IDENTIFIER,
    EXPR_FUNC_CALL,
    EXPR_BIN_OP,
    EXPR_UN_OP,
} Expr_Kind;

typedef union Expr_As {
    Number *number;
    Identifier *identifier;
    Func_Call *func_call;
    Bin_Op *bin_op;
    Un_Op *un_op;
} Expr_As;

struct Expr {
    Expr_Kind kind;
    Expr_As as;
};

typedef union Stmt_As {
    Var_Assign *var_assign;
    If_Stmt *if_stmt;
    Return_Stmt *return_stmt;
    Fn_Decl *fn_decl;
    Var_Decl *var_decl;
    Expr *expr;
} Stmt_As;

typedef enum Stmt_Kind {
    STMT_INVALID,
    STMT_EMPTY,
    STMT_VAR_ASSIGN,
    STMT_IF_STMT,
    STMT_RETURN_STMT,
    STMT_FN_DECL,
    STMT_VAR_DECL,
    STMT_EXPR,
} Stmt_Kind;

struct Stmt {
    Stmt_Kind kind;
    Stmt_As as;
};

#endif
