#ifndef _AST_H
#define _AST_H

#include <stdbool.h>

#include "token.h"

typedef struct Ast Ast;
typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Identifier Identifier;

typedef enum Parser_Type_Basic {
    BASIC_BASE,
    BASIC_POINTER,
    BASIC_ARRAY,
} Parser_Type_Basic;

typedef struct Parser_Type {
    char *id;
    int len;
    Loc loc;
    Parser_Type_Basic type;
    struct Parser_Type *base;
} Parser_Type;

typedef struct Var {
    Parser_Type *type;
    Token name;
} Var;

typedef struct Scope {
    Stmt *data;
    int len;
} Scope;

typedef struct Cast {
    Expr *expr;
    Token op;
    Parser_Type *type;
} Cast;

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

typedef struct String {
    char *value;
} String;

typedef struct Boolean {
    bool value;
} Boolean;

struct Identifier {
    char *name;
    struct Identifier *child;
};

typedef struct Array_Construct {
    struct {
        Expr **data;
        int len;
    } exprs;
} Array_Construct;

typedef struct Construct_Arg {
    Token name;
    Expr *expr;
} Construct_Arg;

typedef struct Struct_Construct {
    struct {
        Construct_Arg *data;
        int len;
    } args;
} Struct_Construct;

typedef struct Func_Call {
    char *name;
    struct {
        Construct_Arg *data;
        int len;
    } args;
} Func_Call;

typedef struct Var_Assign {
    Loc loc;
    Expr *var;
    Expr *expr;
} Var_Assign;

typedef struct If_Stmt {
    Scope body;
    Expr *expr;
} If_Stmt;

typedef struct While_Stmt {
    Scope body;
    Expr *expr;
} While_Stmt;

typedef struct For_Stmt {
    Token var;
    struct {
        Expr *start;
        Expr *end;
    } range;
    Scope body;
} For_Stmt;

typedef struct Return_Stmt {
    Expr *expr;
} Return_Stmt;

typedef struct Var_Decl {
    bool constant;
    Token name;
    Parser_Type *type;
    Expr *value;
    bool zero_init;
} Var_Decl;

typedef struct Fn_Decl {
    Token name;
    Parser_Type *return_type;
    bool eextern;
    bool has_va_arg;
    Scope body;
    struct {
        Var_Decl **data;
        int len;
    } args;
} Fn_Decl;

typedef struct Struct_Decl {
    Token name;
    struct {
        Var_Decl **data;
        int len;
    } vars;
    bool eextern;
} Struct_Decl;

typedef enum Expr_Kind {
    EXPR_INVALID,
    EXPR_NULL,
    EXPR_NUMBER,
    EXPR_STRING,
    EXPR_BOOLEAN,
    EXPR_IDENTIFIER,
    EXPR_FUNC_CALL,
    EXPR_BIN_OP,
    EXPR_UN_OP,
    EXPR_CAST,
    EXPR_STRUCT_CONSTRUCT,
    EXPR_ARRAY_CONSTRUCT,
} Expr_Kind;

typedef union Expr_As {
    Number *number;
    String *string;
    Boolean *boolean;
    Identifier *identifier;
    Func_Call *func_call;
    Bin_Op *bin_op;
    Un_Op *un_op;
    Cast *cast;
    Struct_Construct *struct_construct;
    Array_Construct *array_construct;
} Expr_As;

struct Expr {
    Expr_Kind kind;
    Expr_As as;
    Loc loc;
};

typedef union Stmt_As {
    Var_Assign *var_assign;
    If_Stmt *if_stmt;
    For_Stmt *for_stmt;
    While_Stmt *while_stmt;
    Return_Stmt *return_stmt;
    Fn_Decl *fn_decl;
    Struct_Decl *struct_decl;
    Var_Decl *var_decl;
    Expr *expr;
} Stmt_As;

typedef enum Stmt_Kind {
    STMT_INVALID,
    STMT_EMPTY,
    STMT_VAR_ASSIGN,
    STMT_IF_STMT,
    STMT_FOR_STMT,
    STMT_WHILE_STMT,
    STMT_RETURN_STMT,
    STMT_FN_DECL,
    STMT_STRUCT_DECL,
    STMT_VAR_DECL,
    STMT_EXPR,
} Stmt_Kind;

struct Stmt {
    Stmt_Kind kind;
    Stmt_As as;
};

#endif
