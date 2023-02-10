#ifndef _TYPECHECKER_H
#define _TYPECHECKER_H

#include "parser.h"
#include "utils.h"

typedef struct Checked_Stmt Checked_Stmt;
typedef struct Checked_Expr Checked_Expr;

typedef struct Type {
    char *str;
    int flags;
} Type;

#define Type_Fmt "%c%s"
#define Type_Arg(tp) (tp.flags & TYPE_POINTER) == 0 ? '\0' : '*', tp.str

typedef enum Type_Flag {
    TYPE_NUMBER = BIT(0),
    TYPE_STRING = BIT(1),
    TYPE_BOOLEAN = BIT(2),
    TYPE_USER_DEFINED = BIT(3),
    TYPE_VOID = BIT(4),
    TYPE_NEEDS_INFERRING = BIT(5),
    TYPE_STRUCT = BIT(6) | TYPE_USER_DEFINED,
    TYPE_POINTER = BIT(7),
} Type_Flag;

typedef struct Checked_Var {
    Token name;
    Type type;
} Checked_Var;

typedef struct Var_Array {
    Checked_Var *data;
    int len;
} Var_Array;

typedef struct Checked_Scope {
    Checked_Stmt *data;
    int len;
} Checked_Scope;

typedef struct Checked_Bin_Op {
    Checked_Expr *left;
    Checked_Expr *right;
    Token op;
} Checked_Bin_Op;

typedef struct Checked_Un_Op {
    Checked_Expr *expr;
    Token op;
} Checked_Un_Op;

typedef struct Checked_Cast {
    Checked_Expr *expr;
    Type type;
} Checked_Cast;

typedef struct Checked_Identifier {
    char *name;
    Type type;
    struct Checked_Identifier *child;
} Checked_Identifier;

typedef struct Checked_Struct_Construct_Arg {
    Token name;
    Checked_Expr *expr;
} Checked_Struct_Construct_Arg;

typedef struct Checked_Struct_Construct {
    struct {
        Checked_Struct_Construct_Arg *data;
        int len;
    } args;
    Type type;
} Checked_Struct_Construct;

typedef struct Checked_Func_Call {
    char *name;
    struct {
        Checked_Expr **data;
        int len;
    } args;
} Checked_Func_Call;

typedef struct Checked_Var_Assign {
    Checked_Expr *var;
    Checked_Expr *expr;
} Checked_Var_Assign;

typedef struct Checked_If_Stmt {
    Checked_Scope body;
    Checked_Expr *expr;
} Checked_If_Stmt;

typedef struct Checked_While_Stmt {
    Checked_Scope body;
    Checked_Expr *expr;
} Checked_While_Stmt;

typedef struct Checked_For_Stmt {
    Checked_Var var;
    struct {
        Checked_Expr *start;
        Checked_Expr *end;
    } range;
    Checked_Scope body;
} Checked_For_Stmt;

typedef struct Checked_Return_Stmt {
    Checked_Expr *expr;
} Checked_Return_Stmt;

typedef struct Checked_Var_Decl {
    Token name;
    Checked_Expr *value;
    Type type;
    bool zero_init;
} Checked_Var_Decl;

typedef struct Checked_Fn_Decl {
    Token name;
    Type return_type;
    bool eextern;
    bool has_va_arg;
    Checked_Scope body;
    Var_Array args;
} Checked_Fn_Decl;

typedef struct Checked_Struct_Decl {
    Token name;
    struct {
        Checked_Var_Decl **data;
        int len;
    } vars;
    bool eextern;
} Checked_Struct_Decl;

typedef enum Checked_Expr_Kind {
    CHECKED_EXPR_INVALID,
    CHECKED_EXPR_NUMBER,
    CHECKED_EXPR_STRING,
    CHECKED_EXPR_IDENTIFIER,
    CHECKED_EXPR_FUNC_CALL,
    CHECKED_EXPR_BIN_OP,
    CHECKED_EXPR_UN_OP,
    CHECKED_EXPR_CAST,
    CHECKED_EXPR_STRUCT_CONSTRUCT,
} Checked_Expr_Kind;

typedef union Checked_Expr_As {
    Number *number;
    String *string;
    Checked_Identifier *identifier;
    Checked_Func_Call *func_call;
    Checked_Bin_Op *bin_op;
    Checked_Un_Op *un_op;
    Checked_Cast *cast;
    Checked_Struct_Construct *struct_construct;
} Checked_Expr_As;

struct Checked_Expr {
    Checked_Expr_Kind kind;
    Checked_Expr_As as;
    Loc loc;
    Type type;
};

typedef enum Checked_Stmt_Kind {
    CHECKED_STMT_INVALID,
    CHECKED_STMT_EMPTY,
    CHECKED_STMT_VAR_ASSIGN,
    CHECKED_STMT_IF_STMT,
    CHECKED_STMT_FOR_STMT,
    CHECKED_STMT_WHILE_STMT,
    CHECKED_STMT_RETURN_STMT,
    CHECKED_STMT_FN_DECL,
    CHECKED_STMT_STRUCT_DECL,
    CHECKED_STMT_VAR_DECL,
    CHECKED_STMT_EXPR,
} Checked_Stmt_Kind;

typedef union Checked_Stmt_As {
    Checked_Var_Assign *var_assign;
    Checked_If_Stmt *if_stmt;
    Checked_For_Stmt *for_stmt;
    Checked_While_Stmt *while_stmt;
    Checked_Return_Stmt *return_stmt;
    Checked_Fn_Decl *fn_decl;
    Checked_Struct_Decl *struct_decl;
    Checked_Var_Decl *var_decl;
    Checked_Expr *expr;
} Checked_Stmt_As;

struct Checked_Stmt {
    Checked_Stmt_Kind kind;
    Checked_Stmt_As as;
};

typedef struct Checked_File {
    struct {
        Checked_Fn_Decl **data;
        int len;
    } funcs;
    struct {
        Checked_Struct_Decl **data;
        int len;
    } structs;
    struct {
        Checked_Var_Decl **data;
        int len;
    } top_assignments;
    struct {
        Type *data;
        int len;
    } types;
} Checked_File;

Checked_File typechecker_check(Parsed_File *decls);

void check_structs(Parsed_File *decls);
void check_top_assignments(Parsed_File *decls);
void check_functions(Parsed_File *decls);
Checked_Fn_Decl *function_exist(char *name);
Checked_Struct_Decl *struct_exist(char *name);
void check_function(Fn_Decl *fn);
void check_struct(Struct_Decl *sd);
Checked_Scope check_scope(Var_Array vars_copy, Scope scope, Checked_Fn_Decl *fn, int deep);
Checked_If_Stmt *check_if_stmt(If_Stmt *if_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep);
Checked_While_Stmt *check_while_stmt(While_Stmt *while_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep);
Checked_For_Stmt *check_for_stmt(For_Stmt *for_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep);
Checked_Return_Stmt *check_return_stmt(Return_Stmt *return_stmt, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep);
Checked_Var_Decl *check_var_decl(Var_Decl *var_decl, Var_Array *vars, Checked_Fn_Decl *fn, int deep);
Checked_Var_Assign *check_var_assign(Var_Assign *var_assign, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep);
Checked_Expr *check_expr(Expr *expr, Var_Array vars_copy, Checked_Fn_Decl *fn, int deep, Type wanted_type);
Checked_Identifier *check_identifier(Identifier *id, Loc loc, Var_Array vars_copy);
Checked_Var var_exist(Var_Array vars, char *name);
Checked_Var_Decl *struct_var_exist(Checked_Struct_Decl *sd, char *name);
Type get_actual_id_type(Checked_Identifier *id);
Type type_exist(char *str);
Type check_type(Parser_Type t);
bool type_eq(Type a, Type b);

#endif
