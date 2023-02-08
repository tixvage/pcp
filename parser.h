#ifndef _PARSER_H
#define _PARSER_H

#include "ast.h"
#include "lexer.h"

typedef struct Parsed_File {
    struct {
        Var_Decl **data;
        int len;
    } top_var_decls;
    struct {
        Fn_Decl **data;
        int len;
    } fn_decls;
    struct {
        Struct_Decl **data;
        int len;
    } struct_decls;
} Parsed_File;

typedef struct Parser {
    Lexer lexer;
    Token current_token;
    Parsed_File res;
} Parser;

void init_parser(Parser *parser, Lexer lexer);
void parser_parse(Parser *parser);

Stmt parse_top_stmt(Parser *parser);
Stmt parse_child_stmt(Parser *parser);
Expr *parse_expr(Parser *parser);
Expr *parse_primary_expr(Parser *parser);
Expr *parse_comparative_expr(Parser *parser);
Expr *parse_additive_expr(Parser *parser);
Expr *parse_multiplicitave_expr(Parser *parser);
Struct_Construct *parse_struct_construct_expr(Parser *parser);
Expr *parse_cast_expr(Parser *parser);
Var_Decl *parse_var_decl(Parser *parser);
Fn_Decl *parse_fn_decl(Parser *parser);
Fn_Decl *parse_extern_fn_decl(Parser *parser);
void parse_fn_decl_args(Parser *parser, Fn_Decl *fn_decl);
Struct_Decl *parse_struct_decl(Parser *parser);
Return_Stmt *parse_return_stmt(Parser *parser);
If_Stmt *parse_if_stmt(Parser *parser);
For_Stmt *parse_for_stmt(Parser *parser);
Var_Assign *parse_var_assign(Parser *parser);
Func_Call *parse_func_call(Parser *parser);
void parse_func_call_args(Parser *parser, Func_Call *func_call);
void parse_file(Parser *parser);

#endif
