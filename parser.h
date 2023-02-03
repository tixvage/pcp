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

#endif
