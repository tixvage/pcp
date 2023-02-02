#include "parser.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "utils.h"
#include "error.h"

bool parser_eof(const Parser *parser) {
    return parser->current_token.type == TOKEN_EOF;
}

Token parser_eat(Parser *parser) {
    Token prev = parser->current_token;
    parser->current_token = get_next_token(&parser->lexer);
    return prev;
}

Token parser_expect(Parser *parser, Token_Type type, const char *err) {
    Token prev = parser->current_token;
    parser->current_token = get_next_token(&parser->lexer);
    if (prev.type != type) {
        error_msg(prev.loc, ERROR_FATAL, "%s", err);
        exit(1);
    }

    return prev;
}

Stmt parse_top_stmt(Parser *parser);
Stmt parse_child_stmt(Parser *parser);
Expr *parse_expr(Parser *parser);
Expr *parse_primary_expr(Parser *parser);
Expr *parse_comparative_expr(Parser *parser);
Expr *parse_additive_expr(Parser *parser);
Expr *parse_multiplicitave_expr(Parser *parser);
Var_Decl *parse_var_decl(Parser *parser);
Fn_Decl *parse_fn_decl(Parser *parser);
void parse_fn_decl_args(Parser *parser, Fn_Decl *fn_decl);
Return_Stmt *parse_return_stmt(Parser *parser);
If_Stmt *parse_if_stmt(Parser *parser);
Var_Assign *parse_var_assign(Parser *parser);
Func_Call *parse_func_call(Parser *parser);
void parse_func_call_args(Parser *parser, Func_Call *func_call);
void parse_file(Parser *parser);

Stmt parse_top_stmt(Parser *parser) {
    Token tk = parser->current_token;
    switch (tk.type) {
        case TOKEN_KEYWORD_VAR: {
            return (Stmt){
                .kind = STMT_VAR_DECL,
                .as = {.var_decl = parse_var_decl(parser)},
            };
        } break;
        case TOKEN_KEYWORD_FN: {
            return (Stmt){
                .kind = STMT_FN_DECL,
                .as = {.fn_decl = parse_fn_decl(parser)},
            };
        } break;
        case TOKEN_SEMICOLON: {
            error_msg(tk.loc, ERROR_WARNING, "extra `;`");
            parser_eat(parser);
            return parse_top_stmt(parser);
        } break;
        default: {
            error_msg(tk.loc, ERROR_FATAL, "invalid expression in top");
            exit(1);
        } break;
    }
}

Stmt parse_child_stmt(Parser *parser) {
    Token tk = parser->current_token;
    switch (tk.type) {
        case TOKEN_KEYWORD_VAR: {
            return (Stmt){
                .kind = STMT_VAR_DECL,
                .as = {.var_decl = parse_var_decl(parser)},
            };
        } break;
        case TOKEN_KEYWORD_FN: {
            error_msg(tk.loc, ERROR_FATAL, "functions not allowed inside a function");
            exit(1);
        } break;
        case TOKEN_KEYWORD_RETURN: {
            return (Stmt){
                .kind = STMT_RETURN_STMT,
                .as = {.return_stmt = parse_return_stmt(parser)},
            };
        } break;
        case TOKEN_KEYWORD_IF: {
            return (Stmt){
                .kind = STMT_IF_STMT,
                .as = {.if_stmt = parse_if_stmt(parser)},
            };
        } break;
        case TOKEN_IDENTIFIER: {
            //TODO: lexer peek
            Lexer temp_lexer = parser->lexer;
            Token temp_token = parser->current_token;
            parser_eat(parser);
            if (parser->current_token.type == TOKEN_EQUAL) {
                parser->lexer = temp_lexer;
                parser->current_token = temp_token;
                return (Stmt){
                    .kind = STMT_VAR_ASSIGN,
                    .as = {.var_assign = parse_var_assign(parser)},
                };
            } else {
                parser->lexer = temp_lexer;
                parser->current_token = temp_token;
            }
        } break;
        case TOKEN_SEMICOLON: {
            error_msg(tk.loc, ERROR_WARNING, "extra `;`");
            parser_eat(parser);
            return parse_child_stmt(parser);
        } break;
    }
    Stmt stmt = (Stmt){
        .kind = STMT_EXPR,
        .as = {.expr = parse_expr(parser)},
    };
    if (stmt.as.expr == NULL) {
        stmt.kind = STMT_EMPTY;
    } else {
        parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
    }
    return stmt;
}

Expr *parse_expr(Parser *parser) {
    return parse_comparative_expr(parser);
}

Expr *parse_primary_expr(Parser *parser) {
    Token_Type tk = parser->current_token.type;
    switch (tk) {
        case TOKEN_PLUS: {
            Un_Op *un_op = malloc(sizeof(Un_Op));
            un_op->op = parser_eat(parser);
            un_op->expr = parse_primary_expr(parser);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_UN_OP;
            expr->as = (Expr_As){.un_op = un_op};
            return expr;
        } break;
        case TOKEN_MINUS: {
            Un_Op *un_op = malloc(sizeof(Un_Op));
            un_op->op = parser_eat(parser);
            un_op->expr = parse_primary_expr(parser);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_UN_OP;
            expr->as = (Expr_As){.un_op = un_op};
            return expr;
        } break;
        case TOKEN_IDENTIFIER: {
            Lexer temp_lexer = parser->lexer;
            Token temp_token = parser_eat(parser);
            if (parser->current_token.type == TOKEN_LPAREN) {
                parser->lexer = temp_lexer;
                parser->current_token = temp_token;

                Expr *expr = malloc(sizeof(Expr));
                expr->kind = EXPR_FUNC_CALL;
                expr->as = (Expr_As){.func_call = parse_func_call(parser)};
                return expr;
            } else {
                parser->lexer = temp_lexer;
                parser->current_token = temp_token;

                Identifier *identifier = malloc(sizeof(Identifier));
                identifier->name = parser_eat(parser).value;
                Expr *expr = malloc(sizeof(Expr));
                expr->kind = EXPR_IDENTIFIER;
                expr->as = (Expr_As){.identifier = identifier};
                return expr;
            }
        } break;
        case TOKEN_INTEGER_LITERAL: {
            Number *number = malloc(sizeof(Number));
            number->value = atoi(parser_eat(parser).value);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_NUMBER;
            expr->as = (Expr_As){.number = number};
            return expr;
        } break;
        case TOKEN_LPAREN: {
            parser_eat(parser);
            Expr *expr = parse_expr(parser);
            parser_eat(parser);
            return expr;
        } break;
    }
    return NULL;
}

Expr *parse_comparative_expr(Parser *parser) {
    Expr *root = parse_additive_expr(parser);
    //if?
    while (parser->current_token.type == TOKEN_EQUAL_EQUAL) {
        Token op = parser_eat(parser);
        Expr *right = parse_additive_expr(parser);

        Bin_Op *bin_op = malloc(sizeof(Bin_Op));
        bin_op->left = root;
        bin_op->right = right;
        bin_op->op = op;

        root = malloc(sizeof(Expr));
        root->kind = EXPR_BIN_OP;
        root->as = (Expr_As){.bin_op = bin_op};
    }

    return root;
}

Expr *parse_additive_expr(Parser *parser) {
    Expr *root = parse_multiplicitave_expr(parser);
    while (parser->current_token.type == TOKEN_PLUS || parser->current_token.type == TOKEN_MINUS) {
        Token op = parser_eat(parser);
        Expr *right = parse_multiplicitave_expr(parser);

        Bin_Op *bin_op = malloc(sizeof(Bin_Op));
        bin_op->left = root;
        bin_op->right = right;
        bin_op->op = op;

        root = malloc(sizeof(Expr));
        root->kind = EXPR_BIN_OP;
        root->as = (Expr_As){.bin_op = bin_op};
    }

    return root;
}

Expr *parse_multiplicitave_expr(Parser *parser) {
    Expr *root = parse_primary_expr(parser);
    while (parser->current_token.type == TOKEN_ASTERISK || parser->current_token.type == TOKEN_SLASH) {
        Token op = parser_eat(parser);
        Expr *right = parse_primary_expr(parser);

        Bin_Op *bin_op = malloc(sizeof(Bin_Op));
        bin_op->left = root;
        bin_op->right = right;
        bin_op->op = op;

        root = malloc(sizeof(Expr));
        root->kind = EXPR_BIN_OP;
        root->as = (Expr_As){.bin_op = bin_op};
    }

    return root;
}

Var_Decl *parse_var_decl(Parser *parser) {
    parser_eat(parser);
    Token id = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    Token decl_type = parser_eat(parser);
    Token var_type = (Token){.type = TOKEN_IDENTIFIER, .value = "i32"};
    if (decl_type.type == TOKEN_COLON) {
        var_type = parser_expect(parser, TOKEN_IDENTIFIER, "Expected type for variable");
        parser_expect(parser, TOKEN_EQUAL, "Expected `=`");
    } else if (decl_type.type != TOKEN_COLON_EQUAL) {
        error_msg(decl_type.loc, ERROR_FATAL, "expected `:` or `:=` after `var %s`", id.value);
        exit(1);
    }

    Var_Decl *var_decl = malloc(sizeof(Var_Decl));
    var_decl->constant = false;
    var_decl->name = id.value;
    var_decl->type = var_type.value;
    var_decl->value = parse_expr(parser);

    parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");

    return var_decl;
}

Fn_Decl *parse_fn_decl(Parser *parser) {
    Fn_Decl *fn_decl = malloc(sizeof(Fn_Decl));
    fn_decl->body.data = NULL;
    fn_decl->args.data = NULL;

    parser_eat(parser);
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    parse_fn_decl_args(parser, fn_decl);
    parser_expect(parser, TOKEN_COLON, "Expected `:`");
    Token return_type = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    parser_expect(parser, TOKEN_LBRACE, "Expected `{`");

    fn_decl->name = name;
    fn_decl->return_type = return_type.value;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(fn_decl->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "Expected `}`");

    return fn_decl;
}

void parse_fn_decl_args(Parser *parser, Fn_Decl *fn_decl) {
    parser_expect(parser, TOKEN_LPAREN, "Expected `(`");
    if (parser->current_token.type == TOKEN_RPAREN) {
        parser_eat(parser);
        return;
    }
    
    {
        Func_Arg first_arg = {0};
        Token token = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
        first_arg.name = token.value;
        parser_expect(parser, TOKEN_COLON, "Expected `:`");
        first_arg.type = parser_expect(parser, TOKEN_IDENTIFIER, "Expected type").value;
        array_push(fn_decl->args, first_arg);
    }
    

    if (parser->current_token.type == TOKEN_RPAREN) {
        parser_eat(parser);
        return;
    }

    while (!parser_eof(parser) && parser->current_token.type == TOKEN_COMMA) {
        parser_eat(parser);
        Func_Arg arg = {0};
        Token token = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
        arg.name = token.value;
        parser_expect(parser, TOKEN_COLON, "Expected `:`");
        arg.type = parser_expect(parser, TOKEN_IDENTIFIER, "Expected type").value;
        array_push(fn_decl->args, arg);
    }
    parser_expect(parser, TOKEN_RPAREN, "Expected `)`");
}

Return_Stmt *parse_return_stmt(Parser *parser) {
    parser_eat(parser);
    Expr *expr = parse_expr(parser);
    Return_Stmt *return_stmt = malloc(sizeof(Return_Stmt));
    return_stmt->expr = expr;

    parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
    return return_stmt;
}

If_Stmt *parse_if_stmt(Parser *parser) {
    parser_eat(parser);
    Expr *expr = parse_expr(parser);
    parser_expect(parser, TOKEN_LBRACE, "Expected `{`");

    If_Stmt *if_stmt = malloc(sizeof(If_Stmt));
    if_stmt->expr = expr;
    if_stmt->body.data = NULL;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(if_stmt->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "Expected `}`");

    return if_stmt;
}

Var_Assign *parse_var_assign(Parser *parser) {
    Token var = parser_eat(parser);
    assert(var.type == TOKEN_IDENTIFIER);
    parser_expect(parser, TOKEN_EQUAL, "Expected `=`");

    Expr *expr = parse_expr(parser);

    Var_Assign *var_assign = malloc(sizeof(Var_Assign));
    var_assign->var = var;
    var_assign->expr = expr;

    parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
    return var_assign;
}

Func_Call *parse_func_call(Parser *parser) {
    Token name = parser_eat(parser);

    Func_Call *func_call = malloc(sizeof(Func_Call));
    func_call->args.data = NULL;
    func_call->name = name.value;

    parse_func_call_args(parser, func_call);

    return func_call;
}

void parse_func_call_args(Parser *parser, Func_Call *func_call) {
    parser_expect(parser, TOKEN_LPAREN, "Expected `(`");
    if (parser->current_token.type == TOKEN_RPAREN) {
        parser_eat(parser);
        return;
    }

    Expr *first_arg = parse_expr(parser);
    array_push(func_call->args, first_arg);

    if (parser->current_token.type == TOKEN_RPAREN) {
        parser_eat(parser);
        return;
    }

    while (!parser_eof(parser) && parser->current_token.type == TOKEN_COMMA) {
        parser_eat(parser);
        Expr *arg = parse_expr(parser);
        array_push(func_call->args, arg);
    }

    parser_expect(parser, TOKEN_RPAREN, "Expected `)`");
}

void parse_file(Parser *parser) {
    while (!parser_eof(parser)) {
        Stmt stmt = parse_top_stmt(parser);
        if (stmt.kind == STMT_VAR_DECL) {
            array_push(parser->res.top_var_decls, stmt.as.var_decl);
        } else if (stmt.kind == STMT_FN_DECL) {
            array_push(parser->res.fn_decls, stmt.as.fn_decl);
        }
    }
}

void init_parser(Parser *parser, Lexer lexer) {
    parser->lexer = lexer;
    parser->current_token = get_next_token(&parser->lexer);
}

void parser_parse(Parser *parser) {
    parse_file(parser);
}
