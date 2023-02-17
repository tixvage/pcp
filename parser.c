#include "parser.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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

Token parser_peek_token(Parser *parser) {
    Lexer temp_lexer = parser->lexer;
    Token temp_token = parser->current_token;
    parser_eat(parser);
    Token res = parser->current_token;
    parser->lexer = temp_lexer;
    parser->current_token = temp_token;
    return res;
}

Stmt parse_top_stmt(Parser *parser) {
    Token tk = parser->current_token;
    switch (tk.type) {
        case TOKEN_KEYWORD_CONST: {
            parser_eat(parser);
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
        case TOKEN_KEYWORD_STRUCT: {
            return (Stmt){
                .kind = STMT_STRUCT_DECL,
                .as =  {.struct_decl = parse_struct_decl(parser)},
            };
        } break;
        case TOKEN_KEYWORD_EXTERN: {
            Token peek_tk = parser_peek_token(parser);
            if (peek_tk.type == TOKEN_KEYWORD_FN) {
                return (Stmt){
                    .kind = STMT_FN_DECL,
                    .as = {.fn_decl = parse_extern_fn_decl(parser)},
                };
            } else if (peek_tk.type == TOKEN_KEYWORD_STRUCT) {
                return (Stmt){
                    .kind = STMT_STRUCT_DECL,
                    .as = {.struct_decl = parse_extern_struct_decl(parser)},
                };
            } else {
                error_msg(peek_tk.loc, ERROR_FATAL, "expected `fn` or `struct` after extern");
                exit(1);
            }
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
            error_msg(tk.loc, ERROR_WARNING, "using `var` with variable declaration is deprecated");
            error_msg(tk.loc, ERROR_NOTE, "remove `var`");
            parser_eat(parser);
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
        case TOKEN_KEYWORD_FOR: {
            return (Stmt){
                .kind = STMT_FOR_STMT,
                .as = {.for_stmt = parse_for_stmt(parser)},
            };
        } break;
        case TOKEN_KEYWORD_WHILE: {
            return (Stmt){
                .kind = STMT_WHILE_STMT,
                .as = {.while_stmt = parse_while_stmt(parser)},
            };
        } break;
        case TOKEN_IDENTIFIER: {
            Token peek_tk = parser_peek_token(parser);
            if (peek_tk.type == TOKEN_COLON || peek_tk.type == TOKEN_COLON_EQUAL) {
                return (Stmt){
                    .kind = STMT_VAR_DECL,
                    .as = {.var_decl = parse_var_decl(parser)},
                };
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
        if (parser->current_token.type == TOKEN_EQUAL) {
            if (!(stmt.as.expr->kind == EXPR_IDENTIFIER || stmt.as.expr->kind == EXPR_UN_OP)) {
                error_msg(stmt.as.expr->loc, ERROR_FATAL, "expected lvalue");
                exit(1);
            }
            if (stmt.as.expr->kind == EXPR_UN_OP && stmt.as.expr->as.un_op->op.type != TOKEN_ASTERISK) {
                error_msg(stmt.as.expr->loc, ERROR_FATAL, "expected `*` but got `%s`", token_type_to_str(stmt.as.expr->as.un_op->op));
                exit(1);
            }
            parser_eat(parser);
            Expr *expr = parse_expr(parser);
            parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
            Var_Assign *var_assign = malloc(sizeof(Var_Assign));
            var_assign->var = stmt.as.expr;
            var_assign->expr = expr;
            stmt.kind = STMT_VAR_ASSIGN;
            stmt.as.var_assign = var_assign;
        } else {
            parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
        }
    }
    return stmt;
}

Expr *parse_expr(Parser *parser) {
    return parse_comparative_expr(parser);
}

Expr *parse_primary_expr(Parser *parser) {
    Token tk = parser->current_token;
    switch (tk.type) {
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_CARET:
        case TOKEN_ASTERISK: {
            Un_Op *un_op = malloc(sizeof(Un_Op));
            un_op->op = parser_eat(parser);
            un_op->expr = parse_primary_expr(parser);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_UN_OP;
            if (un_op->op.type == TOKEN_CARET && un_op->expr->kind != EXPR_IDENTIFIER) {
                error_msg(un_op->op.loc, ERROR_FATAL, "lvalue expected");
                exit(1);
            } else if (un_op->op.type == TOKEN_ASTERISK && (un_op->expr->kind != EXPR_IDENTIFIER && (un_op->expr->kind != EXPR_UN_OP && un_op->expr->as.un_op->op.type != TOKEN_ASTERISK))) {
                error_msg(un_op->op.loc, ERROR_FATAL, "lvalue expected");
                exit(1);
            }
            expr->as = (Expr_As){.un_op = un_op};
            expr->loc = un_op->op.loc;
            return expr;
        } break;
        case TOKEN_KEYWORD_FALSE:
        case TOKEN_KEYWORD_TRUE: {
            Token as_token = parser_eat(parser);
            Boolean *boolean = malloc(sizeof(Boolean));
            boolean->value = as_token.type == TOKEN_KEYWORD_TRUE ? true : false;
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_BOOLEAN;
            expr->as = (Expr_As){.boolean = boolean};
            expr->loc = as_token.loc;
            return expr;
        } break;
        case TOKEN_KEYWORD_NULL: {
            Token as_token = parser_eat(parser);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_NULL;
            expr->loc = as_token.loc;
            return expr;
        } break;
        case TOKEN_LBRACKET: {
            Token as_token = parser_eat(parser);
            Array_Construct *ac = calloc(1, sizeof(Array_Construct));

            while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACKET) {
                Expr *expr = parse_expr(parser);
                Token tk = parser->current_token;
                if (tk.type == TOKEN_COMMA) {
                    parser_eat(parser);
                } else if (tk.type != TOKEN_RBRACKET) {
                    parser_expect(parser, TOKEN_RBRACKET, "expected `]`");
                }
                array_push(ac->exprs, expr);
            }
            parser_expect(parser, TOKEN_RBRACKET, "expected `]`");

            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_ARRAY_CONSTRUCT;
            expr->as = (Expr_As){.array_construct = ac};
            expr->loc = as_token.loc;
            return expr;
        } break;
        case TOKEN_IDENTIFIER: {
            Token peek_tk = parser_peek_token(parser);
            if (peek_tk.type == TOKEN_LPAREN) {
                Expr *expr = malloc(sizeof(Expr));
                expr->kind = EXPR_FUNC_CALL;
                expr->as = (Expr_As){.func_call = parse_func_call(parser)};
                expr->loc = tk.loc;
                return expr;
            } else if (peek_tk.type == TOKEN_DOT) {
                Identifier *identifier = malloc(sizeof(Identifier));
                identifier->name = parser_eat(parser).value;
                parser_expect(parser, TOKEN_DOT, "expected `.`");
                Expr *child = parse_primary_expr(parser);
                if (child->kind != EXPR_IDENTIFIER) {
                    error_msg(child->loc, ERROR_FATAL, "right side must be id");
                    exit(1);
                }
                identifier->child = child->as.identifier;

                Expr *expr = malloc(sizeof(Expr));
                expr->kind = EXPR_IDENTIFIER;
                expr->as = (Expr_As){.identifier = identifier};
                expr->loc = tk.loc;
                return expr;
            } else {
                Identifier *identifier = malloc(sizeof(Identifier));
                identifier->name = parser_eat(parser).value;
                identifier->child = NULL;
                Expr *expr = malloc(sizeof(Expr));
                expr->kind = EXPR_IDENTIFIER;
                expr->as = (Expr_As){.identifier = identifier};
                expr->loc = tk.loc;
                return expr;
            }
        } break;
        case TOKEN_INTEGER_LITERAL: {
            Token as_token = parser_eat(parser);
            Number *number = malloc(sizeof(Number));
            number->value = atoi(as_token.value);
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_NUMBER;
            expr->as = (Expr_As){.number = number};
            expr->loc = as_token.loc;
            return expr;
        } break;
        case TOKEN_STRING_LITERAL: {
            Token as_token = parser_eat(parser);
            String *string = malloc(sizeof(String));
            string->value = as_token.value;
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_STRING;
            expr->as = (Expr_As){.string = string};
            expr->loc = as_token.loc;
            return expr;
        } break;
        case TOKEN_LBRACE: {
            Expr *expr = malloc(sizeof(Expr));
            expr->kind = EXPR_STRUCT_CONSTRUCT;
            expr->as = (Expr_As){.struct_construct = parse_struct_construct_expr(parser)};
            expr->loc = tk.loc;
            return expr;
        } break;
        case TOKEN_LPAREN: {
            parser_eat(parser);
            Expr *expr = parse_expr(parser);
            parser_expect(parser, TOKEN_RPAREN, "Expected `)`");
            return expr;
        } break;
    }

    error_msg(tk.loc, ERROR_FATAL, "invalid expression");
    exit(1);
}

Expr *parse_comparative_expr(Parser *parser) {
    Expr *root = parse_additive_expr(parser);
    //if?
    while (parser->current_token.type == TOKEN_EQUAL_EQUAL || parser->current_token.type == TOKEN_BANG_EQUAL) {
        Token op = parser_eat(parser);
        Expr *right = parse_additive_expr(parser);

        Bin_Op *bin_op = malloc(sizeof(Bin_Op));
        bin_op->left = root;
        bin_op->right = right;
        bin_op->op = op;

        root = malloc(sizeof(Expr));
        root->kind = EXPR_BIN_OP;
        root->as = (Expr_As){.bin_op = bin_op};
        root->loc = op.loc;
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
        root->loc = op.loc;
    }

    return root;
}

Expr *parse_multiplicitave_expr(Parser *parser) {
    Expr *root = parse_cast_expr(parser);
    while (parser->current_token.type == TOKEN_ASTERISK || parser->current_token.type == TOKEN_SLASH || parser->current_token.type == TOKEN_MODULO) {
        Token op = parser_eat(parser);
        Expr *right = parse_cast_expr(parser);

        Bin_Op *bin_op = malloc(sizeof(Bin_Op));
        bin_op->left = root;
        bin_op->right = right;
        bin_op->op = op;

        root = malloc(sizeof(Expr));
        root->kind = EXPR_BIN_OP;
        root->as = (Expr_As){.bin_op = bin_op};
        root->loc = op.loc;
    }

    return root;
}

Expr *parse_cast_expr(Parser *parser) {
    Expr *root = parse_primary_expr(parser);
    while (parser->current_token.type == TOKEN_KEYWORD_AS) {
        Token op = parser_eat(parser);
        Parser_Type* type = parse_type(parser);

        Cast *cast = malloc(sizeof(Cast));
        cast->expr = root;
        cast->op = op;
        cast->type = type;

        root = malloc(sizeof(Cast));
        root->kind = EXPR_CAST;
        root->as = (Expr_As){.cast = cast};
        root->loc = op.loc;
    }

    return root;
}

Construct_Arg parse_construct_arg(Parser *parser) {
    Construct_Arg res = {0};

    Token peek_tk = parser_peek_token(parser);

    if (peek_tk.type == TOKEN_EQUAL) {
        res.name = parser_expect(parser, TOKEN_IDENTIFIER, "expected id");
        parser_expect(parser, TOKEN_EQUAL, "expected `=`");
    }

    res.expr = parse_expr(parser);

    return res;
}

Struct_Construct *parse_struct_construct_expr(Parser *parser) {
    parser_expect(parser, TOKEN_LBRACE, "expected `{`");

    Struct_Construct *sc = malloc(sizeof(Struct_Construct));
    sc->args.data = NULL;
    sc->args.len = 0;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        Construct_Arg arg = parse_construct_arg(parser);
        Token tk = parser->current_token;
        if (tk.type == TOKEN_COMMA) {
            parser_eat(parser);
        } else if (tk.type != TOKEN_RBRACE) {
            parser_expect(parser, TOKEN_RBRACE, "expected `}`");
        }
        array_push(sc->args, arg);
    }

    parser_expect(parser, TOKEN_RBRACE, "expected `}`");
    return sc;
}

Var_Decl *parse_var_decl_standalone(Parser *parser) {
    Token id = parser_expect(parser, TOKEN_IDENTIFIER, "expected id");
    Token decl_type = parser_eat(parser);
    Parser_Type *var_type = NULL;
    Var_Decl *var_decl = malloc(sizeof(Var_Decl));
    var_decl->constant = false;
    var_decl->name = id;
    var_decl->value = NULL;
    var_decl->zero_init = false;

    if (decl_type.type == TOKEN_COLON) {
        var_type = parse_type(parser);
        Token next = parser->current_token;
        if (next.type != TOKEN_EQUAL) {
            var_decl->zero_init = true;
            var_decl->type = var_type;
            return var_decl;
        }
        parser_eat(parser);
    } else if (decl_type.type != TOKEN_COLON_EQUAL) {
        error_msg(decl_type.loc, ERROR_FATAL, "expected `:` or `:=` after `var %s`", id.value);
        exit(1);
    }

    var_decl->value = parse_expr(parser);
    if (var_decl->value == NULL) {
        error_msg(decl_type.loc, ERROR_FATAL, "expected expression after `=` or `:=`");
        exit(1);
    }

    var_decl->type = var_type;
    return var_decl;
}

Var_Decl *parse_var_decl(Parser *parser) {
    Var_Decl *var_decl = parse_var_decl_standalone(parser);
    parser_expect(parser, TOKEN_SEMICOLON, "Expected `;`");
    return var_decl;
}

Fn_Decl *parse_fn_decl(Parser *parser) {
    Fn_Decl *fn_decl = malloc(sizeof(Fn_Decl));
    fn_decl->body.data = NULL;
    fn_decl->body.len = 0;
    fn_decl->args.len = 0;
    fn_decl->args.data = NULL;
    fn_decl->eextern = false;
    fn_decl->has_va_arg = false;

    parser_eat(parser);
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    parse_fn_decl_args(parser, fn_decl);
    parser_expect(parser, TOKEN_COLON, "Expected `:`");
    Parser_Type *return_type = parse_type(parser);
    parser_expect(parser, TOKEN_LBRACE, "Expected `{`");

    fn_decl->name = name;
    fn_decl->return_type = return_type;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(fn_decl->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "Expected `}`");

    return fn_decl;
}

Fn_Decl *parse_extern_fn_decl(Parser *parser) {
    Fn_Decl *fn_decl = malloc(sizeof(Fn_Decl));
    fn_decl->body.data = NULL;
    fn_decl->body.len = 0;
    fn_decl->args.len = 0;
    fn_decl->args.data = NULL;
    fn_decl->eextern = true;
    fn_decl->has_va_arg = false;

    parser_eat(parser);
    parser_expect(parser, TOKEN_KEYWORD_FN, "expected `fn` after `extern`");
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    parse_fn_decl_args(parser, fn_decl);
    parser_expect(parser, TOKEN_COLON, "Expected `:`");
    Parser_Type *return_type = parse_type(parser);
    parser_expect(parser, TOKEN_SEMICOLON, "expected `;` after declaration of extern function");
    
    fn_decl->name = name;
    fn_decl->return_type = return_type;

    return fn_decl;
}

void parse_fn_decl_args(Parser *parser, Fn_Decl *fn_decl) {
    parser_expect(parser, TOKEN_LPAREN, "Expected `(`");

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RPAREN) {
        Var_Decl *var_decl = parse_var_decl_standalone(parser);
        if (var_decl->type) {
            if (strcmp(var_decl->type->id, "va_arg") == 0) {
                fn_decl->has_va_arg = true;
            }
        }
        Token tk = parser->current_token;
        if (tk.type == TOKEN_COMMA) {
            parser_eat(parser);
        } else if (tk.type != TOKEN_RPAREN) {
            parser_expect(parser, TOKEN_RPAREN, "expected `)`");
        }
        array_push(fn_decl->args, var_decl);
    }
    parser_expect(parser, TOKEN_RPAREN, "expected `)`");
}

Struct_Decl *parse_struct_decl(Parser *parser) {
    parser_eat(parser);
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "expected id");
    parser_expect(parser, TOKEN_LBRACE, "expected `{`");

    Struct_Decl *struct_decl = malloc(sizeof(Struct_Decl));
    struct_decl->eextern = false;
    struct_decl->name = name;
    struct_decl->vars.data = NULL;
    struct_decl->vars.len = 0;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        Var_Decl *var_decl = parse_var_decl_standalone(parser);
        Token tk = parser->current_token;
        if (tk.type == TOKEN_COMMA) {
            parser_eat(parser);
        } else if (tk.type != TOKEN_RBRACE) {
            parser_expect(parser, TOKEN_RBRACE, "expected `}`");
        }
        array_push(struct_decl->vars, var_decl);
    }
    parser_expect(parser, TOKEN_RBRACE, "expected `}`");

    return struct_decl;
}

Struct_Decl *parse_extern_struct_decl(Parser *parser) {
    Struct_Decl *struct_decl = malloc(sizeof(Struct_Decl));
    parser_eat(parser);
    parser_expect(parser, TOKEN_KEYWORD_STRUCT, "expected `struct` after `extern`");
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "Expected id");
    parser_expect(parser, TOKEN_LBRACE, "expected `{`");

    struct_decl->eextern = true;
    struct_decl->name = name;
    struct_decl->vars.data = NULL;
    struct_decl->vars.len = 0;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(struct_decl->vars, parse_var_decl(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "expected `}`");

    return struct_decl;
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
    if_stmt->body.len = 0;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(if_stmt->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "Expected `}`");

    return if_stmt;
}

For_Stmt *parse_for_stmt(Parser *parser) {
    parser_eat(parser);
    Token name = parser_expect(parser, TOKEN_IDENTIFIER, "expected id after `for`");
    parser_expect(parser, TOKEN_KEYWORD_IN, "expected `in` after id");
    Expr *start = parse_expr(parser);
    parser_expect(parser, TOKEN_DOT_DOT, "expected `..` after starting value");
    Expr *end = parse_expr(parser);
    parser_expect(parser, TOKEN_LBRACE, "expected `{`");
    
    For_Stmt *for_stmt = malloc(sizeof(For_Stmt));
    for_stmt->range.start = start;
    for_stmt->range.end = end;
    for_stmt->body.data = NULL;
    for_stmt->body.len = 0;
    for_stmt->var = name;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(for_stmt->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "expected `}`");

    return for_stmt;
}

While_Stmt *parse_while_stmt(Parser *parser) {
    parser_eat(parser);
    Expr *expr = parse_expr(parser);
    parser_expect(parser, TOKEN_LBRACE, "Expected `{`");

    While_Stmt *while_stmt = malloc(sizeof(While_Stmt));
    while_stmt->expr = expr;
    while_stmt->body.data = NULL;
    while_stmt->body.len = 0;

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RBRACE) {
        array_push(while_stmt->body, parse_child_stmt(parser));
    }
    parser_expect(parser, TOKEN_RBRACE, "Expected `}`");

    return while_stmt;
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

    while (!parser_eof(parser) && parser->current_token.type != TOKEN_RPAREN) {
        Construct_Arg arg = parse_construct_arg(parser);
        Token tk = parser->current_token;
        if (tk.type == TOKEN_COMMA) {
            parser_eat(parser);
        } else if (tk.type != TOKEN_RPAREN) {
            parser_expect(parser, TOKEN_RPAREN, "expected `)`");
        }
        array_push(func_call->args, arg);
    }

    parser_expect(parser, TOKEN_RPAREN, "Expected `)`");
}

Parser_Type *parse_type(Parser *parser) {
    Parser_Type *res = calloc(1, sizeof(Parser_Type));
    if (parser->current_token.type == TOKEN_ASTERISK) {
        res->type = BASIC_POINTER;
        parser_eat(parser);
        res->base = parse_type(parser);
    } else if (parser->current_token.type == TOKEN_LBRACKET) {
        res->type = BASIC_ARRAY;
        parser_eat(parser);
        res->base = parse_type(parser);
        parser_expect(parser, TOKEN_SEMICOLON, "expected `;`");
        Token array_len = parser_expect(parser, TOKEN_INTEGER_LITERAL, "expected number");
        parser_expect(parser, TOKEN_RBRACKET, "expected `]`");
        res->len = atoi(array_len.value);
    } else {
        res->type = BASIC_BASE;
        Token tk = parser_expect(parser, TOKEN_IDENTIFIER, "expected identifier");
        res->id = tk.value;
        res->loc = tk.loc;
    }

    Parser_Type *ptr = res;
    char *id = ptr->id;
    Loc loc = ptr->loc;
    while (ptr->base) {
        id = ptr->base->id;
        loc = ptr->base->loc;
        ptr = ptr->base;
    }

    assert(id);

    ptr = res;
    while (ptr->base) {
        ptr->id = id;
        ptr->loc = loc;
        ptr = ptr->base;
    }

    return res;
}

void parse_file(Parser *parser) {
    while (!parser_eof(parser)) {
        Stmt stmt = parse_top_stmt(parser);
        if (stmt.kind == STMT_VAR_DECL) {
            array_push(parser->res.top_var_decls, stmt.as.var_decl);
        } else if (stmt.kind == STMT_FN_DECL) {
            array_push(parser->res.fn_decls, stmt.as.fn_decl);
        } else if (stmt.kind == STMT_STRUCT_DECL) {
            array_push(parser->res.struct_decls, stmt.as.struct_decl);
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
