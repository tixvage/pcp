#include "token.h"

#include <assert.h>

char* KEYS[COUNT_TOKEN] = {
    [TOKEN_BANG] = "!",
    [TOKEN_EQUAL] = "=",
    [TOKEN_EQUAL_EQUAL] = "==",
    [TOKEN_BANG_EQUAL] = "!=",
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_DOT] = ".",
    [TOKEN_DOT_DOT] = "..",
    [TOKEN_DOT_DOT_DOT] = "...",
    [TOKEN_HASH] = "#",
    [TOKEN_LBRACE] = "{",
    [TOKEN_RBRACE] = "}",
    [TOKEN_LBRACKET] = "[",
    [TOKEN_RBRACKET] = "]",
    [TOKEN_MODULO] = "%",
    [TOKEN_PLUS] = "+",
    [TOKEN_PLUS_PLUS] = "++",
    [TOKEN_PLUS_EQUAL] = "+=",
    [TOKEN_MINUS] = "-",
    [TOKEN_MINUS_MINUS] = "--",
    [TOKEN_MINUS_EQUAL] = "-=",
    [TOKEN_ASTERISK] = "*",
    [TOKEN_ASTERISK_ASTERISK] = "**",
    [TOKEN_ASTERISK_EQUAL] = "*=",
    [TOKEN_SLASH] = "/",
    [TOKEN_SLASH_EQUAL] = "/=",
    [TOKEN_COLON] = ":",
    [TOKEN_COLON_EQUAL] = ":=",
    [TOKEN_COLON_COLON] = "::",
    [TOKEN_COMMA] = ",",
    [TOKEN_QUESTION_MARK] = "?",
    [TOKEN_CARET] = "^",
    [TOKEN_DOLLAR_SIGN] = "$",
    [TOKEN_ANGLE_BRACKET_LEFT] = "<",
    [TOKEN_ANGLE_BRACKET_LEFT_EQUAL] = "<=",
    [TOKEN_ANGLE_BRACKET_RIGHT] = ">",
    [TOKEN_ANGLE_BRACKET_RIGHT_EQUAL] = ">=",
    [TOKEN_KEYWORD_IF] = "if",
    [TOKEN_KEYWORD_FN] = "fn",
    [TOKEN_KEYWORD_STRUCT] = "struct",
    [TOKEN_KEYWORD_VAR] = "var",
    [TOKEN_KEYWORD_FOR] = "for",
    [TOKEN_KEYWORD_WHILE] = "while",
    [TOKEN_KEYWORD_IN] = "in",
    [TOKEN_KEYWORD_AS] = "as",
    [TOKEN_KEYWORD_TRUE] = "true",
    [TOKEN_KEYWORD_FALSE] = "false",
    [TOKEN_KEYWORD_NULL] = "null",
    [TOKEN_KEYWORD_CONST] = "const",
    [TOKEN_KEYWORD_RETURN] = "return",
    [TOKEN_KEYWORD_INLINE] = "inline",
    [TOKEN_KEYWORD_EXTERN] = "extern",
    [TOKEN_KEYWORD_EXPORT] = "export",
};

#define TOKEN_STRING_CASE(token) case TOKEN_##token: return ""#token""; break

Token new_token(char *value, Token_Type type, Loc loc) {
    Token token = {0};
    token.value = value;
    token.type = type;
    token.loc = loc;

    return token;
}

char *token_type_to_str(Token token) {
    switch (token.type){
        TOKEN_STRING_CASE(INVALID);
        TOKEN_STRING_CASE(IDENTIFIER);
        TOKEN_STRING_CASE(STRING_LITERAL);
        TOKEN_STRING_CASE(CHAR_LITERAL);
        TOKEN_STRING_CASE(INTEGER_LITERAL);
        TOKEN_STRING_CASE(FLOAT_LITERAL);
        TOKEN_STRING_CASE(EOF);
        TOKEN_STRING_CASE(HASH);
        TOKEN_STRING_CASE(BANG);
        TOKEN_STRING_CASE(EQUAL);
        TOKEN_STRING_CASE(EQUAL_EQUAL);
        TOKEN_STRING_CASE(BANG_EQUAL);
        TOKEN_STRING_CASE(LPAREN);
        TOKEN_STRING_CASE(RPAREN);
        TOKEN_STRING_CASE(SEMICOLON);
        TOKEN_STRING_CASE(DOT);
        TOKEN_STRING_CASE(LBRACE);
        TOKEN_STRING_CASE(RBRACE);
        TOKEN_STRING_CASE(LBRACKET);
        TOKEN_STRING_CASE(RBRACKET);
        TOKEN_STRING_CASE(PLUS);
        TOKEN_STRING_CASE(PLUS_PLUS);
        TOKEN_STRING_CASE(PLUS_EQUAL);
        TOKEN_STRING_CASE(MINUS);
        TOKEN_STRING_CASE(MINUS_EQUAL);
        TOKEN_STRING_CASE(ASTERISK);
        TOKEN_STRING_CASE(ASTERISK_EQUAL);
        TOKEN_STRING_CASE(COLON);
        TOKEN_STRING_CASE(COLON_EQUAL);
        TOKEN_STRING_CASE(COLON_COLON);
        TOKEN_STRING_CASE(COMMA);
        TOKEN_STRING_CASE(QUESTION_MARK);
        TOKEN_STRING_CASE(DOLLAR_SIGN);
        TOKEN_STRING_CASE(ANGLE_BRACKET_LEFT);
        TOKEN_STRING_CASE(ANGLE_BRACKET_LEFT_EQUAL);
        TOKEN_STRING_CASE(ANGLE_BRACKET_RIGHT);
        TOKEN_STRING_CASE(ANGLE_BRACKET_RIGHT_EQUAL);
        TOKEN_STRING_CASE(KEYWORD_EXTERN);
        TOKEN_STRING_CASE(KEYWORD_IF);
        TOKEN_STRING_CASE(KEYWORD_FN);
        TOKEN_STRING_CASE(KEYWORD_INLINE);
        TOKEN_STRING_CASE(KEYWORD_RETURN);
        TOKEN_STRING_CASE(KEYWORD_EXPORT);
    }

    assert(0 && "invalid token");
}
