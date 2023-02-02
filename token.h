#ifndef _TOKEN_H
#define _TOKEN_H

typedef enum Token_Type {
    TOKEN_INVALID,
    TOKEN_EOF,
    TOKEN_IDENTIFIER,
    TOKEN_STRING_LITERAL,
    TOKEN_CHAR_LITERAL,
    TOKEN_INTEGER_LITERAL,
    TOKEN_FLOAT_LITERAL,
    TOKEN_BANG,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_BANG_EQUAL,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_SEMICOLON,
    TOKEN_DOT,
    TOKEN_HASH,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_PLUS,
    TOKEN_PLUS_PLUS,
    TOKEN_PLUS_EQUAL,
    TOKEN_MINUS,
    TOKEN_MINUS_MINUS,
    TOKEN_MINUS_EQUAL,
    TOKEN_ASTERISK,
    TOKEN_ASTERISK_ASTERISK,
    TOKEN_ASTERISK_EQUAL,
    TOKEN_SLASH,
    TOKEN_SLASH_EQUAL,
    TOKEN_COLON,
    TOKEN_COLON_EQUAL,
    TOKEN_COLON_COLON,
    TOKEN_COMMA,
    TOKEN_QUESTION_MARK,
    TOKEN_DOLLAR_SIGN,
    TOKEN_ANGLE_BRACKET_LEFT,
    TOKEN_ANGLE_BRACKET_LEFT_EQUAL,
    TOKEN_ANGLE_BRACKET_RIGHT,
    TOKEN_ANGLE_BRACKET_RIGHT_EQUAL,
    KEYWORD_START,
    TOKEN_KEYWORD_VAR,
    TOKEN_KEYWORD_FN,
    TOKEN_KEYWORD_CONST,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_EXTERN,
    TOKEN_KEYWORD_INLINE,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_EXPORT,
    COUNT_TOKEN,
} Token_Type;

typedef struct Token {
    char *value;
    Token_Type type;
} Token;

extern char* KEYS[COUNT_TOKEN];

Token new_token(char *value, Token_Type type);
char *token_type_to_str(Token token);

#endif