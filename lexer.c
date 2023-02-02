#include "lexer.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

void lexer_advance(Lexer *lexer);
Token lexer_advance_with(Lexer *lexer, Token token);
Token lexer_advance_current(Lexer *lexer, Token_Type type);
Token lexer_advance_two(Lexer *lexer, Token_Type type);
void lexer_skip_ws(Lexer *lexer);
char lexer_peek(Lexer *lexer, int offset);
Token lex_id(Lexer *lexer);
Token lex_number(Lexer *lexer);

void init_lexer(Lexer *lexer, char *src, const char *filepath) {
    lexer->src_len = strlen(src);
    lexer->src = src;
    lexer->i = 0;
    lexer->c = src[0];
    lexer->current_loc = (Loc){1, 1, filepath};
}

void lexer_advance(Lexer *lexer) {
    if (lexer->i < lexer->src_len && lexer->c != '\0') {
        if (lexer->c == '\n') {
            lexer->current_loc.col += 1;
            lexer->current_loc.row = 0;
        }
        lexer->i += 1;
        lexer->c = lexer->src[lexer->i];
        if (lexer->c != '\n') {
            lexer->current_loc.row += 1;
        }
    }
}

Token lexer_advance_with(Lexer *lexer, Token token) {
    lexer_advance(lexer);
    return token;
}

Token lexer_advance_current(Lexer *lexer, Token_Type type) {
    const char value[] = {lexer->c, '\0'};

    char *buffer = calloc(2, sizeof(char));
    strcpy(buffer, value);

    Token token = new_token(buffer, type, lexer->current_loc);
    lexer_advance(lexer);

    return token;
}

Token lexer_advance_two(Lexer *lexer, Token_Type type) {
    return lexer_advance_with(lexer, lexer_advance_with(lexer, new_token(KEYS[type], type, lexer->current_loc)));
}

void lexer_skip_ws(Lexer *lexer) {
    while (isspace(lexer->c)) lexer_advance(lexer);
}

char lexer_peek(Lexer *lexer, int offset) {
    return lexer->src[lexer->i + offset];
}

Token lex_id(Lexer *lexer) {
    char *value = calloc(1, sizeof(char));
    Loc loc = lexer->current_loc;

    while (isalpha(lexer->c) || isdigit(lexer->c) || lexer->c == '_') {
        value = realloc(value, (strlen(value) + 2) * sizeof(char));
        char final[] = {lexer->c, 0};
        strcat(value, final);
        lexer_advance(lexer);
    }

    for (int i = KEYWORD_START + 1; i < COUNT_TOKEN; i++) {
        if (strcmp(KEYS[i], value) == 0) return new_token(value, i, loc);
    }

    return new_token(value, TOKEN_IDENTIFIER, loc);
}

Token lex_number(Lexer *lexer) {
    char *value = calloc(1, sizeof(char));
    Loc loc = lexer->current_loc;

    while (isdigit(lexer->c)) {
        value = realloc(value, (strlen(value) + 2) * sizeof(char));
        char final[] = {lexer->c, 0};
        strcat(value, final);
        lexer_advance(lexer);
    }

    return new_token(value, TOKEN_INTEGER_LITERAL, loc);
}

Token lex_string(Lexer *lexer) {
    char *value = calloc(1, sizeof(char));
    Loc loc = lexer->current_loc;
    
    lexer_advance(lexer);
    bool escaped = false;
    char current_char = 0;

    while (lexer->c != '"' && !escaped) {
        current_char = lexer->c;

        if (lexer->c == '\\') {
            escaped = true;
            lexer_advance(lexer);
        }
        
        if (escaped) {
            if (lexer->c == 'n') current_char = 'n';
            else if (lexer->c == 't') current_char = 't';
            else if (lexer-> c == '"') current_char = '"';
            else {
                printf("Invalid escape character: '%c'\n", lexer->c);
                exit(1); // TODO: errors
            }
            value = realloc(value, (strlen(value) + 2) * sizeof(char));
            char final[] = {'\\', 0};
            strcat(value, final);
            escaped = false;
        }

        value = realloc(value, (strlen(value) + 2) * sizeof(char));
        char final[] = {current_char, 0};
        strcat(value, final);
        lexer_advance(lexer);
    }

    lexer_advance(lexer);
    
    return new_token(value, TOKEN_STRING_LITERAL, loc);
}

void lexer_skip_comment(Lexer *lexer) {
    while (lexer->c != '\n' && lexer->c != '\0') {
        lexer_advance(lexer);
    }
    lexer_advance(lexer);
    //assert(0 && "Not implemented yet");
}

Token get_next_token(Lexer *lexer) {
    while (lexer->c != '\0') {
        lexer_skip_ws(lexer);
        if (isalpha(lexer->c) || lexer->c == '_') return lex_id(lexer);
        else if (isdigit(lexer->c)) return lex_number(lexer);
        else if (lexer->c == '\"') return lex_string(lexer);

        switch (lexer->c) {
            case '!':
                if (lexer_peek(lexer, 1) == '=') return lexer_advance_two(lexer, TOKEN_BANG_EQUAL);
                return lexer_advance_current(lexer, TOKEN_BANG);
                break;
            case ':':
                if (lexer_peek(lexer, 1) == ':') return lexer_advance_two(lexer, TOKEN_COLON_COLON);
                else if (lexer_peek(lexer, 1) == '=') return lexer_advance_two(lexer, TOKEN_COLON_EQUAL);
                return lexer_advance_current(lexer, TOKEN_COLON);
                break;
            case '+':
                if (lexer_peek(lexer, 1) == '=') return lexer_advance_two(lexer, TOKEN_PLUS_EQUAL);
                return lexer_advance_current(lexer, TOKEN_PLUS);
                break;
            case '-':
                if (lexer_peek(lexer, 1) == '=') return lexer_advance_two(lexer, TOKEN_MINUS_EQUAL);
                return lexer_advance_current(lexer, TOKEN_MINUS);
                break;
            case '=':
                if (lexer_peek(lexer, 1) == '=') return lexer_advance_two(lexer, TOKEN_EQUAL_EQUAL);
                return lexer_advance_current(lexer, TOKEN_EQUAL);
                break;
            case '(':
                return lexer_advance_current(lexer, TOKEN_LPAREN);
                break;
            case ')':
                return lexer_advance_current(lexer, TOKEN_RPAREN);
                break;
            case '{':
                return lexer_advance_current(lexer, TOKEN_LBRACE);
                break;           
            case '}':
                return lexer_advance_current(lexer, TOKEN_RBRACE);
                break;
            case '[':
                return lexer_advance_current(lexer, TOKEN_LBRACKET);
                break;
            case ']':
                return lexer_advance_current(lexer, TOKEN_RBRACKET);
                break;
            case ',':
                return lexer_advance_current(lexer, TOKEN_COMMA);
                break;
            case ';':
                return lexer_advance_current(lexer, TOKEN_SEMICOLON);
                break;
            case '.':
                return lexer_advance_current(lexer, TOKEN_DOT);
                break;
            case '#':
                return lexer_advance_current(lexer, TOKEN_HASH);
                break;
            case '*':
                return lexer_advance_current(lexer, TOKEN_ASTERISK);
                break;
            case '>':
                return lexer_advance_current(lexer, TOKEN_ANGLE_BRACKET_LEFT);
                break;
            case '$':
                return lexer_advance_current(lexer, TOKEN_DOLLAR_SIGN);
                break;
            case '?':
                return lexer_advance_current(lexer, TOKEN_QUESTION_MARK);
                break;
            case '/':
                if (lexer_peek(lexer, 1) == '/') {
                    lexer_skip_comment(lexer);
                    continue;
                }
                return lexer_advance_current(lexer, TOKEN_SLASH);
                break;
            case '\0': break;
            default: {
                printf("Unexpected character `%c`\n", lexer->c);
                exit(1);
            }
        }
    }

    return new_token(0, TOKEN_EOF, lexer->current_loc);
}
