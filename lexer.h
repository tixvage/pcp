#ifndef _LEXER_H
#define _LEXER_H

#include <stdint.h>

#include "token.h"

typedef struct Lexer {
    char *src;
    uint32_t src_len;
    uint32_t i;
    char c;
} Lexer;

void init_lexer(Lexer *lexer, char *src);
Token get_next_token(Lexer *lexer);

#endif
