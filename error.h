#ifndef _ERROR_H
#define _ERROR_H

#include <stdlib.h>

#include "token.h"

#if defined(__GNUC__) || defined(__clang__)
#define PRINTF_FORMAT(STRING_INDEX, FIRST_TO_CHECK) __attribute__ ((format (printf, STRING_INDEX, FIRST_TO_CHECK)))
#else
#define PRINTF_FORMAT(STRING_INDEX, FIRST_TO_CHECK)
#endif

typedef enum Error_Level {
    ERROR_NOTE,
    ERROR_WARNING,
    ERROR_FATAL,
} Error_Level;

void error_msg(Loc loc, Error_Level level, const char *fmt, ...) PRINTF_FORMAT(3, 4);

#endif
