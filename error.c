#include "error.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>

const char *level_to_str(Error_Level level) {
    switch (level) {
        case ERROR_NOTE: {
            return "note";
        } break;
        case ERROR_WARNING: {
            return "warning";
        } break;
        case ERROR_FATAL: {
            return "error";
        } break;
        default: {
            assert(0 && "unreachable");
        } break;
    }
}

void error_msg(Loc loc, Error_Level level, const char *fmt, ...) {
    fprintf(stderr, "%s:%d:%d: %s: ", loc.path, loc.col, loc.row, level_to_str(level));

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    fprintf(stderr, "\n");
}
