#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

char *read_entire_file(const char *filename) {
    FILE* f;
    char* text;
    long len;

    f = fopen(filename, "rb");
    if (f == NULL) {
        fprintf(stderr, "error: failed to open file at `%s`\n", filename);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    len = ftell(f);
    assert(len > 0);
    fseek(f, 0, SEEK_SET);
    text = calloc(len + 1, 1);
    assert(text != NULL);
    fread(text, 1, len, f);
    assert(strlen(text) > 0);
    fclose(f);

    return text;
}
