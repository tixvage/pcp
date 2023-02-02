#ifndef _UTILS_H
#define _UTILS_H

#define array_push(array, element) do { \
    if (array.data == NULL) { \
        array.data = malloc(sizeof(array.data[0])); \
        array.len = 1; \
        array.data[array.len - 1] = element; \
    } else { \
        array.len += 1; \
        array.data = realloc(array.data, sizeof(array.data[0]) * (array.len)); \
        array.data[array.len - 1] = element; \
    } \
} while(0)

#endif
