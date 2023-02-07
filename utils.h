#ifndef _UTILS_H
#define _UTILS_H

#define array_push(array, element) do { \
    if ((array).data == NULL) { \
        (array).data = malloc(sizeof((array).data[0])); \
        (array).len = 1; \
        (array).data[(array).len - 1] = element; \
    } else { \
        (array).len += 1; \
        (array).data = realloc((array).data, sizeof((array).data[0]) * ((array).len)); \
        (array).data[(array).len - 1] = element; \
    } \
} while(0)

#define array_copy(array, copy) do { \
    (array).data = malloc(sizeof((array).data[0]) * copy.len); \
    (array).data = memcpy((array).data, copy.data, sizeof((array).data[0]) * copy.len); \
    (array).len = copy.len; \
} while(0)

#define array_append(array, x, l) do { \
    for (int i = 0; i < l; i++) { \
        array_push(array, x[i]); \
    } \
} while(0)

#define BIT(x) (1<<(x))

#endif
