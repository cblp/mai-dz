#ifndef cblp_maidz_clientserver_common_h
#define cblp_maidz_clientserver_common_h

#include <stdio.h>
#include <stdlib.h>

#define ASSERT(cond) {                          \
    if (! (cond)) {                             \
        perror("assertion failure: " #cond);    \
        exit(EXIT_FAILURE);                     \
    }                                           \
}

#define ASSERT_OK(code) {   \
    int result = (code);    \
    if (result != 0) {      \
        perror(#code);      \
        exit(EXIT_FAILURE); \
    }                       \
}

enum {
    Port = 8000,
};

typedef struct {
    int id, a, b;
} Request;

typedef struct {
    int id, r;
} Response;

#endif // cblp_maidz_clientserver_common_h
