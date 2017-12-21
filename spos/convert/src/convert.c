#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "convert.h"

uintmax_t decode(const char * input, unsigned base) {
    uintmax_t result = 0;
    for (; *input; ++input) {
        const char digitChar = *input;
        unsigned digit;
        if (
            '0' <= digitChar && digitChar <= '9'
            && digitChar - '0' < (signed)base
        ) {
            digit = digitChar - '0';
        } else if (base > 10) {
            if (
                'a' <= digitChar && digitChar <= 'z'
                && digitChar - 'a' < (signed)base - 10
            ) {
                digit = digitChar - 'a' + 10;
            } else if (
                'A' <= digitChar && digitChar <= 'Z'
                && digitChar - 'A' < (signed)base - 10
            ) {
                digit = digitChar - 'A' + 10;
            } else {
                continue;
            }
        } else {
            continue;
        }
        result = result * base + digit;
    }
    return result;
}

int encode(
    const uintmax_t number, unsigned base, char * output, size_t outputSize
) {
    uintmax_t n = number;
    size_t magnitude = 0;
    do {
        n /= base;
        ++magnitude;
    } while (n);

    if (magnitude >= outputSize)
        return ENOMEM;

    n = number;
    size_t i = magnitude - 1;
    do {
        const uintmax_t digit = n % base;
        output[i] = '0' + digit;
        n /= base;
        --i;
    } while (n);
    output[magnitude] = 0;
    return 0;
}
