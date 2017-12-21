#include <stdint.h>
#include <stdlib.h>

uintmax_t decode(const char * input, unsigned base);

/// Encode number to a string.
/// @return 0 on success, error code on error.
int encode(uintmax_t number, unsigned base, char * output, size_t outputSize);
