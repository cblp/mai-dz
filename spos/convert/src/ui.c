#include <errno.h>
#include <stdio.h>
#include <strings.h>

#include "convert.h"
#include "ui.h"

int ui_convert(unsigned baseFrom, unsigned baseTo) {
    printf("Enter the number:\n");
    char input[1000];
    bzero(input, sizeof(input));
    char * fgets_result = fgets(input, sizeof(input), stdin);
    if (fgets_result == NULL)
        return EINVAL;
    if (input[sizeof(input) - 1])
        return ENOMEM;

    uintmax_t number = decode(input, baseFrom);
    if (errno)
        return errno;

    char output[1000];
    bzero(output, sizeof(output));
    int err = encode(number, baseTo, output, sizeof(output));
    if (err)
        return err;
    if (input[sizeof(input) - 1])
        return ENOMEM;

    printf("Result: %s\n", output);
    return 0;
}

Choice ui_menu() {
    printf(
        "This program converts numbers from one numeral system into another\n"
        "1. from 16 to 10\n"
        "2. from 8 to 10\n"
        "3. from 10 to 2\n"
        "4. exit\n"
        "> "
    );

    char buffer[1000];
    bzero(buffer, sizeof(buffer));
    char * fgets_result = fgets(buffer, sizeof(buffer), stdin);
    if (fgets_result == NULL || buffer[sizeof(buffer) - 1] != 0)
        return (Choice){.tag = CHOICE_ERROR};

    unsigned choice;
    int sscanf_result = sscanf(buffer, "%u", &choice);
    if (sscanf_result != 1)
        return (Choice){.tag = CHOICE_ERROR};

    switch (choice) {
        case 1:
            return (Choice)
                {.tag = CHOICE_CONVERT, .baseFrom = 16, .baseTo = 10};
        case 2:
            return (Choice)
                {.tag = CHOICE_CONVERT, .baseFrom = 8, .baseTo = 10};
        case 3:
            return (Choice)
                {.tag = CHOICE_CONVERT, .baseFrom = 10, .baseTo = 2};
        case 4:
            return (Choice){.tag = CHOICE_EXIT};
    }
    return (Choice){.tag = CHOICE_ERROR};
}
