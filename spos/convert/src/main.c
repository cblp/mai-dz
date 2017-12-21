#include <errno.h>
#include <stdio.h>

#include "convert.h"
#include "ui.h"

int main() {
    Choice choice;
    while (1) {
        choice = ui_menu();
        switch (choice.tag) {
            case CHOICE_CONVERT: {
                int err = ui_convert(choice.baseFrom, choice.baseTo);
                if (err) {
                    errno = err;
                    perror("Error during conversion");
                    return 2;
                }
                break;
            }
            case CHOICE_EXIT:
                return 0;
            case CHOICE_ERROR:
                fprintf(stderr, "Bad choice\n");
                return 1;
        }
    }
}
