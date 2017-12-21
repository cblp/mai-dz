#ifndef convert_ui_h
#define convert_ui_h

/// User choice in ui_menu().
/// If `tag` is `CHOICE_CONVERT`,
/// fields `baseFrom` and `baseTo` must be set
/// to the input and output bases respectively.
typedef struct Choice {
    enum {
        CHOICE_ERROR,
        CHOICE_CONVERT,
        CHOICE_EXIT,
    } tag;
    unsigned baseFrom;
    unsigned baseTo;
} Choice;

/// Conversion dialog
int ui_convert(unsigned baseFrom, unsigned baseTo);

/// Main menu dialog
Choice ui_menu();

#endif // convert_ui_h
