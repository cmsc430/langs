#ifndef TYPES_H
#define TYPES_H

/*
 * Bit layout of runtime values
 *
 * Values are either:
 *   - immediates, tagged with low bits ending in #b000
 *   - pointers, tagged with one of the pointer tags below
 *
 * Immediates include:
 *   - integers
 *   - characters
 *   - booleans
 *   - eof
 *   - void
 *   - empty list
 */

/* low bits reserved for pointer/immediate discrimination */
#define imm_shift        3
#define ptr_type_mask    ((1 << imm_shift) - 1)

/* pointer tags */
#define box_type_tag     1
#define cons_type_tag    2
#define vect_type_tag    3
#define str_type_tag     4
#define proc_type_tag    5
#define symb_type_tag    6
#define struct_type_tag  7

/* integer immediates */
#define int_shift        (1 + imm_shift)
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))

/* character immediates */
#define char_shift       (int_shift + 1)
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)

/* distinguished immediate constants */
#define val_true  ((0 << char_shift) | nonchar_type_tag)
#define val_false ((1 << char_shift) | nonchar_type_tag)
#define val_eof   ((2 << char_shift) | nonchar_type_tag)
#define val_void  ((3 << char_shift) | nonchar_type_tag)
#define val_empty ((4 << char_shift) | nonchar_type_tag)

#endif /* TYPES_H */
