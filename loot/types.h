#ifndef TYPES_H
#define TYPES_H

/*
  Important: must agree with types.rkt!

  Bit layout of values

  Values:
  - Immediates:   end in #b0
  - Pointers:     end in #b1
  Immediates:
  - Integers:   end in  #b00
  - Characters: end in #b010
  - #t:             #b000110
  - #f:             #b001110
  - eof:            #b010110
  - void:           #b011110
  - '():            #b100110

  Addresses are assumed to have 0s in two most significant bytes
  (canonical address form) So a tagged pointer shifts an address to
  the left by 16 and uses those 16 bits to tag the pointer type.
*/

#define imm_shift        1
#define ptr_type_mask    ((16 << imm_shift) - 1)

#define ptr_type_tag            1
#define box_immutable_type_tag  ((0 << imm_shift) | ptr_type_tag)
#define box_mutable_type_tag    ((1 << imm_shift) | ptr_type_tag)
#define cons_type_tag           ((2 << imm_shift) | ptr_type_tag)
#define proc_type_tag           ((3 << imm_shift) | ptr_type_tag)
#define vect_immutable_type_tag ((4 << imm_shift) | ptr_type_tag)
#define vect_mutable_type_tag   ((5 << imm_shift) | ptr_type_tag)
#define str_immutable_type_tag  ((6 << imm_shift) | ptr_type_tag)
#define str_mutable_type_tag    ((7 << imm_shift) | ptr_type_tag)
#define ptr_shift               16

#define int_shift        (1 + imm_shift)
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))
#define char_shift       (int_shift + 1)
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)
#define val_true  ((0 << char_shift) | nonchar_type_tag)
#define val_false ((1 << char_shift) | nonchar_type_tag)
#define val_eof   ((2 << char_shift) | nonchar_type_tag)
#define val_void  ((3 << char_shift) | nonchar_type_tag)
#define val_empty ((4 << char_shift) | nonchar_type_tag)

#endif
