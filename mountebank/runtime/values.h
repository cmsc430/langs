#ifndef VALUES_H
#define VALUES_H

#include <stdint.h>

/*
 * Abstract runtime value.
 *
 * All language values are represented as a tagged 64-bit word.
 */
typedef int64_t val_t;

/*
 * Dynamic type tags used by the runtime and printing code.
 */
typedef enum type_t {
  T_INVALID = -1,

  /* immediates */
  T_INT,
  T_BOOL,
  T_CHAR,
  T_EOF,
  T_VOID,
  T_EMPTY,

  /* heap objects */
  T_BOX,
  T_CONS,
  T_VECT,
  T_STR,
  T_SYMB,
  T_PROC,
  T_STRUCT,
} type_t;

typedef uint32_t val_char_t;

/*
 * Heap object layouts.
 *
 * These layouts correspond to the pointer-tagged representations in types.h.
 */
typedef struct val_box_t {
  val_t val;
} val_box_t;

typedef struct val_cons_t {
  val_t snd;
  val_t fst;
} val_cons_t;

typedef struct val_vect_t {
  uint64_t len;
  val_t elems[];
} val_vect_t;

typedef struct val_str_t {
  uint64_t len;
  val_char_t codepoints[];
} val_str_t;

typedef struct val_symb_t {
  uint64_t len;
  val_char_t codepoints[];
} val_symb_t;

typedef struct val_struct_t {
  val_t name;
  val_t *vals;
} val_struct_t;

/*
 * Classify a runtime value.
 */
type_t val_typeof(val_t x);

/*
 * Wrap/unwrap operations.
 *
 * The behavior of unwrap functions is undefined on type mismatch.
 */

/* integers */
int64_t val_unwrap_int(val_t x);
val_t val_wrap_int(int64_t i);

/* booleans */
int val_unwrap_bool(val_t x);
val_t val_wrap_bool(int b);

/* characters */
val_char_t val_unwrap_char(val_t x);
val_t val_wrap_char(val_char_t c);

/* special values */
val_t val_wrap_eof(void);
val_t val_wrap_void(void);

/* heap objects */
val_box_t   *val_unwrap_box(val_t x);
val_t        val_wrap_box(val_box_t *b);

val_cons_t  *val_unwrap_cons(val_t x);
val_t        val_wrap_cons(val_cons_t *c);

val_vect_t  *val_unwrap_vect(val_t x);
val_t        val_wrap_vect(val_vect_t *v);

val_str_t   *val_unwrap_str(val_t x);
val_t        val_wrap_str(val_str_t *v);

val_symb_t  *val_unwrap_symb(val_t x);
val_t        val_wrap_symb(val_symb_t *v);

val_struct_t *val_unwrap_struct(val_t x);
val_t         val_wrap_struct(val_struct_t *v);

#endif /* VALUES_H */
