#ifndef RUNTIME_H
#define RUNTIME_H

#include "values.h"

/*
 * Entry point for compiled programs.
 *
 * The caller supplies the heap pointer. Compiled code may use this as its
 * initial allocation pointer / runtime heap base according to the language's
 * calling convention.
 */
val_t entry(val_t *heap);

/*
 * Language-facing runtime operations used by compiled code.
 *
 * These are implemented by the runtime core, typically in terms of lower-level
 * host hooks declared in host.h.
 */
val_t read_byte(void);
val_t peek_byte(void);
val_t write_byte(val_t);

_Noreturn void raise_error(void);

#endif /* RUNTIME_H */
