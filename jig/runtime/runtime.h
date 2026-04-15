#ifndef RUNTIME_H
#define RUNTIME_H

#include "values.h"

val_t entry(val_t *heap);

val_t read_byte(void);
val_t peek_byte(void);
val_t write_byte(val_t);

_Noreturn void raise_error(void);

#endif /* RUNTIME_H */
