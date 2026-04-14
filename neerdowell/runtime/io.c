#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "runtime.h"

val_t read_byte(void)
{
  int c = getc(stdin);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int(c);
}

val_t peek_byte(void)
{
  int c = getc(stdin);
  if (c != EOF)
    ungetc(c, stdin);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int(c);
}

val_t write_byte(val_t c)
{
  int b = val_unwrap_int(c);
  putc((unsigned char)b, stdout);
  return val_wrap_void();
}
