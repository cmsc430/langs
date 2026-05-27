#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

_Noreturn void raise_error(void)
{
  printf("err\n");
  exit(1);
}
