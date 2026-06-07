#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

void raise_error(void)
{
  printf("err\n");
  exit(1);
}
