#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

int main(int argc, char** argv)
{
  val_t result = entry();

  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  return 0;
}
