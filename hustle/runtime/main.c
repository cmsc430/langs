#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "runtime.h"

/* in words */
#define heap_size 10000

int main(int argc, char **argv)
{
  val_t *heap = malloc(8 * heap_size);
  if (!heap) {
    fprintf(stderr, "out of memory\n");
    return 1;
  }
  
  val_t result = entry(heap);
  
  print_result(result);
  if (val_typeof(result) != T_VOID)
    putchar('\n');

  free(heap);
  return 0;
}
