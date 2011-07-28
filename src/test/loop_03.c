#include <stdlib.h>

int main () {
  int i = 0;
  int *y = (int*)malloc(sizeof(int)); *y = 0;

  for ( i = 0; i != 100; i = *y ) {
    int *x = (int*)malloc(sizeof(int)); *x = 1;
    *y = *y + *x;
    free(x);

    if (i < 0) free(y); // UNREACHEABLE
  }

  free(y);
}
