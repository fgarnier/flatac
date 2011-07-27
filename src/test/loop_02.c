#include <stdlib.h>

int main() {
  int i = 0; // DEBUG PURPOSE
  int *x, *y, *z;

  while ( i < 100 ) {
    if ( i % 2 == 0 ) x = y = z = (int*)malloc(sizeof(int));
    else free(x);
    ++i;
  }
}
