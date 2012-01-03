#include <assert.h>
#include <stdlib.h>

void swap(int * x, int * y) {
  assert (x != NULL);
  assert (y != NULL);
  int t = *x;
  *x = *y;
  *y = t;
}

int main() {
  int a = 0, b = 1;
  swap(&a, &b);
  assert (a == 1);
  assert (b == 0);
  return 0;
}
