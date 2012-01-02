#include <assert.h>
#include <stdlib.h>

int swap(int * x, int * y) {
  if (x == NULL || y == NULL) return -1;
  int t = *x;
  *x = *y;
  *y = t;
  return 0;
}

int main() {
  int a = 0, b = 1;
  int rv = swap(&a, &b);
  assert (rv == 0);
  assert (a == 1);
  assert (b == 0);
  rv = swap(&a, NULL);
  assert (rv != 0);
  assert (a == 1);
  return 0;
}
