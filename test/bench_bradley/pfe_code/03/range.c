#include <assert.h>
#include <stdlib.h>

// Insert minmax here.

int range(int * a, int n, int * rng) {
  if (!a || !rng || n <= 0) return -1;

  int min, max;
  minmax(a, n, &min, &max);
  *rng = max - min;

  return 0;
}

int main() {
  // initializes a to a constant array of 5 elements
  int a[] = {7, -1, 13, -3, 9};
  int r;
  // test main functionality
  range(a, 5, &r);
  assert (r == 16);
  // corner cases
  assert (!range(NULL, 5, &r));
  assert (!range(a, 5, NULL));
  assert (!range(a, 0, &r));
  return 0;  
}
