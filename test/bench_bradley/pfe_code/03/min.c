#include <assert.h>
#include <stdlib.h>

int min(int * a, int n, int * min) {
  if (a == NULL || min == NULL) 
    return -1;
  if (n <= 0) 
    return -2;
  // the minimum value so far is at position 0
  int m = a[0], i;
  for (i = 1; i < n; i++)
    if (a[i] < m)
      // a[i] is even smaller than previously seen elements
      m = a[i];
  *min = m;
  return 0;
}

int main() {
  // initializes a to a constant array of 5 elements
  int a[] = {7, -1, 13, -3, 9};
  int x;
  min(a, 5, &x);
  assert (x == -3);
  // corner cases
  assert (min(a, 0, &x) != 0);
  assert (min(NULL, 0, &x) != 0);
  assert (min(a, 0, NULL) != 0);
  return 0;
}
