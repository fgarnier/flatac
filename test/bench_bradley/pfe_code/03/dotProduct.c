#include <stdio.h>
#include <stdlib.h>

int dotProduct(int x[], int y[], int n, int * dp) {
  // check if input is well-formed
  if (x == NULL || y == NULL || dp == NULL)
    return -1;
  if (n <= 0)
    return -2;

  // compute the dot product
  *dp = 0;
  int i;
  for (i = 0; i < n; i++)
    *dp += x[i] * y[i];

  // indicate success
  return 0;
}

int main() {
  int a[] = {0, 1, 2, 3};
  int b[] = {0, -1, -2, -3};
  int dp;
  dotProduct(a, b, 4, &dp);
  printf("%d (should be -14)\n", dp);
  return 0;
}
