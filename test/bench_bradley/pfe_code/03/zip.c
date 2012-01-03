#include <stdio.h>

int zip(int * a, int * b, int * c, int n) {
  if (!a || !b || !c) return -1;
  int i;
  for (i = 0; i < n; i++) {
    c[2*i] = a[i];
    c[2*i+1] = b[i];
  }
  return 0;
}

int main() {
  int a[] = {0, 1, 2};
  int b[] = {3, 4, 5};
  int c[6];
  zip(a, b, c, 3);
  int i;
  for (i = 0; i < 6; i++)
    printf("%d ", c[i]);
  printf("\n");
  return 0;
}
