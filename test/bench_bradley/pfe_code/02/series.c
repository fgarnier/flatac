#include <stdio.h>

int series(int n) {
  int i, a = 1, sum = 1;
  for (i = 1; i <= n; i++) {
    a = 2*a + 1;
    sum += a;
  }
  return sum;
}

int main() {
  printf("1: %d\n4: %d\n", series(1), series(4));
  return 0;
}
