#include <stdio.h>

int seq(int n) {
  int i, a = 1;
  for (i = 1; i <= n; i++)
    a = 2*a + 1;
  return a;
}

int main() {
  printf("0: %d\n4: %d\n", seq(0), seq(4));
  return 0;
}
