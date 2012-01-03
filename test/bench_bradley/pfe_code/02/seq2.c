#include <stdio.h>

int seq(int n) {
  int i, a = 1, b = 1;
  for (i = 2; i <= n; i++) {
    int t = b;  // temporary variable
    b = a + b;
    a = t; 
  }
  return b;
}

int main() {
  printf("3: %d\n5: %d\n", seq(3), seq(5));
  return 0;
}
