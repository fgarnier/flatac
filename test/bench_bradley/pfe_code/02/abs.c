#include <assert.h>

int abs(int a) {
  if (a < 0) return -a;
  return a;
}

int main() {
  int x = -3;
  int y = abs(x);
  assert (x == y || -x == y);
  assert (y >= 0);
  x = abs(y);
  assert (y == x);
  return 0;
}
