#include <assert.h>

int sign(int x) {
  if (x < 0)       return -1;
  else if (x == 0) return 0;
  else             return 1;
}

int main() {
  assert (sign(-3) < 0);
  assert (sign(0) == 0);
  assert (sign(42) > 0);
  return 0;
}
