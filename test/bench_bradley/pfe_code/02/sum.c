#include <assert.h>

int sum(int n) {
  int upto = 0;
  // n must be positive
  assert (n > 0);
  if (n == 1)
    // the sum of 1 is just 1
    return 1;
  else {
    // the sum 1 + ... + n == (the sum 1 + ... + n-1) + n
    upto = sum(n-1);
    return upto + n;
  }
}

int main() {
  int s = sum(3);
  return 0;
}
