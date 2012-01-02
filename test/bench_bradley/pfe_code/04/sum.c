#include <assert.h>
#include <stdio.h>

#define N 5

int _sum(int n) {
  assert (n > 0);
  if (n == 1)
    return 0;
  else {
    int upto = _sum(n-1);
    return upto + n;
  }
}

int sum(int n, int * s) {
  if (n <= 0) return -1;
  if (!s) return -2;
  *s = _sum(n);
  return 0;
}

int main() {
  int s;
  int err = sum(N, &s);
  assert (!err);
  printf("%d\n", s);
  return 0;
}
