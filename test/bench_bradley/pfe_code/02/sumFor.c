#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int sum(int n, int * s) {
  int i;

  // check for malformed input
  if (n <= 0 || s == NULL) return -1;

  *s = 0;
  for (i = 1; i <= n; i++)
    *s += i;

  return 0;
}

int main() {
  // test the sum function
  int s, err;
  err = sum(5, &s);
  assert (err == 0);
  // print the result to the console
  printf("%d\n", s);
  // test bad input
  err = sum(-3, &s);
  assert (err != 0);
  return 0;
}
