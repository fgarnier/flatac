#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int sum(int n, int * s) {
  int i = 1;

  // check for well-formed input
  if (n <= 0 || s == NULL)
    // indicate malformed input
    return -1;

  *s = 0;
  while (i <= n) {
    *s += i;  // short for *s = *s + i;
    i++;      // short for i = i + 1;
  }
  // indicate successful execution
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
