#include <assert.h>
#include <stdlib.h>

/* Given an array, fib, with length n, computes the first n
 * elements of the Fibonacci sequence.  Returns 0 to 
 * indicate success and negative values for bad input.
 */
int fibonacci(int * fib, int n) {
  // check for well-formed input
  if (fib == NULL)
    return -1;
  if (n <= 0)
    return -2;

  fib[0] = 1;
  if (n >= 2)
    fib[1] = 1;
  int i;
  for (i = 2; i < n; i++)
    fib[i] = fib[i-2] + fib[i-1];

  // indicate successful computation
  return 0;
}

#define N 3

int main() {
  int a[N];
  int error;
  error = fibonacci(a, N);
  assert (!error);
  return 0;
}
