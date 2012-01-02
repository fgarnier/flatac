#include <assert.h>
#include <stdio.h>

/* Given an array, fib, with length n, computes the first n
 * elements of the Fibonacci sequence.  Returns 0 to 
 * indicate success and negative values for bad input.
 */
int fibonacci(int * fib, int n) {
  // check for well-formed input
  if (!fib)
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

/* Prints the n integers of a to stdout.  Returns -1 if a 
 * is NULL or n < 0; otherwise, returns 0.
 */
int printIntArray(int a[], int n) {
  // check for well-formed input
  if (!a || n < 0) return -1;

  // print to stdout: one number per line
  int i;
  for (i = 0; i < n; i++) {
    // print the index and element followed by spaces
    printf("%d. %d  ", i, a[i]);
    // print a newline every 4'th entry
    if (i % 4 == 3)
      printf("\n");
  }
  // print a newline if one was not just printed
  if (i % 4 != 0)
    printf("\n");

  return 0;
}

#define N 5

int main() {
  int fib[N];
  int err = fibonacci(fib, N);
  assert (!err);
  err = printIntArray(fib, N);
  assert (!err);
  return 0;
}
