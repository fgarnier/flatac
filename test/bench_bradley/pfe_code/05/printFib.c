#include <assert.h>
#include <stdio.h>
#include <string.h>

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

#define MAX_N 46

void printUsage() {
  printf("Usage: [-b <bound>] [-h]\n  where <bound>"
         " is a number between 0 and 46\n");
}

int main(int argc, char ** argv) {
  int n = MAX_N;
  int fib[MAX_N];
  int i = 0, numRead = 0;

  // parse command line, skipping argv[0] (program's name)
  for (i = 1; i < argc; i++) {
    // strcmp, defined in string.h, returns 0 if the two 
    // strings are equal
    if (strcmp("-h", argv[i]) == 0) {
      // user requested usage message
      printUsage();
    }
    else if (strcmp("-b", argv[i]) == 0) {
      if (i+1 == argc) {
        // -b should be followed by another argument
        printUsage();
        return -1;
      }
      // convert the next argument into the integer n
      numRead = sscanf(argv[i+1], "%d", &n);
      i++;
      // numRead == 0 if the next argument isn't an integer
      if (numRead == 0 || n < 0 || n > MAX_N) {
        printUsage();
        return -1;
      }
      assert (numRead == 1);
    }
    else {
      // unrecognized argument
      printUsage();
      return -1;
    }
  }

  fibonacci(fib, n);
  printIntArray(fib, n);
  return 0;
}
