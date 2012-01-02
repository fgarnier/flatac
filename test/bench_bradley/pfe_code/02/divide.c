#include <assert.h>
#include <stdlib.h>

int divide(int dividend, int divisor,
           int * quotient, int * remainder) {
  if (divisor <= 0 ||
      quotient == NULL ||
      remainder == NULL) {
    // error: malformed input
    return -1;
  }
  else {
    *quotient = dividend / divisor;
    *remainder = dividend % divisor;
    // successful computation
    return 0;
  }
}

int main() {
  int q, r;
  int errorCode = divide(7, 3, &q, &r); 
  assert (!errorCode);
  return 0;
}
