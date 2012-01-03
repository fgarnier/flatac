#include <assert.h>
#include <stdlib.h>

void divide(int dividend, int divisor, 
            int * quotient, int * remainder) {
  assert (divisor > 0);
  assert (quotient != NULL);
  assert (remainder != NULL);
  *quotient = dividend / divisor;
  *remainder = dividend % divisor;
  assert (divisor * (*quotient) + (*remainder) == dividend);
}

int main() {
  int q, r;
  divide(7, 3, &q, &r);
  return 0;
}
