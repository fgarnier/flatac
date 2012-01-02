#include "complex.h"
#include <stdio.h>

int main() {
  complex c1 = newComplex();
  complex c2 = newComplex();

  // create c1 with value 1
  setReal(c1, 1.0);
  setImaginary(c1, 0.0);
  printComplex(c1);
  printf(", ");

  // create c2 with initial value i
  setReal(c2, 0.0);
  setImaginary(c2, 1.0);
  printComplex(c2);
  printf("\n");

  // set c1 = c1 * c2, which is i
  multiplyBy(c1, c2);
  printComplex(c1);
  printf("\n");

  // negate c1 so that it becomes -i
  multiplyByReal(c1, -1);
  printComplex(c1);
  printf("\n");

  // set c1 = c1 + c2, which is -i + i, or 0
  addTo(c1, c2);
  printComplex(c1);
  printf("\n");

  // clean up
  deleteComplex(c1);
  deleteComplex(c2);

  return 0;
}
