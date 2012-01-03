#include "complex.h"
#include <stdio.h>
#include <stdlib.h>

// represents a complex number as a pair of doubles
struct _complex {
  double r;  // the real part
  double i;  // the imaginary part
};

complex newComplex() {
  complex c = (complex) malloc(sizeof(struct _complex));
  c->r = 0.0;
  c->i = 0.0;
  return c;
}

void deleteComplex(complex c) {
  free(c);
}

void setReal(complex c, double r) {
  c->r = r;
}

void setImaginary(complex c, double i) {
  c->i = i;
}

double getReal(complex c) {
  return c->r;
}

double getImaginary(complex c) {
  return c->i;
}

void multiplyByReal(complex c, double r) {
  c->r = r * c->r;
  c->i = r * c->i;
}

void addTo(complex a, complex b) {
  a->r += b->r;
  a->i += b->i;
}

void multiplyBy(complex a, complex b) {
  double r = a->r * b->r - a->i * b->i;
  double i = a->r * b->i + a->i * b->r;
  a->r = r;
  a->i = i;
}

void printComplex(complex c) {
  printf("%f + %fi", c->r, c->i);
}
