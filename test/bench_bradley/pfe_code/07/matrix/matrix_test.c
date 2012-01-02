#include <stdio.h>

#include "matrix.h"

int main() {
  matrix A, Ac, B, c, d, M, ct, mdp;
  double dp;

  A = newMatrix(3, 3);
  setElement(A, 1, 1, 1.0);
  setElement(A, 1, 2, .25);
  setElement(A, 1, 3, -.1);
  setElement(A, 2, 2, .4);
  setElement(A, 2, 3, .3);
  setElement(A, 3, 2, .1);
  setElement(A, 3, 3, -.3);
  printf("Matrix A:\n");
  printMatrix(A);

  Ac = copyMatrix(A);
  printf("\nCopy of A:\n");
  printMatrix(Ac);

  B = newMatrix(3, 3);
  setElement(B, 1, 1, .5);
  setElement(B, 2, 2, 2.0);
  setElement(B, 3, 3, 1.0);
  printf("\nMatrix B:\n");
  printMatrix(B);

  c = newMatrix(3, 1);
  setElement(c, 1, 1, 1.0);
  setElement(c, 3, 1, 1.0);
  printf("\nVector c:\n");
  printMatrix(c);

  d = newMatrix(3, 1);
  setElement(d, 2, 1, 1.0);
  setElement(d, 3, 1, 1.0);
  printf("\nVector d:\n");
  printMatrix(d);

  M = newMatrix(3, 3);
  transpose(A, M);
  printf("\nA':\n");
  printMatrix(M);

  ct = newMatrix(1, 3);
  transpose(c, ct);
  printf("\nc':\n");
  printMatrix(ct);

  sum(A, B, M);
  printf("\nA + B:\n");
  printMatrix(M);

  product(A, B, M);
  printf("\nA * B:\n");
  printMatrix(M);

  mdp = newMatrix(1, 1);
  product(ct, d, mdp);
  getElement(mdp, 1, 1, &dp);
  printf("\nDot product (1): %.2f\n", dp);

  dotProduct(c, d, &dp);
  printf("\nDot product (2): %.2f\n", dp);

  product(A, c, d);
  printf("\nA * c:\n");
  printMatrix(d);

  deleteMatrix(A);
  deleteMatrix(Ac);
  deleteMatrix(B);
  deleteMatrix(c);
  deleteMatrix(d);
  deleteMatrix(M);
  deleteMatrix(ct);
  deleteMatrix(mdp);

  return 0;
}
