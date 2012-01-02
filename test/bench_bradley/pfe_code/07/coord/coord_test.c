#include "coord.h"
#include <stdio.h>

int main() {
  coord c = newCoord();
  double th;

  setX(c, 1.0);
  // c is (1, 0), so th should be 0
  getTheta(c, &th);
  printf("%f\n", th);

  setY(c, .5);
  // c is (1, .5), so th should be atan(.5/1)
  getTheta(c, &th);
  printf("%f\n", th);

  setX(c, 0.0);
  setR(c, 1.0);
  // c is (0, 1)
  printf("%f %f\n", getX(c), getY(c));

  setTheta(c, 3.14159265);
  // c is (-1, 0)
  printf("%f %f\n", getX(c), getY(c));

  deleteCoord(c);

  return 0;
}
