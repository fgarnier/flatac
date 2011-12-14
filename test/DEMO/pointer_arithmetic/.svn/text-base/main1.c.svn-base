#include <stdlib.h>

int *foo(char *x, short *y, int *z) {
  unsigned char *p = x + 2;
  unsigned short *q = y + 3*(x-(char*)z) + 5;
  unsigned int *r = (unsigned int *) malloc(5*((int *)x-(int *)y)+7);

  return (int *) p + 5*((unsigned int *)q-r)+7;
}

int main() {
  void *x, *y, *z;

  while (1) {
    x = malloc(3*(y-z)+7);

    if (!x)
      break;

    free(x);
  }

  return 0;
}
