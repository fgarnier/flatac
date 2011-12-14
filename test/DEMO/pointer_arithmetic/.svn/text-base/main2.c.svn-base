#include <stdlib.h>

int *alloc(int size) {
  char *p = (char *) malloc(size+4);
  *(int *) p = size;
  return (int *)(p+1);
}

void dispose(int *x, int *y) {
  if (x == y)
    free(x);
  else {
    free(x);
    free(y);
  }
}

int main() {
  void *p, *q;
  int i = 0; 
  
  while (1) {
    p = alloc(i ++);
    q = alloc(i ++);
    dispose(p,q);
  }

  return 0;
}
