#include <stdlib.h> 

int main () {
  int z = 0; // TO BE SURE

  int* x, *y;
  x = (int*)malloc(sizeof(int) * 1);
  y = NULL;

  while(z++ < 100) {
    y = x;
    x = (int*)malloc(sizeof(int) * 1);
    free(y);
  }

  return 0;
}
