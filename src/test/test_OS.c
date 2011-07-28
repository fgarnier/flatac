#include <stdlib.h>

void _OSRelease(int* j) {
  free(j);
}

int main () { 
  int* j = (int*)malloc(sizeof(int) * 1);
  return 0;
}
