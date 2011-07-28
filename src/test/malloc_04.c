#include <stdlib.h>

void release (int* j) {
  free(j);
}

int main () {
  // FOR DEBUG PURPOSE
  int i = 0;

  int* ptr_i = (int*)malloc(sizeof(int) * 1);
  release (ptr_i); // Interprocedurale release

  return 0;
}
