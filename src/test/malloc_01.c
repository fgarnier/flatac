#include <stdlib.h>

int main () {
  // FOR DEBUG PURPOSE
  int i = 0;

  int* ptr_i = (int*)malloc(sizeof(int) * 1);
  free(ptr_i);

  return 0;
}
