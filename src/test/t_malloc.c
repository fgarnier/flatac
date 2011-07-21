#include <stdlib.h>

int main ( ) { 
  int* tab = (int*)(malloc(sizeof(int) * 10));
  int* tab2;

  free(tab);
  free(tab);

  free(tab2);

  return 0;
}
