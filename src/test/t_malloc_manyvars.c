#include <stdlib.h>

int main ( ) { 
  int j=0;
  int* tab = (int*)(malloc(sizeof(int) * 10));
  int *y;
  int *x;	

 y = (int *)malloc(malloc(sizeof(int) * 50));
 x=tab;
 tab = y;
 x = tab ;
 free((void *) x);

  return 0;
}
