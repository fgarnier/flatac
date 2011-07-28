#include <stdlib.h>


int inc ( int x) {
	return x+1 ;
}

int main ( ) { 
  int x=0;
  int* tab;
  tab = (int*)(malloc(sizeof(int) * 10));
  int* tab2;
  malloc(sizeof(int) * 5) ;
  free(tab);
  free(tab);
  x = inc (x);
  free(tab2);

  return 0;
}
