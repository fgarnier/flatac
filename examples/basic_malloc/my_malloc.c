#include <stdlib.h>

char *mymalloc(int size)
{
  short *newmem=NULL;
  
  newmem = (short *) malloc(size + sizeof(short));
  *newmem = 1; /* initialize reference count */
  return (char *) (newmem + 1);
}


main() {
  int *i;
  i = (int *) mymalloc(sizeof(int));
  *i = 10;
} 
