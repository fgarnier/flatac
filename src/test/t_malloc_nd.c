#include <stdlib.h>

int rand (void);

int main ( int argc , char ** argv ){

int *x;
int r;
r = rand ();

if (r > 0){
x = (int *)malloc (10 * sizeof (int));	
}

free((void *)x);
return (0);
}

