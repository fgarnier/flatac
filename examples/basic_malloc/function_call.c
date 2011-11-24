#include <stdlib.h>


int incr ( int x){

 int ret; 

 ret = x++;
 return (ret); 

}


int main (int argc , char ** argv ){

 char * buffer = (char *) malloc (10*sizeof(char));
 buffer ++;
 buffer = (char *)(incr((int)buffer));
 free (buffer);
 return(0);

}

