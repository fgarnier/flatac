#include <stdlib.h>
#include "useless.h"



size_t sum_prod ( int a, int b, size_t type_a, size_t type_b){

 size_t size_in_bits_a = ((size_t)a)*type_a;
 size_t size_in_bits_b =  ((size_t)b)*type_b;
 size_t ret =  size_in_bits_a * size_in_bits_b;
return(ret);
 	

}

int incr ( int x){

 int ret; 

 ret = x++;
 return (ret); 

}


int main (int argc , char ** argv ){

 size_t total_mem;
 uselesstruct t;
	
 char * buffer = (char *) malloc (10*sizeof(char));
 buffer ++;
 buffer = (char *)(incr((int)buffer));
 free (buffer);

 total_mem = 
 sum_prod( 5 , 35*(int)buffer , sizeof(uselesstruct) , sizeof(char) ); 

 int j = 1;
 j = incr(j);
 
 return(0);

}

