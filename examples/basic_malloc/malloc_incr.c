#include <stdlib.h>

int main (int argc, char** argv){

 char * buffer;
 int i=0;

 buffer = NULL;
 buffer ++;
 
 for(i=0; i< 2 ; i++){
 buffer = (char *)malloc(10*sizeof(char));
 buffer ++;
 
}

 free((void *)buffer);
 return(0);
}
