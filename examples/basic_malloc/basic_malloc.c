#include <stdlib.h>




int main (int argc, char **argv){


 int taille_zone = 20;
 int *buffer;

 buffer = (int *)malloc((taille_zone+7)*sizeof(int));
 
 if ( buffer == NULL ){
   free(buffer);		
 }
	
 return (0);

}
