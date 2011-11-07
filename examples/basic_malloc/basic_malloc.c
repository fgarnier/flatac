#include <stdlib.h>




int main (int argc, char **argv){


 int taille_zone;
 int *buffer;

 taille_zone =20;
 taille_zone = 40 +2;	
 buffer = NULL;
 
 buffer = (int *)malloc((taille_zone+7)*sizeof(int));
 
 if ( buffer == NULL ){
   free(buffer);		
 }
	
 return (0);

}
