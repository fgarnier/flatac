#include <stdio.h>
#include <stdlib.h>


int * get_addr( void ){
 int i = 21;
 int *retval = &i;
 return (retval);
}

void incr_val(int * i){
 int j = *i;
 j++;
 *i=j;	
}

int main (int argc , char ** argv ){

 int val1 = 0;
 int *ptr1 = NULL;

 printf("valeur initiale de val1 %d \n", val1);	
 incr_val(&val1);
 printf("valeur modifiée de val1 %d \n", val1);

 ptr1 = get_addr();
 printf("ploc \n"); 
 val1=++(*ptr1);
 /* Là, on doit avoir un segmentation fault,  car l'adresse
de retour de get_addr correspond à celle de la variable i
déclarée dans le contexte créé par l'appel de get_addr, addresse
qui ne pointe plus sur une variable existante. Mais en fait,
il n'y a pas de core dump, même pas un warning avec l'option
-Wall de gcc.
 */
 printf("valeur modifiée de val1 %d \n", val1);  


 return 0;
}
