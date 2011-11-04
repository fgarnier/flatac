#include "basic_struct.h"
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char ** argv){

/*basic_struct *tmp=NULL;*/
	int 	i=0;
	i++;
 	basic_struct *tmp;
	i++;
 tmp = (basic_struct * )malloc( 1 * sizeof(basic_struct));
 tmp->next = (basic_struct * )malloc(10*sizeof(basic_struct));


	tmp++;	
 tmp = tmp->next;
 tmp->next = (basic_struct * )malloc(3*sizeof(basic_struct));

 free((void *)tmp);

 return (0);

}
