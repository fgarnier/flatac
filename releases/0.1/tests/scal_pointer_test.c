#include <stdio.h>
#include <stdlib.h>


int main (int argc, char ** argv){


int *ptr=NULL;

	int j;
	if (ptr){
	printf ("Failure, ptr is NULL \n");
	}

	ptr=(int *)(300*(((int)ptr==2)));
	j=(int)ptr;

	printf("Valeur de l'adresse de j %d \n",j);
	

	printf("Program terminated normaly");
	return(0);
}
