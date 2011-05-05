#include <stdio.h>


int main (int argc, char **argv){

if (argc == 2)
	{
	printf("Bon nombre d'argument: %s %s \n", argv[0], argv[1] );
	exit (1);

	}
else {

	printf("Ce programme inutile réclame exactement 1 argument \n");

	}

	exit(0);
}
