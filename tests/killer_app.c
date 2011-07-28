#include <assert.h>

void dummy_assert(int);

int 
main (void)
{
	

  int *a; /* This souldn't appean in the AST yet.*/
		
	/*Rajouter le traitement des modulus*/
	/*Rajouter la division Euclidienne*/
	/*Calculer la table de correspondance sId -> Position dans le code
	source*/
	/* tester */

	/*Développer : Fait

	 et simplifier les expressions dues au
	produits d'expressions entières

     */
	/*Rajouter expressions du type  cst * variable et
	variable * cst : Fait.	*/

  int i=0;
  int j = 0;

  while (i < 100)
    {

      if (i < 50)
	{
	a=0;
	  i++;
	  j++;
	}

      else
	{

	  i++;
	  j--;
	}

    }

  assert(i== (j+100)); 

  assert ((i-j) <= 200 && (i-j)> -100);
/*
	assert (i!=j);
	assert (i>j+60 || i != 0);
i=j++; */
return( 0 );
}
/*
int paar(int k)
{
	if (k%2 ==0){
	return (1);
	}
	else{
	k++;
	}
	 return (0);
}

int fact (int i ){
	
	int cmpt;
	int return_v = 1;
  	
	if (i<= 1)  return 1;
	else 
		for( cmpt=1; cmpt<=i ; cmpt++ ){
			return_v = 2-(cmpt-(return_v+52*cmpt));
			
			}


	return (return_v);

} */
