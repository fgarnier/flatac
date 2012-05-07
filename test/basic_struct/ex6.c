#include "basic_struct.h"
#include <assert.h>

int main (int argc, char ** argv){

 personne *alice, *charly, bob;
 
 
 char* alice_name, *firstname;
 alice = (personne *)malloc(sizeof(personne));
 charly =  (personne *)malloc(sizeof(personne)); 

 alice->firstname=firstname;
 alice->name=alice_name;
 alice->sex=female;
 alice->year_of_birth=1985;
 alice->mounth_of_birth=Germinal;

 
 charly->firstname=firstname;
 charly->name=alice_name;
 charly->sex=male;
 charly->year_of_birth=1975;
 alice->mounth_of_birth=Nivose;

/* bob.firstname=firstname;
 bob.name=alice_name;
 bob.sex=male;
 bob.year_of_birth=1980;
 bob.mounth_of_birth=Floreal;
*/

 if(alice->sex != charly->sex)
	return(1);
 
 assert(alice->name != charly->name);

 return(0);

}
