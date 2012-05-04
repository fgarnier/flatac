#include "basic_struct.h"
#include <assert.h>

int main (int argc, char ** argv){

 personne *alice, *charly;
 
 
 char* alice_name, *firstname;

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


 if(alice->sex != charly->sex)
	return(1);
 
 assert(alice->name != charly->name);

 return(0);

}
