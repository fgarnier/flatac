#include "basic_struct.h"

int main (int argc, char ** argv){

 personne alice, charly;
 alice.firstname="alice";
 alice.name="Good";
 alice.sex=female;
 alice.year_of_birth=1985;
 alice.mounth_of_birth=Germinal;

 
 charly.firstname="Charly";
 charly.name="Evil";
 charly.sex=male;
 charly.year_of_birth=1975;
 alice.mounth_of_birth=Nivose;


 assert(alice.sex != charly.sex);
 return(0);

}
