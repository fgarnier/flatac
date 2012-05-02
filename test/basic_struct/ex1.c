#include <assert.h>
#include <stdlib.h>
#include "basic_struct.h"


int main (int argc, char ** argv){

personne sylvain;
personne ghislain;
personne toto;


 int test = strcmp(sylvain.name,ghislain.name);
 test +=  strcmp(sylvain.name,toto.name);
 assert(test);
 return 0;

}
