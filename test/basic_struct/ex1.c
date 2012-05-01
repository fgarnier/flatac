#include <assert.h>
#include <stdlib.h>
#include "basic_struct.h"


int main (int argc, char ** argv){

personne sylvain;
personne ghislain;


 int test = strcmp(sylvain.name,ghislain.name);
 assert(test);
 return 0;

}
