#include "basic_struct.h"
#include "more_complex_struct.h"
#include <stdio.h>
#include <stdlib.h>


int main (int argc, char ** argv){

less_basic_struct * tmp = (less_basic_struct * )malloc(1*sizeof(basic_struct));
tmp->infos.next = NULL;
return(0);

}
