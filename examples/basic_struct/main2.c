#include "basic_struct.h"
#include "more_complex_struct.h"
#include <stdio.h>
#include <stdlib.h>


int main (int argc, char ** argv){

less_basic_struct * tmp = (less_basic_struct * )malloc(1*sizeof(basic_struct));
tmp->infos.storage = (char *)malloc(10);
tmp->infos.next = (basic_struct * )malloc(2*sizeof(basic_struct));

return(0);

}
