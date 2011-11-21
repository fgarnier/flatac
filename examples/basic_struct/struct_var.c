#include "basic_struct.h"
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char ** argv){

basic_struct tmp;
basic_struct * tmp1 = (basic_struct *)malloc(10*sizeof(basic_struct));

tmp.next = (basic_struct *) tmp1;

return (0);


}

