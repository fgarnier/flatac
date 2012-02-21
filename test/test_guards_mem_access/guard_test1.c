#include <stdio.h>
#include <stdlib.h>



int set_addr_to_val (int * addr, int val ){
	*addr=val;
}

int main (int argc, char ** argv) {
 
 int *buffer = malloc(10*sizeof(int));
 set_addr_to_val(buffer + argc , *(buffer+argc));
	

 return (0);

}
