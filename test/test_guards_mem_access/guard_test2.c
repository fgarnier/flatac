#include <stdio.h>
#include <stdlib.h>



int set_addr_to_val (int * addr, int val ){
	*addr=val;
}

int main (int argc, char ** argv) {
 int cnt;
 int *buffer = malloc(10*sizeof(int));

 for(cnt=0;cnt <= 20; cnt++)
	cnt = *(buffer + cnt)+15;
	

 return (0);

}
