#include <assert.h>

void dummy_assert (int);

int main (int argc , char **argv){
	int x= 3*argc;
	
	/*dummy_assert (argc > 2);
	dummy_assert(argc !=3 && argc > 10); */
	assert ( argc <= 98 || argc > 6 );
	/*dummy_assert (argc >10 && argc < 90878);*/
	assert (x==3 );
	
	return 0;
	
}
