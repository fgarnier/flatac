#include <assert.h>

int skip;
int x[10];
int i,j,n;

main(){

	n=10;
    for(i = 0; i < n; i++){
      x[i] = i;
    }

    if ( 0 <= j && j < n){
      if ( i > n ){
      	assert(0);
	}
    }

}
