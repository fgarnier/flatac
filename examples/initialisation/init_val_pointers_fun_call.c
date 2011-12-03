#include <stdlib.h>


int * incr_ptr (int * i){
 int * ret_val=i;
 ret_val++;
 return (ret_val);
}

int main(int argc, char **argv){

 int *a;
 int i;
 a=NULL;
 a = malloc(1*sizeof(int));
 a++;
 a = incr_ptr(a);
 i=0 ;
 i++;
 
return (i);


}
