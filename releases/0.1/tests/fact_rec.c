#include <assert.h>

int fibonnacci (int i ){
	
   int res;
	

   if(i == 1 || i==0 ){
     return (1);
   }
   else 
    res= fibonnacci((i-1)*3)+fibonnacci(i-2);

   return(res);
   
}

int plus (int a , int b){
 return(a+b);

}


int nat (int i){
  int res=0;

  if (i > 0){
   res= 1 + nat(i-1);
  }

  return(res);
}


int main (int argc, char ** argv){

int pouet = plus(argc,fibonacci(3*(4-2)-16));
return (pouet);
}
