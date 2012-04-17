#include <assert.h>


int plus(int a, int b){
 return a+b;
}


int times(int a, int b){
 return a*b;
}


int main(int argc, char** argv){
 
 int op_res = 0;
 int argc = 5;

 switch ( argc%7 ){

	case 0: 
	op_res = plus(argc,argc);
	break;	
	case 1:
	op_res = times(argc,argc);
	break;

	case 3:
	case 5:
	op_res++;
 	break;

	default:
	 assert(1==2);
	
 }

 return op_res;	

}
