
int plus(int a, int b){
 return a+b;
}


int times(int a, int b){
 return a*b;
}


int main(int argc, char** argv){
 
 int op_res = 0;

 switch ( argc%2 ){

	case 0: 
	op_res = plus(argc,argc);	
	case 1:
	op_res = times(argc,argc);
 }

 return op_res;	

}
