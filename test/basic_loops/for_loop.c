
int zero()
{
	return 0;
}

int main( int argc, char ** argv){
 

 int i = argc;
 int j = 10;

 for (i=argc; i>0 ; i--) {
    j++;
    j=j+6;
    argc++;
    argc=zero();
 }
 j=j+65;
 
 return 0;

}
