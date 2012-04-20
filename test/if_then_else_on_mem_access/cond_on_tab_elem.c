#include <stdio.h>


int main (int argc, char **argv){

 int tab[50];
 int i=0;
 while(tab[0]< 10 ){

 tab[0]=i;
 tab[i]=i;
 i=tab[i];
 i++;


 }


 return(0);

}
