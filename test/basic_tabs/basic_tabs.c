#include <assert.h>
#include <stdio.h>


int main(int argc, char **argv){

 int tab0[10];
 int i;
 int j=1;

 for(i=0;i<11;i++){
 assert(j<10);
 tab0[j]=i;
 }

 for(i=0;i<=10;i++){
 // printf("tab0[%d]=%d \n",i,tab0[i]);

 j=tab0[i]; 
 }

 return(0);

}
