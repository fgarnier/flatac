#include <assert.h>
int NonDetInt();

main(){
  
  char x[100], y[100];
  int i,j,k;
  
  k = __BLAST_NONDET;
  
  i = 0;
  while(x[i] != 0){
    y[i] = x[i];
    i++;
  }
  y[i] = 0;
  
  if(k >= 0 && k < i)
    if(y[k] == 0)
      {ERROR: goto ERROR;}
}
