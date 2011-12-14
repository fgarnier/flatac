int __BLAST_NONDET;

main(){
  
  char x[100], y[100], z[200];
  int i,j,k;
  
  k = __BLAST_NONDET;
  
  i = 0;
  while(x[i] != 0){
    z[i] = x[i];
    i++;
  }

  j = 0;
  while(y[j] != 0){
    z[i] = y[j];
    i++;
    j++;
  }

  z[j] = 0;
  
  if(k >= 0 && k < j)
    if(z[k] == 0)
      {ERROR: goto ERROR;}
}
