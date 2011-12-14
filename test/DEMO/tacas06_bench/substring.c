int __BLAST_NONDET1;
int __BLAST_NONDET2;
int __BLAST_NONDET3;

main(){
  
  char x[101], y[101], z[201];
  int from,to,i,j,k;
  
  from = __BLAST_NONDET1;
  to = __BLAST_NONDET2;
  k = __BLAST_NONDET3;

  i = from;
  j = 0;
  while(x[i] != 0 && i < to){
    z[j] = x[i];
    i++;
    j++;
  }
  
  if(k >= 0 && k < j)
    if(z[k] == 0)
      {ERROR: goto ERROR;}  /* prove strlen(z) == j */
}
