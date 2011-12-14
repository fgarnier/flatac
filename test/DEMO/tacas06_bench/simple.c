int __BLAST_NONDET1;
int __BLAST_NONDET2;
int __BLAST_NONDET3;

main(){
  
  char x[101], y[101], z[201];
  int from,to,i,j,k;
  
  i = __BLAST_NONDET1;
  k = __BLAST_NONDET3;
  
  if(!(k >=0 && k <= 100 && x[k] == 0))  /* assume strlen(x) <= 100 */
    {stuck1: goto stuck1;}
  
  if(!(i <= k))            /* assume "from" index is O.K. */
    {stuck1: goto stuck1;}
  
  while(x[i] != 0){
    i++;
  }


  if(!(i <= 100 && x[k] == 0))
    {ERROR: goto ERROR;}  /* prove we don't overflow z */
}
