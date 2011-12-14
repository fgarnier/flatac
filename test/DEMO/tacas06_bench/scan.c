int __BLAST_NONDET;

main(){

  int i,j, x[100];

  i = 0;
  j = __BLAST_NONDET;

  while(x[i] != 0)
    i++;

  if(j >= 0 && j < i)
    if(x[j] == 0)
      {ERROR: goto ERROR;}
}
