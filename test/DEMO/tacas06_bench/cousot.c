int __BLAST_NONDET;

main(){
  
  int x,y;
  
  x = 0; y = 0;
  
  while(__BLAST_NONDET){
    x++; y++;
  }
  
  while(x > 0){
    x--;
    y--;
  }
  
  if(y != 0){
  ERROR: goto ERROR;
  }

  return 1;

}
