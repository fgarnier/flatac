// This gives a bogus counterexample as we don't encode:
// i=j -> a[i] = a[j] 
// need to generalize the boolean closure operation of the POPL04 paper



int skip;
int x[10], y[10];
int i,j,n;

main(){
    for(i = 0; i < n; i++){
      y[i] = x[i];
    }

    if(j >= 0 && j < n && x[j] != y[j]){
    ERROR: goto ERROR;
    }
  
END : goto END;

}
