
int main(){

   int * a;
   int k = __NONDET__();
   int i;
   if ( k <= 0 ) return -1;
   
   a= malloc( k * sizeof(int));
   
   for (i =0 ; i != k; i++)
      if (a[i]) return 1;

   return 0;

}
