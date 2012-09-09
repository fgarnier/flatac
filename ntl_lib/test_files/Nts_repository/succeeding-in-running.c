#include <assert.h>
int NonDetInt();

main(){
  
  int x,y;
  
  x = 1000;
  y = 0;
  
  while(x > 0){
    x--;
    while(NonDetInt())
      y = 2 * y;
    y = y + 2;
  }
  assert(y % 2 == 0 && x == 0);
}
