#include <assert.h>

int fun(int x, int m) {
  int y = x, n = 2*m-x;

  while (x < n) {
    if (x < m) {
      x ++;
      y ++;
    } else {
      x ++;
      y --;
    }
  }

  return y;
}

int main (void)
{	
  int i;

  for (i = 0; i < 100; i ++)
    assert(fun(i,100-i) == i);

return 0;
}

