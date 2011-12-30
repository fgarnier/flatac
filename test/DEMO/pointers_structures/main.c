#include <assert.h>
#include <stdlib.h>

struct S {
  int a;
  char *p;
};

void foo(struct S x, int b) {
  x.a = b;
  x.p = (char *) malloc(x.a+sizeof(char));
  /*x.p[x.a] = b;*/
}

int main() {
  struct S s;
  int z;

  foo(s,100);
 /* z = s.p[100];*/

  assert(z == 100);

  return 0;
}
