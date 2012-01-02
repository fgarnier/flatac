#include <assert.h>
#include <stdlib.h>

int strlen(char * str) {
  if (!str) return 0;
  int n;
  for (n = 0; str[n]; n++);
  return n;
}

int main() {
  assert (strlen("I hope that this message finds you well in M82.") == 47);
  assert (strlen("") == 0);
  assert (strlen(NULL) == 0);
  return 0;
}
