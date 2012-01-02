#include <assert.h>
#include <stdlib.h>

int strlen(char * str) {
  if (!str) return 0;
  char * start = str;
  for (; *str; str++);
  return str - start;
}

int main() {
  assert (strlen("I hope that this message finds you well in M82.") == 47);
  assert (strlen("") == 0);
  assert (strlen(NULL) == 0);
  return 0;
}
