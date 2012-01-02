#include <assert.h>
#include <stdlib.h>

int strlen(char * str) {
  if (str == NULL) return 0;
  int n = 0;
  while (str[n] != '\0') {
    n++;
  }
  return n;
}

int main() {
  assert (strlen("I hope that this message finds you well in M82.") == 47);
  assert (strlen("") == 0);
  assert (strlen(NULL) == 0);
  return 0;
}
