#include <assert.h>
#include <stdio.h>

// Write xvowelize here.

int main() {
  char str[] = "Hello universe!";
  char out[32];
  int err = xvowelize(str, out);
  assert (!err);
  printf("%s\n%s\n", str, out);
  return 0;
}
