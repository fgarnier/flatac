#include <assert.h>
#include <stdio.h>

int reverse(char * in, char * out) {
  // check for well-formed input
  if (!in || !out) return -1;

  // find the end of the string in
  int i;
  for (i = 0; in[i] != '\0'; i++);

  // i should index the terminator of in
  assert (in[i] == '\0');

  // read in backwards, write out forwards
  for (i--; i >= 0; i--) {
    assert (in[i] != '\0');
    *out = in[i];
    out++;
  }
  // terminate out
  *out = '\0';

  // indicate success
  return 0;
}

int main() {
  char str[] = "Hello universe!";
  char out[32];
  int err = reverse(str, out);
  assert (!err);
  printf("%s\n%s\n", str, out);
  return 0;
}
