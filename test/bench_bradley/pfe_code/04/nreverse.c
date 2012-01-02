#include <assert.h>
#include <stdio.h>

/* Reverses the C string in into out.  maxLength indicates
 * the maximum possible length of in; if this length is
 * exceeded, returns -2.  Returns 0 if successful, and -1
 * if either in or out is NULL.
 */
int nreverse(char * in, char * out, int maxLength) {
  // check for well-formed input
  if (!in || !out) return -1;

  // find the end of the string in
  int i;
  for (i = 0; in[i]; i++) {
    if (maxLength <= 0) return -2;
    maxLength--;
  }

  // i should index the terminator of in
  assert (in[i] == '\0');

  // read in backward, write out forwards
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
  int err = nreverse(str, out, 10);
  assert (err == -2);
  err = nreverse(str, out, 31);
  assert (!err);
  printf("%s\n%s\n", str, out);
  return 0;
}
