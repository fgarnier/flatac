#include <assert.h>
#include <stdio.h>

// Write toneItDown here.

int main() {
  // unit test
  char email[] = "Hi friends!  Im so excited about "
    "programming!!!  Its so kewl!";
  char out[128];
  printf("%s\n", email);
  int err = toneItDown(email, out);
  assert (!err);
  printf("%s\n", out);
  return 0;
}
