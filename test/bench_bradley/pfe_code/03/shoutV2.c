#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Alternate implementation of shout. */
int shout(char * msgIn, char * msgOut) {
  // check for well-formed input
  if (!msgIn || !msgOut) return -1;

  // loop over msgIn until the string terminator is found
  while (*msgIn != '\0') {
    // transfer the (possibly modified) character to msgOut
    if ('a' <= *msgIn && *msgIn <= 'z')
      *msgOut = *msgIn + ('A' - 'a');
    else
      *msgOut = *msgIn;
    // increment the pointers
    msgIn++;
    msgOut++;
  }
  // terminate msgOut
  *msgOut = '\0';

  // indicate success
  return 0;
}

int main() {
  char msg[] = "Hi!";
  char out[4];
  int err = shout(msg, out);
  assert (!err);
  printf("%s -> %s\n", msg, out);
  return 0;
}
