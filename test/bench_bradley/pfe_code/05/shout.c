#include <stdio.h>

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
  // assume that any word is at most 127 characters
  char in[128], out[128];
  while (scanf("%s", in) != EOF) {
    // read one word, now process it
    shout(in, out);
    // print the result
    printf("%s ", out);
  }
  printf("\n");
  return 0;
}
