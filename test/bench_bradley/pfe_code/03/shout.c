#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Writes the message of msgIn into msgOut, except with all
 * capitals.  Returns 0 if successful and -1 if either of 
 * msgIn or msgOut is NULL.
 */
int shout(char * msgIn, char * msgOut) {
  int i = 0;
  char c;

  // check for well-formed input
  if (msgIn == NULL || msgOut == NULL)
    return -1;

  // loop over msgIn until the string terminator is found
  while (msgIn[i] != '\0') {
    // obtain the i'th character of the message
    c = msgIn[i];
    // if it's a lowercase letter, capitalize it
    if ('a' <= c && c <= 'z')
      c += 'A' - 'a';
    // write the character to msgOut
    msgOut[i] = c;
    // don't forget to increment i
    i++;
  }
  // terminate msgOut
  msgOut[i] = '\0';

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
