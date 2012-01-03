#include <stdio.h>

/* Writes the message of msgIn into msgOut, except with all
 * capitals.  Returns 0 if successful and -1 if either of 
 * msgIn or msgOut is NULL.
 */
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

/* Concatenates arrays a and b, of lengths an and bn, 
 * respectively, storing the result in c.  Returns -1 for 
 * erroneous input, and 0 otherwise.
 */
int concat(char * str1, char * str2, char * out) {
  // check for well-formed input
  if (!str1 || !str2 || !out)
    return -1;

  // write str1 to out, skipping the terminator
  while (*str1)
    *out++ = *str1++;
  // write str2 to out
  while (*str2)
    *out++ = *str2++;
  // terminate out
  *out = '\0';

  // indicate success
  return 0;
}

/* Reverses the string in into the string out.  Returns 0 
 * if successful and -1 if in or out is NULL.
 */
int reverse(char * in, char * out) {
  // check for well-formed input
  if (!in || !out) return -1;

  // find the end of the string in
  int i;
  for (i = 0; in[i] != '\0'; i++);

  // read in backwards, write out forwards
  for (i--; i >= 0; i--) {
    *out = in[i];
    out++;
  }
  // terminate out
  *out = '\0';

  // indicate success
  return 0;
}

/* Prints an array of strings.  Return 0 if successful, and
 * -1 if a is NULL, n < 0, or any entry of a is NULL.
 */
int printStringArray(char ** a, int n) {
  // check for well-formed input
  if (!a || n < 0) return -1;

  int i;
  for (i = 0; i < n; i++) {
    // return -1 if a[i] is NULL
    if (!a[i]) return -1;
    printf("%s ", a[i]);
  }
  printf("\n");

  return 0;
}

int main(int argc, char ** argv) {
  char str1[32] = "Hello universe!";
  char str2[32], str3[64];

  // 1. Print the command-line argument array.
  printStringArray(argv, argc);

  // 2. Print the "shouted" version of str1.
  shout(str1, str2);
  printf("%s -> %s\n", str1, str2);

  // 3. Print the concatenation of str1 and str2.
  concat(str1, str2, str3);
  printf("%s + %s =\n  %s\n", str1, str2, str3);

  // 4. Print the reversal of str1.
  reverse(str1, str2);
  printf("%s -> %s\n", str1, str2);

  return 0;
}
