#include <assert.h>
#include <stdio.h>

/* Given a string str, writes the final word of each line 
 * of str into msg.  Returns 0 or -1 to indicate 
 * success/input error, as usual.  See the unit test below 
 * for an example application.
 */
int decode(char * str, char * msg) {
  if (!str || !msg) return -1;

  // Step 1: points to the word currently being read
  char * word = str;

  while (*str) {
    // reached the end of a line?
    if (*str == '\n') {
      // Step 3: copy the last word into msg
      while (word != str)
        *msg++ = *word++;
      *msg++ = ' ';
      // Step 1
      word++;
    }
    // reached the end of a word?
    else if (*str == ' ') {
      // Steps 4, 1: set word to point to the next position
      word = str + 1;
    }
    // keep reading
    str++;
  }
  // Step 5: don't forget to terminate the string
  *msg = '\0';

  return 0;
}

// unit test of decode
int main() {
  // C allows writing constant strings across multiple 
  // lines as follows:
  char * letter = "Hey, old friend.  I need to go\n"
                  "to the market today or tonight\n"
                  "to fetch some drinks and food to\n"
                  "bring -- maybe also a pint or two\n"
                  "for some jolly times, hey?  Oh\n"
                  "I almost forgot: Martha got seven\n"
                  "if you can believe it.  From Elmer\n"
                  "no doubt. Cheerio -- ST\n";
  // buffer to hold decoded message
  char decoded[128];

  int err = decode(letter, decoded);
  assert (!err);
  printf("%s\n%s\n", letter, decoded);
  return 0;
}
