#include <stdio.h>
#include <stdlib.h>

int concat(char * str1, char * str2, char * out) {
  // check for well-formed input
  if (str1 == NULL || str2 == NULL || out == NULL)
    return -1;

  // write str1 to out, skipping the terminator
  int i = 0;
  while (str1[i] != '\0') {
    out[i] = str1[i];
    i++;
  }
  // write str2 to out
  int j = 0;
  while (str2[j] != '\0') {
    out[i] = str2[j];
    i++;
    j++;
  }
  // terminate out
  out[i] = '\0';

  // indicate success
  return 0;
}

int main() {
  char s1[] = "Greetings,";
  char s2[] = " inhabitants of M13.";
  char s3[128];
  concat(s1, s2, s3);
  printf("%s + %s = %s\n", s1, s2, s3);
  return 0;
}
