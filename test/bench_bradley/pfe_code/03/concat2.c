#include <stdio.h>

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

int main() {
  char s1[] = "Greetings,";
  char s2[] = " inhabitants of M13.";
  char s3[128];
  concat(s1, s2, s3);
  printf("%s + %s = %s\n", s1, s2, s3);
  return 0;
}
