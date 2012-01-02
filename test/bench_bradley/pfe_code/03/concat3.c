#include <stdio.h>

int concat(char * str1, char * str2, char * out) {
  if (!str1 || !str2 || !out) return -1;
  --out;
  while (*++out = *str1++);
  while (*out++ = *str2++);
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
