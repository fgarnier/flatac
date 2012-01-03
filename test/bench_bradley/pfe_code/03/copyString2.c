#include <stdio.h>

/* Copy string in into the buffer referenced by out. */
int copyString(char * in, char * out) {
  if (!in || !out) return -1;
  do { *out++ = *in++; } while (*in);
  return 0;
}

int main() {
  char s1[] = "A shout out to my good friends in M81.";
  char s2[128];
  copyString(s1, s2);
  printf("%s\n%s\n", s1, s2);
  return 0;
}
