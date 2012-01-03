#include <stdio.h>

int main() {
  char c;
  while (scanf("%c", &c) != EOF) {
    if ('a' <= c && c <= 'z')
      c += 'A' - 'a';
    printf("%c", c);
  }
  printf("\n");
  return 0;
}
