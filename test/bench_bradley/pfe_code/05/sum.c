#include <stdio.h>

int main(int argc, char ** argv) {
  if (argc != 2) {
    printf("*cough* Expected precisely one argument.\n");
    return -1;
  }
  int n;
  if (sscanf(argv[1], "%d", &n) == 0) {
    printf("Erm, expected an integer.\n");
    return -1;
  }
  if (n <= 0) {
    printf("You've got to be kidding: positive!\n");
    return -1;
  }

  int i, sum = 0;
  for (i = 1; i <= n; i++) sum += i;
  printf("Sum: %d\n", sum);
  return 0;
}
