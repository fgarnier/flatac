#include <stdio.h>

int main() {
  int sum = 0;
  int cnt = 0;

  while (1) {
    // read an integer
    int num;
    int ret = scanf("%d", &num);
    // check if stdin has closed
    if (ret == EOF)
      // stdin has closed, so exit the loop
      break;
    // check if scanf returned 0, indicating bad input
    if (ret == 0) {
      printf("Expected an integer.\n");
      return -1;
    }
    sum += num;
    cnt++;
  }

  printf("Sum: %d\nInteger mean: %d\n", sum, sum/cnt);
  return 0;
}
