#include <stdio.h>

void printUsage() {
  printf("Usage: rev [n] < [data file], where the file is "
         "a list of n integers\n");
}

int main(int argc, char ** argv) {
  if (argc != 2) {
    // argument n not provided
    printUsage();
    return -1;
  }
  int n;
  if (sscanf(argv[1], "%d", &n) != 1) {
    // the argument is not an integer
    printUsage();
    return -1;
  }

  // variable-length array
  int nums[n];

  int i;
  for (i = 0; i < n; ++i) {
    // Tricky!  Tell scanf to write the value directly 
    // into the correct position of nums.
    int rc = scanf("%d", nums+i);
    if (rc == EOF) {
      printf("Unexpected end of file.\n");
      printUsage();
      return -1;
    }
    if (rc == 0) {
      printf("Expected an integer.\n");
      printUsage();
      return -1;
    }
  }

  // print the numbers in reverse
  for (i = n-1; i >= 0; --i)
    printf("%d ", nums[i]);
  printf("\n");

  return 0;
}
