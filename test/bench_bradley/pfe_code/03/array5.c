// defines N to be synonymous with 100
#define N 100

int main() {
  // declare an array of N integers
  int fib[N];
  int i;

  // define the first two elements
  fib[0] = 1;
  fib[1] = 1;

  // define the remaining elements up to the N'th
  for (i = 2; i < N; i++)
    fib[i] = fib[i-2] + fib[i-1];

  return 0;
}
