#define N 100
int main() {
  // declare an array of N integers
  int fib[N];
  int i;

  // define the first two elements
  fib[0] = 1;
  fib[1] = 1;

  while (1) {
    fib[i] = fib[i-2] + fib[i-1];
    i++;
    if (i = N) break; // break exits the loop
  }
}
