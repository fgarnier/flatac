/* Computes and returns ``10 * a'' without using 
 * multiplication.
 */
int times10(int a) {  // input: int a, output: an int
  int x, y;           // local variables
  x = a + a;          // 2 * a
  y = x + x;          // 4 * a
  y = y + y;          // 8 * a
  return x + y;       // 2*a + 8*a == 10*a
}

int main() {
  int n = 42;         // n is local to main
  n = times10(n);
  return 0;
}
