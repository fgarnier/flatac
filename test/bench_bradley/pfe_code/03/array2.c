int main() {
  int a[4];
  int * x;
  x = a;  // Notice that the right expression is a, not &a!
  x[0] = 1;
  x[1] = 1;
  x[2] = x[0] + x[1];
  x[3] = x[1] + x[2];
  return 0;
}
