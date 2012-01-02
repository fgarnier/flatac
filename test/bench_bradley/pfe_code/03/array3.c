int main() {
  int a[4];
  int * x;
  x = a;
  *x = 1;
  *(x + 1) = 1;
  *(x + 2) = *x + *(x + 1);
  *(x + 3) = *(x + 1) + *(x + 2);
  return 0;
}
