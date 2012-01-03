int sum3(int a, int b, int c) {
  int sum = 0;
  sum = a + b + c;
  return sum;
}

int main(int argc, char ** argv) {
  int x = 1;
  x = sum3(x, x, x);
  x = sum3(x, x, x);
  return 0;
}
