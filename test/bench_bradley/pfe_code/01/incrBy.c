void incrBy(int * x, int a) {
  *x = *x + a;
}

int main() {
  int a = 0;
  incrBy(&a, 3);
  incrBy(&a, a);
  return 0;
}
