void incr(int * x) {
  *x = *x + 1;
}

int main() {
  int a = 0;
  incr(&a);
  incr(&a);
  return 0;
}
