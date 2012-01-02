int main() {
  int a[4];
  *a = 1;
  *(a + 1) = 1;
  *(a + 2) = *a + *(a + 1);
  *(a + 3) = *(a + 1) + *(a + 2);
  return 0;
}
