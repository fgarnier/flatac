int foo(int x, int y, int z) {
  return 5*((x+12)*(3*y+7*z+1));
}

int bar(int x, int y, int z) {
  int m,n,p = 0;

  while (x > 0) {
    if (x < y+z) 
      if (2*y > 3*y + z) 
	m += 9*(x+y)+12;
      else
	n -= m+5*x+11*y;
    else
      break;
  }

  return 3*m+5*(n-1)+7*(p-2)+11;
}

int main() {
  int x,i;

  for (i = 0; i < 100; i ++) 
    x = foo(i,2*i+1,3*i+5) + bar(5*i+1,7*i+2,9*i+3);

  return 0;
}
