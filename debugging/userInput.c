#include <stdio.h>

int main (void)
{
  int n;
  while (scanf("%d", &n) != 1) {}
  printf ("Entered: %d\n", n);
  return 0;
}
