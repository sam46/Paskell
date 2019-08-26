int main (void)
{
  int i[5];
  i[0] = 1;
  i[1] = 2;
  i[2] = 3;
  i[3] = 4;
  int j = 0;
  while (j < 5) {
    printf("%d\n", i + j);
    j += 1;
  }
  return 0;
}
