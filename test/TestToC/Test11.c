#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void test11(){
  double *ptr = malloc(sizeof(double) * 2);
  (ptr[0]) = 1.0;
  (ptr[1]) = cosh(ptr[0]);
  printf("%f ",(ptr[1]));
  free(ptr);
}