#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void test3(){
  double *ptr = malloc(sizeof(double) * 3);
  (ptr[0]) = 15.0;
  (ptr[1]) = 3.0;
  (ptr[2]) = (ptr[0]) * (ptr[1]);
  printf("%f ",(ptr[2]));
  free(ptr);
}