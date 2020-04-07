#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void test28(){
  double *ptr = malloc(sizeof(double) * 7);
  (ptr[0]) = 1.0;
  (ptr[1]) = 15.0;
  (ptr[2]) = 3.0;
  (ptr[6]) = pow((ptr[2]),-1);
  (ptr[3]) = (ptr[1]) * (ptr[6]);
  (ptr[5]) = pow((ptr[0]),-1);
  (ptr[4]) = (ptr[3]) * (ptr[5]);
  printf("%f ",(ptr[4]));
  free(ptr);
}