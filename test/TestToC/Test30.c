#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(){
  double *ptr = malloc(sizeof(double) * 5);
  (ptr[0]) = 1.0;
  (ptr[3]) = sin(ptr[0]);
  (ptr[4]) = cos(ptr[0]);
  (ptr[2]) = -(ptr[4]);
  (ptr[1]) = (ptr[3]) + (ptr[2]);
  //printf("%f ",(ptr[1]));
  free(ptr);
}