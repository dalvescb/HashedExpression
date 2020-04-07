#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(){
  double *ptr = malloc(sizeof(double) * 4);
  (ptr[0]) = 1.0;
  (ptr[1]) = 15.0;
  (ptr[3]) = sin(ptr[0]);
  (ptr[2]) = (ptr[1]) + (ptr[3]);
  //printf("%f ",(ptr[2]));
  free(ptr);
}