#include<stdio.h>
float count(int n, float *ptr);
int main()
{
  float array[4] = {0,1,2,3};
  count (2,array);
  return 0;
}
float count(int n, float *ptr){
  float c = 0;
  for (int i =0;i<n;i++){
    c = c+ptr[i];
  }
  return c;
}
