#include<stdio.h>
int add(int a, int b);
//float fadd(float a, float b);
int swap (int a,int b);
//int count(int n);
int main()
{
  float arr[16] = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0};
  int a = 10, b = 20,c;
  float af = 10.5, bf = 20.5;
  //c = count(5);
  printf(" %d + %d = %d\n",a,b,add(a,b));
  //printf(" %f + %f = %f\n",af,bf,fadd(af,bf));
  //printf("count: %d\n",c);
  printf("Before swap %d %d:\n",a,b);
  swap(a,b);
  printf("After swap %d %d\n",a,b);
  return 0;
  for (int i = 0; i < 16; i++) {
    printf("%f",arr[i]);
  }
  return 0;
}
