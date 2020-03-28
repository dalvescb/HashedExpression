#include<stdio.h>
#include<math.h>
//int add(int a, int b);
//float fadd(float a, float b);
//int swap (int a,int b);
//int count(int n);
double func(double a, double b);

int main()
{
  float arr[16] = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0};
  //int a = 10, b = 20,c;
  //float af = 10.5, bf = 20.5;
  //c = count(5);
  double af = 5, bf = 10;
  //printf(" %d + %d = %d\n",a,b,func(a,b));
  printf(" %f + %f = %f\n",af,bf,func(af,bf));
  printf("%f",sqrt(49));
  printf("%f",exp(1));
  //printf("count: %d\n",c);
  //printf("Before swap %d %d:\n",a,b);
  //swap(a,b);
  //printf("After swap %d %d\n",a,b);
  return 0;
  for (int i = 0; i < 16; i++) {
    printf("%f",arr[i]);
  }
  return 0;
}
