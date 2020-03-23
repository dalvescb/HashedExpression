#include<stdio.h>
void doubleVal(int *a, int *b);
int main (int argc, char* argv[])
{
int a[3]={1,2,3}, b[3];
doubleVal(a,b);
for(int i=0;i<3;i++){
printf("%d\n",b[i]);
}
return 0;
}
void doubleVal(int *a, int *b)
{
for (int i=0;i<3;i++){
*b=*a+2;
a++;
b++;
 }
}