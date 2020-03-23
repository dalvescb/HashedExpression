#include<stdio.h>
int add(int a, int b);
int main (int argc, char *argv[]) {

   int a[3]={1,2,3}, b[3]={2,4,6},c[3];

   /* for loop execution */
   for( int i = 0; i < argc; i = i + 1 ){
      c[i]=add (a[i],b[i]);
      printf("value of c[%d]: %d\n", i,c[i]);
   }

   return 0;
}
