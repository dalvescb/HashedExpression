#include<stdio.h>
int main (int argc, char* argv) {

   int a[3]={1,2,3},b[3];

   /* for loop execution */
   for( int i = 0; i < argc; i = i + 1 ){
      b[i]=a[i]+2;
      printf("%d",a[i]);
   }

   return 0;
}
