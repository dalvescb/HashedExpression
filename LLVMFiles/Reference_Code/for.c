#include <stdio.h>

int main (int argc, char *argv[]) {

   int a;

   /* for loop execution */
   for( a = 10; a < argc; a = a + 1 ){
      printf("value of a: %d\n", a);
   }

   return 0;
}
