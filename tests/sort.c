#include <stdio.h>
#include <stdlib.h>


void bubblesort(float *c, int nn)
{
   int i, n, done;
   float tmp;

   n = nn;
   do {
      done = 1; --n;
      for (i=0; i<n; ++i) {
         if (c[i] > c[i+1]) {
            tmp = c[i];
            c[i] = c[i+1];
            c[i+1] = tmp;
            done = 0;
         }
      }
   } while (!done);
}
      
int main()
{
   int i;
   float val[20];

   for (i=0; i<20; ++i)
      val[i] = (float) rand();

   bubblesort(val, 20);

   for (i=0; i<20; ++i)
      printf("%f\n", val[i]);
   printf("\n");

   return 0;
}
