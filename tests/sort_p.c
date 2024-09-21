#include <stdio.h>
#include <stdlib.h>

#define SORT_SIZE 2500

void ibubblesort(int *cc, int nn)
{
   int v1, v2, *c;
   int i, done;
   int n = nn;

   do {
      c = cc;
      done = 1;
      v1 = *c++;
      for (i=1; i<n; ++i) {
         v2 = *c++;
         if (v1 > v2) {
            *--c = v1;
            *--c = v2;
            c += 2;
            done = 0;
         }
         else
            v1 = v2;
      }
      --n;
   } while (!done);
}

void fbubblesort(float *cc, int nn)
{
   float v1, v2, *c;
   int i, done;
   int n = nn;

   do {
      c = cc;
      done = 1;
      v1 = *c++;
      for (i=1; i<n; ++i) {
         v2 = *c++;
         if (v1 > v2) {
            *--c = v1;
            *--c = v2;
            c += 2;
            done = 0;
         }
         else
            v1 = v2;
      }
      --n;
   } while (!done);
}
      
int main()
{
   int i;
   float *val1 = (float *)malloc(SORT_SIZE*sizeof(float));
   float *val2 = (float *)malloc(SORT_SIZE*sizeof(float));

   for (i=0; i<SORT_SIZE; ++i)
      val1[i] = (float) (SORT_SIZE - i);

   ibubblesort((int *) val1, SORT_SIZE);

   for (i=0; i<SORT_SIZE; ++i)
      val2[i] = (float) (SORT_SIZE - i);

   fbubblesort(val2, SORT_SIZE);

   if (val1[0] == 1.0 && val1[SORT_SIZE-1] == (float) SORT_SIZE &&
       val1[0] == val2[0] && val1[SORT_SIZE-1] == val2[SORT_SIZE-1])
      printf("passed\n");
      
   free(val2);
   free(val1);

   return 0;
}
