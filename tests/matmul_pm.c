#include <stdlib.h>

#define SIZE 256

float *A, *B, *D;

void f()
{
   float sum;
   float *a, *b = B, *bc, *d = D;
   int r, c, k;
   for (r=0; r<SIZE; ++r) {
      a = A + r*SIZE;
      for (c=0; c<SIZE; ++c) {
         sum = 0.0f;
         bc = b + c;
         for (k=SIZE; k > 0; --k) {
            sum += *a++ * *bc;
            bc += SIZE;
         }
         a -= SIZE;
         *d++ = sum;
      }
   }
}

int main()
{
   A = (float *) malloc(SIZE*SIZE*sizeof(float));
   B = (float *) malloc(SIZE*SIZE*sizeof(float));
   D = (float *) malloc(SIZE*SIZE*sizeof(float));
   f();
   free(D);
   free(B);
   free(A);

   return 0;
}
