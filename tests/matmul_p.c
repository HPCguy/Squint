#define SIZE 256

float A[SIZE][SIZE], B[SIZE][SIZE], D[SIZE][SIZE];

int main()
{
   float sum;
   float *a, *b = &B[0][0], *bc, *d = &D[0][0];
   int r, c, k;
   for (r=0; r<SIZE; ++r) {
      a = &A[r][0];
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

   return 0;
}
