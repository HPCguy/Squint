#define SIZE 256

float A[SIZE][SIZE], B[SIZE][SIZE], C[SIZE][SIZE];

int main()
{
   float sum;
   int r, c, k;
   for (r=0; r<SIZE; ++r) {
      for (c=0; c<SIZE; ++c) {
         sum = 0.0f;
         for (k=0; k<SIZE; ++k) {
            sum += A[r][k] * B[k][c];
         }
         C[r][c] = sum;
      }
   }

   return 0;
}
