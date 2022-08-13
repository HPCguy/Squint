#define SIZE 256

float A[SIZE][SIZE], B[SIZE][SIZE], C[SIZE][SIZE];

int main()
{
   float sum;
   int r, c, k;
   for (r=0; r<SIZE; ++r) {
      for (c=0; c<SIZE; ++c) {
         sum = 0.0;
         for (k=0; k<SIZE; ++k) {
            sum = sum + A[r][k] * B[k][c];
            // sum += A[r][k] * B[k][c]; compound assign has bug 
         }
         C[r][c] = sum;
      }
   }

   return 0;
}
