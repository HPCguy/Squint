#include <stdlib.h>
#include <stdio.h>

#define SIZE 3

int foo[4][4] =
{
   { 0, 1, 2.0, SIZE },
   { 4, 5, -6 } ,
   { },
   { 4*SIZE, 13, 14.0, 15.0 }
};

float goo[4][4] =
{
   { 0.0, 1.0, 2, SIZE },
   { 4.0, 5.0, -6.0 } ,
   { } ,
   { 4*SIZE, 13.0, 14, 15 }
};

int assert_eq(int a, int b)
{
    if (a != b) {
        printf("Assertion: %d != %d\n", a, b);
        exit(1);
    }
    return 0;
}

int main()
{
   int *p, i, j, k;
   int B[SIZE][SIZE][SIZE];
   int B_t[SIZE][SIZE][SIZE];
   int A[SIZE][SIZE];
   int A_t[SIZE][SIZE];

   printf("\nint foo[4][4]\n--------\n");

   for (i=0; i<4; ++i) {
      for(j=0; j<4; ++j)
         printf("%d ", foo[i][j]);
      printf("\n");
   }

   printf("\nfloat goo[4][4]\n--------\n");

   for (i=0; i<4; ++i) {
      for(j=0; j<4; ++j)
         printf("%f ", goo[i][j]);
      printf("\n");
   }

   p = (int *) A;

   for (i = 0; i < SIZE*SIZE; ++i) p[i] = i;

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         A_t[j][i] = A[i][j];
      }
   }

   printf("\nA[%d][%d]\n--------\n", SIZE, SIZE);

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         printf("%2d ", A[i][j]);
      }
      printf("\n");
   }

   printf("\nA^T[%d][%d]\n--------\n", SIZE, SIZE);

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         printf("%2d ", A_t[i][j]);
      }
      printf("\n");
   }

   p = (int *) B;

   for (i = 0; i < SIZE*SIZE*SIZE; ++i) p[i] = i;

   printf("\nB[%d][%d][%d]\n--------\n", SIZE, SIZE, SIZE);

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         for (k = 0; k < SIZE; ++k) {
            printf("%2d ", B[i][j][k]);
         }
         printf("\n");
      }
      printf("\n");
   }

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         for (k = 0; k < SIZE; ++k) {
            B_t[k][j][i] = B[i][j][k];
         }
      }
   }

   printf("\nB^T[%d][%d][%d]\n--------\n", SIZE, SIZE, SIZE);

   for (i = 0; i < SIZE; ++i) {
      for (j = 0; j < SIZE; ++j) {
         for (k = 0; k < SIZE; ++k) {
            printf("%2d ", B_t[i][j][k]);
         }
         printf("\n");
      }
      printf("\n");
   }

   printf("\n");

   i = SIZE - 1;

   assert_eq(B[SIZE-1][SIZE-1][SIZE-1], SIZE*SIZE*SIZE -1);
   assert_eq(B[SIZE-1][SIZE-1][  i   ], SIZE*SIZE*SIZE -1);
   assert_eq(B[SIZE-1][  i   ][SIZE-1], SIZE*SIZE*SIZE -1);
   assert_eq(B[SIZE-1][  i   ][  i   ], SIZE*SIZE*SIZE -1);
   assert_eq(B[  i   ][SIZE-1][SIZE-1], SIZE*SIZE*SIZE -1);
   assert_eq(B[  i   ][SIZE-1][  i   ], SIZE*SIZE*SIZE -1);
   assert_eq(B[  i   ][  i   ][SIZE-1], SIZE*SIZE*SIZE -1);
   assert_eq(B[  i   ][  i   ][  i   ], SIZE*SIZE*SIZE -1);

   return 0;
}
