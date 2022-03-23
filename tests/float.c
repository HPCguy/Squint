#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#if 0
float atan2f(float y, float x);
float acosf(float x);
#endif

float dot(float *xx, float *yy, int n)
{
   float retVal = 0.0;
   int i;
   for (i = 0; i < n; ++i) {
      retVal = retVal + xx[i] * yy[i];
   }
   return retVal;
}

float length(float *xx, int n)
{
   return sqrtf(dot(xx, xx, n));
}

int main()
{
   float i;
   int ii;
   float *xx = (float *) malloc(10*sizeof(float));
   float *yy = (float *) malloc(10*sizeof(float));
   float a = 1.0;
   float b = 2.0;
   float c = ( a + 3.0) * b + 8.0;
   float d = 0.0;

   printf("%f\n", c);
   c = (a / b) * (-b + a);
   printf("%f\n", c);
   // c = sqrtf((7.0 + 3.0 * 2.0 - -3.0) / 4.0);
   // printf("%f\n", c);

   // if (1.0 < b) {
   //    printf("1.0 is less than %f\n", b);
   // }

   c = atan2f(sqrtf(3.0), 1.0)*3.0;
   printf("%f\n", c);

   for (i = 0.0, ii = 0; i < 10.0; i = i + 1.0, ++ii) {
      xx[ii] = i + 1.0; yy[ii] = 10.0 - i;
   }

   c = dot(xx, yy, 10) / (length(xx, 10) * length(yy, 10));
   printf("%f\n", c);
   // d = acosf(c) ;
   // printf("%f\n", d);

   ii = 2.0;
   i = 3;
   printf("%d %f\n", ii, i);
   printf("%f %d\n", i, ii);

   return 0;
}


