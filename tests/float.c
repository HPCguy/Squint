#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float atan2f(float y, float x);
float acosf(float x);

float dot(float *xx, float *yy, int n)
{
   float *x = xx, *y = yy;
   float retVal = 0.0f;
   for (int i = n; i > 0; --i) {
      retVal += *x++ * *y++;
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
   int ii = (int) 3.0f + 7;
   float xx[10], yy[10];
   float a = 1.0f;
   float b = 2.0f;
   float c = ( a + 3.0f) * b + 8.0f;
   float d = 3.0f + (float) 5;

   printf("%f %d\n", d, ii);

   printf("%f\n", c);
   c = (a / b) * (-b + a);
   printf("%f\n", c);
   c = sqrtf((7.0f + 3.0f * 2.0f - -3.0f) / 4.0f);
   printf("%f\n", c);

   if (1.0f < b) {
      printf("1.0 is less than %f\n", b);
   }

   c = atan2f(sqrtf(3.0f), 1.0f)*3.0f;
   printf("%f\n", c);

   for (i = 0.0f, ii = 0; i < 10.0f; i = i + 1.0f, ++ii) {
      xx[ii] = i + 1.0f; yy[ii] = 10.0f - i;
   }

   c = dot(xx, yy, 10) / (length(xx, 10) * length(yy, 10));
   printf("%f\n", c);
   d = acosf(c) ;
   printf("%f\n", d);

   ii = 2.0f;
   i = 3;
   printf("%d %f\n", ii, i);
   printf("%f %d\n", i, ii);

   return 0;
}
