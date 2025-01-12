#include <stdio.h>

float fQ_rsqrt( float number )
{
   int i;
   float x2, y;

   x2 = number * 0.5F;
   i  = 0x5f3759df - ( (* ( int * ) &number) >> 1 );
   y  = * ( float * ) &i;
   y  = y * ( 1.5F - ( x2 * y * y ) );
   y  = y * ( 1.5F - ( x2 * y * y ) );
   y  = y * ( 1.5F - ( x2 * y * y ) );

   return y;
}

float Q_rsqrt(int zumber)
{
  union num { int i; float f; } n;
  float xd2, y;

  n.i = zumber - (1 << 23);
  xd2 = n.f;
  n.i = 0x5f3759df - ( zumber >> 1 );
  y = n.f;
  y = y * ( 1.5f - ( xd2 * y * y ) );
  y = y * ( 1.5f - ( xd2 * y * y ) );
  y = y * ( 1.5f - ( xd2 * y * y ) );

  return y;
}

int main()
{
   union zum { int i; float f; } z;
   z.f = 81.0;
   printf("%10.8f %10.8f\n", Q_rsqrt(z.i), fQ_rsqrt(z.f));

   return 0;
}
