
void factv(int n)
{
   if (n <= 1) 1;
   else (n * factv(n-1)) ;
}

void fibv(int n)
{
   if (n <= 1) 1;
   else ( fibv(n-1) + fibv(n-2) ) ;
}

float factf(float n)
{
   if (n <= 1.0) return 1.0;
   else return (n * factf(n-1.0)) ;
}

float fibf(float n)
{
   if (n <= 1.0) return 1.0;
   else return ( fibf(n-1.0) + fibf(n-2.0) ) ;
}

int fact(int n)
{
   if (n <= 1) return 1;
   else return (n * fact(n-1)) ;
}

int fib(int n)
{
   if (n <= 1) return 1;
   else return ( fib(n-1) + fib(n-2) ) ;
}

inline float dot(float *a, float *b, int len)
{
   float sum = 0.0;
   int i;
   for (i=0; i<len; ++i)
      sum += a[i]*b[i];
   return sum;
}

float norm(float *vec, int len)
{
   return sqrtf( dot(vec, vec, len) );
}

// float vec[4] = { 1.0, 2.0, 3.0, 4.0 };

int main()
{
  float vec[4];
  vec[0] = 1.0; vec[1] = 1.0;
  vec[2] = 1.0; vec[3] = 1.0;

  int x0 =        fib(7) + inline fact(4);
  int x1 = inline fib(7) + inline fact(4);
  int x2 =        fib(7) + inline fact(4);
  int x3 = inline fib(7) + inline fact(4);
  int x4 =        fib(7) +        fact(4);
  int x5 = inline fib(7) +        fact(4);
  int x6 =        fib(7) +        fact(4);
  int x7 = inline fib(7) +        fact(4);

  printf("%d %d %d %d %d %d %d %d\n",
         x0, x1, x2, x3, x4, x5, x6, x7);

  int v0 =        fibv(       factv(3));
  int v1 =        fibv(inline factv(3));
  int v2 = inline fibv(inline factv(3));
  // int v3 = inline fibv(       factv(1));

  printf("%d %d %d\n", v0, v1, v2 /* , v3 */);

  int y0 =        fib(       fact(3));
  int y1 =        fib(inline fact(3));
  int y2 = inline fib(inline fact(3));
  // int y3 = inline fib(       fact(3));

  printf("%d %d %d\n", y0, y1, y2 /* , y3 */);

  float z0 =        fibf((float)        fact(3));
  float z1 =        fibf((float) inline fact(3));
  float z2 = inline fibf((float) inline fact(3));
  // int z3 = inline fibf(       fact(3));

  printf("%f %f %f\n", z0, z1, z2 /* , y3 */);

  printf("%f %f\n", norm(vec, 4), inline norm(vec, 4) );

  return 0;
}
