#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SAMPLES (32*1024)
#define BIAS 1000000000
#define BIAS_LEN 11

void conv_itoa(char *ss, int *ii, int nn)
{
   int *i = ii + nn;
   char *s = ss + nn*BIAS_LEN;

   do {
      int val = *--i;
      *--s = 0; // null terminator
      int k = BIAS_LEN - 1;
      do {
         int tmp = val % 10;
         *--s = tmp + '0';
         val /= 10;
      } while(--k > 0);
   } while (i > ii);

   return;
}

void conv_atoi(int *oo, char *ss, int nn)
{
   int *o = oo + nn;
   char *s = ss + nn*BIAS_LEN;
   int k = BIAS_LEN - 2;

   do {
      s -= BIAS_LEN; 
      char *ts = s;
      int val = *ts++ - '0';
      do {
        val = val * 10 + (*ts++ - '0');
      } while (--k > 0);
      k = BIAS_LEN - 2;
      *--o = val;
   } while (o > oo);

   return;
}

int main()
{
   int *i = (int *) malloc(SAMPLES*sizeof(int));
   int *o = (int *) malloc(SAMPLES*sizeof(int));
   char *s = (char *) malloc(SAMPLES*BIAS_LEN); 
   int *begin = i; 
   int *end = i + SAMPLES;

   do {
      int tmp = rand() & 0x7fffffff;
      *begin = tmp % BIAS + BIAS; 
   } while (++begin < end);

   for (int iter=0; iter < 30; ++iter) {
      conv_itoa(s, i, SAMPLES);
      conv_atoi(o, s, SAMPLES);
   }

   return memcmp(i, o, SAMPLES*sizeof(int));
}
