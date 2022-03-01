
#if 0
int strfromf(char *out, int n, char *f, float fp);
#endif

int main()
{
   char *result = (char *) malloc(32);
   float a = 1.0;
   float b = 2.0;
   float c = ( a + 3.0) * b + 8.0;
   strfromf(result, 32, "%f", c);
   printf("%s\n", result);
   c = a / b;
   strfromf(result, 32, "%f", c);
   printf("%s\n", result);
   c = a - b;
   strfromf(result, 32, "%f", c);
   printf("%s\n", result);
   c =  (7.0 + 3.0 * 2.0 - -3.0) / 4.0;
   strfromf(result, 32, "%f", c);
   printf("%s\n", result);
   return 0;
}


