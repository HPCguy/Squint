#include <stdio.h>

int main()
{
    int i;
    int *s, *e, v;
    int *first, *last;
    int data[10];

    s = (int *) 0xbebebeb0;
    e = (int *) 0xbebebeb4;
    v = e - s;
    if (v == 1)
        printf("passed\n");
    else
        printf("failed, e - s = %x\n", v);
    v = (int) (e - 1);
    if ((int *) v == s)
        printf("passed\n");
    else
        printf("failed, e - s = %x\n", v);

    for (i = 0; i < 10; ++i) data[i] = i;

    first = data; last = &data[9];

    for (i = 0; i < 10; ++i) {
       if (first[i] != *(first + i) ||
           last[-i] != *(last - i)) {
          printf("failed, iter %d\n", i);
       }
    }

    return 0;
}
