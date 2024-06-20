#include <stdio.h>
#include <stdlib.h>

int mcmp(char *aa, char *bb, int nn)
{
    int ret, n;
    char *a, *b;
    n = nn; a = aa; b = bb;
    while (n--) {
        ret = *a++ - *b++;
        if (ret)
            return ret;
    }
    return 0;
}

void mcpy(char *aa, char *bb, int nn)
{
    int n;
    char *a, *b;

    n = nn; a = aa; b = bb;
    while (n--)
        *a++ = *b++;
}

int main()
{
    char p[128];
    int v;
    mcpy(p, "hello world", 12);
    printf("%s\n", p);
    printf("memcmp = %d\n", mcmp(p, "hello world", 12));
    printf("memcmp = %d\n", mcmp(p, "hello xorld", 12));
    printf("memcmp = %d\n", mcmp(p, "hello yorld", 12));
    p[0] = -1;
    v = p[0];
    printf("%x %d %d %x\n", p[0], p[0], v, p[1]);
    printf("\0"); /* shall be nothing generated */

    return 0;
}
