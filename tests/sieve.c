/* create a baseline for optimization comparison */
/* Sieve of Eratosthenes using bit arrays */

#include <stdlib.h>
#include <string.h>

void sieve1(char *buf, int max)
{
    int i;
    int p = 2;

    do {
        i = 2 * p;
        while (i < max) {
            buf[i / 8] &= ~(1 << (i % 8));
            i += p;
        }
        ++p;
        while (p < max && !(buf[p / 8] & (1 << (p % 8))))
            ++p;
    } while (p < max);
}

void sieve2(char *buf, int max)
{
    int i;
    int p = 2;

    do {
        i = 2 * p;
        while (i < max) {
            switch (i % 8) {
            case 0: buf[i / 8] &= 0xfe; break;
            case 1: buf[i / 8] &= 0xfd; break;
            case 2: buf[i / 8] &= 0xfb; break;
            case 3: buf[i / 8] &= 0xf7; break;
            case 4: buf[i / 8] &= 0xef; break;
            case 5: buf[i / 8] &= 0xdf; break;
            case 6: buf[i / 8] &= 0xbf; break;
            case 7: buf[i / 8] &= 0x7f; break;
            }
            i += p;
        }
        ++p;
        while (p < max && !(buf[p / 8] & (1 << (p % 8))))
            ++p;
    } while (p < max);
}

void sieve3(char *buf, int max)
{
    int *bits = (int *) buf;
    int i;
    int p = 2;

    do {
        i = 2 * p;
        while (i < max) {
            bits[i / 32] &= ~(1 << (i % 32));
            i += p;
        }
        ++p;
        while (p < max && !(bits[p / 32] & (1 << (p % 32))))
            ++p;
    } while (p < max);
}

int main()
{
    int size = 256 * 1024;
    char *buf = (char *) malloc(size);

    memset(buf, 255, size);
    sieve1(buf, 8 * size);

    memset(buf, 255, size);
    sieve2(buf, 8 * size);

    memset(buf, 255, size);
    sieve3(buf, 8 * size);

    free(buf);

    return 0;
}
