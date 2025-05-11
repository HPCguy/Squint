#include <stdio.h>
#include <stdlib.h>

inline int max(int a, int b)
{
    return a > b ? a : b;
}

int fannkuchredux(int n)
{
    int perm[16];
    int perm1[16];
    int count[16];
    int maxFlipsCount = 0;
    int permCount = 0;
    int checksum = 0;

    for (int i=0; i<n; ++i)
        perm1[i] = i;

    int r = n;

    do {
        while (r != 1) {
            count[r-1] = r;
            --r;
        }

        for (int i=0; i<n; ++i)
            perm[i] = perm1[i];

        {
           int flipsCount = 0;
           int k;

           while ( (k = perm[0]) != 0 ) {
             int k2 = (k+1) >> 1;
             for (int i=0; i<k2; ++i) {
                int temp = perm[i];
                perm[i] = perm[k];
                perm[k] = temp;
                --k;
             }
             ++flipsCount;
           }

           maxFlipsCount = max(maxFlipsCount, flipsCount);
           checksum += permCount % 2 ? -flipsCount : flipsCount;
        }

        /* Use incremental change to generate another permutation */
        do {
            if (r == n) {
                // printf("%d\n", checksum);
                // *n = checksum;
                return maxFlipsCount;
            }

            int perm0 = perm1[0];
            int i = 0;
            while (i < r) {
                int j = i + 1;
                perm1[i] = perm1[j];
                i = j;
            }
            perm1[r] = perm0;
            count[r] = count[r] - 1;
            if (count[r] > 0) break;
            ++r;
        } while(1);
        ++permCount;
    } while(1);
}

int main(int argc, char *argv[])
{
    int checksum;
    int n = argc > 1 ? atoi(argv[1]) : 7;
    printf("Pfannkuchen(%d) = %d\n", n, fannkuchredux(n));
    // printf("%d\n", n);
    return 0;
}
