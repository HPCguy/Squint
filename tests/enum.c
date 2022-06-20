#include <stdio.h>

enum color { RED, GREEN, YELLO };
enum { BLACK = 10, BLUE };

int main(void)
{
    int a = GREEN;
    printf("blue: %d\n", BLUE);
    printf("a:%d\n", a);
    printf("a + 1:%d\n", a + 1);
    return 0;
}
