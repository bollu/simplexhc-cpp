#include <stdint.h>
#include <stdio.h>

extern int64_t popInt();
void printInt() {
    int64_t i = popInt();
    printf("%d", i);
}
