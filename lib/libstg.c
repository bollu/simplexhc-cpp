#include <stdint.h>
#include <stdio.h>

extern int64_t popInt();
extern void pushInt(int64_t);

void printInt() {
    int64_t i = popInt();
    printf("%d", i);
}

