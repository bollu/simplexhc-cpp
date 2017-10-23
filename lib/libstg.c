#include <stdint.h>
#include <stdio.h>

extern int64_t popInt();
extern void pushInt(int64_t);

void printInt() {
    int64_t i = popInt();
    printf("%d", i);
}

void primMultiply() {
    int64_t i = popInt();
    int64_t j = popInt();
    pushInt(i * j);
}
