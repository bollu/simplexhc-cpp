#include <stdint.h>
#include <stdio.h>

typedef void(*Continuation)(void);

typedef struct {
    Continuation *cont;
} Closure;

extern void *stackReturn[5000];
extern uint64_t stackReturnTop;

extern int64_t popInt();
extern void pushInt(int64_t);


void printOnlyInt(int64_t i) {
    printf("%ld", i);
}
