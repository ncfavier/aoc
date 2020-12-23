#include <stdio.h>

#define NCUPS 1000000
#define NMOVES 10000000
#define WRAP(n) (((n) - 1 + NCUPS) % NCUPS + 1)

typedef struct cup {
    long label;
    struct cup *next;
} cup;

int main(void) {
    static cup cups[NCUPS];

    long init[9] = {3,9,8,2,5,4,7,1,6};
    int rinit[9] = {7,3,0,5,4,8,6,2,1};

    for (long i = 0; i < NCUPS; i++)
        cups[i] = (cup) { .label = i < 9 ? init[i] : i + 1
                        , .next = cups + (i + 1) % NCUPS };

    cup *cur = cups;
    for (int i = 0; i < NMOVES; i++){
        cup *moved[3] = {cur->next, cur->next->next, cur->next->next->next};

        int dest = cur->label;
        do
            dest = WRAP(dest - 1);
        while (  dest == moved[0]->label
              || dest == moved[1]->label
              || dest == moved[2]->label );

        cur->next = moved[2]->next;
        cup *pdest = &cups[dest < 10 ? rinit[dest - 1] : dest - 1];
        moved[2]->next = pdest->next;
        pdest->next = moved[0];
        cur = cur->next;
    }

    printf("%ld\n", cups[rinit[0]].next->label
                  * cups[rinit[0]].next->next->label);

    return 0;
}
