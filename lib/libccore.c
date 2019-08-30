
#include <stdio.h>
#include <string.h>

#include "molten.h"


#define BUFFER_SIZE     1024

int run_lib_libcore_c(void) {
    return 0;
}

char *readline(void) {
    int i = 0;
    char ch;
    char *buffer;

    buffer = molten_malloc(BUFFER_SIZE);
    while (i < BUFFER_SIZE - 1) {
        ch = fgetc(stdin);
        buffer[i++] = ch;
        if (ch == '\n')
            break;
    }

    buffer[i] = '\0';
    return molten_realloc(buffer, strlen(buffer));
}

