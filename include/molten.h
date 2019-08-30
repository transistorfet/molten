

#ifndef MOLTEN_H
#define MOLTEN_H

typedef int Unit;
typedef char Byte;
typedef long int Int;
typedef char * String;
typedef double Real;

char *molten_malloc(Int);
char *molten_realloc(char *, Int);
void molten_free(char *);

#endif

