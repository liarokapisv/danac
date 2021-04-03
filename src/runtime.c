#include <stdio.h>
#include <string.h>

void __writeString(char * ptr)
{
    fputs(ptr, stdout);
}

void __writeChar(char c)
{
    putchar(c); 
}

int __strlen(char * ptr)
{
    return strlen(ptr);
}

void __writeInteger(int c)
{
    printf("%d", c);
}

int __readInteger()
{
    int res;
    scanf(" %d", &res);
    return res;
}
