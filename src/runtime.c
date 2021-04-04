#include <stdio.h>
#include <string.h>
#include <inttypes.h>

void __writeInteger(int32_t n)
{
    printf("%" PRId32, n);
}

void __writeByte(uint8_t b)
{
    printf("%" PRIu8, b);
}

void __writeChar(uint8_t b)
{
    putchar(b); 
}

void __writeString(uint8_t * s)
{
    fputs((char const *)s, stdout);
}

int32_t __readInteger()
{
    int32_t i;
    scanf(" %" SCNd32, &i);
    return i;
}

uint8_t __readByte()
{
    uint8_t b;
    scanf(" %" SCNu8, &b);
    return b;
}

uint8_t __readChar()
{
    return getchar();
}

void __readString(int32_t n, uint8_t * s)
{
    if (fgets((char *)s, n, stdin) != NULL)
	s[strcspn((char *)s, "\n")] = '\0';
}

int32_t __extend(uint8_t b)
{
    return b; 
}

uint8_t __shrink(int32_t i)
{
    return (uint8_t)(i & 0xff);
}

int32_t __strlen(uint8_t * s)
{
    return strlen((char const *)s);
}

int32_t __strcmp(uint8_t * s1, uint8_t * s2)
{
    return strcmp((char const *)s1, (char const *)s2);
}

void __strcpy(uint8_t * trg, uint8_t * src)
{
    strcpy((char *)trg, (char const *)src);
}

void __strcat(uint8_t * trg, uint8_t * src)
{
    strcat((char *)trg, (char const *)src);
}
