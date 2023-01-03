#include <stdio.h>

void fun_0()
{
    putchar('{');
    putchar('%');
}

void fun_1()
{
    fun_0();
    putchar('\n');
    fun_0();
}

int main()
{
    fun_1();
    return 0;
}
