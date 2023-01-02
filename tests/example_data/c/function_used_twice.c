#include <stdio.h>

void fun_b()
{
    putchar('{');
    putchar('%');
}

void fun_a()
{
    fun_b();
    putchar('\n');
    fun_b();
}

int main()
{
    fun_a();
    return 0;
}
