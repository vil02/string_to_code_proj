#include <stdio.h>

void fun_b()
{}

void fun_a()
{
    fun_b();
    putchar('C');
}

int main()
{
    fun_a();
    return 0;
}
