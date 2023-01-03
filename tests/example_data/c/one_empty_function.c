#include <stdio.h>

void fun_0()
{}

void fun_1()
{
    fun_0();
    putchar('C');
}

int main()
{
    fun_1();
    return 0;
}
