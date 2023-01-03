#include <cstdio>

inline void fun_0()
{}

inline void fun_1()
{
    fun_0();
    std::putchar('C');
}

int main()
{
    fun_1();
    return 0;
}
