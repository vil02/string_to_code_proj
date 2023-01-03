#include <cstdio>

inline void fun_0()
{
    std::putchar('{');
    std::putchar('%');
}

inline void fun_1()
{
    fun_0();
    std::putchar('\n');
    fun_0();
}

int main()
{
    fun_1();
    return 0;
}
