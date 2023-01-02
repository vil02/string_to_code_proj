#include <cstdio>

inline void fun_b()
{
    std::putchar('{');
    std::putchar('%');
}

inline void fun_a()
{
    fun_b();
    std::putchar('\n');
    fun_b();
}

int main()
{
    fun_a();
    return 0;
}
