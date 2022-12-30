#include <cstdio>

inline void fun_b()
{}

inline void fun_a()
{
    fun_b();
    std::putchar('C');
}

int main()
{
    fun_a();
    return 0;
}
