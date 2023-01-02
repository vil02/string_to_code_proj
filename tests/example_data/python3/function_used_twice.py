def fun_b():
    print("{", end="")
    print("%", end="")


def fun_a():
    fun_b()
    print()
    fun_b()


fun_a()
