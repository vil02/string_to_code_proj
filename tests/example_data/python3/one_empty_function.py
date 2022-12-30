def fun_b():
    pass


def fun_a():
    fun_b()
    print("C", end="")


fun_a()
