proc fun0() =
    stdout.write '{'
    stdout.write '%'


proc fun1() =
    fun0()
    stdout.write '\n'
    fun0()


fun1()
