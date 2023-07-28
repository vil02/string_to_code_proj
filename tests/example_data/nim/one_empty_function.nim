func fun0() =
  discard


proc fun1() =
  fun0()
  stdout.write 'C'


fun1()
