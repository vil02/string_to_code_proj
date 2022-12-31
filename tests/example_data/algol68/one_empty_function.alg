PROC fun_b = VOID :();


PROC fun_a = VOID :(
  fun_b;
  print("C")
);


fun_a
