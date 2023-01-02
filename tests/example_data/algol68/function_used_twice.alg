PROC fun_b = VOID :(
  print("{");
  print("%")
);


PROC fun_a = VOID :(
  fun_b;
  print(newline);
  fun_b
);


fun_a
