PROC p0 = VOID :(
  print("{");
  print("%")
);


PROC p1 = VOID :(
  p0;
  print(newline);
  p0
);


p1
