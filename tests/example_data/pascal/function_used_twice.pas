program Main;

procedure p_0();
begin
  write('{');
  write('%')
end;


procedure p_1();
begin
  p_0();
  WriteLn();
  p_0()
end;


begin
  p_1()
end.
