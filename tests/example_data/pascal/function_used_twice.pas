program Main;

procedure fun_b();
begin
  write('{');
  write('%')
end;


procedure fun_a();
begin
  fun_b();
  WriteLn();
  fun_b()
end;


begin
  fun_a()
end.
