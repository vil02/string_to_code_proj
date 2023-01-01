program Main;

procedure fun_b();
begin
end;


procedure fun_a();
begin
  fun_b();
  write('C')
end;


begin
  fun_a()
end.
