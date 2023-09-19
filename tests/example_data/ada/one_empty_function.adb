with Ada.Text_IO;

procedure Main is
   procedure Proc_0 is
   begin
      null;
   end Proc_0;

   procedure Proc_1 is
   begin
      Proc_0;
      Ada.Text_IO.Put(Character'Val(67));
   end Proc_1;

begin
   Proc_1;
end Main;
