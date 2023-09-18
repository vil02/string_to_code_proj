with Ada.Text_IO;

procedure Main is
   procedure Fun_0 is
   begin
      null;
   end Fun_0;

   procedure Fun_1 is
   begin
      Fun_0;
      Ada.Text_IO.Put(Character'Val(67));
   end Fun_1;

begin
   Fun_1;
end Main;
