-- CoreA CPU Benchmark (Ada)
-- Measures CPU performance with arithmetic loop

with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

procedure CPU_Bench is
   function Sys_Exit (Status : int) return int;
   pragma Import (C, Sys_Exit, "sys_exit");

   Iterations : constant := 1000000;
   Result : Long_Long_Integer := 0;
begin
   for I in 1 .. Iterations loop
      Result := Result + Long_Long_Integer(I) * 2;
   end loop;
   Put_Line("CPU Benchmark Result: " & Long_Long_Integer'Image(Result));
   Sys_Exit(0);
end CPU_Bench;
