-- CoreA Scheduling Benchmark (Ada)
-- Tests process creation with sys_fork

with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

procedure Sched_Bench is
   function Sys_Fork return int;
   pragma Import (C, Sys_Fork, "sys_fork");
   function Sys_Exit (Status : int) return int;
   pragma Import (C, Sys_Exit, "sys_exit");

   Iterations : constant := 100;
   Count : Natural := 0;
begin
   for I in 1 .. Iterations loop
      if Sys_Fork = 0 then
         Sys_Exit(0); -- Child exits
      else
         Count := Count + 1;
      end if;
   end loop;
   Put_Line("Scheduling Benchmark: Created " & Natural'Image(Count) & " processes");
   Sys_Exit(0);
end Sched_Bench;
