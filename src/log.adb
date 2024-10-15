with Ada.Text_IO;
use Ada.Text_IO;

package body Log is

   Tags : constant Tag_List :=
      [Trace => "[TRACE]",
       Info  => "[INFO ]",
       Warn  => "[WARN ]",
       Error => "[ERROR]",
       Fatal => "[FATAL]"];

   procedure Print (L : Level; Msg : String)
   is
      Tag : constant String := Tags (L);
   begin
      Put_Line (Tag & " " & Msg);
   end Print;

end Log;