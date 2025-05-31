with VSS.Strings;
use VSS.Strings;

with VSS.Text_Streams;

with VSS.Text_Streams.Standards;
use VSS.Text_Streams.Standards;

package body Log is

   Tags : constant Tag_List :=
      [Trace => "[TRACE]",
      Info  => "[INFO ]",
      Warn  => "[WARN ]",
      Error => "[ERROR]",
      Fatal => "[FATAL]"];

   procedure Print (L : Level; Msg : VSS.Strings.Virtual_String)
   is
      Tag : constant VSS.Strings.Virtual_String := Tags (L);
      Log_Line : constant VSS.Strings.Virtual_String :=
         Tag & ' ' & Msg;

      Success : Boolean := True;
      O : VSS.Text_Streams.Output_Text_Stream'Class := Standard_Error;
   begin
      VSS.Text_Streams.Put_Line (O, Log_Line, Success);
   end Print;

end Log;
