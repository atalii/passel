with Ada.Command_Line;

with Ada.Directories;
use  Ada.Directories;

with Ada.Text_IO;
use  Ada.Text_IO;

with TMK;
with Formats;

procedure Passel
is
   type Action_Flag is (Help, Convert);

   type Action (F : Action_Flag := Help) is record
      case F is
         when Help => null;
         when Convert => Target : Positive;
      end case;
   end record;

   function Check_Args return Action
   is
      N : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if N < 1 then
         return (F => Help);
      end if;

      declare
         T : constant String := Ada.Command_Line.Argument (1);
      begin
         if T = "--help" then
            return (F => Help);
         end if;

         return (F => Convert, Target => 1);
      end;
   end Check_Args;

   procedure Show_Help
   is
      Bin_Name : constant String := Ada.Command_Line.Command_Name;
   begin
      Ada.Text_IO.Put_Line (Bin_Name & ": [--help] <fname>");
   end Show_Help;

   procedure Do_Convert (Target : String)
   is
      Ent : Directory_Entry_Type;
      Dir_Search : Search_Type;

      Parser : TMK.Parser;
      W : Formats.Web := Formats.Ret;

      procedure Convert_File (Path : String) is
         File : File_Type;
      begin

         Open (File, In_File, Path);

         while not End_Of_File (File) loop
            Parser.Feed (Get_Line (File));
         end loop;

         Close (File);

         W := Formats.Render (Parser, W);

      end Convert_File;

   begin
      Start_Search (Dir_Search, Target, "*.tmk");

      loop
         Get_Next_Entry (Dir_Search, Ent);
         if Kind (Ent) = Ordinary_File then
            Convert_File (Full_Name (Ent));
         end if;

         exit when not More_Entries (Dir_Search);
      end loop;

      Formats.Write_Out (W, "/tmp/target");
   end Do_Convert;

   Action_Requested : constant Action := Check_Args;

begin
   case Action_Requested.F is
      when Help => Show_Help;
      when Convert => Do_Convert
         (Ada.Command_Line.Argument (Action_Requested.Target));
   end case;
end Passel;
