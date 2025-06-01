with Ada.Command_Line;

with Ada.Directories;
use  Ada.Directories;

with Ada.Text_IO;
use  Ada.Text_IO;

with TMK;
with Formats;
with Log;

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
      Ada.Text_IO.Put_Line (Bin_Name & ": [--help] <dir>");
   end Show_Help;

   procedure Do_Convert (Target : String)
   is
      W : Formats.Web.Web := Formats.Web.Ret;

      procedure Convert_File (Dir : Directory_Entry_Type) is
         Path : constant String := Full_Name (Dir);
         File : File_Type;
         Parser : TMK.Parser;
      begin
         Open (File, In_File, Path);

         while not End_Of_File (File) loop
            Parser.Feed (Get_Line (File));
         end loop;

         Close (File);

         W := Formats.Web.Render (Parser, W);

      end Convert_File;

   begin

      Search (
         Target,
         "",
         [Ordinary_File => True, others => False],
         Convert_File'Access);

      Formats.Web.Write_Out (W, "/tmp/target");

   end Do_Convert;

   Action_Requested : constant Action := Check_Args;

begin
   case Action_Requested.F is
      when Help => Show_Help;
      when Convert => Do_Convert
         (Ada.Command_Line.Argument (Action_Requested.Target));
   end case;

exception

   when Formats.Target_Exists =>
      Log.Print (Log.Error, "Target dir exists.");
      Ada.Command_Line.Set_Exit_Status (1);

end Passel;
