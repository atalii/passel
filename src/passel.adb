with Ada.Directories;
use  Ada.Directories;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Command_Line;

with Ada.Strings.UTF_Encoding;
use Ada.Strings.UTF_Encoding;

with Ada.Strings.UTF_Encoding.Strings;
use Ada.Strings.UTF_Encoding.Strings;

with TMK;
with Formats;
with Log;

with VSS.Strings;
use VSS.Strings;

with VSS.Strings.Conversions;
use VSS.Strings.Conversions;

with VSS.Command_Line;

with GNAT.OS_Lib;

procedure Passel
is
   type Action is record
      Target : Virtual_String;
   end record;

   function Check_Args return Action
   is

      Target_Opt : constant VSS.Command_Line.Positional_Option :=
         (Name => "target",
         Description => "Location of site source.");

   begin
      VSS.Command_Line.Add_Option (Target_Opt);
      VSS.Command_Line.Add_Help_Option;

      VSS.Command_Line.Process;

      if not VSS.Command_Line.Is_Specified (Target_Opt) then
         Put_Line (Standard_Error, "Missing required argument.");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      return (Target => VSS.Command_Line.Value (Target_Opt));
   end Check_Args;

   procedure Do_Convert (Target : Virtual_String)
   is
      W : Formats.Web.Web := Formats.Web.Empty;

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
         Decode (To_UTF_8_String (Target)),
         "",
         [Ordinary_File => True, others => False],
         Convert_File'Access);

      Formats.Web.Write_Out (W, "/tmp/target");

   end Do_Convert;

   Action_Requested : constant Action := Check_Args;

begin

   Do_Convert (Action_Requested.Target);

exception

   when Formats.Target_Exists =>
      Log.Print (Log.Error, "Target dir exists.");
      Ada.Command_Line.Set_Exit_Status (1);

end Passel;
