with Ada.Characters.Latin_1;

with Passel.Log;

with VSS.Strings;
use  VSS.Strings;

with VSS.Strings.Conversions;
use  VSS.Strings.Conversions;

package body TMK is

   function Is_Metadata (X : String) return Boolean is
      (SF.Head (X, 2) = "//");

   procedure Feed (P : in out Parser; Line : String)
   is

      Trimmed_Line : constant String := Eat_Space (Line);

      function Transition_State_Fallible return Boolean is
      begin
         case P.State is
            when Header =>
               return P.Feed_Header_Line (Trimmed_Line);
            when Expecting_Block =>
               return P.Feed_Expecting_Block (Trimmed_Line);
            when In_Par =>
               return P.Feed_In_Par (Trimmed_Line);
         end case;
      end Transition_State_Fallible;

   begin
      --  Transition_State_Fallible branches on P.State to determine how
      --  to treat this line. If all's well, it returns True. Otherwise,
      --  it changes P.State and tries again.
      loop
         exit when Transition_State_Fallible;
      end loop;

   end Feed;

   function Eat_Space_L (X : String) return String
   is
      Start : Natural := X'First;
   begin
      for I in X'Range loop
         --  TODO: do the unicode thing; we should be using VSS rather than
         --  builtin strings.
         Start := I;
         exit when X (I) /= ' ' and then X (I) /= Ada.Characters.Latin_1.LF;
      end loop;

      return X (Start .. X'Last);
   end Eat_Space_L;

   function Eat_Space_R (X : String) return String
   is
      Last : Natural := X'Last;
   begin
      for I in reverse X'Range loop
         Last := I;
         exit when X (I) /= ' ' and then X (I) /= Ada.Characters.Latin_1.LF;
      end loop;

      return X (X'First .. Last);
   end Eat_Space_R;

   function Eat_Space (X : String) return String
   is (Eat_Space_L (Eat_Space_R (X)));

   function Feed_Header_Line (P : in out Parser; Line : String)
      return Boolean is
   begin
      if Line = "" then
         return True;
      end if;

      if not Is_Metadata (Line) then
         P.State := Expecting_Block;
         return False;
      end if;

      P.Parse_Metadata (Line (Line'First + 1 .. Line'Last));
      return True;
   end Feed_Header_Line;

   function Feed_Expecting_Block (P : in out Parser; Line : String)
      return Boolean
   is

      use type SU.Unbounded_String;

      Unbounded_Line : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Line);

      Par : constant Block := (T => Paragraph, Text => Unbounded_Line);

   begin
      if SU.Head (Unbounded_Line, 2, '-') = "* " then
         declare
            Head : constant Block := (T => Heading, Heading =>
               SU.To_Unbounded_String
                  (SU.Slice (Unbounded_Line, 2, SU.Length (Unbounded_Line))));
         begin
            P.Block_List.Append (Head);
         end;

      elsif Unbounded_Line /= "" then

         P.Block_List.Append (Par);
         P.State := In_Par;

      end if;

      return True;
   end Feed_Expecting_Block;

   function Feed_In_Par (P : in out Parser; Line : String)
      return Boolean
   is
      Unbounded_Line : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Line);

      procedure Add_Line (E : in out Block) is
      begin
         SU.Append (E.Text, " ");
         SU.Append (E.Text, Unbounded_Line);
      end Add_Line;
   begin
      if Line = "" then
         P.State := Expecting_Block;
         return True;
      end if;

      P.Block_List.Update_Element
         (P.Block_List.Last, Add_Line'Access);

      return True;
   end Feed_In_Par;

   procedure Parse_Metadata (P : in out Parser; Line : String)
   is

      Cut_Line : constant String := Eat_Space (Line);
      Delim_Idx : constant Natural := SF.Index (Cut_Line, ":");

      Key : constant String := Eat_Space
         (Cut_Line (Cut_Line'First + 1 .. Delim_Idx - 1));

      Val : constant String := Eat_Space
         (Cut_Line (Delim_Idx + 1 .. Cut_Line'Last));

      procedure With_Metadata (Key, Val : String)
      is
         Val_UB : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Val);
      begin
         if Key = "title" then
            P.Meta.Title := Val_UB;
         elsif Key = "author" then
            P.Meta.Author := Val_UB;
         elsif Key = "index" then
            if Val = "true" then
               P.Meta.Index := True;
            else
               Passel.Log.Print (
                  Passel.Log.Warn,
                  To_Virtual_String ("Ignoring unknown index value: " & Val));
            end if;
         else
            Passel.Log.Print (
               Passel.Log.Warn,
               To_Virtual_String ("Ignoring unknown metadata key: " & Key));
         end if;
      end With_Metadata;

   begin
      Passel.Log.Print (
         Passel.Log.Trace,
         To_Virtual_String ("Found metadata: " & Key & " := " & Val));

      With_Metadata (Key, Val);
   end Parse_Metadata;

   function Render (P : Parser; R : Renderer) return Renderer
   is
      Tmp : Renderer := R;
   begin
      Tmp := Feed (R, (K => Metadata_Kind, M => P.Meta));

      for Block of P.Block_List loop
         Tmp := Feed (Tmp, (K => Block_Kind, B => Block));
      end loop;

      return Tmp;
   end Render;

end TMK;
