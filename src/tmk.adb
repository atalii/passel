with Ada.Characters.Latin_1;

with Passel.Log;

with VSS.Characters;

with VSS.Strings;
use  VSS.Strings;

with VSS.Strings.Conversions;
use  VSS.Strings.Conversions;

with VSS.Strings.Cursors.Iterators.Characters;

package body TMK is
   pragma Assertion_Policy
     (Dynamic_Predicate => Check);

   function Is_Metadata (X : String) return Boolean is
      (SF.Head (X, 2) = "//");

   procedure Feed (P : in out Parser; Line : String)
   is

      function Transition_State_Fallible return Boolean is
      begin
         case P.State is
            when Header =>
               return P.Feed_Header_Line (Line);
            when Expecting_Block =>
               return P.Feed_Expecting_Block (Line);
            when In_Par =>
               return P.Feed_In_Par (Line, False);
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

   function Feed_In_Link (P : in out Parser; Line : String) return Boolean
   is
      I : Positive := 3;
      Label_Start : Positive;
      Label_End : Positive;
      Loc_Start : Positive;
   begin
      while Line (I) /= ' ' loop
         I := @ + 1;
      end loop;

      Label_Start := I;

      while Line (I) /= ':' loop
         I := @ + 1;
      end loop;

      Label_End := I - 1;
      Loc_Start := I + 1;

      declare
         B : constant Block :=
           (T => Link,
            Label => To_Virtual_String (Line (Label_Start .. Label_End)),
            Location => To_Virtual_String (Line (Loc_Start .. Line'Last)));
      begin
         P.Block_List.Append (B);
      end;

      return True;
   end Feed_In_Link;

   function Feed_Expecting_Block (P : in out Parser; Line : String)
      return Boolean
   is

      use type SU.Unbounded_String;

      Unbounded_Line : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Line);

      --  First, we'll calculate the header level. If this isn't in range of
      --  appropriate header levels (e.g., 0 in the case of no header), we'll
      --  parse as a paragraph.

      Observed_Header_Level : Natural := 0;
      Header_Scan_Index : Positive := 1;

   begin
      loop

         exit when Header_Scan_Index >= SU.Length (Unbounded_Line);
         exit when SU.Element (Unbounded_Line, Header_Scan_Index) /= '*';

         Observed_Header_Level := @ + 1;
         Header_Scan_Index := @ + 1;

      end loop;

      if Observed_Header_Level in Heading_Level then

         declare
            Head : constant Block :=
              (T => Heading,
                Heading => SU.To_Unbounded_String
                 (SU.Slice
                   (Unbounded_Line,
                    Header_Scan_Index, SU.Length (Unbounded_Line))),
                Level => Heading_Level (Observed_Header_Level));
         begin
            P.Block_List.Append (Head);
         end;

      elsif SU.Head (Unbounded_Line, 3) = "-> " then

         return P.Feed_In_Link (Line);

      elsif Unbounded_Line /= "" then

         P.State := In_Par;
         return Feed_In_Par (P, Line, True);

      end if;

      return True;
   end Feed_Expecting_Block;

   function Feed_In_Par
     (P : in out Parser; Line : String; Should_Break : Boolean)
     return Boolean
   is

      use VSS.Characters;
      use VSS.Strings.Cursors.Iterators.Characters;

      function Split_Phrases return Phrase_Vectors.Vector
      is

         Result : Phrase_Vectors.Vector := Phrase_Vectors.Empty_Vector;
         Virtual_Line : constant Virtual_String := To_Virtual_String (Line);

         Cursor : Character_Iterator := Virtual_Line.Before_First_Character;
         Last_Slash : Character_Iterator := Virtual_Line.At_First_Character;

         New_Phrase : Phrase;

         procedure Splice (Style : Phrase_Style) is
         begin
            New_Phrase :=
              (Text => Virtual_Line.Slice (Last_Slash, Cursor),
               Style => Style);

            Result.Append (New_Phrase);
         end Splice;

         Flag : Boolean;
         --  We want to be able to move iterators without caring whether they
         --  run off the end or start of the line.
         pragma Unreferenced (Flag);

      begin

         while Cursor.Forward loop
            if Cursor.Element = '/' then

               --  Move the cursor after and then before the '/'. These `exit
               --  when not`s will never trigger.
               Flag := Cursor.Backward;
               Splice (if P.In_Italics then Italic else Normal);
               Flag := Cursor.Forward;

               Flag := Cursor.Forward;
               Last_Slash.Set_At (Cursor);

               P.In_Italics := not P.In_Italics;
            end if;
         end loop;

         if Last_Slash.First_Character_Index
           /= Cursor.First_Character_Index
           and then Cursor.Backward
         then
            if Last_Slash.First_Character_Index = 0 then
               Last_Slash.Set_At (Virtual_Line.At_First_Character);
            end if;

            Splice (if P.In_Italics then Italic else Normal);
         end if;

         return Result;

      end Split_Phrases;

      procedure Add_Line (B : in out Block)
      is
         Phrases : Phrase_Vectors.Vector := Split_Phrases;
      begin
         if B.Text.Is_Empty then
            B.Text := Phrases;
            return;
         end if;

         if Phrases.Is_Empty then
            return;
         end if;

         declare
            F_P : Phrase_Vectors.Cursor := Phrases.First;
            Last_Old_Phrase : Phrase := B.Text (B.Text.Last);
            First_New_Phrase : constant Phrase := Phrases (F_P);
         begin
            if Last_Old_Phrase.Style = First_New_Phrase.Style then
               Last_Old_Phrase.Text.Append (" ");
               Last_Old_Phrase.Text.Append (First_New_Phrase.Text);
               B.Text.Replace_Element (B.Text.Last, Last_Old_Phrase);
               Phrases.Delete (F_P);
            end if;
         end;

         B.Text.Append (Phrases);
      end Add_Line;

   begin
      if Line = "" then

         P.State := Expecting_Block;

      elsif Should_Break then

         declare
            New_Block : constant Block :=
             (T => Paragraph, Text => Split_Phrases);
         begin
            P.Block_List.Append (New_Block);
         end;

      else

         P.Block_List.Update_Element (P.Block_List.Last, Add_Line'Access);

      end if;

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

         Val_VS : constant Virtual_String := To_Virtual_String (Val);
      begin
         if Key = "title" then
            P.Meta.Title := Val_UB;
         elsif Key = "author" then
            P.Meta.Author := Val_UB;
         elsif Key = "date" then
            P.Meta.Date := Val_VS;
         elsif Key = "footer" then
            P.Meta.Footer := Val_UB;
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
      Tmp := Feed (R, (K => Metadata_Kind, M => Final_Meta_T (P.Meta)));

      for Block of P.Block_List loop
         Tmp := Feed (Tmp, (K => Block_Kind, B => Block));
      end loop;

      return Tmp;
   end Render;

end TMK;
