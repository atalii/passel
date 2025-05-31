with Log;

with VSS.Strings.Conversions;
use VSS.Strings.Conversions;

package body TMK is

   function Is_Metadata (X : String) return Boolean is
      (SF.Head (X, 2) = "//");

   procedure Feed (P : in out Parser; Line : String)
   is
      Trimmed_Line : constant String := Eat_Space (Line);
   begin
      if Trimmed_Line'Length = 0 then
         if P.State = In_Par then
            P.State := Expecting_Block;
         end if;

         return;
      end if;

      if P.State = Header then
         P.Feed_Header_Line (Trimmed_Line);
      end if;

      --  The previous call might decide this isn't a header
      --  line. Then, we try the line again with different
      --  prod rules.

      if P.State = Header then
         return;
      end if;

      P.Feed_Paragraph_Line (Line);

   end Feed;

   function Eat_Space_L (X : String) return String
   is
      Start : Natural := X'First;
   begin
      for I in X'Range loop
         --  TODO: do the unicode thing; we should be using VSS rather than
         --  builtin strings.
         Start := I;
         exit when X (I) /= ' ';
      end loop;

      return X (Start .. X'Last);
   end Eat_Space_L;

   function Eat_Space_R (X : String) return String
   is
      Last : Natural := X'Last;
   begin
      for I in reverse X'Range loop
         Last := I;
         exit when X (I) /= ' ';
      end loop;

      return X (X'First .. Last);
   end Eat_Space_R;

   function Eat_Space (X : String) return String
   is (Eat_Space_L (Eat_Space_R (X)));

   procedure Feed_Header_Line (P : in out Parser; Line : String) is
   begin
      if not Is_Metadata (Line) then
         P.State := Expecting_Block;
         return;
      else
         P.Parse_Metadata (Line (Line'First + 1 .. Line'Last));
      end if;
   end Feed_Header_Line;

   procedure Feed_Paragraph_Line (P : in out Parser; Line : String)
   is
      Unbounded_Line : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Line);
      Par : constant Block := (T => Paragraph, Text => Unbounded_Line);

      procedure Add_Line (E : in out Block) is
      begin
         SU.Append (E.Text, " ");
         SU.Append (E.Text, Unbounded_Line);
      end Add_Line;
   begin
      case P.State is
         when Expecting_Block => P.Block_List.Append (Par);
         when In_Par =>
            P.Block_List.Update_Element
               (P.Block_List.Last, Add_Line'Access);

         --  TODO: we have a precond guaranteeing that this
         --  can't happen. we shouldn't need this branch.
         when others => null;
      end case;

      P.State := In_Par;
   end Feed_Paragraph_Line;

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
         else
            Log.Print (
               Log.Warn,
               To_Virtual_String ("Ignoring unknown: " & Key));

         end if;
      end With_Metadata;

   begin
      Log.Print (
         Log.Trace,
         To_Virtual_String ("Found metadata: " & Key & " := " & Val));

      With_Metadata (Key, Val);
   end Parse_Metadata;

   function Render (P : Parser; R : Renderer) return Renderer
   is
      Tmp : Renderer := R;
   begin
      Tmp := Bind (R, (K => Metadata_Kind, M => P.Meta));

      for Block of P.Block_List loop
         Tmp := Bind (Tmp, (K => Block_Kind, B => Block));
      end loop;

      return Tmp;
   end Render;

end TMK;
