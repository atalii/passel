with Log;

package body TMK is

   function Is_Metadata (X : String) return Boolean is
      (SF.Head (X, 2) = "//");

   procedure Feed (P : in out Parser; Line : String)
   is
      Trimmed_Line : constant String := Eat_Space (Line);
   begin
      if Trimmed_Line'Length = 0 then
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
         --  TODO: do the unicode thing
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
         P.State := None;
         return;
      else
         P.Parse_Metadata (Line (Line'First + 1 .. Line'Last));
      end if;
   end Feed_Header_Line;

   procedure Feed_Paragraph_Line (P : in out Parser; Line : String)
   is
      Unbounded_Line : constant SU.Unbounded_String :=
         SU.To_Unbounded_String (Line);
      Par : Block := (T => Paragraph, Text => Unbounded_Line);
   begin
      -- FIXME: literally just one line/par?? hello????
      P.Block_List.Append (Par);
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
            Log.Print (Log.Warn, "Ignoring unknown: " & Key);
         end if;
      end With_Metadata;

   begin
      Log.Print (Log.Trace, "Found metadata: " & Key & " := " & Val);
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