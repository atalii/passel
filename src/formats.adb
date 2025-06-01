with Ada.Directories;

with VSS.Text_Streams.File_Output;
use VSS.Text_Streams.File_Output;

with VSS.Text_Streams;
use VSS.Text_Streams;

with VSS.Strings;
use VSS.Strings;

with VSS.Strings.Conversions;
use VSS.Strings.Conversions;

with Log;

package body Formats is

   Styles : constant String :=
      "main {" &
      "   max-width: 8.5in;" &
      "   margin-left: auto;" &
      "   margin-right: auto;" &
      "}";

   package body Web is

      procedure Write_Out (W : in out Web; Dir : String)
      is
         package Dirs renames Ada.Directories;

         procedure Write_Page (P : Page)
         is
            Out_Path : constant Virtual_String :=
               To_Virtual_String (Dir) &
               '/' &
               AHTML.Strings.Unwrap (P.File_Name) & ".html";

            F : File_Output_Text_Stream;
            Success : Boolean := True;
         begin
            Log.Print (Log.Info, "Writing file: " & Out_Path);

            Create (F, Out_Path);
            Put (F, P.Doc.To_String, Success);

            if not Success then
               Log.Print (Log.Error, "File IO failed.");
            end if;
            Close (F);
         end Write_Page;

         procedure Write_CSS
         is
            F : File_Output_Text_Stream;
            Success : Boolean := True;
         begin
            Log.Print (Log.Info, "Writing styles.css");
            Create (F, To_Virtual_String (Dir & "/styles.css"));
            Put (F, To_Virtual_String (Styles), Success);
            Close (F);
         end Write_CSS;

      begin
         Finalize_Page (W);

         if not Dirs.Exists (Dir) then
            Log.Print (Log.Info,
              To_Virtual_String ("Creating dir: " & Dir));

            Dirs.Create_Directory (Dir);
         else
            Log.Print (Log.Error,
               To_Virtual_String ("Dir exists: " & Dir));

            raise Target_Exists;
         end if;

         for P of W.Pages loop
            Write_Page (P);
         end loop;

         Write_CSS;
      end Write_Out;

      function Empty return Web is
         (State => Init, Pages => Page_Vec.Empty);

      function Bind (Self : Web; F : TMK.Renderer_Feed) return Web
      is

         Ret : Web := Self;

         procedure New_Page
         is
            use Page_Vec;

            D : AHTML.Node.Doc := AHTML.Node.HTML_Doc;
            R : constant AHTML.Node.Node_Handle := D.Mk_Element ("html");
            H : constant AHTML.Node.Node_Handle := D.Mk_Element ("head");
            T : constant AHTML.Node.Node_Handle := D.Mk_Element ("title");
            B : constant AHTML.Node.Node_Handle := D.Mk_Element ("body");
            M : constant AHTML.Node.Node_Handle := D.Mk_Element ("main");

            Style_Link : constant AHTML.Node.Node_Handle :=
               D.Mk_Element ("link");

            Style_Rel : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (AHTML.Strings.Denote ("rel"),
               AHTML.Strings.Cook ("stylesheet"));

            Style_Type : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (AHTML.Strings.Denote ("type"),
               AHTML.Strings.Cook ("text/css"));

            Style_Href : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (AHTML.Strings.Denote ("href"),
               AHTML.Strings.Cook ("styles.css"));

            Viewport : constant AHTML.Node.Node_Handle :=
               D.Mk_Element ("meta");

            Viewport_Name : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (AHTML.Strings.Denote ("name"),
               AHTML.Strings.Cook ("viewport"));

            Viewport_Content : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (AHTML.Strings.Denote ("content"), AHTML.Strings.Cook
                  ("width=device-width, initial-scale=1.0"));

            Cooked_Title : constant AHTML.Strings.Cooked :=
               AHTML.Strings.Cook (To_Virtual_String (F.M.Title));

            Title_Text : constant AHTML.Node.Node_Handle :=
               D.Mk_Text (Cooked_Title);

            Pages : Page_Vec.Vector := Self.Pages;
         begin
            D.With_Attribute (Style_Link, Style_Rel);
            D.With_Attribute (Style_Link, Style_Type);
            D.With_Attribute (Style_Link, Style_Href);

            D.With_Attribute (Viewport, Viewport_Name);
            D.With_Attribute (Viewport, Viewport_Content);

            D.With_Child (R, H);
            D.With_Child (H, T);
            D.With_Child (T, Title_Text);
            D.With_Child (R, Style_Link);
            D.With_Child (R, Viewport);

            D.With_Child (R, B);
            D.With_Child (B, M);

            if Self.State = Building_Page then
               Pages := @ & Self.Active;
            end if;

            Ret :=
               (State => Building_Page,
                Pages => Pages,
                Active =>
                   (File_Name => Cooked_Title, -- XXX: wrong escapes
                   Doc => D, Handle => M));
         end New_Page;

         procedure Add_Block
         is
            B : constant AHTML.Node.Node_Handle := Ret.Active.Handle;
            P : constant AHTML.Node.Node_Handle :=
               Ret.Active.Doc.Mk_Element ("p");

            Cooked_Content : constant AHTML.Strings.Cooked :=
               AHTML.Strings.Cook (To_Virtual_String (F.B.Text));

            T : constant AHTML.Node.Node_Handle :=
               Ret.Active.Doc.Mk_Text (Cooked_Content);
         begin
            Ret.Active.Doc.With_Child (P, T);
            Ret.Active.Doc.With_Child (B, P);
         end Add_Block;

      begin
         case F.K is
            when TMK.Metadata_Kind => New_Page;
            when TMK.Block_Kind => Add_Block;
         end case;

         return Ret;
      end Bind;

   end Web;

   procedure Finalize_Page (Self : in out Web.Web)
   is begin
      if Self.State = Building_Page then
         Self.Pages.Append (Self.Active);
      end if;

      Self := (State => Init, Pages => Self.Pages);
   end Finalize_Page;

end Formats;
