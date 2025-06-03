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

package body Formats.Web is

   Styles : constant String :=
      "main {" &
      "   max-width: 8.5in;" &
      "   margin-left: auto;" &
      "   margin-right: auto;" &
      "}";

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

      Add_Index (W);
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
      (State => Init,
      Index_Links => Empty_Virtual_String_Vector,
      Pages => Page_Vec.Empty);

   function Feed (Self : Web; F : TMK.Renderer_Feed) return Web
   is

      Ret : Web := Self;

      procedure New_Page
      is

         Cooked_Title : constant AHTML.Strings.Cooked :=
            AHTML.Strings.Cook (To_Virtual_String (F.M.Title));

         P : constant Page := Scaffold_Page (Cooked_Title);

      begin
         if Self.State = Building_Page then
            Finalize_Page (Ret);
         end if;

         Ret :=
            (State => Building_Page,
             Pages => Ret.Pages,
             Index_Links => Ret.Index_Links,
             Active => P);

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
   end Feed;

   procedure Finalize_Page (Self : in out Web)
   is
      use AHTML.Strings;
   begin
      if Self.State = Building_Page then
         Self.Pages.Append (Self.Active);
         Append (Self.Index_Links, Unwrap (Self.Active.File_Name));
      end if;

      Self :=
         (State => Init,
         Pages => Self.Pages,
         Index_Links => Self.Index_Links);

   end Finalize_Page;

   ---------------
   -- Add_Index --
   ---------------

   procedure Add_Index (Self : in out Web)
   is

      use type Page_Vec.Vector;
      use AHTML.Strings;

      P : Page := Scaffold_Page (AHTML.Strings.Cook ("index"));
      U : constant AHTML.Node.Node_Handle := P.Doc.Mk_Element ("ul");

   begin

      P.Doc.With_Child (P.Handle, U);

      for Link of Self.Index_Links loop
         declare
            L : constant AHTML.Node.Node_Handle := P.Doc.Mk_Element ("li");
            A : constant AHTML.Node.Node_Handle := P.Doc.Mk_Element ("a");
            T : constant AHTML.Node.Node_Handle := P.Doc.Mk_Text (Cook (Link));
            H : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (Denote ("href"), Cook ("/" & Link & ".html"));
         begin
            P.Doc.With_Child (U, L);
            P.Doc.With_Child (L, A);
            P.Doc.With_Child (A, T);

            P.Doc.With_Attribute (A, H);
         end;
      end loop;

      Self.Pages := @ & P;

   end Add_Index;

   -------------------
   -- Scaffold_Page --
   -------------------

   function Scaffold_Page (Title : AHTML.Strings.Cooked) return Page
   is

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

         Title_Text : constant AHTML.Node.Node_Handle := D.Mk_Text (Title);

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

         --  TODO: Using the given title as a file name and <title> is bad
         --  (and also a vulnerability if it contains ../).
         return (File_Name => Title, Doc => D, Handle => M);

   end Scaffold_Page;

end Formats.Web;
