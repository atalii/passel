with Ada.Directories;

with VSS.Text_Streams.File_Output;
use VSS.Text_Streams.File_Output;

with VSS.Text_Streams;
use VSS.Text_Streams;

with VSS.Strings;
use VSS.Strings;

with VSS.Strings.Conversions;
use VSS.Strings.Conversions;

with VSS.Transformers;
with VSS.Transformers.Casing;

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
            To_Virtual_String (Dir) & AHTML.Strings.Unwrap (P.File_Name);
            --  This is okay because all P.File_Name's end with a '/'.

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
         Style_Path : constant Virtual_String
           := To_Virtual_String (Dir) & "/styles.css";
      begin
         Log.Print (Log.Info, "Writing " & Style_Path);
         Create (F, Style_Path);
         Put (F, To_Virtual_String (Styles), Success);
         Close (F);
      end Write_CSS;

   begin

      Finalize_Page (W);
      Fixup_Index (W);

      if not Dirs.Exists (Dir) then
         Log.Print (Log.Info, "Creating dir: " & To_Virtual_String (Dir));
         Dirs.Create_Directory (Dir);

         Log.Print (Log.Info, "Creating dir: "
           & To_Virtual_String (Dir) & "/item");

         Dirs.Create_Directory (Dir & "/item");
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
      Index_Page => Scaffold_Page ("Index", True),
      Pages => Page_Vec.Empty);

   function Feed (Self : Web; F : TMK.Renderer_Feed) return Web
   is

      Ret : Web := Self;

      procedure New_Page
      is

         Title : constant Virtual_String :=
            To_Virtual_String (F.M.Title);

         P : constant Page := Scaffold_Page (Title, F.M.Index);

      begin

         if Self.State = Building_Page then
            Finalize_Page (Ret);
         end if;

         Ret :=
            (State => Building_Page,
             Pages => Ret.Pages,
             Index_Links => Ret.Index_Links,
             Index_Page => Ret.Index_Page,
             Active => P,
             Is_Index => F.M.Index);

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
      if Self.State = Building_Page and then not Self.Is_Index then
         Self.Pages.Append (Self.Active);
         Append (Self.Index_Links, Unwrap (Self.Active.File_Name));
      elsif Self.State = Building_Page and then Self.Is_Index then
         Self.Index_Page := Self.Active;
      end if;

      Self :=
         (State => Init,
         Pages => Self.Pages,
         Index_Links => Self.Index_Links,
         Index_Page => Self.Index_Page);

   end Finalize_Page;

   -----------------
   -- Fixup_Index --
   -----------------

   procedure Fixup_Index (Self : in out Web)
   is

      use type Page_Vec.Vector;
      use AHTML.Strings;

      U : constant AHTML.Node.Node_Handle
        := Self.Index_Page.Doc.Mk_Element ("ul");

   begin

      Self.Index_Page.Doc.With_Child (Self.Index_Page.Handle, U);

      for Link of Self.Index_Links loop
         declare
            L : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Element ("li");

            A : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Element ("a");

            T : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Text (Cook (Link));

            H : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (Denote ("href"), Cook (Link));
         begin
            Self.Index_Page.Doc.With_Child (U, L);
            Self.Index_Page.Doc.With_Child (L, A);
            Self.Index_Page.Doc.With_Child (A, T);

            Self.Index_Page.Doc.With_Attribute (A, H);
         end;
      end loop;

      --  Add this index to the pages list so that it'll actually be written.
      Self.Pages := @ & Self.Index_Page;

   end Fixup_Index;

   -------------------
   -- Scaffold_Page --
   -------------------

   function Scaffold_Page (Title : Virtual_String; Is_Index : Boolean := False)
      return Page
   is

      function Make_File_Name return AHTML.Strings.Cooked
      is

         use VSS.Transformers;
         use VSS.Transformers.Casing;

         Slug : constant Virtual_String := To_Lowercase.Transform (Title);
         --   FIXME: This is a vulnerability if title contains ../.

      begin
         if Is_Index then
            return AHTML.Strings.Cook ("/index.html");
         else
            return AHTML.Strings.Cook ("/item/" & Slug & ".html");
         end if;
      end Make_File_Name;

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
         AHTML.Strings.Cook ("/styles.css"));

      Viewport : constant AHTML.Node.Node_Handle :=
         D.Mk_Element ("meta");

      Viewport_Name : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
         (AHTML.Strings.Denote ("name"),
         AHTML.Strings.Cook ("viewport"));

      Viewport_Content : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
         (AHTML.Strings.Denote ("content"), AHTML.Strings.Cook
            ("width=device-width, initial-scale=1.0"));

      Title_Text : constant AHTML.Node.Node_Handle := D.Mk_Text
         (AHTML.Strings.Cook (Title));

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
         return (File_Name => Make_File_Name, Doc => D, Handle => M);

   end Scaffold_Page;

end Formats.Web;
