with Ada.Directories;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with VSS.Text_Streams.File_Output;
use VSS.Text_Streams.File_Output;

with VSS.Text_Streams;
use VSS.Text_Streams;

with VSS.Strings.Conversions;
use VSS.Strings.Conversions;

with VSS.Strings.Formatters;
with VSS.Strings.Formatters.Integers;

with VSS.Transformers;
with VSS.Transformers.Casing;

with Passel.Log;
with Passel.Util;

package body Formats.Web is

   pragma Extensions_Allowed (All_Extensions);

   Styles : constant String with External_Initialization => "styles.css";

   procedure Write_Out (W : in out Web; Dir : Virtual_String)
   is

      package Dirs renames Ada.Directories;

      procedure Write_Page (P : Page)
      is
         Out_Path : constant Virtual_String :=
            Dir & AHTML.Strings.Unwrap (P.File_Name);
            --  This is okay because all P.File_Name's end with a '/'.

         F : File_Output_Text_Stream;
         Success : Boolean := True;
      begin
         Passel.Log.Print (Passel.Log.Info, "Writing file: " & Out_Path);

         Create (F, Out_Path);
         Put (F, P.Doc.To_String, Success);

         if not Success then
            Passel.Log.Print (Passel.Log.Error, "File IO failed.");
         end if;
         Close (F);
      end Write_Page;

      procedure Write_CSS
      is
         F : File_Output_Text_Stream;
         Success : Boolean := True;
         Style_Path : constant Virtual_String := Dir & "/styles.css";
      begin
         Passel.Log.Print (Passel.Log.Info, "Writing " & Style_Path);
         Create (F, Style_Path);
         Put (F, To_Virtual_String (Styles), Success);
         Close (F);
      end Write_CSS;

      String_Dir : constant String := Passel.Util.To_String (Dir);
      --  VSS doesn't give us anything to work with directories, it seems, so
      --  we need to do a whole thing to get it into a string that we can use
      --  with Ada.Directories. (We could presumably use Wide_Wide_Strings
      --  instead, but the eventual goal is to get VSS to have the interfaces
      --  that we need.)

   begin

      Finalize_Page (W);
      Fixup_Index (W);

      if not Dirs.Exists (String_Dir) then
         Passel.Log.Print (Passel.Log.Info, "Creating dir: " & Dir);
         Dirs.Create_Directory (String_Dir);

         Passel.Log.Print (Passel.Log.Info, "Creating dir: " & Dir & "/item");

         Dirs.Create_Directory (String_Dir & "/item");
      else
         Passel.Log.Print (Passel.Log.Error, "Dir exists: " & Dir);

         raise Target_Exists;
      end if;

      for P of W.Pages loop
         Write_Page (P);
      end loop;

      Write_CSS;
   end Write_Out;

   function Empty return Web is
      (State => Init,
      Index_Links => TOC.Empty_Vector,
      Index_Page => Scaffold_Page ((
         Title => To_Unbounded_String ("Index"),
         Author => To_Unbounded_String (""),
         Footer => To_Unbounded_String (""),
         Index => True)),
      Pages => Page_Vec.Empty);

   function Feed (Self : Web; F : TMK.Renderer_Feed) return Web
   is

      Ret : Web := Self;

      Basic_Formatting : VSS.Strings.Formatters.Format_Information;

      procedure New_Page
      is

         P : constant Page := Scaffold_Page (F.M);

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

         use TMK;

         B : constant AHTML.Node.Node_Handle := Ret.Active.Handle;

      begin
         case F.B.T is
            when TMK.Paragraph =>

               declare

                  P : constant AHTML.Node.Node_Handle :=
                     Ret.Active.Doc.Mk_Element ("p");

               begin
                  for Phrase of F.B.Text loop
                     case Phrase.Style is
                        when Normal =>
                           Ret.Active.Doc.With_Child
                             (P, Ret.Active.Doc.Mk_Text
                               (AHTML.Strings.Cook (Phrase.Text)));
                        when Italic =>
                           declare
                              E : constant AHTML.Node.Node_Handle :=
                                Ret.Active.Doc.Mk_Element ("em");
                           begin
                              Ret.Active.Doc.With_Child (P, E);
                              Ret.Active.Doc.With_Child
                                (E, Ret.Active.Doc.Mk_Text
                                  (AHTML.Strings.Cook (Phrase.Text)));
                           end;
                     end case;
                  end loop;

                  Ret.Active.Doc.With_Child (B, P);
               end;

            when TMK.Heading =>

               declare
                  H : constant AHTML.Node.Node_Handle :=
                    Ret.Active.Doc.Mk_Element (AHTML.Strings.Denote (
                      ("h" &
                       VSS.Strings.Formatters.Integers.Image
                         (F.B.Level).Format (Basic_Formatting))));

                  Cooked_Content : constant AHTML.Strings.Cooked :=
                     AHTML.Strings.Cook (To_Virtual_String (F.B.Heading));

                  T : constant AHTML.Node.Node_Handle :=
                     Ret.Active.Doc.Mk_Text (Cooked_Content);
               begin
                  Ret.Active.Doc.With_Child (H, T);
                  Ret.Active.Doc.With_Child (B, H);
               end;

            when TMK.Link =>

               declare
                  P : constant AHTML.Node.Node_Handle
                    := Ret.Active.Doc.Mk_Element (AHTML.Strings.Denote ("p"));

                  A : constant AHTML.Node.Node_Handle
                    := Ret.Active.Doc.Mk_Element (AHTML.Strings.Denote ("a"));

                  L : constant AHTML.Node.Node_Handle
                    := Ret.Active.Doc.Mk_Text
                      (AHTML.Strings.Cook (F.B.Label));

                  H : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
                    (AHTML.Strings.Denote ("href"),
                     AHTML.Strings.Cook (F.B.Location));

               begin
                  Ret.Active.Doc.With_Child (B, P);
                  Ret.Active.Doc.With_Child (P, A);
                  Ret.Active.Doc.With_Child (A, L);
                  Ret.Active.Doc.With_Attribute (A, H);
               end;

         end case;
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
      if Self.State = Building_Page and then
         Self.Active.Metadata.Footer /= ""
      then
         declare
            F : constant AHTML.Node.Node_Handle
              := Self.Active.Doc.Mk_Element ("footer");
            T : constant AHTML.Node.Node_Handle
              := Self.Active.Doc.Mk_Text (Cook
                 (To_Virtual_String (To_String
                    (Self.Active.Metadata.Footer))));
         begin
            Self.Active.Doc.With_Child (Self.Index_Page.Body_Handle, F);
            Self.Active.Doc.With_Child (F, T);
         end;
      end if;

      if Self.State = Building_Page and then not Self.Is_Index then
         Self.Pages.Append (Self.Active);

         declare
            --  Note: This type annotation is necessary to disambiguate calls
            --  to Self.Index_Links.Append.
            Link : constant Page_Item :=
              (Name => To_Virtual_String (Self.Active.Metadata.Title),
              Target => Unwrap (Self.Active.File_Name));
         begin
            Self.Index_Links.Append (Link);
         end;

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

      Self :=
         (State => Building_Page,
         Pages => Self.Pages,
         Index_Links => Self.Index_Links,
         Index_Page => Self.Index_Page,
         Active => Self.Index_Page,
         Is_Index => True);

      --  Add links to other pages.
      Self.Index_Page.Doc.With_Child (Self.Index_Page.Handle, U);

      for Link of Self.Index_Links loop
         declare
            L : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Element ("li");

            A : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Element ("a");

            T : constant AHTML.Node.Node_Handle
              := Self.Index_Page.Doc.Mk_Text (Cook (Link.Name));

            H : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
               (Denote ("href"), Cook (Link.Target));
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

   function Scaffold_Page (Meta : TMK.Final_Meta_T)
      return Page
   is

      Title : constant VSS.Strings.Virtual_String :=
         To_Virtual_String (To_String (Meta.Title));

      function Make_File_Name return AHTML.Strings.Cooked
      is

         use VSS.Transformers;
         use VSS.Transformers.Casing;

         Slug : constant Virtual_String := To_Lowercase.Transform (Title);
         --   FIXME: This is a vulnerability if title contains ../.

      begin
         if Meta.Index then
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

      Charset : constant AHTML.Node.Node_Handle :=
         D.Mk_Element ("meta");

      Charset_Attr : constant AHTML.Node.Attr := AHTML.Node.Mk_Attr
         (AHTML.Strings.Denote ("charset"), AHTML.Strings.Cook
            ("utf-8"));

      Title_Text : constant AHTML.Node.Node_Handle := D.Mk_Text
         (AHTML.Strings.Cook (Title));

   begin
         D.With_Attribute (Style_Link, Style_Rel);
         D.With_Attribute (Style_Link, Style_Type);
         D.With_Attribute (Style_Link, Style_Href);

         D.With_Attribute (Viewport, Viewport_Name);
         D.With_Attribute (Viewport, Viewport_Content);

         D.With_Attribute (Charset, Charset_Attr);

         D.With_Child (R, H);
         D.With_Child (H, T);
         D.With_Child (T, Title_Text);
         D.With_Child (R, Style_Link);
         D.With_Child (R, Viewport);
         D.With_Child (R, Charset);

         D.With_Child (R, B);
         D.With_Child (B, M);

         return (
            File_Name => Make_File_Name,
            Doc => D,
            Handle => M,
            Body_Handle => B,
            Metadata => TMK.Meta_T (Meta));

   end Scaffold_Page;

end Formats.Web;
