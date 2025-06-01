with Ada.Containers.Vectors;

with AHTML.Node;
with AHTML.Strings;

with TMK;

package Formats is

   Target_Exists : exception;

   type State_Flag is (Init, Building_Page);

   type Page is record
      File_Name : AHTML.Strings.Cooked;
      Doc : AHTML.Node.Doc;
      Handle : AHTML.Node.Node_Handle;
   end record;

   package Page_Vec is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Page);

   package Web is
      type Web (State : State_Flag := Init) is record
         Pages : Page_Vec.Vector;

         case State is
            when Init => null;
            when Building_Page => Active : Page;
         end case;
      end record;

      function Empty return Web;
      function Feed (Self : Web; F : TMK.Renderer_Feed) return Web;
      function Render is new TMK.Render (Renderer => Web, Feed => Feed);

      procedure Write_Out (W : in out Web; Dir : String);
   end Web;

private

   procedure Finalize_Page (Self : in out Web.Web);

end Formats;
