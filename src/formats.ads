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

   type Web (State : State_Flag := Init) is record
      Pages : Page_Vec.Vector;

      case State is
         when Init => null;
         when Building_Page => Active : Page;
      end case;
   end record;

   function Ret return Web;
   function Bind (Self : Web; F : TMK.Renderer_Feed) return Web;
   function Render is new TMK.Render (Renderer => Web, Bind => Bind);

   procedure Write_Out (W : in out Web; Dir : String);

private

   procedure Finalize_Page (Self : in out Web);

end Formats;
