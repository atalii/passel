with TMK;

with VSS.String_Vectors;

package Formats.Web is

   use  VSS.String_Vectors;

   type Web (State : State_Flag := Init) is record
      Pages : Page_Vec.Vector;
      Index_Links : Virtual_String_Vector;

      case State is
         when Init => null;
         when Building_Page => Active : Page;
      end case;
   end record;

   function Empty return Web;
   function Feed (Self : Web; F : TMK.Renderer_Feed) return Web;
   function Render is new TMK.Render (Renderer => Web, Feed => Feed);

   procedure Write_Out (W : in out Web; Dir : String);

private

   procedure Finalize_Page (Self : in out Web);
   procedure Add_Index (Self : in out Web);
   function Scaffold_Page (Title : AHTML.Strings.Cooked) return Page;

end Formats.Web;
