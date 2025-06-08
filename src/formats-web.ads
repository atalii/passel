with TMK;

with VSS.String_Vectors;
use VSS.String_Vectors;

with VSS.Strings;
use VSS.Strings;

package Formats.Web is

   type Web (State : State_Flag := Init) is record
      Pages : Page_Vec.Vector;
      Index_Links : Virtual_String_Vector;
      Index_Page : Page;

      case State is
         when Init => null;
         when Building_Page =>
            Active : Page;
            Is_Index : Boolean;
      end case;
   end record;

   function Empty return Web;
   function Feed (Self : Web; F : TMK.Renderer_Feed) return Web;
   function Render is new TMK.Render (Renderer => Web, Feed => Feed);

   procedure Write_Out (W : in out Web; Dir : Virtual_String);

private

   procedure Finalize_Page (Self : in out Web);
   procedure Fixup_Index (Self : in out Web);
   function Scaffold_Page (Title : Virtual_String; Is_Index : Boolean := False)
      return Page;

end Formats.Web;
