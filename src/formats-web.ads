with TMK;

with VSS.Strings;
use VSS.Strings;

with VSS.Characters;
use VSS.Characters;

package Formats.Web is

   type Page_Item is record
      Name : Virtual_String;
      Target : Virtual_String;
   end record;

   package TOC is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Page_Item);

   type Web (State : State_Flag := Init) is record
      Pages : Page_Vec.Vector;
      Index_Links : TOC.Vector;
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

   function Replace
      (X : Virtual_String; Y : Virtual_Character; Replacement : Virtual_String)
      return Virtual_String;

   procedure Finalize_Page (Self : in out Web);
   procedure Fixup_Index (Self : in out Web);
   function Scaffold_Page (Meta : TMK.Final_Meta_T) return Page;

end Formats.Web;
