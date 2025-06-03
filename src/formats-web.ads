with TMK;

package Formats.Web is

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

private

   procedure Finalize_Page (Self : in out Web);

end Formats.Web;
