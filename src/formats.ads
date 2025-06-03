with Ada.Containers.Vectors;

with AHTML.Node;
with AHTML.Strings;

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

end Formats;
