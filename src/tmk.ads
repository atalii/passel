--  Tali's MarKup language
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package TMK is

   type Parser is tagged limited private;

   procedure Feed (P : in out Parser; Line : String);

   package SU renames Ada.Strings.Unbounded;
   package SF renames Ada.Strings.Fixed;

   type Block_Type is (Paragraph);

   type Block (T : Block_Type := Paragraph) is record
      case T is
         when Paragraph => Text : SU.Unbounded_String;
      end case;
   end record;

   type Meta_T is record
      Title : SU.Unbounded_String;
      Author : SU.Unbounded_String;
   end record;

   type Feed_Kind is (Block_Kind, Metadata_Kind);
   type Renderer_Feed (K : Feed_Kind := Block_Kind) is record
      case K is
         when Block_Kind => B : Block;
         when Metadata_Kind => M : Meta_T;
      end case;
   end record;

   generic
      type Renderer is private;

      with function Bind (Self : Renderer; F : Renderer_Feed) return Renderer;
   function Render (P : Parser; R : Renderer) return Renderer;

private

   type Parser_States is (Header, None);

   package Blocks is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Block);

   type Parser is tagged limited record
      Meta : Meta_T;
      Block_List : Blocks.Vector;
      State : Parser_States := Header;
   end record;

   procedure Feed_Header_Line (P : in out Parser; Line : String)
      with Pre => P.State = Header;
   procedure Feed_Paragraph_Line (P : in out Parser; Line : String)
      with Pre => P.State = None;

   procedure Parse_Metadata (P : in out Parser; Line : String);

   function Eat_Space (X : String) return String;

end TMK;