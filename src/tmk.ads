--  Tali's MarKup language
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with VSS.Strings;

package TMK is

   type Parser is tagged limited private;

   procedure Feed (P : in out Parser; Line : String);

   package SU renames Ada.Strings.Unbounded;

   type Block_Type is (Paragraph, Heading);

   subtype Heading_Level is Natural range 1 .. 6;

   type Phrase_Style is (Normal, Italic);

   type Phrase is record
     Text : VSS.Strings.Virtual_String;
     Style : Phrase_Style;
   end record;

   package Phrase_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Phrase);

   type Block (T : Block_Type := Paragraph) is record
      case T is
         when Paragraph => Text : Phrase_Vectors.Vector;
         when Heading =>
            Heading : SU.Unbounded_String;
            Level : Heading_Level;
      end case;
   end record;

   type Meta_T is record
      Title : SU.Unbounded_String;
      Author : SU.Unbounded_String;
      Index : Boolean := False;
   end record;

   type Final_Meta_T is new Meta_T
     with Dynamic_Predicate => SU.Length (Final_Meta_T.Title) /= 0;

   type Feed_Kind is (Block_Kind, Metadata_Kind);
   type Renderer_Feed (K : Feed_Kind := Block_Kind) is record
      case K is
         when Block_Kind => B : Block;
         when Metadata_Kind => M : Final_Meta_T;
      end case;
   end record;

   generic
      type Renderer is private;

      with function Feed (Self : Renderer; F : Renderer_Feed) return Renderer;
   function Render (P : Parser; R : Renderer) return Renderer;

private

   package SF renames Ada.Strings.Fixed;

   type Parser_States is (Header, Expecting_Block, In_Par);

   package Blocks is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Block);

   type Parser is tagged limited record
      Meta : Meta_T;
      Block_List : Blocks.Vector;
      State : Parser_States := Header;
   end record;

   function Feed_Header_Line (P : in out Parser; Line : String)
      return Boolean
      with Pre => P.State = Header;

   function Feed_Expecting_Block (P : in out Parser; Line : String)
      return Boolean;

   function Feed_In_Par
     (P : in out Parser; Line : String; Should_Break : Boolean)
     return Boolean
     with Pre => P.State = In_Par;

   procedure Parse_Metadata (P : in out Parser; Line : String);

   function Eat_Space (X : String) return String;

end TMK;
