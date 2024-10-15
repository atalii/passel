with TMK;

package Out is

   type HTML is private;

   function Ret return HTML;
   function Bind (Self : HTML; F : TMK.Renderer_Feed) return HTML;

   function Render is new TMK.Render with
      Renderer => HTML,
      Bind => Bind;

private

   type HTML is record
      null;
   end record;

end Out;