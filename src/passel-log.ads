with VSS.Strings;

package Passel.Log is

   type Level is (Trace, Info, Warn, Error, Fatal);

   procedure Print (L : Level; Msg : VSS.Strings.Virtual_String);

private

   type Tag_List is array (Level) of VSS.Strings.Virtual_String;

end Passel.Log;
