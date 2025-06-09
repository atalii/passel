with Ada.Strings.UTF_Encoding.Strings;
use  Ada.Strings.UTF_Encoding.Strings;

with Ada.Strings.UTF_Encoding;
use  Ada.Strings.UTF_Encoding;

with VSS.Strings.Conversions;
use  VSS.Strings.Conversions;

package body Passel.Util is

   function To_String (Source : Virtual_String) return String is
      (Decode (To_UTF_8_String (Source)));

end Passel.Util;
