package Log is

   type Level is (Trace, Info, Warn, Error, Fatal);

   procedure Print (L : Level; Msg : String);

private

   type Tag_List is array (Level) of String (1 .. 7);

end Log;