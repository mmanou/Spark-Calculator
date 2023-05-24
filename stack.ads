package Stack with
   SPARK_Mode
is
    type StackIndexRange is range 1 .. 512;
    type StackContent is array (StackIndexRange) of Integer;
    type Stack is record
        Length  : Natural;
        Content : StackContent;
    end record;

    procedure Push (St : in out Stack; I : in Integer) with
       Pre  => St.Length < 512,
       Post =>
        St.Length = St'Old.Length + 1 and St.Content (St'Old.Length) = I;

    function Pop (St : in out Stack) return Integer with
       Pre  => St.Length > 1,
       Post =>
        St.Length = St'Old.Length - 1 and St.Content = St'Old.Content and
        Pop'Result = St.Content (St'Old.Length);
end Stack;
