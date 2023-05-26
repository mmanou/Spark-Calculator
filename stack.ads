package Stack with
   SPARK_Mode
is
    type StackContent is array (Natural range 1 .. 512) of Integer;
    type Stack is record
        Length  : Natural;
        Content : StackContent;
    end record;

    procedure Init (St : out Stack) with
       Post => St.Length = 0;

    procedure Push (St : in out Stack; I : in Integer) with
       Pre  => St.Length < 512,
       Post =>
        St.Length = St'Old.Length + 1 and St.Content (St.Length) = I;

    procedure Pop (St : in out Stack; I : out Integer) with
       Pre  => St.Length >= 1 and St.Length <= 512,
       Post =>
        St.Length = St'Old.Length - 1 and St.Content = St'Old.Content and
        I = St.Content (St'Old.Length);
end Stack;
