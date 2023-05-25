package body Stack with
   SPARK_Mode
is
    procedure Init (St : out Stack) is
    begin
        St.Length := 0;
        St.Content := (others => 0);
    end Init;

    procedure Push (St : in out Stack; I : in Integer) is
    begin
        St.Length              := St.Length + 1;
        St.Content (St.Length) := I;
    end Push;

    procedure Pop (St : in out Stack; I : out Integer) is
    begin
        I := St.Content (St.Length);
        St.Length := St.Length - 1;
    end Pop;
end Stack;
