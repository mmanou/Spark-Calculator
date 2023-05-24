package body Stack with
   SPARK_Mode
is
    procedure Push (St : in out Stack; I : in Integer) is
    begin
        St.Content (St.Length) := I;
        St.Length              := St.Length + 1;
    end Push;

    function Pop (St : in out Stack) return Integer is
    begin
        St.Length := St.Length - 1;
        return St.Content (St.Length + 1);
    end Pop;
end Stack;
