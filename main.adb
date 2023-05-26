pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;      use Ada.Containers;
with Ada.Long_Long_Integer_Text_IO;
with Calculator;
with Utils;

procedure Main is
    C : Calculator.Calculator;
    package Lines is new MyString (Max_MyString_Length => 2_048);
    S         : Lines.MyString;
    T         : MyStringTokeniser.TokenArray (1 .. 2) :=
       (others => (Start => 1, Length => 0));
    NumTokens : Natural;
begin

    if MyCommandLine.Argument_Count < 1 then
        Put_Line ("Need to provide a pin");
        return;
    end if;
    if Utils.Invalid_Pin (MyCommandLine.Argument (1)) then
        Put_Line ("Invalid Pin");
        return;
    end if;
    Calculator.Init (C, PIN.From_String (MyCommandLine.Argument (1)));

    loop
        if C.Locked then
            Put ("locked>   ");
            Lines.Get_Line (S);
            MyStringTokeniser.Tokenise (Lines.To_String (S), T, NumTokens);
            if NumTokens >= 1 then
                declare
                    Cmd : String :=
                       Lines.To_String
                          (Lines.Substring
                              (S, T (1).Start,
                               T (1).Start + T (1).Length - 1));
                begin
                    if Cmd = "unlock" and NumTokens >= 2 then
                        declare
                            SP : String :=
                               Lines.To_String
                                  (Lines.Substring
                                      (S, T (2).Start,
                                       T (2).Start + T (2).Length - 1));
                        begin
                            if Utils.Invalid_Pin (SP) then
                                Put_Line ("Invalid Pin.");
                                return;
                            end if;
                            declare
                                P : PIN.PIN := PIN.From_String (SP);
                            begin
                                Calculator.Unlock (C, P);
                            end;
                        end;
                    elsif Cmd = "lock" then
                        Put_Line ("Already locked.");
                    elsif Cmd = "+" or Cmd = "-" or Cmd = "*" or Cmd = "/" or
                       Cmd = "push" or Cmd = "pop" or Cmd = "load" or
                       Cmd = "store" or Cmd = "remove" or Cmd = "list"
                    then
                        Put_Line
                           ("Calculator is locked, cannot perform action.");
                    else
                        Put_Line ("Invalid Command.");
                        return;
                    end if;
                end;
            end if;
        else
            Put ("unlocked> ");
            Lines.Get_Line (S);
            MyStringTokeniser.Tokenise (Lines.To_String (S), T, NumTokens);
            if NumTokens >= 1 then
                declare
                    Cmd : String :=
                       Lines.To_String
                          (Lines.Substring
                              (S, T (1).Start,
                               T (1).Start + T (1).Length - 1));
                begin
                    if Cmd = "+" then
                        Calculator.Plus (C);
                    elsif Cmd = "-" then
                        Calculator.Minus (C);
                    elsif Cmd = "*" then
                        Calculator.Multiply (C);
                    elsif Cmd = "/" then
                        Calculator.Divide (C);
                    elsif Cmd = "push" then
                        if NumTokens >= 2 then
                            declare
                                I : Integer :=
                                   StringToInteger.From_String
                                      (Lines.To_String
                                          (Lines.Substring
                                              (S, T (2).Start,
                                               T (2).Start + T (2).Length -
                                               1)));
                            begin
                                Calculator.Push (C, I);
                            end;
                        end if;
                    elsif Cmd = "pop" then
                        Calculator.Pop (C);
                    elsif (Cmd = "load" or Cmd = "store" or Cmd = "remove") and
                       NumTokens >= 2
                    then
                        declare
                            SV : String :=
                               Lines.To_String
                                  (Lines.Substring
                                      (S, T (2).Start,
                                       T (2).Start + T (2).Length - 1));
                        begin
                            if SV'Length > VariableStore.Max_Variable_Length then
                                Put_Line ("Variable name too long.");
                                return;
                            end if;
                            declare
                                V : VariableStore.Variable :=
                                   VariableStore.From_String (SV);
                            begin
                                if Cmd = "load" then
                                    Calculator.Load (C, V);
                                elsif Cmd = "store" then
                                    Calculator.Store (C, V);
                                elsif Cmd = "remove" then
                                    Calculator.Remove (C, V);
                                end if;
                            end;
                        end;
                    elsif Cmd = "list" then
                        Calculator.List (C);
                    elsif Cmd = "lock" and NumTokens >= 2 then
                        declare
                            SP : String :=
                               Lines.To_String
                                  (Lines.Substring
                                      (S, T (2).Start,
                                       T (2).Start + T (2).Length - 1));
                        begin
                            if Utils.Invalid_Pin (SP) then
                                Put_Line ("Invalid Pin");
                                return;
                            end if;
                            declare
                                P : PIN.PIN := PIN.From_String (SP);
                            begin
                                Calculator.Lock (C, P);
                            end;
                        end;
                    elsif Cmd = "unlock" then
                        Put_Line ("Already Unlocked.");
                    else
                        Put_Line ("Invalid Command.");
                        return;
                    end if;
                end;
            end if;
        end if;
    end loop;
end Main;
