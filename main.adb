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
                                Put_Line ("Invalid Pin");
                                return;
                            end if;
                            declare
                                P : PIN.PIN := PIN.From_String (SP);
                            begin
                                Calculator.Unlock (C, P);
                            end;
                        end;
                    elsif Cmd = "lock" then
                        Put_Line ("Already locked");
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
                    elsif Cmd = "load" and NumTokens >= 2 then
                        declare
                            V : VariableStore.Variable :=
                               VariableStore.From_String
                                  (Lines.To_String
                                      (Lines.Substring
                                          (S, T (2).Start,
                                           T (2).Start + T (2).Length - 1)));
                        begin
                            Calculator.Load (C, V);
                        end;
                    elsif Cmd = "store" and NumTokens >= 2 then
                        declare
                            V : VariableStore.Variable :=
                               VariableStore.From_String
                                  (Lines.To_String
                                      (Lines.Substring
                                          (S, T (2).Start,
                                           T (2).Start + T (2).Length - 1)));
                        begin
                            Calculator.Store (C, V);
                        end;
                    elsif Cmd = "remove" and NumTokens >= 2 then
                        declare
                            V : VariableStore.Variable :=
                               VariableStore.From_String
                                  (Lines.To_String
                                      (Lines.Substring
                                          (S, T (2).Start,
                                           T (2).Start + T (2).Length - 1)));
                        begin
                            Calculator.Remove (C, V);
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
                    end if;
                end;
            end if;
        end if;
    end loop;
end Main;
