--  We have proven the following security properties about our implementation:
--
--  The Lock, +, -, /, *, push, pop, load, store, remove operations can only be
--    performed while in the unlocked state.
--    This is validated by pre-condition: not Is_Locked(C)
--
--  The Unlock operation can only be performed while in the locked state.
--    This is validated by pre-condition: Is_Locked(C)
--
--  After performing the Lock operation, the stored PIN is equal to the provided PIN.
--    This is validated by post-condition: PIN."=" (Get_Pin(C), P)
--
--  The calculator starts in the locked state when initialised:
--    This is validated by post-condition: Is_Locked(C)
--
-- The Unlock, +, -, /, *, Push, Pop, Load, Store, Remove operations do not modify the PIN.
--    This is validated by post-condition: PIN."=" (Get_Pin(C), Get_Pin(C'Old))
--
-- The +, -, /, *, push, pop, load, store, remove do not modify the lock state.
--    This is validated by post-condition: C.Locked = C'Old.Locked
--
--  The Lock, Unlock and Remove operations do not modify the stack.
--    This is validated by post-condition: Stack."=" (C.St, C'Old.St)
--
--  The +, -, *, and / operations do not modify the stack if it contains less than 2 Integers.
--    This is validated by post-condition: (C'Old.St.Length < 2 and Stack."=" (C.St, C'Old.St))
--
--  The store operation does not modify the stack if it contains less than 1 Integer.
--    This is validated by post-condition: (C'Old.St.Length < 1 and Stack."=" (C.St, C'Old.St))
--
--  If the stack contains at least 2 Integers, the +, -, *, and / operations will leave it
--    it in a state with either the same number of elements, or one fewer.
--    This is validated by post-condition:
--      (C'Old.St.Length >= 2 and
--       (C.St.Length = C'Old.St.Length - 1 or C.St.Length = C'Old.St.Length))
--
--  If the stack contains at least 1 Integer, the store operation will leave it it in
--     a state with either the same number of elements, or one fewer.
--    This is validated by post-condition:
--      (C'Old.St.Length >= 1 and
--       (C.St.Length = C'Old.St.Length - 1 or C.St.Length = C'Old.St.Length))
--
--  The List operation makes no changes to calculator. It is an Input only.

pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Long_Long_Integer_Text_IO;
with Calculator;
with Utils;

procedure Main is
    C : Calculator.Calculator;
    package Lines is new MyString (Max_MyString_Length => 2_048);
    S : Lines.MyString;
    T : MyStringTokeniser.TokenArray (1 .. 2) :=
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
        if Calculator.Is_Locked (C) then
            Put ("locked>   ");
        else
            Put ("unlocked> ");
        end if;
        Lines.Get_Line (S);
        MyStringTokeniser.Tokenise (Lines.To_String (S), T, NumTokens);
        if NumTokens < 1 then
            Put_Line ("Invalid Input: empty command.");
            return;
        end if;
        if Lines.To_String (S)'Length > 2_048 then
            Put_Line ("Invalid Input: command too long.");
            return;
        end if;
        declare
            Cmd : String :=
               Lines.To_String
                  (Lines.Substring
                      (S, T (1).Start, T (1).Start + T (1).Length - 1));
        begin

            -- Assert valid input
            if (Cmd = "lock" or Cmd = "unlock") and NumTokens = 2 then
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
                end;
            elsif (Cmd = "load" or Cmd = "store" or Cmd = "remove") and
               NumTokens = 2
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
                end;
            elsif not
               ((
                 (Cmd = "+" or Cmd = "-" or Cmd = "*" or Cmd = "/" or
                  Cmd = "pop" or Cmd = "list") and
                 NumTokens = 1) or
                (Cmd = "push" and NumTokens = 2))
            then
                Put_Line ("Invalid command.");
                return;
            end if;

            -- Start processing
            -- Note some if statement have unneccessary condition to speed up proof time
            if Calculator.Is_Locked (C) then
                if Cmd = "unlock" and NumTokens = 2 then
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
                else
                    Put_Line ("Calculator is locked, cannot perform action.");
                end if;
            else
                if Cmd = "+" then
                    Calculator.Plus (C);
                elsif Cmd = "-" then
                    Calculator.Minus (C);
                elsif Cmd = "*" then
                    Calculator.Multiply (C);
                elsif Cmd = "/" then
                    Calculator.Divide (C);
                elsif Cmd = "push" and NumTokens = 2 then
                    declare
                        I : Integer :=
                           StringToInteger.From_String
                              (Lines.To_String
                                  (Lines.Substring
                                      (S, T (2).Start,
                                       T (2).Start + T (2).Length - 1)));
                    begin
                        Calculator.Push (C, I);
                    end;
                elsif Cmd = "pop" then
                    Calculator.Pop (C);
                elsif (Cmd = "load" or Cmd = "store" or Cmd = "remove") and
                   NumTokens = 2
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
                elsif Cmd = "lock" and NumTokens = 2 then
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
                end if;
            end if;
        end;
    end loop;
end Main;
