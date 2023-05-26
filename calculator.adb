with VariableStore;
with PIN;
with Stack;
with MyString;

-- delete this later
with Ada.Text_IO; use Ada.Text_IO;

package body Calculator with
   SPARK_Mode
is
    procedure Init (C : out Calculator; P : in PIN.PIN) is
    begin
        C.P      := P;
        C.Locked := True;
        Stack.Init (C.St);
        VariableStore.Init (C.DB);
    end Init;

    procedure Lock (C : in out Calculator; P : in PIN.PIN) is
    begin
        if (C.Locked = True) then
            return;
        end if;
        C.P      := P;
        C.Locked := True;
    end Lock;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) is
    begin
        if (C.Locked = False) then
            return;
        end if;
        if PIN."=" (C.P, P) then
            C.Locked := False;
        end if;
    end Unlock;

    procedure Plus (C : in out Calculator) is
        A : Integer;
        B : Integer;
    begin
        if C.St.Length < 2 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Expected 2 numbers on stack.");
            return;
        end if;
        Stack.Pop (C.St, A);
        Stack.Pop (C.St, B);

        if (if A >= 0 and B >= 0 then A > Integer'Last - B
            elsif A < 0 and B < 0 then A < Integer'First - B)
        then
            Put_Line
               ("Invalid operation: Result would cause integer overflow.");
            Stack.Push (C.St, B);
            Stack.Push (C.St, A);
            return;
        end if;
        Stack.Push (C.St, A + B);
    end Plus;

    procedure Minus (C : in out Calculator) is
        A : Integer;
        B : Integer;
    begin
        if C.St.Length < 2 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Expected 2 numbers on stack.");
            return;
        end if;
        Stack.Pop (C.St, A);
        Stack.Pop (C.St, B);

        if (if A >= 0 and B >= 0 then A < Integer'Last - B
            elsif A < 0 and B < 0 then A > Integer'First - B)
        then
            Put_Line
               ("Invalid operation: Result would cause integer overflow.");
            Stack.Push (C.St, B);
            Stack.Push (C.St, A);
            return;
        end if;
        Stack.Push (C.St, A - B);
    end Minus;

    procedure Multiply (C : in out Calculator) is
        I1 : Integer;
        I2 : Integer;
    begin
        Stack.Pop (C.St, I1);
        Stack.Pop (C.St, I2);
        Stack.Push (C.St, I1 * I2);
    end Multiply;

    procedure Divide (C : in out Calculator) is
        I1 : Integer;
        I2 : Integer;
    begin
        if C.St.Length < 2 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Expected 2 numbers on stack.");
            return;
        end if;
        Stack.Pop (C.St, I1);
        Stack.Pop (C.St, I2);
        if I2 = 0 then
            Put_Line ("Invalid operation: Divide-by-zero is not allowed.");
            Stack.Push (C.St, I2);
            Stack.Push (C.St, I1);
            return;
        end if;
        if I1 = Integer'First and I2 = -1 then
            Put_Line
               ("Invalid operation: Result would cause integer overflow.");
            Stack.Push (C.St, I2);
            Stack.Push (C.St, I1);
            return;
        end if;
        Stack.Push (C.St, I1 / I2);
    end Divide;

    procedure Push (C : in out Calculator; I : Integer) is
    begin
        Stack.Push (C.St, I);
    end Push;

    procedure Pop (C : in out Calculator) is
        I : Integer;
    begin
        Stack.Pop (C.St, I);
        pragma Unreferenced (I);
    end Pop;

    procedure Load (C : in out Calculator; V : VariableStore.Variable) is
        I : Integer;
    begin
        I := VariableStore.Get (C.DB, V);
        Stack.Push (C.St, I);
    end Load;

    procedure Store (C : in out Calculator; V : VariableStore.Variable) is
        I : Integer;
    begin
        Stack.Pop (C.St, I);
        VariableStore.Put (C.DB, V, I);
    end Store;

    procedure Remove (C : in out Calculator; V : VariableStore.Variable) is
    begin
        VariableStore.Remove (C.DB, V);
    end Remove;

    procedure List (C : in Calculator) is
    begin
        VariableStore.Print (C.DB);
    end List;
end Calculator;
