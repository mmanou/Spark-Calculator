with VariableStore;
with PIN;
with Stack;
with MyString;
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
        C.P      := P;
        C.Locked := True;
    end Lock;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) is
    begin
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

        if (if A >= 0 and B <= 0 then A > Integer'Last + B
            elsif A < 0 and B > 0 then A < Integer'First + B)
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
        A : Integer;
        B : Integer;
    begin
        if C.St.Length < 2 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Expected 2 numbers on stack.");
            return;
        end if;
        Stack.Pop (C.St, A);
        Stack.Pop (C.St, B);
        if ((A > 0 and B > 0) and then A > Integer'Last / B) or
           ((A < 0 and B > 0) and then A < Integer'First / B) or
            -- B = -1 will never fail

           ((A > 0 and B < -1) and then A > Integer'First / B) or
           ((A < 0 and B < 0) and then A < Integer'Last / B)
        then
            Put_Line
               ("Invalid operation: Result would cause integer overflow.");
            Stack.Push (C.St, B);
            Stack.Push (C.St, A);
            return;
        end if;
        Stack.Push (C.St, A * B);
    end Multiply;

    procedure Divide (C : in out Calculator) is
        A : Integer;
        B : Integer;
    begin
        if C.St.Length < 2 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Expected 2 numbers on stack.");
            return;
        end if;
        Stack.Pop (C.St, A);
        Stack.Pop (C.St, B);
        if B = 0 then
            Put_Line ("Invalid operation: Divide-by-zero is not allowed.");
            Stack.Push (C.St, B);
            Stack.Push (C.St, A);
            return;
        end if;
        if A = Integer'First and B = -1 then
            Put_Line
               ("Invalid operation: Result would cause integer overflow.");
            Stack.Push (C.St, B);
            Stack.Push (C.St, A);
            return;
        end if;
        Stack.Push (C.St, A / B);
    end Divide;

    procedure Push (C : in out Calculator; I : Integer) is
    begin
        if C.St.Length >= 512 then
            Put_Line ("Invalid operation: Stack is full.");
            return;
        end if;
        Stack.Push (C.St, I);
    end Push;

    procedure Pop (C : in out Calculator) is
        I : Integer;
    begin
        if C.St.Length < 1 or C.St.Length > 512 then
            Put_Line ("Invalid operation: Stack is empty.");
            return;
        end if;
        Stack.Pop (C.St, I);
        pragma Unreferenced (I);
    end Pop;

    procedure Load (C : in out Calculator; V : VariableStore.Variable) is
        I : Integer;
    begin
        if C.St.Length < 0 or C.St.Length >= 512 then
            Put_Line ("Invalid operation: Stack is full.");
            return;
        end if;
        if not VariableStore.Has_Variable (C.DB, V) then
            Put_Line ("Invalid operation: Variable not found in storage.");
            return;
        end if;
        I := VariableStore.Get (C.DB, V);
        Stack.Push (C.St, I);
    end Load;

    procedure Store (C : in out Calculator; V : VariableStore.Variable) is
        I : Integer;
    begin
        if C.St.Length < 1 or C.St.Length > 512 then
            Put_Line
               ("Invalid operation: Expected at least 1 number on stack.");
            return;
        end if;
        if VariableStore.Length (C.DB) >= VariableStore.Max_Entries and
           not VariableStore.Has_Variable (C.DB, V)
        then
            Put_Line
               ("Invalid operation: Storage is full. Try removing first.");
            return;
        end if;
        Stack.Pop (C.St, I);
        VariableStore.Put (C.DB, V, I);
    end Store;

    procedure Remove (C : in out Calculator; V : VariableStore.Variable) is
    begin
        if not VariableStore.Has_Variable (C.DB, V) then
            Put_Line ("Invalid operation: Variable not found in storage.");
            return;
        end if;
        VariableStore.Remove (C.DB, V);
    end Remove;

    procedure List (C : in Calculator) is
    begin
        VariableStore.Print (C.DB);
    end List;
end Calculator;
