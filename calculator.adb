with VariableStore;
with PIN;
with Stack;
with MyString;

-- delete this later
with Ada.Text_IO; use Ada.Text_IO;

package body Calculator with
   SPARK_Mode
is
    type Calculator is record
        St     : Stack.Stack;
        DB     : VariableStore.Database;
        P      : PIN.PIN;
        Locked : Boolean;
    end record;

    procedure Init (C : out Calculator; P : in PIN.PIN) is
    begin
        C.P := P;
    end Init;

    procedure Lock (C : in out Calculator; P : in PIN.PIN) is
    begin
        Put_Line ("Hey");
    end Lock;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) is
    begin
        Put_Line ("Hey");
    end Unlock;

    procedure Plus (C : in out Calculator; R : in out Integer) is
    begin
        Put_Line ("Hey");
    end Plus;

    procedure Minus (C : in out Calculator; R : in out Integer) is
    begin
        Put_Line ("Hey");
    end Minus;

    procedure Multiply (C : in out Calculator; R : in out Integer) is
    begin
        Put_Line ("Hey");
    end Multiply;

    procedure Push (C : in out Calculator; N : MyString.MyString) is
    begin
        Put_Line ("Hey");
    end Push;

    procedure Pop (C : in out Calculator) is
    begin
        Put_Line ("Hey");
    end Pop;
end Calculator;
