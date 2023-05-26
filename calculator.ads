with Ada.Containers; use Ada.Containers;
with VariableStore;  use VariableStore;
with PIN;
with Stack;
with MyString;

package Calculator with
   SPARK_Mode
is
    type Calculator is record
        St     : Stack.Stack;
        DB     : VariableStore.Database;
        P      : PIN.PIN;
        Locked : Boolean;
    end record;

    procedure Init (C : out Calculator; P : in PIN.PIN) with
       Post => PIN."=" (C.P, P) and C.Locked = True;

    procedure Lock (C : in out Calculator; P : in PIN.PIN) with
       Post =>
        Stack."=" (C.St, C'Old.St) and VariableStore."=" (C.DB, C'Old.DB) and
        ((C'Old.Locked = False and PIN."=" (C.P, P)) or
         C'Old.Locked = True) and
        C.Locked = True;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) with
       Post =>
        PIN."=" (C.P, C'Old.P) and Stack."=" (C.St, C'Old.St) and
        VariableStore."=" (C.DB, C'Old.DB) and
        ((PIN."=" (C.P, P) and C.Locked = False) or
         (not PIN."=" (C.P, P) and C.Locked = C'Old.Locked));

    procedure Plus (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
       -- enough inputs => try operation

        (C'Old.St.Length >= 2 and PIN."=" (C.P, C'Old.P) and
         C.Locked = C'Old.Locked and
         (C.St.Length = C'Old.St.Length - 1 or
          C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

        (C'Old.St.Length < 2 and PIN."=" (C.P, C'Old.P) and
         C.Locked = C'Old.Locked and Stack."=" (C.St, C'Old.St));

    procedure Minus (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
       -- enough inputs => try operation

        (C'Old.St.Length >= 2 and PIN."=" (C.P, C'Old.P) and
         C.Locked = C'Old.Locked and
         (C.St.Length = C'Old.St.Length - 1 or
          C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

        (C'Old.St.Length < 2 and PIN."=" (C.P, C'Old.P) and
         C.Locked = C'Old.Locked and Stack."=" (C.St, C'Old.St));

    procedure Multiply (C : in out Calculator) with
       Pre  => C.Locked = False and C.St.Length >= 1,
       Post => PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked;

    procedure Divide (C : in out Calculator) with
       Pre  => C.Locked = False and C.St.Length >= 1,
       Post => PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked;

    procedure Push (C : in out Calculator; I : Integer) with
       Pre  => C.Locked = False,
       Post => PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked;

    procedure Pop (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post => PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked;

    procedure Load (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
        VariableStore."=" (C.DB, C'Old.DB) and
        C.St.Length = C'Old.St.Length + 1;

    procedure Store (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => C.Locked = False and C.St.Length >= 1,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
        VariableStore.Length (C.DB) = VariableStore.Length (C'Old.DB) + 1 and
        C.St.Length = C'Old.St.Length - 1;

    procedure Remove (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
        VariableStore.Length (C.DB) = VariableStore.Length (C'Old.DB) - 1 and
        Stack."=" (C.St, C'Old.St);

    procedure List (C : in Calculator) with
       Global => null, Pre => C.Locked = False;
end Calculator;
