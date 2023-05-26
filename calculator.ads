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
       Pre  => C.Locked = False,
       Post =>
        Stack."=" (C.St, C'Old.St) and
        ((C'Old.Locked = False and PIN."=" (C.P, P)) or
         C'Old.Locked = True) and
        C.Locked = True;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) with
       Pre  => C.Locked = True,
       Post =>
        PIN."=" (C.P, C'Old.P) and Stack."=" (C.St, C'Old.St) and
        ((PIN."=" (C.P, P) and C.Locked = False) or
         (not PIN."=" (C.P, P) and C.Locked = True));

    procedure Plus (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
       -- enough inputs => try operation

        ((C'Old.St.Length >= 2 and
          (C.St.Length = C'Old.St.Length - 1 or
           C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

         (C'Old.St.Length < 2 and Stack."=" (C.St, C'Old.St)));

    procedure Minus (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
       -- enough inputs => try operation

        ((C'Old.St.Length >= 2 and
          (C.St.Length = C'Old.St.Length - 1 or
           C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

         (C'Old.St.Length < 2 and Stack."=" (C.St, C'Old.St)));

    procedure Multiply (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
       -- enough inputs => try operation

        ((C'Old.St.Length >= 2 and
          (C.St.Length = C'Old.St.Length - 1 or
           C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

         (C'Old.St.Length < 2 and Stack."=" (C.St, C'Old.St)));

    procedure Divide (C : in out Calculator) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
       -- enough inputs => try operation

        ((C'Old.St.Length >= 2 and
          (C.St.Length = C'Old.St.Length - 1 or
           C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

         (C'Old.St.Length < 2 and Stack."=" (C.St, C'Old.St)));

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
        (Stack."=" (C.St, C'Old.St) or C.St.Length - 1 = C'Old.St.Length);

    procedure Store (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
       -- enough inputs => try operation

        ((C'Old.St.Length >= 1 and
          (C.St.Length = C'Old.St.Length - 1 or
           C.St.Length = C'Old.St.Length)) or
       -- not enough inputs => do nothing

         (C'Old.St.Length < 1 and Stack."=" (C.St, C'Old.St)));

    procedure Remove (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => C.Locked = False,
       Post =>
        PIN."=" (C.P, C'Old.P) and C.Locked = C'Old.Locked and
        Stack."=" (C.St, C'Old.St);

    procedure List (C : in Calculator) with
       Global => null, Pre => C.Locked = False;
end Calculator;
