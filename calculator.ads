with Ada.Containers; use Ada.Containers;
with VariableStore;  use VariableStore;
with PIN;
with Stack;
with MyString;

package Calculator with
   SPARK_Mode
is
    type Calculator is private;

    procedure Init (C : out Calculator; P : in PIN.PIN) with
       Post => PIN."=" (Get_Pin (C), P) and Is_Locked (C) = True;

    procedure Lock (C : in out Calculator; P : in PIN.PIN) with
       Pre  => not Is_Locked (C),
       Post =>
        Stack."=" (Get_Stack (C), Get_Stack (C'Old)) and
        ((not Is_Locked (C'Old) and PIN."=" (Get_Pin (C), P)) or
         Is_Locked (C'Old) = True) and
        Is_Locked (C) = True;

    procedure Unlock (C : in out Calculator; P : in PIN.PIN) with
       Pre  => Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Stack."=" (Get_Stack (C), Get_Stack (C'Old)) and
        ((PIN."=" (Get_Pin (C), P) and Is_Locked (C) = False) or
         (not PIN."=" (Get_Pin (C), P) and Is_Locked (C) = True));

    procedure Plus (C : in out Calculator) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
       -- enough inputs => try operation

        ((Get_Stack (C'Old).Length >= 2 and
          (Get_Stack (C).Length = Get_Stack (C'Old).Length - 1 or
           Get_Stack (C).Length = Get_Stack (C'Old).Length)) or
       -- not enough inputs => do nothing

         (Get_Stack (C'Old).Length < 2 and
          Stack."=" (Get_Stack (C), Get_Stack (C'Old))));

    procedure Minus (C : in out Calculator) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
       -- enough inputs => try operation

        ((Get_Stack (C'Old).Length >= 2 and
          (Get_Stack (C).Length = Get_Stack (C'Old).Length - 1 or
           Get_Stack (C).Length = Get_Stack (C'Old).Length)) or
       -- not enough inputs => do nothing

         (Get_Stack (C'Old).Length < 2 and
          Stack."=" (Get_Stack (C), Get_Stack (C'Old))));

    procedure Multiply (C : in out Calculator) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
       -- enough inputs => try operation

        ((Get_Stack (C'Old).Length >= 2 and
          (Get_Stack (C).Length = Get_Stack (C'Old).Length - 1 or
           Get_Stack (C).Length = Get_Stack (C'Old).Length)) or
       -- not enough inputs => do nothing

         (Get_Stack (C'Old).Length < 2 and
          Stack."=" (Get_Stack (C), Get_Stack (C'Old))));

    procedure Divide (C : in out Calculator) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
       -- enough inputs => try operation

        ((Get_Stack (C'Old).Length >= 2 and
          (Get_Stack (C).Length = Get_Stack (C'Old).Length - 1 or
           Get_Stack (C).Length = Get_Stack (C'Old).Length)) or
       -- not enough inputs => do nothing

         (Get_Stack (C'Old).Length < 2 and
          Stack."=" (Get_Stack (C), Get_Stack (C'Old))));

    procedure Push (C : in out Calculator; I : Integer) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old);

    procedure Pop (C : in out Calculator) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old);

    procedure Load (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
        (Stack."=" (Get_Stack (C), Get_Stack (C'Old)) or
         Get_Stack (C).Length - 1 = Get_Stack (C'Old).Length);

    procedure Store (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
       -- enough inputs => try operation

        ((Get_Stack (C'Old).Length >= 1 and
          (Get_Stack (C).Length = Get_Stack (C'Old).Length - 1 or
           Get_Stack (C).Length = Get_Stack (C'Old).Length)) or
       -- not enough inputs => do nothing

         (Get_Stack (C'Old).Length < 1 and
          Stack."=" (Get_Stack (C), Get_Stack (C'Old))));

    procedure Remove (C : in out Calculator; V : VariableStore.Variable) with
       Pre  => not Is_Locked (C),
       Post =>
        PIN."=" (Get_Pin (C), Get_Pin (C'Old)) and
        Is_Locked (C) = Is_Locked (C'Old) and
        Stack."=" (Get_Stack (C), Get_Stack (C'Old));

    procedure List (C : in Calculator) with
       Global => null, Pre => Is_Locked (C) = False;

    function Is_Locked (C : in Calculator) return Boolean;

    function Get_Pin (C : in Calculator) return PIN.PIN;

    function Get_Stack (C : in Calculator) return Stack.Stack;
private
    type Calculator is record
        St     : Stack.Stack;
        DB     : VariableStore.Database;
        P      : PIN.PIN;
        Locked : Boolean;
    end record;

    function Is_Locked (C : in Calculator) return Boolean is (C.Locked);

    function Get_Pin (C : in Calculator) return PIN.PIN is (C.P);

    function Get_Stack (C : in Calculator) return Stack.Stack is (C.St);
end Calculator;
