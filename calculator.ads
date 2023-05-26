--  We have proved the following:
--
--  The Lock, +, -, /, *, push, pop, load, store, remove operations can only be
--    performed while in the unlocked state.
--    This is validated by pre-condition: C.Locked = False
--
--  The Unlock operation can only be performed while in the locked state.
--    This is validated by pre-condition: C.Locked = True;
--
-- The Unlock, +, -, /, *, Push, Pop, Load, Store, Remove operations do not modify the PIN.
--    This is validated by post-condition: PIN."=" (C.P, C'Old.P)
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
--  The List operation makes no changes to calculator. Tt is an Input only.

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
