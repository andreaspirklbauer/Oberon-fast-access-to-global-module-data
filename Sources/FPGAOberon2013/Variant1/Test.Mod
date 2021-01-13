MODULE A;  (*(c) J.Straube*)
  CONST x* = 14; y* = 35;
    S1* = "Dies ist ein langer String";
    S2* = "Hier ein anderer";
    S3* = "...und einer";

  VAR a*: ARRAY 70000 OF CHAR;
    b*: INTEGER;

  PROCEDURE Do*; END Do;
END A.

ORP.Compile A.Mod/s ~   ORP.Compile @/s ~
ORTool.DecSym A.smb ~
ORTool.DecObj A.rsc ~
ORTool.DecMod A

-------------------------------------

MODULE D;
  IMPORT A;
  CONST Str* = " Hallo";
  TYPE R = RECORD a: ARRAY 70000 OF CHAR; c: CHAR END;
 
  VAR r: ARRAY 1 OF R;
    b: ARRAY 32 OF CHAR;
    i*: INTEGER;

  PROCEDURE Do*; END Do;

BEGIN i := A.b;
END D.

ORP.Compile D.Mod/s ~   ORP.Compile @/s ~
System.Free D ~

ORTool.DecObj D.rsc ~
ORTool.DecSym D.smb ~
ORTool.DecMod D ~

D.Do
