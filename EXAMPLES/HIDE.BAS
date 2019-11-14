' HIDE.BAS
'
' Changes file attribute bytes to hide or unhide a file's
' directory entry.
'
DECLARE SUB TestError (AXReg%, flags%)
' $INCLUDE: 'QB.BI'

DEFINT A-Z
DIM InRegs AS RegType, OutRegs AS RegType

' Get the file name and what to do.
CLS
PRINT "Hidden File Program": PRINT
INPUT "Enter file name: ", FileName$
DO
   INPUT "Hide or unhide (H or U): ", Action$
   Action$ = UCASE$(Action$)
LOOP WHILE Action$ <> "H" AND Action$ <> "U"

' Make the file name an ASCIIZ string for DOS.
FileName$ = FileName$ + CHR$(0)

' Get the current file attribute.
' Current attribute comes back in OutRegs.AX
InRegs.ax = &H4300
InRegs.dx = SADD(FileName$)
CALL INTERRUPT(&H21, InRegs, OutRegs)
CALL TestError(OutRegs.ax, OutRegs.flags)

' Change the hidden attribute bit in the old attribute byte value.
IF Action$ = "U" THEN
   InRegs.cx = OutRegs.cx AND &HFD
ELSE
   InRegs.cx = OutRegs.cx OR &H2
END IF
' Set AX to indicate the Change Atribute DOS function.
InRegs.ax = &H4301
CALL INTERRUPT(&H21, InRegs, OutRegs)
CALL TestError(OutRegs.ax, OutRegs.flags)

END
' If carry flag set, print error message and end program.
SUB TestError (AXReg, flags) STATIC
   IF (&H1 AND flags) <> 0 THEN
      ' Get the error number out of AX.
      SELECT CASE AXReg AND &HF
         CASE 2
            PRINT "File not found."
         CASE 3
            PRINT "Path not found."
         CASE 5
            PRINT "Access denied."
         CASE ELSE
            PRINT "Unrecognized error."
      END SELECT
      END
   END IF
END SUB
