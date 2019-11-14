'***
' QBX.BI - Assembly Support Include File
'
'       Copyright <C> 1987-1989 Microsoft Corporation
'
' Purpose:
'      This include file defines the types and gives the DECLARE
'       statements for the assembly language routines Absolute,
'       Interrupt, InterruptX, Int86Old, and Int86XOld.
'
'***************************************************************************
'
' Define the type needed for Interrupt
'
TYPE RegType
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE
'
' Define the type needed for InterruptX
'
TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE
'
'                 DECLARE statements for the 5 routines
'                 -------------------------------------
'
' Generate a software interrupt, loading all but the segment registers
'
DECLARE SUB Interrupt (intnum AS INTEGER,inreg AS RegType,outreg AS RegType)
'
' Generate a software interrupt, loading all registers
'
DECLARE SUB InterruptX (intnum AS INTEGER,inreg AS RegTypeX, outreg AS RegTypeX)
'
' Call a routine at an absolute address.
' NOTE: If the routine called takes parameters, then they will have to
'       be added to this declare statement before the parameter given.
'
DECLARE SUB Absolute (address AS INTEGER)
'
' Generate a software interrupt, loading all but the segment registers
'       (old version)
'
DECLARE SUB Int86Old (intnum AS INTEGER,_
		      inarray(1) AS INTEGER,_
		      outarray(1) AS INTEGER)
'
' Generate a software interrupt, loading all the registers
'       (old version)
'
DECLARE SUB Int86XOld (intnum AS INTEGER,_
		       inarray(1) AS INTEGER,_
		       outarray(1) AS INTEGER)
'
