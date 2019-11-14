' ===========================================================================
'
' GENERAL.BI
'
'  Copyright (C) 1989 Microsoft Corporation, All Rights Reserved
'
' ===========================================================================

' ===========================================================================
' RegType contains the register structure for performing BIOS calls using
' CALL INTERRUPT
' ===========================================================================

TYPE RegType              'for CALL INTERRUPT
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
END TYPE

' ===========================================================================
' GLOBAL CONSTANTS
' ===========================================================================

CONST FALSE = 0
CONST TRUE = -1
CONST MINROW = 2
CONST MAXROW = 25
CONST MINCOL = 1
CONST MAXCOL = 80
CONST MAXMENU = 10
CONST MAXITEM = 20
CONST MAXWINDOW = 10
CONST MAXBUTTON = 50
CONST MAXEDITFIELD = 20
CONST MAXHOTSPOT = 20

' ===========================================================================
' DECLARATIONS
' ===========================================================================

DECLARE SUB Interrupt (intnum AS INTEGER, inregs AS RegType, outregs AS RegType)
DECLARE SUB GetCopyBox (row1%, col1%, row2%, col2%, buffer$)
DECLARE SUB PutCopyBox (row%, col%, buffer$)
DECLARE SUB AttrBox (row1%, col1%, row2%, col2%, attr%)
DECLARE SUB PutBackground (row%, col%, buffer$)
DECLARE SUB GetBackground (row1%, col1%, row2%, col2%, buffer$)
DECLARE SUB Box (row1%, col1%, row2%, col2%, fore%, back%, border$, fillFlag%)
DECLARE SUB Scroll (row1%, col1%, row2%, col2%, lines%, attr%)
DECLARE FUNCTION GetShiftState% (bit%)
DECLARE FUNCTION AltToASCII$ (kbd$)

