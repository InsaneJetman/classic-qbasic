' ===========================================================================
'
' MOUSE.BI
'
'  Copyright (C) 1989 Microsoft Corporation, All Rights Reserved
'
' ===========================================================================

' ===========================================================================
' DECLARATIONS
' ===========================================================================

DECLARE SUB MouseHide ()
DECLARE SUB MouseShow ()
DECLARE SUB MousePoll (row%, col%, lButton%, rButton%)
DECLARE SUB MouseBorder (row1%, col1%, row2%, col2%)
DECLARE SUB MouseDriver (m0%, m1%, m2%, m3%)
DECLARE SUB MouseInit ()

COMMON SHARED /uitools/ MousePresent AS INTEGER

