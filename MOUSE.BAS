'============================================================================
'
'    MOUSE.BAS - Mouse Support Routines for the User Interface Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
' NOTE:     This sample source code toolbox is intended to demonstrate some
'           of the extended capabilities of Microsoft BASIC 7.1 Professional
'           Development system that can help to leverage the professional
'           developer's time more effectively.  While you are free to use,
'           modify, or distribute the routines in this module in any way you
'           find useful, it should be noted that these are examples only and
'           should not be relied upon as a fully-tested "add-on" library.
'
'  PURPOSE: These routines are required for mouse support in the user
'           interface toolbox, but they may be used independently as well.
'
'  For information on creating a library and QuickLib from the routines
'  contained in this file, read the comment header of GENERAL.BAS.
'
'============================================================================

DEFINT A-Z

'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'menu.bi'

COMMON SHARED /uitools/ GloMenu      AS MenuMiscType
COMMON SHARED /uitools/ GloTitle()   AS MenuTitleType
COMMON SHARED /uitools/ GloItem()    AS MenuItemType

SUB MouseBorder (row1, col1, row2, col2) STATIC

    ' =======================================================================
    ' Sets max and min bounds on mouse movement both vertically, and
    ' horizontally
    ' =======================================================================

    MouseDriver 7, 0, (col1 - 1) * 8, (col2 - 1) * 8
    MouseDriver 8, 0, (row1 - 1) * 8, (row2 - 1) * 8

END SUB

SUB MouseDriver (m0, m1, m2, m3) STATIC

    DIM regs AS RegType

    IF MouseChecked = FALSE THEN
        DEF SEG = 0

        MouseSegment& = 256& * PEEK(207) + PEEK(206)
        MouseOffset& = 256& * PEEK(205) + PEEK(204)

        DEF SEG = MouseSegment&

        IF (MouseSegment& = 0 AND MouseOffset& = 0) OR PEEK(MouseOffset&) = 207 THEN
            MousePresent = FALSE
            MouseChecked = TRUE
            DEF SEG
        END IF
    END IF

    IF MousePresent = FALSE AND MouseChecked = TRUE THEN
        EXIT SUB
    END IF

    ' =======================================================================
    ' Calls interrupt 51 to invoke mouse functions in the MS Mouse Driver.
    ' =======================================================================
    
    regs.ax = m0
    regs.bx = m1
    regs.cx = m2
    regs.dx = m3

    Interrupt 51, regs, regs

    m0 = regs.ax
    m1 = regs.bx
    m2 = regs.cx
    m3 = regs.dx

    IF MouseChecked THEN EXIT SUB

    ' =======================================================================
    ' Check for successful mouse initialization
    ' =======================================================================

    IF m0 AND NOT MouseChecked THEN
        MousePresent = TRUE
        DEF SEG
    END IF

    MouseChecked = TRUE
    
END SUB

SUB MouseHide

    ' =======================================================================
    ' Decrements internal cursor flag
    ' =======================================================================

   MouseDriver 2, 0, 0, 0

END SUB

SUB MouseInit

    ' =======================================================================
    ' Mouse driver's initialization routine
    ' =======================================================================

    MouseDriver 0, 0, 0, 0

END SUB

SUB MousePoll (row, col, lButton, rButton) STATIC

    ' =======================================================================
    ' Polls mouse driver, then sets parms correctly
    ' =======================================================================

    MouseDriver 3, button, col, row
    row = row / 8 + 1
    col = col / 8 + 1
                                                
    IF button AND 1 THEN
        lButton = TRUE
    ELSE
        lButton = FALSE
    END IF

    IF button AND 2 THEN
        rButton = TRUE
    ELSE
        rButton = FALSE
    END IF

END SUB

SUB MouseShow

    ' =======================================================================
    ' Increments mouse's internal cursor flag
    ' =======================================================================

    MouseDriver 1, 0, 0, 0

END SUB

