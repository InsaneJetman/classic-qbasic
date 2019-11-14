'============================================================================
'
'     GENERAL.BAS - General Routines for the User Interface Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  NOTE:    This sample source code toolbox is intended to demonstrate some
'           of the extended capabilities of Microsoft BASIC 7.1 Professional
'           Development system that can help to leverage the professional
'           developer's time more effectively.  While you are free to use,
'           modify, or distribute the routines in this module in any way you
'           find useful, it should be noted that these are examples only and
'           should not be relied upon as a fully-tested "add-on" library.
'
'  PURPOSE: These are the general purpose routines needed by the other
'           modules in the user interface toolbox.
'
'  To create a library and QuickLib containing the routines found
'  in this file, follow these steps:
'       BC /X/FS general.bas
'       LIB general.lib + general + uiasm + qbx.lib;
'       LINK /Q general.lib, general.qlb,,qbxqlb.lib;
'  Creating a library and QuickLib for any of the other UI toolbox files
'  (WINDOW.BAS, MENU.BAS and MOUSE.BAS) is done this way also.
'
'  To create a library and QuickLib containing all routines from
'  the User Interface toolbox follow these steps:
'       BC /X/FS general.bas
'       BC /X/FS window.bas
'       BC /X/FS mouse.bas
'       BC /X/FS menu.bas
'       LIB uitb.lib + general + window + mouse + menu + uiasm + qbx.lib;
'       LINK /Q uitb.lib, uitb.qlb,,qbxqlb.lib;
'  If you are going to use this QuickLib in conjunction with the font source
'  code (FONTB.BAS) or the charting source code (CHRTB.BAS), you need to
'  include the assembly code routines referenced in these files.  For the font
'  routines, perform the following LIB command after creating the library but
'  before creating the QuickLib as described above:
'       LIB uitb.lib + fontasm;
'  For the charting routines, perform the following LIB command after creating
'  the library but before creating the QuickLib as described above:
'       LIB uitb.lib + chrtasm;
'
'============================================================================

DEFINT A-Z

'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'

FUNCTION AltToASCII$ (kbd$)
    ' =======================================================================
    ' Converts Alt+A to A,Alt+B to B, etc.  You send it a string.  The right
    ' most character is compared to the string below, and is converted to
    ' the proper character.
    ' =======================================================================
    index = INSTR("xyz{|}~ÄÅ !" + CHR$(34) + "#$%&,-./012ÇÉ", RIGHT$(kbd$, 1))

    IF index = 0 THEN
        AltToASCII = ""
    ELSE
        AltToASCII = MID$("1234567890QWERTYUIOPASDFGHJKLZXCVBNM-=", index, 1)
    END IF

END FUNCTION

SUB Box (row1, col1, row2, col2, fore, back, border$, fillFlag) STATIC

    '=======================================================================
    '  Use default border if an illegal border$ is passed
    '=======================================================================

    IF LEN(border$) < 9 THEN
        t$ = "⁄ƒø≥ ≥¿ƒŸ"
    ELSE
        t$ = border$
    END IF

    ' =======================================================================
    ' Check coordinates for validity, then draw box
    ' =======================================================================

    IF col1 <= (col2 - 2) AND row1 <= (row2 - 2) AND col1 >= MINCOL AND row1 >= MINROW AND col2 <= MAXCOL AND row2 <= MAXROW THEN
        MouseHide
        BoxWidth = col2 - col1 + 1
        BoxHeight = row2 - row1 + 1
        LOCATE row1, col1
        COLOR fore, back
        PRINT LEFT$(t$, 1); STRING$(BoxWidth - 2, MID$(t$, 2, 1)); MID$(t$, 3, 1)
        LOCATE row2, col1
        PRINT MID$(t$, 7, 1); STRING$(BoxWidth - 2, MID$(t$, 8, 1)); MID$(t$, 9, 1);

        FOR a = row1 + 1 TO row1 + BoxHeight - 2
            LOCATE a, col1
            PRINT MID$(t$, 4, 1);

            IF fillFlag THEN
                PRINT STRING$(BoxWidth - 2, MID$(t$, 5, 1));
            ELSE
                LOCATE a, col1 + BoxWidth - 1
            END IF

            PRINT MID$(t$, 6, 1);
        NEXT a
        LOCATE row1 + 1, col1 + 1
        MouseShow
    END IF

END SUB

SUB GetBackground (row1, col1, row2, col2, buffer$) STATIC

    ' =======================================================================
    ' Create enough space in buffer$ to hold the screen info behind the box
    ' Then, call GetCopyBox to store the background in buffer$
    ' =======================================================================

    IF row1 >= 1 AND row2 <= MAXROW AND col1 >= 1 AND col2 <= MAXCOL THEN
        Wid = col2 - col1 + 1
        Hei = row2 - row1 + 1
        size = 4 + (2 * Wid * Hei)
        buffer$ = SPACE$(size)

        CALL GetCopyBox(row1, col1, row2, col2, buffer$)
    END IF

END SUB

FUNCTION GetShiftState (bit)

    ' =======================================================================
    ' Returns the shift state after calling interrupt 22
    '    bit 0 : right shift
    '        1 : left shift
    '        2 : ctrl key
    '        3 : alt key
    '        4 : scroll lock
    '        5 : num lock
    '        6 : caps lock
    '        7 : insert state
    ' =======================================================================

    IF bit >= 0 AND bit <= 7 THEN
        DIM regs AS RegType
        regs.ax = 2 * 256
        INTERRUPT 22, regs, regs
      
        IF regs.ax AND 2 ^ bit THEN
            GetShiftState = TRUE
        ELSE
            GetShiftState = FALSE
        END IF
    ELSE
        GetShiftState = FALSE
    END IF

END FUNCTION

SUB PutBackground (row, col, buffer$)

    ' =======================================================================
    ' This sub checks the boundries before executing the put command
    ' =======================================================================

    IF row >= 1 AND row <= MAXROW AND col >= 1 AND col <= MAXCOL THEN
        CALL PutCopyBox(row, col, buffer$)
    END IF

END SUB

SUB scroll (row1, col1, row2, col2, lines, attr)

    ' =======================================================================
    ' Make sure coordinates are in proper order
    ' =======================================================================

    IF row1 > row2 THEN
        SWAP row1, row2
    END IF
   
    IF col1 > col2 THEN
        SWAP col1, col2
    END IF

     ' ======================================================================
     ' If coordinates are valid, prepare registers, and call interrupt
     ' ======================================================================

    IF row1 >= MINROW AND row2 <= MAXROW AND col1 >= MINCOL AND col2 <= MAXCOL THEN
        DIM regs AS RegType

        IF lines < 0 THEN
            regs.ax = 256 * 7 + (-lines)
            regs.bx = 256 * attr
            regs.cx = 256 * (row1 - 1) + (col1 - 1)
            regs.dx = 256 * (row2 - 1) + (col2 - 1)
        ELSE
            regs.ax = 256 * 6 + lines
            regs.bx = 256 * (attr MOD 8) * 16
            regs.cx = 256 * (row1 - 1) + (col1 - 1)
            regs.dx = 256 * (row2 - 1) + (col2 - 1)
        END IF

        INTERRUPT 16, regs, regs
    END IF

END SUB

