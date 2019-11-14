'       FONTDEMO.BAS - FONTB demonstration program.
'
'  Copyright (C) 1989-1990 Microsoft Corporation, All Rights Reserved
'
'  This program demonstrates some of the capabilities of the fonts
'  toolbox.  It loads font files found in the current directory and
'  and allows you to select a font for display.
'
'  The following font files are provided with BASIC 7.1:
'     - Raster fonts designed for screen resolution of 640x200
'           COURA.FON
'           HELVA.FON
'           TMSRA.FON
'     - Raster fonts designed for screen resolution of 640x350
'           COURB.FON
'           HELVB.FON
'           TMSRB.FON
'     - Raster fonts designed for screen resolution of 640x480
'           COURE.FON
'           HELVE.FON
'           TMSRE.FON
'
' $INCLUDE: 'FONTB.BI'
CONST TRUE = -1
CONST FALSE = 0
DECLARE SUB DriveScreen ()
DECLARE SUB GetFiles ()
DECLARE SUB GetModes ()
DECLARE SUB ShowScreen ()

DIM SHARED FI AS FontInfo
DIM SHARED totalmodes AS INTEGER
DIM SHARED modes(1 TO 13) AS INTEGER
DIM SHARED fontfiles(1 TO 18) AS STRING
DIM SHARED totalfonts AS INTEGER
DIM SHARED currentfont AS INTEGER
DIM SHARED currentmode AS INTEGER
GetModes
GetFiles
currentfont = 1
DO
    DriveScreen
    ShowScreen
LOOP
END

'
'DriveScreen displays the lists of available fonts and screen modes and
'scrolls through them with arrow keys.
'
SUB DriveScreen STATIC
IF init% = 0 THEN
    set$ = "f"
    max% = totalfonts
    posit% = currentfont
    modedim$ = "320x200640x200720x348640x400              320x200"
    modedim$ = modedim$ + "640x200640x350640x350640x480640x480320x200"

    'Check if monitor supports color or mono.

    SELECT CASE modes(1)
        CASE 13, 9, 8, 7
            mode$ = "color"
        CASE 3, 4, 10
            mode$ = "mono"
        CASE 2
            IF modes(2) = 1 THEN
                mode$ = "color"
            ELSE
                mode$ = "mono"
            END IF
    END SELECT
    FOR i% = 1 TO totalmodes
        IF modes(i%) = 4 THEN mode$ = "mono"
    NEXT i%

    'Set colors based on type of monitor.

    SELECT CASE mode$
        CASE "color"
            listfore% = 7
            listback% = 0
            titleon% = 15
            titleoff% = 7
            titleback% = 1
            back% = 1
            high% = 15
        CASE "mono"
            listfore% = 7
            listback% = 0
            titleon% = 0
            titleoff% = 2
            titleback% = 7
            back% = 0
            high% = 7
    END SELECT
    init% = 1
END IF

'Display the screen with the current selections.

SCREEN 0
WIDTH 80, 25
LOCATE , , 0: COLOR 0, back%
PRINT SPACE$(1920)
LOCATE 2, 1: COLOR high%, back%
PRINT "  Font Toolbox Demo"
COLOR titleoff%, back%
PRINT "  Copyright (C) 1989-1990 Microsoft Corporation"
LOCATE 22, 1: COLOR titleoff%, back%
PRINT SPC(55); "<CR> to view fontfile"
PRINT SPC(55); "ESC to exit"

GOSUB swaptitles
GOSUB swaptitles
FOR i% = 1 TO totalfonts
    LOCATE 5 + i%, 20
    COLOR listfore%, listback%
    PRINT LEFT$(fontfiles(i%) + "       ", 12)
NEXT i%
LOCATE 5 + currentfont, 20
COLOR listback%, listfore%
PRINT LEFT$(fontfiles(currentfont) + "       ", 12)

FOR i% = 1 TO totalmodes
    LOCATE 5 + i%, 50
    COLOR listfore%, listback%
    PRINT LEFT$(STR$(modes(i%)) + "   ", 4) + MID$(modedim$, 7 * modes(i%) - 6, 7)
NEXT i%
LOCATE 5 + currentmode, 50
COLOR listback%, listfore%
PRINT LEFT$(STR$(modes(currentmode)) + "   ", 4) + MID$(modedim$, 7 * modes(currentmode) - 6, 7)

'Scroll through choices

DO
    SELECT CASE INKEY$
        CASE CHR$(0) + CHR$(72)
            GOSUB upone
        CASE CHR$(0) + CHR$(80)
            GOSUB downone
        CASE CHR$(9), CHR$(0) + CHR$(15), CHR$(0) + CHR$(75), CHR$(0) + CHR$(77)
            GOSUB swaptitles
        CASE CHR$(13), CHR$(32): EXIT DO
        CASE CHR$(27)
          COLOR 15, 0
          CLS
          END
    END SELECT
LOOP
EXIT SUB

swaptitles:
    IF set$ = "f" THEN
        set$ = "m"
        max% = totalmodes
        posit% = currentmode
        LOCATE 5, 20: COLOR titleoff%, back%
        PRINT "Font files:"
        LOCATE 5, 50: COLOR titleon%, titleback%
        PRINT "Screen Modes:"
    ELSEIF set$ = "m" THEN
        set$ = "f"
        max% = totalfonts
        posit% = currentfont
        LOCATE 5, 20: COLOR titleon%, titleback%
        PRINT "Font files:"
        LOCATE 5, 50: COLOR titleoff%, back%
        PRINT "Screen Modes:"
    END IF
RETURN

upone:
    oldpos% = posit%
    posit% = (posit% + max% - 2) MOD max% + 1
    GOSUB redraw
RETURN

downone:
    oldpos% = posit%
    posit% = posit% MOD max% + 1
    GOSUB redraw
RETURN

redraw:
    IF set$ = "f" THEN
        LOCATE 5 + oldpos%, 20
        COLOR listfore%, listback%
        PRINT LEFT$(fontfiles(oldpos%) + "       ", 12)
        LOCATE 5 + posit%, 20
        COLOR listback%, listfore%
        PRINT LEFT$(fontfiles(posit%) + "       ", 12)
        currentfont = posit%
    ELSE
        LOCATE 5 + oldpos%, 50
        COLOR listfore%, listback%
        PRINT LEFT$(STR$(modes(oldpos%)) + "   ", 4) + MID$(modedim$, 7 * modes(oldpos%) - 6, 7)
        LOCATE 5 + posit%, 50
        COLOR listback%, listfore%
        PRINT LEFT$(STR$(modes(posit%)) + "   ", 4) + MID$(modedim$, 7 * modes(posit%) - 6, 7)
        currentmode = posit%
    END IF
RETURN

END SUB

'
'GetFiles finds all *.fon files in the current working directory and checks
'if they are legitimate.  If the files are ok, they are added to files list.
'
SUB GetFiles
SCREEN 0
WIDTH 80, 25
tryagain:
CLS
PRINT "Checking fontfiles..."
totalfonts = 0
X$ = DIR$("*.fon")
IF X$ = "" THEN
    PRINT "No font files found in current directory."
    PRINT "Push a shell to change directories? [yn]"
    try$ = "a"
    DO UNTIL INSTR(1, "NYny", try$)
        try$ = INPUT$(1)
    LOOP
    SELECT CASE UCASE$(try$)
        CASE "Y"
            PRINT "Type 'EXIT' to return to demo."
            SHELL
            GOTO tryagain
        CASE "N"
            END
    END SELECT
ELSE
    DO WHILE X$ <> ""
        PRINT "   "; UCASE$(X$); "--";
        SetMaxFonts 10, 10
        Reg% = RegisterFonts(X$)
        IF Reg% = 0 THEN
            PRINT "bad font file"
        ELSE
            totalfonts = totalfonts + 1
            fontfiles(totalfonts) = UCASE$(X$)
            PRINT "OK"
            IF totalfonts = 18 THEN EXIT DO
        END IF
        X$ = DIR$
    LOOP
END IF
SLEEP 1
END SUB

'
'GetModes tries all screen modes from 1-13 to see if they are supported.
'If a mode is supported, it is added to the list of available modes.
'
SUB GetModes
ON LOCAL ERROR GOTO badmode
nextactive% = 1
totalmodes = 0
FOR i% = 13 TO 1 STEP -1
    good% = TRUE
    SCREEN i%
    IF good% THEN
        modes(nextactive%) = i%
        nextactive% = nextactive% + 1
        totalmodes = totalmodes + 1
    END IF
NEXT i%
IF totalmodes = 0 THEN
    PRINT "No graphics modes available"
    END
END IF

IF modes(1) = 13 THEN
    currentmode = 2
ELSE
    currentmode = 1
END IF
EXIT SUB
badmode:
    good% = FALSE
    RESUME NEXT
END SUB

'
'ShowScreen displays all the fonts in the current font file and current
'graphics mode.
'
SUB ShowScreen
    SetMaxFonts 10, 10
    TotalReg% = RegisterFonts(fontfiles(currentfont))
    SCREEN modes(currentmode)
    PRINT "Please wait..."

    IF FontErr THEN
        CLS
        PRINT "Unable to continue, FontErr ="; FontErr
        C$ = INPUT$(1)
        EXIT SUB
    END IF
    IF TotalReg% > 10 THEN TotalReg% = 10

    StrLen% = TotalReg% * 3 - 1
    IF TotalReg% > 9 THEN StrLen% = StrLen% + TotalReg% - 9
    LoadStr$ = LEFT$("N1/N2/N3/N4/N5/N6/N7/N8/N9/N10", StrLen%)
    TotalLoad% = LoadFont(LoadStr$)

    SELECT CASE modes(currentmode)
        CASE 1: XS% = 160: YS% = 100
        CASE 2: XS% = 320: YS% = 100
        CASE 3: XS% = 360: YS% = 174
        CASE 4: XS% = 320: YS% = 200
        CASE 7: XS% = 160: YS% = 100
        CASE 8: XS% = 320: YS% = 100
        CASE 9: XS% = 320: YS% = 175
        CASE 10: XS% = 320: YS% = 175
        CASE 11: XS% = 320: YS% = 240
        CASE 12: XS% = 320: YS% = 240
        CASE 13: XS% = 160: YS% = 100
    END SELECT

    prompt$ = "Press any key."
    FOR i% = 1 TO TotalLoad%
        CLS
        SelectFont INT(i%)
        GetFontInfo FI
        SetGTextDir 0
        SetGTextColor 14
        Length% = OutGText(1, 1, RTRIM$(FI.FaceName))
        Length% = OutGText(1, 1 + FI.PixHeight, LTRIM$(STR$(FI.Points) + " Point"))
        FOR Dir% = 0 TO 3
            SetGTextDir Dir%
            SetGTextColor 15 - Dir%
            SELECT CASE Dir%
                CASE 0: X% = XS%: Y% = YS% - FI.PixHeight
                CASE 1: X% = XS% - FI.PixHeight: Y% = YS%
                CASE 2: X% = XS%: Y% = YS% + FI.PixHeight
                CASE 3: X% = XS% + FI.PixHeight: Y% = YS%
            END SELECT
            Length% = OutGText(CSNG(X%), CSNG(Y%), "Microsoft")
        NEXT Dir%
        SelectFont 2
        GetFontInfo FI
        SetGTextColor 14
        SetGTextDir 0
        IF i% = TotalLoad% THEN prompt$ = "Press ESC to go on."
        Length% = GetGTextLen(prompt$)
        Length% = OutGText(2 * XS% - Length% - 10, 2 * YS% - FI.PixHeight - 1, prompt$)
        IF i% = TotalLoad% THEN
            DO UNTIL INKEY$ = CHR$(27): LOOP
        ELSE
            a$ = INPUT$(1)
        END IF
    NEXT i%
END SUB

