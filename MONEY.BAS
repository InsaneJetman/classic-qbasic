'
'                    Q B a s i c   M O N E Y   M A N A G E R
'
'                   Copyright (C) Microsoft Corporation 1990
'
'   The Money Manager is a personal finance manager that allows you
'   to enter account transactions while tracking your account balances
'   and net worth.
'

'For faster operation
DEFINT A-Z

'Sub and function declarations
DECLARE SUB TransactionSummary (Item%)
DECLARE SUB Lcenter (Text$)
DECLARE SUB ScrollUp ()
DECLARE SUB ScrollDown ()
DECLARE SUB Initialize ()
DECLARE SUB DoIntro ()
DECLARE SUB Sparkle ()
DECLARE SUB Center (Row%, Text$)
DECLARE SUB FancyCls (Dots%, Background%)
DECLARE SUB LoadState ()
DECLARE SUB SaveState ()
DECLARE SUB MenuSystem ()
DECLARE SUB MakeBackup ()
DECLARE SUB RestoreBackup ()
DECLARE SUB Box (Row1%, Col1%, Row2%, Col2%)
DECLARE SUB NetWorthReport ()
DECLARE SUB EditAccounts ()
DECLARE SUB PrintHelpLine (Help$)
DECLARE SUB EditTrans (Item%)
DECLARE FUNCTION Cvdt$ (X#)
DECLARE FUNCTION Cvst$ (X!)
DECLARE FUNCTION Cvit$ (X%)
DECLARE FUNCTION Menu% (CurrChoiceX%, MaxChoice%, Choice$(), ItemRow%(), ItemCol%(), Help$(), BarMode%)
DECLARE FUNCTION GetString$ (Row%, Col%, Start$, End$, Vis%, Max%)
DECLARE FUNCTION Trim$ (X$)

'Constants
CONST TRUE = -1
CONST FALSE = NOT TRUE

TYPE AccountType
    Title        AS STRING * 20
    AType        AS STRING * 1
    Desc         AS STRING * 50
END TYPE

TYPE Recordtype
    Date     AS STRING * 8
    Ref      AS STRING * 10
    Desc     AS STRING * 50
    Fig1     AS DOUBLE
    Fig2     AS DOUBLE
END TYPE

DIM SHARED account(1 TO 18)  AS AccountType     'Stores the 18 account titles
DIM SHARED ColorPref                            'Color Preference
DIM SHARED Colors(0 TO 20, 1 TO 4)              'Different Colors
DIM SHARED ScrollUpAsm(1 TO 7)                  'Assembly Language Routines
DIM SHARED ScrollDownAsm(1 TO 7)
DIM SHARED maxrow AS INTEGER

    'Open data file.  If not there, goto error handler to initialize it
    ON ERROR GOTO FileNotFound
    OPEN "money.dat" FOR INPUT AS #1
    CLOSE
    ON ERROR GOTO 0

    Initialize      'Initialize global variables
    DoIntro         'Do fancy intro
    MenuSystem      'This is the main program
    COLOR 15, 0     'Clear screen and end
    CLS
    END

FileNotFound:       'if the file is not found, then set initial state & save
    SELECT CASE ERR
        CASE 53
            CLOSE
            ColorPref = 1
            FOR A = 1 TO 18
                account(A).Title = ""
                account(A).AType = ""
                account(A).Desc = ""
            NEXT A
            SaveState
        CASE ELSE
    END SELECT
    RESUME

    'Following is data for the different colors.

    'scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 0,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

    'Following is data which is actually a machine language program to
    'scroll the screen up or down very fast using a BIOS call.

DATA &HB8,&H01,&H06,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB

'Draws a character based box on the screen between the given coordinates
SUB Box (Row1, Col1, Row2, Col2) STATIC

    BoxWidth = Col2 - Col1 + 1

    LOCATE Row1, Col1
    PRINT "⁄"; STRING$(BoxWidth - 2, "ƒ"); "ø";

    FOR A = Row1 + 1 TO Row2 - 1
        LOCATE A, Col1
        PRINT "≥"; SPACE$(BoxWidth - 2); "≥";
    NEXT A

    LOCATE Row2, Col1
    PRINT "¿"; STRING$(BoxWidth - 2, "ƒ"); "Ÿ";

END SUB

'Centers Text$ on the given ROW
SUB Center (Row, Text$)

    LOCATE Row, 41 - LEN(Text$) / 2
    PRINT Text$;

END SUB

'Function to convert a double precision number to a string WITHOUT a
'leading space
FUNCTION Cvdt$ (X#)

    Cvdt$ = RIGHT$(STR$(X#), LEN(STR$(X#)) - 1)

END FUNCTION

'Function to convert an integer to a string WITHOUT a leading space
FUNCTION Cvit$ (X)
    Cvit$ = RIGHT$(STR$(X), LEN(STR$(X)) - 1)
END FUNCTION

'Function to convert a single precision number to a string WITHOUT a
'leading space
FUNCTION Cvst$ (X!)
    Cvst$ = RIGHT$(STR$(X!), LEN(STR$(X!)) - 1)
END FUNCTION

'Draw nice title screen, and call SparklePause
SUB DoIntro

    COLOR 15, 1
    CLS
    Center 4, "The"
    COLOR 14
    DIM A$(10)
    Center 5, "‹     ‹ ‹‹‹‹ ‹   ‹ ‹‹‹‹ ‹   ‹      ‹     ‹ ‹‹‹‹ ‹   ‹ ‹‹‹‹ ‹‹‹‹‹ ‹‹‹‹ ‹‹‹‹‹"
    Center 6, "€ﬂ‹ ‹ﬂ€ €  € €‹  € €    €‹‹‹€      €ﬂ‹ ‹ﬂ€ €  € €‹  € €  € €     €    €   €"
    Center 7, "€  ﬂ  € €  € € ﬂ‹€ €ﬂﬂﬂ   €        €  ﬂ  € €ﬂﬂ€ € ﬂ‹€ €ﬂﬂ€ € ﬂﬂ€ €ﬂﬂﬂ €ﬂ€ﬂﬂ"
    Center 8, "€     € €‹‹€ €   € €‹‹‹   €        €     € €  € €   € €  € €‹‹‹€ €‹‹‹ €  ﬂ‹"

    COLOR 15
    Center 11, "A Personal Finance Manager written in"
    Center 12, "MS-DOS QBasic"
    Center maxrow - 1, "Push any key to continue"

    COLOR 13
    Sparkle


END SUB

'This is the full-screen editor which allows you to change your account
'titles and descriptions
SUB EditAccounts

    'Information about each column
    REDIM Help$(4), Col(4), Vis(4), Max(4), Edit$(18, 3)

    'Draw the screen
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    Box 2, 1, maxrow - 1, 80

    COLOR Colors(5, ColorPref), Colors(4, ColorPref)
    LOCATE 1, 68
    PRINT "Edit Accounts";
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)

    LOCATE 3, 2: PRINT "  ≥                    ≥                                                  ≥Ass"
    LOCATE 4, 2: PRINT "No≥ Account Title      ≥ Description                                      ≥Lia"
    LOCATE 5, 2: PRINT "ƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒ"
                  u$ = "##≥\                  \≥\                                                \≥ ! "
    FOR A = 6 TO 23
        LOCATE A, 2
        X = A - 5
        PRINT USING u$; X; account(X).Title; account(X).Desc; account(X).AType;
    NEXT A

    'Initialize variables
    Help$(1) = "Hit <F2> save changes, or <Escape> to abort"
    Help$(2) = "Hit <F2> save changes, or <Escape> to abort"
    Help$(3) = "In this column, enter [A] for asset, or [L] for liability"

    Col(1) = 5: Col(2) = 26: Col(3) = 78
    Vis(1) = 20: Vis(2) = 50: Vis(3) = 1
    Max(1) = 20: Max(2) = 50: Max(3) = 1

    FOR A = 1 TO 18
        Edit$(A, 1) = account(A).Title
        Edit$(A, 2) = account(A).Desc
        Edit$(A, 3) = account(A).AType
    NEXT A

    Finished = FALSE

    CurrRow = 1
    CurrCol = 1

    'Loop until F2 or <ESC> is pressed
    DO
        GOSUB EditAccountsShowCursor            'Show Cursor
        DO                                      'Wait for key
            Kbd$ = INKEY$
        LOOP UNTIL Kbd$ <> ""

        IF Kbd$ >= " " AND Kbd$ < "~" THEN      'If legal, edit item
            GOSUB EditAccountsEditItem
        END IF
        GOSUB EditAccountsHideCursor            'Hide Cursor so it can move
                                                'If it needs to
        SELECT CASE Kbd$
            CASE CHR$(0) + "H"                              'Up Arrow
                CurrRow = (CurrRow + 16) MOD 18 + 1
            CASE CHR$(0) + "P"                              'Down Arrow
                CurrRow = (CurrRow) MOD 18 + 1
            CASE CHR$(0) + "K", CHR$(0) + CHR$(15)          'Left or Shift+Tab
                CurrCol = (CurrCol + 1) MOD 3 + 1
            CASE CHR$(0) + "M", CHR$(9)                     'Right or Tab
                CurrCol = (CurrCol) MOD 3 + 1
            CASE CHR$(0) + "<"                              'F2
                Finished = TRUE
                Save = TRUE
            CASE CHR$(27)                                   'Esc
                Finished = TRUE
                Save = FALSE
            CASE CHR$(13)                                   'Return
            CASE ELSE
                BEEP
        END SELECT
    LOOP UNTIL Finished

    IF Save THEN
        GOSUB EditAccountsSaveData
    END IF

    EXIT SUB

EditAccountsShowCursor:
    COLOR Colors(8, ColorPref), Colors(9, ColorPref)
    LOCATE CurrRow + 5, Col(CurrCol)
    PRINT LEFT$(Edit$(CurrRow, CurrCol), Vis(CurrCol));
    PrintHelpLine Help$(CurrCol)
    RETURN

EditAccountsEditItem:
    COLOR Colors(8, ColorPref), Colors(9, ColorPref)
    ok = FALSE
    DO
        Kbd$ = GetString$(CurrRow + 5, Col(CurrCol), Kbd$, back$, Vis(CurrCol), Max(CurrCol))
        Edit$(CurrRow, CurrCol) = LEFT$(back$ + SPACE$(Max(CurrCol)), Max(CurrCol))

        IF CurrCol = 3 THEN
            X$ = UCASE$(back$)
            IF X$ = "A" OR X$ = "L" OR X$ = "" OR X$ = " " THEN
                ok = TRUE
                Edit$(CurrRow, CurrCol) = X$
            ELSE
                BEEP
            END IF
        ELSE
            ok = TRUE
        END IF

    LOOP UNTIL ok
    RETURN

EditAccountsHideCursor:
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    LOCATE CurrRow + 5, Col(CurrCol)
    PRINT SPACE$(Vis(CurrCol));
    LOCATE CurrRow + 5, Col(CurrCol)
    PRINT LEFT$(Edit$(CurrRow, CurrCol), Vis(CurrCol));
    PrintHelpLine Help$(CurrCol)
    RETURN


EditAccountsSaveData:
    FOR A = 1 TO 18
        account(A).Title = Edit$(A, 1)
        account(A).Desc = Edit$(A, 2)
        account(A).AType = Edit$(A, 3)
    NEXT A
    SaveState
    RETURN

END SUB

'This is the full-screen editor which allows you to enter and change
'transactions
SUB EditTrans (Item)

    'Stores info about each column
    REDIM Help$(6), Col(6), Vis(6), Max(6), CurrString$(3), CurrFig#(5)
    'Array to keep the current balance at all the transactions
    REDIM Balance#(1000)

    'Open random access file
    File$ = "money." + Cvit$(Item)
    OPEN File$ FOR RANDOM AS #1 LEN = 84
    FIELD #1, 8 AS IoDate$, 10 AS IoRef$, 50 AS IoDesc$, 8 AS IoFig1$, 8 AS IoFig2$
    FIELD #1, 11 AS valid$, 5 AS IoMaxRecord$, 8 AS IoBalance$

    'Initialize variables
    CurrString$(1) = ""
    CurrString$(2) = ""
    CurrString$(3) = ""
    CurrFig#(4) = 0
    CurrFig#(5) = 0

    GET #1, 1
    IF valid$ <> "THISISVALID" THEN
        LSET IoDate$ = ""
        LSET IoRef$ = ""
        LSET IoDesc$ = ""
        LSET IoFig1$ = MKD$(0)
        LSET IoFig2$ = MKD$(0)
        PUT #1, 2
        LSET valid$ = "THISISVALID"
        LSET IoMaxRecord$ = "1"
        LSET IoBalance$ = MKD$(0)
        PUT #1, 1
    END IF

    MaxRecord = VAL(IoMaxRecord$)

    Balance#(0) = 0
    A = 1
    WHILE A <= MaxRecord
        GET #1, A + 1
        Balance#(A) = Balance#(A - 1) + CVD(IoFig1$) - CVD(IoFig2$)
        A = A + 1
    WEND
    GOSUB EditTransWriteBalance


    Help$(1) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    Help$(2) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    Help$(3) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    Help$(4) = "Transactions that Increase value of asset, or increase debt owed for liability"
    Help$(5) = "Transactions that decrease value of asset, or decrease debt owed for liability"

    Col(1) = 2
    Col(2) = 11
    Col(3) = 19
    Col(4) = 50
    Col(5) = 60

    Vis(1) = 8
    Vis(2) = 7
    Vis(3) = 30
    Vis(4) = 9
    Vis(5) = 9

    Max(1) = 8
    Max(2) = 10
    Max(3) = 50
    Max(4) = 8
    Max(5) = 8


    'Draw Screen
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    Box 2, 1, maxrow - 1, 80

    COLOR Colors(5, ColorPref), Colors(4, ColorPref)
    X$ = Trim$(account(Item).Title)
    LOCATE 1, 80 - LEN(X$)
    PRINT X$;

    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    LOCATE 3, 2: PRINT "  Date  ≥ Ref#  ≥Description                   ≥Increase ≥Decrease ≥ Balance  "
    LOCATE 4, 2: PRINT "ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ"

     u$ = "\      \≥\     \≥\                            \≥"
    u1$ = "        ≥       ≥                              ≥         ≥         ≥          "
    u1x$ = "ﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
    u2$ = "##,###.##"
    u3$ = "###,###.##"
    u4$ = "         "

    CurrTopline = 1
    GOSUB EditTransPrintWholeScreen

    CurrRow = 1
    CurrCol = 1
    GOSUB EditTransGetLine

    Finished = FALSE


    'Loop until <F2> is pressed
    DO
        GOSUB EditTransShowCursor                   'Show Cursor, Wait for key
        DO: Kbd$ = INKEY$: LOOP UNTIL Kbd$ <> ""
        GOSUB EditTransHideCursor

        IF Kbd$ >= " " AND Kbd$ < "~" THEN        'If legal key, edit item
            GOSUB EditTransEditItem
        END IF

        SELECT CASE Kbd$                            'Handle Special keys
            CASE CHR$(0) + "H"                      'up arrow
                GOSUB EditTransMoveUp
            CASE CHR$(0) + "P"                      'Down arrow
                GOSUB EditTransMoveDown
            CASE CHR$(0) + "K", CHR$(0) + CHR$(15)  'Left Arrow,BackTab
                CurrCol = (CurrCol + 3) MOD 5 + 1
            CASE CHR$(0) + "M", CHR$(9)             'Right Arrow,Tab
                CurrCol = (CurrCol) MOD 5 + 1
            CASE CHR$(0) + "G"                      'Home
                CurrCol = 1
            CASE CHR$(0) + "O"                      'End
                CurrCol = 5
            CASE CHR$(0) + "I"                      'Page Up
                CurrRow = 1
                CurrTopline = CurrTopline - 19
                IF CurrTopline < 1 THEN
                    CurrTopline = 1
                END IF
                GOSUB EditTransPrintWholeScreen
                CurrRow = 1
                GOSUB EditTransGetLine
            CASE CHR$(0) + "Q"                      'Page Down
                CurrRow = 1
                CurrTopline = CurrTopline + 19
                IF CurrTopline > MaxRecord THEN
                    CurrTopline = MaxRecord
                END IF
                GOSUB EditTransPrintWholeScreen
                CurrRow = 1
                GOSUB EditTransGetLine
            CASE CHR$(0) + "<"                      'F2
                Finished = TRUE
            CASE CHR$(0) + "C"                      'F9
                GOSUB EditTransAddRecord
            CASE CHR$(0) + "D"                      'F10
                GOSUB EditTransDeleteRecord
            CASE CHR$(13)                           'Enter
            CASE ELSE
                'BEEP
        END SELECT
    LOOP UNTIL Finished

    CLOSE

    EXIT SUB


EditTransShowCursor:
    COLOR Colors(8, ColorPref), Colors(9, ColorPref)
    LOCATE CurrRow + 4, Col(CurrCol)
    PRINT SPACE$(Vis(CurrCol));
    LOCATE CurrRow + 4, Col(CurrCol)
    SELECT CASE CurrCol
        CASE 1, 2, 3
            PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
        CASE 4
            IF CurrFig#(4) <> 0 OR CurrFig#(5) = 0 THEN
                PRINT USING u2$; CurrFig#(4);
            END IF
        CASE 5
            IF CurrFig#(5) <> 0 THEN
                PRINT USING u2$; CurrFig#(5);
            END IF
    END SELECT
    PrintHelpLine Help$(CurrCol)
    RETURN


EditTransHideCursor:
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    LOCATE CurrRow + 4, Col(CurrCol)
    PRINT SPACE$(Vis(CurrCol));
    LOCATE CurrRow + 4, Col(CurrCol)
    SELECT CASE CurrCol
        CASE 1, 2, 3
            PRINT LEFT$(CurrString$(CurrCol), Vis(CurrCol));
        CASE 4
            IF CurrFig#(4) <> 0 OR CurrFig#(5) = 0 THEN
                PRINT USING u2$; CurrFig#(4);
            END IF
        CASE 5
            IF CurrFig#(5) <> 0 THEN
                PRINT USING u2$; CurrFig#(5);
            END IF
    END SELECT
    RETURN


EditTransEditItem:

    CurrRecord = CurrTopline + CurrRow - 1
    COLOR Colors(8, ColorPref), Colors(9, ColorPref)

    SELECT CASE CurrCol
        CASE 1, 2, 3
            Kbd$ = GetString$(CurrRow + 4, Col(CurrCol), Kbd$, new$, Vis(CurrCol), Max(CurrCol))
            CurrString$(CurrCol) = new$
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
        CASE 4
            DO
                Kbd$ = GetString$(CurrRow + 4, Col(4), Kbd$, new$, Vis(4), Max(4))
                new4# = VAL(new$)
            LOOP WHILE new4# > 99999.99 OR new4# < 0

            A = CurrRecord
            WHILE A <= MaxRecord
                Balance#(A) = Balance#(A) + new4# - CurrFig#(4) + CurrFig#(5)
                A = A + 1
            WEND
            CurrFig#(4) = new4#
            CurrFig#(5) = 0
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
            GOSUB EditTransPrintBalances
            GOSUB EditTransWriteBalance
        CASE 5
            DO
                Kbd$ = GetString$(CurrRow + 4, Col(5), Kbd$, new$, Vis(5), Max(5))
                new5# = VAL(new$)
            LOOP WHILE new4# > 99999.99 OR new5# < 0

            A = CurrRecord
            WHILE A <= MaxRecord
                Balance#(A) = Balance#(A) - new5# + CurrFig#(5) - CurrFig#(4)
                A = A + 1
            WEND
            CurrFig#(4) = 0
            CurrFig#(5) = new5#
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
            GOSUB EditTransPrintBalances
            GOSUB EditTransWriteBalance
        CASE ELSE
    END SELECT
    GOSUB EditTransPrintLine
    RETURN

EditTransMoveUp:
    IF CurrRow = 1 THEN
        IF CurrTopline = 1 THEN
            BEEP
        ELSE
            ScrollDown
            CurrTopline = CurrTopline - 1
            GOSUB EditTransGetLine
            GOSUB EditTransPrintLine
        END IF
    ELSE
        CurrRow = CurrRow - 1
        GOSUB EditTransGetLine
    END IF
    RETURN

EditTransMoveDown:
    IF (CurrRow + CurrTopline - 1) >= MaxRecord THEN
        BEEP
    ELSE
        IF CurrRow = 19 THEN
            ScrollUp
            CurrTopline = CurrTopline + 1
            GOSUB EditTransGetLine
            GOSUB EditTransPrintLine
        ELSE
            CurrRow = CurrRow + 1
            GOSUB EditTransGetLine
        END IF
    END IF


EditTransPrintLine:
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    CurrRecord = CurrTopline + CurrRow - 1
    LOCATE CurrRow + 4, 2
    IF CurrRecord = MaxRecord + 1 THEN
        LOCATE CurrRow + 4, 2
        PRINT u1x$;
    ELSEIF CurrRecord > MaxRecord THEN
        LOCATE CurrRow + 4, 2
        PRINT u1$;
    ELSE
        PRINT USING u$; CurrString$(1); CurrString$(2); CurrString$(3);
        IF CurrFig#(5) = 0 THEN
            PRINT USING u2$ + "≥" + u4$ + "≥" + u3$; CurrFig#(4); Balance#(CurrRecord)
        ELSE
            PRINT USING u4$ + "≥" + u2$ + "≥" + u3$; CurrFig#(5); Balance#(CurrRecord)
        END IF
    END IF
    RETURN

EditTransPrintBalances:
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    FOR A = 1 TO 19
        CurrRecord = CurrTopline + A - 1
        IF CurrRecord <= MaxRecord THEN
            LOCATE 4 + A, 70
            PRINT USING u3$; Balance#(CurrTopline + A - 1);
        END IF
    NEXT A
    RETURN

EditTransDeleteRecord:
    IF MaxRecord = 1 THEN
        BEEP
    ELSE
        CurrRecord = CurrTopline + CurrRow - 1
        MaxRecord = MaxRecord - 1
        A = CurrRecord
        WHILE A <= MaxRecord
            GET #1, A + 2
            PUT #1, A + 1
            Balance#(A) = Balance#(A + 1) - CurrFig#(4) + CurrFig#(5)
            A = A + 1
        WEND

        LSET valid$ = "THISISVALID"
        LSET IoMaxRecord$ = Cvit$(MaxRecord)
        PUT #1, 1
        GOSUB EditTransPrintWholeScreen
        CurrRecord = CurrTopline + CurrRow - 1
        IF CurrRecord > MaxRecord THEN
            GOSUB EditTransMoveUp
        END IF
        GOSUB EditTransGetLine
        GOSUB EditTransWriteBalance
    END IF
    RETURN

EditTransAddRecord:
    CurrRecord = CurrTopline + CurrRow - 1
    A = MaxRecord
    WHILE A > CurrRecord
        GET #1, A + 1
        PUT #1, A + 2
        Balance#(A + 1) = Balance#(A)
        A = A - 1
    WEND
    Balance#(CurrRecord + 1) = Balance#(CurrRecord)
    MaxRecord = MaxRecord + 1
    LSET IoDate$ = ""
    LSET IoRef$ = ""
    LSET IoDesc$ = ""
    LSET IoFig1$ = MKD$(0)
    LSET IoFig2$ = MKD$(0)
    PUT #1, CurrRecord + 2

    LSET valid$ = "THISISVALID"
    LSET IoMaxRecord$ = Cvit$(MaxRecord)
    PUT #1, 1
    GOSUB EditTransPrintWholeScreen
    GOSUB EditTransGetLine
    RETURN

EditTransPrintWholeScreen:
    temp = CurrRow
    FOR CurrRow = 1 TO 19
        CurrRecord = CurrTopline + CurrRow - 1
        IF CurrRecord <= MaxRecord THEN
            GOSUB EditTransGetLine
        END IF
        GOSUB EditTransPrintLine
    NEXT CurrRow
    CurrRow = temp
    RETURN

EditTransWriteBalance:
    GET #1, 1
    LSET IoBalance$ = MKD$(Balance#(MaxRecord))
    PUT #1, 1
    RETURN

EditTransPutLine:
    CurrRecord = CurrTopline + CurrRow - 1
    LSET IoDate$ = CurrString$(1)
    LSET IoRef$ = CurrString$(2)
    LSET IoDesc$ = CurrString$(3)
    LSET IoFig1$ = MKD$(CurrFig#(4))
    LSET IoFig2$ = MKD$(CurrFig#(5))
    PUT #1, CurrRecord + 1
    RETURN

EditTransGetLine:
    CurrRecord = CurrTopline + CurrRow - 1
    GET #1, CurrRecord + 1
    CurrString$(1) = IoDate$
    CurrString$(2) = IoRef$
    CurrString$(3) = IoDesc$
    CurrFig#(4) = CVD(IoFig1$)
    CurrFig#(5) = CVD(IoFig2$)
    RETURN
END SUB

'Clears screen in the right color, and draws nice dots.
SUB FancyCls (Dots, Background)

    VIEW PRINT 2 TO maxrow - 1
    COLOR Dots, Background
    CLS 2

    FOR A = 95 TO 80 * (maxrow - 2) STEP 45
        Row = A / 80 + 1
        Col = A MOD 80 + 1
        LOCATE Row, Col
        PRINT CHR$(250);
    NEXT A

    VIEW PRINT

END SUB

'Given a row and col, and an initial string, edit a string
'VIS is the length of the visible field of entry
'MAX is the maximum number of characters allowed in the string
FUNCTION GetString$ (Row, Col, Start$, End$, Vis, Max)


    curr$ = Trim$(LEFT$(Start$, Max))

    LOCATE , , 1

    Finished = FALSE
    DO
        GOSUB GetStringShowText
        GOSUB GetStringGetKey

        IF LEN(Kbd$) > 1 THEN
            Finished = TRUE
            GetString$ = Kbd$
        ELSE
            SELECT CASE Kbd$
                CASE CHR$(13), CHR$(27), CHR$(9)
                    Finished = TRUE
                    GetString$ = Kbd$

                CASE CHR$(8)
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                    END IF

                CASE " " TO "}"
                    IF LEN(curr$) < Max THEN
                        curr$ = curr$ + Kbd$
                    END IF

                CASE ELSE
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL Finished

    End$ = curr$

    EXIT FUNCTION


GetStringShowText:
    LOCATE Row, Col
    IF LEN(curr$) > Vis THEN
        PRINT RIGHT$(curr$, Vis);
    ELSE
        PRINT curr$; SPACE$(Vis - LEN(curr$));
        LOCATE Row, Col + LEN(curr$)
    END IF
    RETURN

GetStringGetKey:
    Kbd$ = ""
    WHILE Kbd$ = ""
        Kbd$ = INKEY$
    WEND
    RETURN



END FUNCTION

'Read colors in, and set up assembly routines
SUB Initialize

    DEF SEG = 0
    IF PEEK(&H484) = 0 THEN
        maxrow = 25
    ELSE
        maxrow = PEEK(&H484) + 1
    END IF
    DEF SEG

    WIDTH , maxrow
    VIEW PRINT

    FOR ColorSet = 1 TO 4
        FOR X = 1 TO 10
            READ Colors(X, ColorSet)
        NEXT X
    NEXT ColorSet

    LoadState

    P = VARPTR(ScrollUpAsm(1))
    DEF SEG = VARSEG(ScrollUpAsm(1))
    FOR I = 0 TO 13
       READ J
       POKE (P + I), J
    NEXT I

    P = VARPTR(ScrollDownAsm(1))
    DEF SEG = VARSEG(ScrollDownAsm(1))
    FOR I = 0 TO 13
       READ J
       POKE (P + I), J
    NEXT I

    DEF SEG

END SUB

'Center TEXT$ on the line printer
SUB Lcenter (Text$)
    LPRINT TAB(41 - LEN(Text$) / 2); Text$
END SUB

'Load the color preference and the account info from MONEY.DAT
SUB LoadState

    OPEN "money.dat" FOR INPUT AS #1
    INPUT #1, ColorPref

    FOR A = 1 TO 18
        LINE INPUT #1, account(A).Title
        LINE INPUT #1, account(A).AType
        LINE INPUT #1, account(A).Desc
    NEXT A

    CLOSE

END SUB

'Handles Menu Selection for a single menu (either sub menu, or menu bar)
'currChoiceX  :  Number of current choice
'maxChoice    :  Number of choices in the list
'choice$()    :  Array with the text of the choices
'itemRow()    :  Array with the row of the choices
'itemCol()    :  Array with the col of the choices
'help$()      :  Array with the help text for each choice
'barMode      :  Boolean:  TRUE = menu bar style, FALSE = drop down style
'
'  This function sinply returns the number of the choice that was made
'  by changing currChoiceX, and returns the scan code of the key that was
'  pressed to exit
'
FUNCTION Menu (CurrChoiceX, MaxChoice, Choice$(), ItemRow(), ItemCol(), Help$(), BarMode)

    currChoice = CurrChoiceX

    'Color in Help Bar
    COLOR Colors(5, ColorPref), Colors(4, ColorPref)
    LOCATE maxrow, 1
    PRINT SPACE$(80);

    'if in bar mode, color in menu bar, else color box/shadow
    'bar mode means you are currently in the menu bar, not a sub menu
    IF BarMode THEN
        COLOR Colors(7, ColorPref), Colors(4, ColorPref)
        LOCATE 1, 1
        PRINT SPACE$(80);
    ELSE
        COLOR Colors(7, ColorPref), Colors(4, ColorPref)
        Box ItemRow(1) - 1, ItemCol(1) - 1, ItemRow(MaxChoice) + 1, ItemCol(1) + LEN(Choice$(1)) + 1

        COLOR Colors(10, ColorPref), Colors(6, ColorPref)
        FOR A = 1 TO MaxChoice + 1
            LOCATE ItemRow(1) + A - 1, ItemCol(1) + LEN(Choice$(1)) + 2
            PRINT CHR$(178); CHR$(178);
        NEXT A
        LOCATE ItemRow(MaxChoice) + 2, ItemCol(MaxChoice) + 2
        PRINT STRING$(LEN(Choice$(MaxChoice)) + 2, 178);
    END IF

    'print the choices
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    FOR A = 1 TO MaxChoice
        LOCATE ItemRow(A), ItemCol(A)
        PRINT Choice$(A);
    NEXT A

    Finished = FALSE

    WHILE NOT Finished

        GOSUB MenuShowCursor
        GOSUB MenuGetKey
        GOSUB MenuHideCursor

        SELECT CASE Kbd$
            CASE CHR$(0) + "H": GOSUB MenuUp
            CASE CHR$(0) + "P": GOSUB MenuDown
            CASE CHR$(0) + "K": GOSUB MenuLeft
            CASE CHR$(0) + "M": GOSUB MenuRight
            CASE CHR$(13): GOSUB MenuEnter
            CASE CHR$(27): GOSUB MenuEscape
            CASE ELSE:  BEEP
        END SELECT
    WEND

    Menu = currChoice

    EXIT FUNCTION


MenuEnter:
    Finished = TRUE
    RETURN

MenuEscape:
    currChoice = 0
    Finished = TRUE
    RETURN

MenuUp:
    IF BarMode THEN
        BEEP
    ELSE
        currChoice = (currChoice + MaxChoice - 2) MOD MaxChoice + 1
    END IF
    RETURN

MenuLeft:
    IF BarMode THEN
        currChoice = (currChoice + MaxChoice - 2) MOD MaxChoice + 1
    ELSE
        currChoice = -2
        Finished = TRUE
    END IF
    RETURN

MenuRight:
    IF BarMode THEN
        currChoice = (currChoice) MOD MaxChoice + 1
    ELSE
        currChoice = -3
        Finished = TRUE
    END IF
    RETURN

MenuDown:
    IF BarMode THEN
        Finished = TRUE
    ELSE
        currChoice = (currChoice) MOD MaxChoice + 1
    END IF
    RETURN

MenuShowCursor:
    COLOR Colors(8, ColorPref), Colors(9, ColorPref)
    LOCATE ItemRow(currChoice), ItemCol(currChoice)
    PRINT Choice$(currChoice);
    PrintHelpLine Help$(currChoice)
    RETURN

MenuGetKey:
    Kbd$ = ""
    WHILE Kbd$ = ""
        Kbd$ = INKEY$
    WEND
    RETURN

MenuHideCursor:
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    LOCATE ItemRow(currChoice), ItemCol(currChoice)
    PRINT Choice$(currChoice);
    RETURN


END FUNCTION

'This is the heart of the application.  This sub uses the function
'MENU to do a menu bar.  Then it uses MENU again to do the sub menu.
'Then it calls the appropriate function to handle the users choice
SUB MenuSystem

    DIM Choice$(20), menuRow(20), menuCol(20), Help$(20)

    Choice = 1
    Finished = FALSE

    WHILE NOT Finished
        FancyCls Colors(2, ColorPref), Colors(1, ColorPref)
        GOSUB MenuSystemMain

        subchoice = -1
        WHILE subchoice < 0
            SELECT CASE Choice
                CASE 1: GOSUB MenuSystemFile
                CASE 2: GOSUB MenuSystemEdit
                CASE 3: GOSUB MenuSystemAccount
                CASE 4: GOSUB MenuSystemReport
                CASE 5: GOSUB MenuSystemColors
            END SELECT
            FancyCls Colors(2, ColorPref), Colors(1, ColorPref)

            SELECT CASE subchoice
                CASE -2: Choice = (Choice + 3) MOD 5 + 1
                CASE -3: Choice = (Choice) MOD 5 + 1
            END SELECT
        WEND
    WEND
    EXIT SUB


MenuSystemMain:
    Choice$(1) = " File "
    Choice$(2) = " Accounts "
    Choice$(3) = " Transactions "
    Choice$(4) = " Reports "
    Choice$(5) = " Colors "

    menuRow(1) = 1: menuCol(1) = 2
    menuRow(2) = 1: menuCol(2) = 8
    menuRow(3) = 1: menuCol(3) = 18
    menuRow(4) = 1: menuCol(4) = 32
    menuRow(5) = 1: menuCol(5) = 41

    Help$(1) = "Make backups, Restore from Backup, Exit the Money Manager"
    Help$(2) = "Add New Accounts, Delete old Accounts"
    Help$(3) = "Select an account to work with"
    Help$(4) = "Select a report to print"
    Help$(5) = "Select your favorite screen colors"

    DO
        NewChoice = Menu((Choice), 5, Choice$(), menuRow(), menuCol(), Help$(), TRUE)
    LOOP WHILE NewChoice = 0
    Choice = NewChoice
    RETURN

MenuSystemFile:
    Choice$(1) = " Exit           "

    menuRow(1) = 3: menuCol(1) = 2

    Help$(1) = "Exit the Money Manager"

    subchoice = Menu(1, 1, Choice$(), menuRow(), menuCol(), Help$(), FALSE)

    SELECT CASE subchoice
        CASE 1: Finished = TRUE
        CASE ELSE
    END SELECT
    RETURN


MenuSystemEdit:
    Choice$(1) = " Edit Account Titles "

    menuRow(1) = 3: menuCol(1) = 8

    Help$(1) = "Add/Edit/Delete Accounts."

    subchoice = Menu(1, 1, Choice$(), menuRow(), menuCol(), Help$(), FALSE)

    SELECT CASE subchoice
        CASE 1: EditAccounts
        CASE ELSE
    END SELECT
    RETURN


MenuSystemAccount:

    FOR A = 1 TO 18
        IF Trim$(account(A).Title) = "" THEN
            Choice$(A) = RIGHT$(STR$(A), 2) + ". ------------------- "
        ELSE
            Choice$(A) = RIGHT$(STR$(A), 2) + ". " + account(A).Title
        END IF
        menuRow(A) = A + 2
        menuCol(A) = 18
        Help$(A) = account(A).Desc
    NEXT A

    subchoice = Menu(1, 18, Choice$(), menuRow(), menuCol(), Help$(), FALSE)

    IF subchoice > 0 THEN
        EditTrans (subchoice)
    END IF
    RETURN


MenuSystemReport:
    Choice$(1) = " Net Worth Report       "
    menuRow(1) = 3: menuCol(1) = 32
    Help$(1) = "Computes your net worth."

    FOR A = 1 TO 18
        IF Trim$(account(A).Title) = "" THEN
            Choice$(A + 1) = RIGHT$(STR$(A), 2) + ". ------------------- "
        ELSE
            Choice$(A + 1) = RIGHT$(STR$(A), 2) + ". " + account(A).Title
        END IF
        menuRow(A + 1) = A + 3
        menuCol(A + 1) = 32
        Help$(A + 1) = account(A).Desc
    NEXT A

    subchoice = Menu(1, 19, Choice$(), menuRow(), menuCol(), Help$(), FALSE)

    SELECT CASE subchoice
        CASE 1
            NetWorthReport
        CASE 2 TO 19
            TransactionSummary (subchoice - 1)
        CASE ELSE
    END SELECT
    RETURN

MenuSystemColors:
    Choice$(1) = " Monochrome Scheme "
    Choice$(2) = " Blue/Cyan Scheme  "
    Choice$(3) = " Cyan/Blue Scheme  "
    Choice$(4) = " Grey/Red Scheme   "

    menuRow(1) = 3: menuCol(1) = 41
    menuRow(2) = 4: menuCol(2) = 41
    menuRow(3) = 5: menuCol(3) = 41
    menuRow(4) = 6: menuCol(4) = 41

    Help$(1) = "Proper color scheme for monochrome and LCD displays"
    Help$(2) = "Color scheme featuring cyan"
    Help$(3) = "Color scheme featuring blue"
    Help$(4) = "Color scheme featuring red"

    subchoice = Menu(1, 4, Choice$(), menuRow(), menuCol(), Help$(), FALSE)

    SELECT CASE subchoice
        CASE 1 TO 4
            ColorPref = subchoice
            SaveState
        CASE ELSE
    END SELECT
    RETURN


END SUB

'Prints the net worth report to the line printer
SUB NetWorthReport

    DIM assetIndex(18), liabilityIndex(18)

    maxAsset = 0
    maxLiability = 0

    FOR A = 1 TO 18
        IF account(A).AType = "A" THEN
            maxAsset = maxAsset + 1
            assetIndex(maxAsset) = A
        ELSEIF account(A).AType = "L" THEN
            maxLiability = maxLiability + 1
            liabilityIndex(maxLiability) = A
        END IF
    NEXT A

    FancyCls Colors(2, ColorPref), Colors(1, ColorPref)
    PrintHelpLine ""
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    Box 8, 20, 14, 62
    Center 10, "Prepare printer for Report!"
    Center 12, "Hit <Enter> to print, or <Esc> to abort"

    DO: Kbd$ = INKEY$: LOOP WHILE Kbd$ <> CHR$(13) AND Kbd$ <> CHR$(27)

    IF Kbd$ = CHR$(13) THEN
        CLS
        u0$ = "                     \                  \ "
        u1$ = "                          \                  \  +$$,###.##"
        u2$ = "                                                ----------"
        u3$ = "                                                =========="
        LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT
        Lcenter "T H E   M O N E Y   M A N A G E R"
        LPRINT : LPRINT
        Lcenter "NET WORTH REPORT -- " + DATE$
        Lcenter "-------------------------------------------"
        LPRINT USING u0$; "ASSETS:"
        assetTotal# = 0
        A = 1
        WHILE A <= maxAsset
            File$ = "money." + Cvit$(assetIndex(A))
            OPEN File$ FOR RANDOM AS #1 LEN = 84
            FIELD #1, 11 AS valid$, 5 AS IoMaxRecord$, 8 AS IoBalance$
            GET #1, 1
            IF valid$ = "THISISVALID" THEN
                LPRINT USING u1$; account(assetIndex(A)).Title; CVD(IoBalance$)
                assetTotal# = assetTotal# + CVD(IoBalance$)
            END IF
            CLOSE
            A = A + 1
        WEND
        LPRINT u2$
        LPRINT USING u1$; "Total assets"; assetTotal#
        LPRINT
        LPRINT
        LPRINT USING u0$; "LIABILITIES:"
        liabilityTotal# = 0
        A = 1
        WHILE A <= maxLiability
            File$ = "money." + Cvit$(liabilityIndex(A))
            OPEN File$ FOR RANDOM AS #1 LEN = 84
            FIELD #1, 11 AS valid$, 5 AS IoMaxRecord$, 8 AS IoBalance$
            GET #1, 1
            IF valid$ = "THISISVALID" THEN
                LPRINT USING u1$; account(liabilityIndex(A)).Title; CVD(IoBalance$)
                liabilityTotal# = liabilityTotal# + CVD(IoBalance$)
            END IF
            CLOSE
            A = A + 1
        WEND
        LPRINT u2$
        LPRINT USING u1$; "Total liability"; liabilityTotal#
        LPRINT

        LPRINT
        LPRINT u2$
        LPRINT USING u1$; "NET WORTH"; assetTotal# - liabilityTotal#
        LPRINT u3$
    END IF

END SUB

' Given a help text string, print it on the bottom row, in the proper color
SUB PrintHelpLine (Help$)

    COLOR Colors(5, ColorPref), Colors(4, ColorPref)
    LOCATE maxrow, 1
    PRINT SPACE$(80);
    Center maxrow, Help$

END SUB

'Save the color preference and the account info
SUB SaveState

    OPEN "money.dat" FOR OUTPUT AS #2
    PRINT #2, ColorPref

    FOR A = 1 TO 18
        PRINT #2, account(A).Title
        PRINT #2, account(A).AType
        PRINT #2, account(A).Desc
    NEXT A

    CLOSE #2

END SUB

'Call the assembly program to scroll the screen down
SUB ScrollDown

    DEF SEG = VARSEG(ScrollDownAsm(1))
    CALL Absolute(VARPTR(ScrollDownAsm(1)))
    DEF SEG

END SUB

'Scroll the screen down by calling the assembly language program
SUB ScrollUp

    DEF SEG = VARSEG(ScrollUpAsm(1))
    CALL Absolute(VARPTR(ScrollUpAsm(1)))
    DEF SEG

END SUB

'Nifty routing that rotates '$' and waits for a keypress
SUB Sparkle

    A$ = "$    $    $    $    $    $    $    $    $    $    $    $    $    $    $    $    $    "

    WHILE INKEY$ = ""
        FOR A = 1 TO 5
            LOCATE 1, 1
            PRINT MID$(A$, A, 80);
            LOCATE 20, 1
            PRINT MID$(A$, 6 - A, 80);

            FOR B = 2 TO 19
                c = (A + B) MOD 5
                IF c = 1 THEN
                    LOCATE B, 80
                    PRINT "$";
                    LOCATE 21 - B, 1
                    PRINT "$";
                ELSE
                    LOCATE B, 80
                    PRINT " ";
                    LOCATE 21 - B, 1
                    PRINT " ";
                END IF
            NEXT B
        NEXT A
    WEND

END SUB

'Print transaction summary to line printer
SUB TransactionSummary (Item)

    FancyCls Colors(2, ColorPref), Colors(1, ColorPref)
    PrintHelpLine ""
    COLOR Colors(7, ColorPref), Colors(4, ColorPref)
    Box 8, 20, 14, 62
    Center 10, "Prepare printer for Report!"
    Center 12, "Press <Enter> to print, or <Esc> to abort"

    DO: Kbd$ = INKEY$: LOOP WHILE Kbd$ <> CHR$(13) AND Kbd$ <> CHR$(27)

    IF Kbd$ = CHR$(13) THEN
        CLS

        LPRINT : LPRINT : LPRINT : LPRINT : LPRINT : LPRINT
        Lcenter "T H E   M O N E Y   M A N A G E R"
        LPRINT
        Lcenter "TRANSACTION SUMMARY -- " + DATE$
        LPRINT : LPRINT
        Lcenter Trim$(account(Item).Title)
        LPRINT
        u5$ = "--------|-------|------------------------------|---------|---------|----------"
        LPRINT u5$
        LPRINT "  Date  | Ref#  |Description                   |Increase |Decrease | Balance  "
        LPRINT u5$
        u0$ = "\      \|\     \|\                            \|"
        u2$ = "##,###.##"
        u3$ = "###,###.##"
        u4$ = "         "

        File$ = "money." + Cvit$(Item)
        OPEN File$ FOR RANDOM AS #1 LEN = 84
        FIELD #1, 8 AS IoDate$, 10 AS IoRef$, 50 AS IoDesc$, 8 AS IoFig1$, 8 AS IoFig2$
        FIELD #1, 11 AS valid$, 5 AS IoMaxRecord$, 8 AS IoBalance$
        GET #1, 1
        IF valid$ = "THISISVALID" THEN
            Balance# = 0
            MaxRecord = VAL(IoMaxRecord$)
            CurrRecord = 1
            WHILE CurrRecord <= MaxRecord

                GET #1, CurrRecord + 1
                Fig1# = CVD(IoFig1$)
                Fig2# = CVD(IoFig2$)

                LPRINT USING u0$; IoDate$; IoRef$; IoDesc$;
                IF Fig2# = 0 THEN
                    Balance# = Balance# + Fig1#
                    LPRINT USING u2$ + "|" + u4$ + "|" + u3$; Fig1#; Balance#
                ELSE
                    Balance# = Balance# - Fig2#
                    LPRINT USING u4$ + "|" + u2$ + "|" + u3$; Fig2#; Balance#
                END IF
                CurrRecord = CurrRecord + 1
            WEND
            LPRINT u5$
        END IF
        CLOSE
    END IF
END SUB

'Remove null and spaces from the end of a string.
FUNCTION Trim$ (X$)

    IF X$ = "" THEN
        Trim$ = ""
    ELSE
        lastChar = 0
        FOR A = 1 TO LEN(X$)
            y$ = MID$(X$, A, 1)
            IF y$ <> CHR$(0) AND y$ <> " " THEN
                lastChar = A
            END IF
        NEXT A
        Trim$ = LEFT$(X$, lastChar)
    END IF

END FUNCTION

