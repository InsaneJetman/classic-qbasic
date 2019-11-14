DEFINT A-Z
DECLARE SUB TransactionSummary (item%)
DECLARE SUB lcenter (text$)
DECLARE SUB pause ()
DECLARE SUB ScrollUp ()
DECLARE SUB ScrollDown ()
DECLARE SUB Initialize ()
DECLARE SUB Dointro ()
DECLARE SUB Sparkle ()
DECLARE SUB center (row%, text$)
DECLARE SUB fancyCls (dots%, background%)
DECLARE SUB LoadState ()
DECLARE SUB SaveState ()
DECLARE SUB MenuSystem ()
DECLARE SUB MakeBackup ()
DECLARE SUB RestoreBackup ()
DECLARE SUB box (row1%, col1%, row2%, col2%)
DECLARE SUB NetWorthReport ()
DECLARE SUB EditAccounts ()
DECLARE SUB printHelpLine (help$)
DECLARE SUB EditTrans (item%)
DECLARE FUNCTION Cvdt$ (x#)
DECLARE FUNCTION Cvst$ (x!)
DECLARE FUNCTION Cvit$ (x%)
DECLARE FUNCTION Menu% (currChoiceX%, maxChoice%, choice$(), itemRow%(), itemCol%(), help$(), barMode%)
DECLARE FUNCTION GetString$ (row%, col%, start$, end$, vis%, max%)
DECLARE FUNCTION Trim$ (x$)

CONST TRUE = -1
CONST FALSE = NOT TRUE
WIDTH , 25

TYPE accountType
    title        AS STRING * 20
    aType        AS STRING * 1
    desc         AS STRING * 50
END TYPE

TYPE recordtype
    date     AS STRING * 8
    ref      AS STRING * 10
    desc     AS STRING * 50
    fig1     AS DOUBLE
    fig2     AS DOUBLE
END TYPE

DIM SHARED account(1 TO 18)  AS accountType
DIM SHARED monitorType
DIM SHARED colors(0 TO 20, 1 TO 4)
DIM SHARED ScrollUpAsm(1 TO 7)
DIM SHARED ScrollDownAsm(1 TO 7)

    ON ERROR GOTO FileNotFound
    OPEN "money.dat" FOR INPUT AS #1
    CLOSE
    ON ERROR GOTO 0

    Initialize
    Dointro
    MenuSystem
    END

FileNotFound:
    SELECT CASE ERR
        CASE 53
            CLOSE
            monitorType = 1
            SaveState
        CASE ELSE
    END SELECT
    RESUME

    'scrn  dots  bar  back   title  shdow  choice  curs   cursbk  shdow
DATA 3,    7,    15,  7,     0,     7,     0,      15,    0,      0
DATA 1,    9,    12,  3,     0,     1,     15,     0,     7,      0
DATA 3,    15,   13,  1,     14,    3,     15,     0,     7,      0
DATA 7,    12,   15,  4,     14,    0,     15,     15,    1,      0

DATA &HB8,&H01,&H06,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H01,&H04,&HBA,&H4E,&H16,&HB7,&H00,&HCD,&H10,&HCB

SUB box (row1, col1, row2, col2) STATIC

    boxWidth = col2 - col1 + 1

    LOCATE row1, col1
    PRINT "⁄"; STRING$(boxWidth - 2, "ƒ"); "ø";

    FOR a = row1 + 1 TO row2 - 1
        LOCATE a, col1
        PRINT "≥"; SPACE$(boxWidth - 2); "≥";
    NEXT a

    LOCATE row2, col1
    PRINT "¿"; STRING$(boxWidth - 2, "ƒ"); "Ÿ";

END SUB

SUB center (row, text$)

    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;

END SUB

FUNCTION Cvdt$ (x#)

    Cvdt$ = RIGHT$(STR$(x#), LEN(STR$(x#)) - 1)

END FUNCTION

FUNCTION Cvit$ (x)
    Cvit$ = RIGHT$(STR$(x), LEN(STR$(x)) - 1)
END FUNCTION

FUNCTION Cvst$ (x!)
    Cvst$ = RIGHT$(STR$(x!), LEN(STR$(x!)) - 1)
END FUNCTION

SUB Dointro

    COLOR 15, 1
    CLS
    center 4, "The"
    COLOR 14
    DIM a$(10)
    center 5, "‹     ‹ ‹‹‹‹ ‹   ‹ ‹‹‹‹ ‹   ‹      ‹     ‹ ‹‹‹‹ ‹   ‹ ‹‹‹‹ ‹‹‹‹‹ ‹‹‹‹ ‹‹‹‹‹"
    center 6, "€ﬂ‹ ‹ﬂ€ €  € €‹  € €    €‹‹‹€      €ﬂ‹ ‹ﬂ€ €  € €‹  € €  € €     €    €   €"
    center 7, "€  ﬂ  € €  € € ﬂ‹€ €ﬂﬂﬂ   €        €  ﬂ  € €ﬂﬂ€ € ﬂ‹€ €ﬂﬂ€ € ﬂﬂ€ €ﬂﬂﬂ €ﬂ€ﬂﬂ"
    center 8, "€     € €‹‹€ €   € €‹‹‹   €        €     € €  € €   € €  € €‹‹‹€ €‹‹‹ €  ﬂ‹"

    COLOR 15
    center 11, "A Personal Finance Manager written in"
    center 12, "MS-DOS QBasic"
    center 24, "Push Any Key To Continue"

    COLOR 13
    Sparkle


END SUB

SUB EditAccounts

    REDIM help$(4), col(4), vis(4), max(4), edit$(18, 3)

    COLOR colors(7, monitorType), colors(4, monitorType)
    box 2, 1, 24, 80

    COLOR colors(5, monitorType), colors(4, monitorType)
    LOCATE 1, 68

    PRINT "Edit Accounts";
    COLOR colors(7, monitorType), colors(4, monitorType)

    LOCATE 3, 2: PRINT "  ≥                    ≥                                                  ≥Ass"
    LOCATE 4, 2: PRINT "No≥ Account Title      ≥ Description                                      ≥Lia"
    LOCATE 5, 2: PRINT "ƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒ"
                  u$ = "##≥\                  \≥\                                                \≥ ! "
    FOR a = 6 TO 23
        LOCATE a, 2
        x = a - 5
        PRINT USING u$; x; account(x).title; account(x).desc; account(x).aType;
    NEXT a

    help$(1) = "Hit <F2> save changes, or <Escape> to abort"
    help$(2) = "Hit <F2> save changes, or <Escape> to abort"
    help$(3) = "In this column, enter [A] for asset, or [L] for liability"
    

    col(1) = 5: col(2) = 26: col(3) = 78
    vis(1) = 20: vis(2) = 50: vis(3) = 1
    max(1) = 20: max(2) = 50: max(3) = 1

    FOR a = 1 TO 18
        edit$(a, 1) = account(a).title
        edit$(a, 2) = account(a).desc
        edit$(a, 3) = account(a).aType
    NEXT a

    finished = FALSE

    currRow = 1
    currCol = 1

    DO
        GOSUB EditAccountsShowCursor
        DO: kbd$ = INKEY$: LOOP UNTIL kbd$ <> ""
        IF kbd$ >= " " AND kbd$ < "~" THEN
            GOSUB EditAccountsEditItem
        END IF
        GOSUB EditAccountsHideCursor


        SELECT CASE kbd$
            CASE CHR$(0) + "H"
                currRow = (currRow + 16) MOD 18 + 1
            CASE CHR$(0) + "P"
                currRow = (currRow) MOD 18 + 1
            CASE CHR$(0) + "K", CHR$(0) + CHR$(15)
                currCol = (currCol + 1) MOD 3 + 1
            CASE CHR$(0) + "M", CHR$(9)
                currCol = (currCol) MOD 3 + 1
            CASE CHR$(0) + "G"
                currCol = 1
            CASE CHR$(0) + "O"
                currCol = 4
            CASE CHR$(0) + "I"
                currRow = 1
            CASE CHR$(0) + "Q"
                currRow = 18
            CASE CHR$(0) + "<"
                finished = TRUE
                save = TRUE
            CASE CHR$(27)
                finished = TRUE
                save = FALSE
            CASE CHR$(13)
            CASE ELSE
                BEEP
        END SELECT
    LOOP UNTIL finished

    IF save THEN
        GOSUB EditAccountsSaveData
    END IF

    EXIT SUB

EditAccountsShowCursor:
    COLOR colors(8, monitorType), colors(9, monitorType)
    LOCATE currRow + 5, col(currCol)
    PRINT LEFT$(edit$(currRow, currCol), vis(currCol));
    printHelpLine help$(currCol)
    RETURN

EditAccountsEditItem:
    COLOR colors(8, monitorType), colors(9, monitorType)
    ok = FALSE
    DO
        kbd$ = GetString$(currRow + 5, col(currCol), kbd$, back$, vis(currCol), max(currCol))
        edit$(currRow, currCol) = LEFT$(back$ + SPACE$(max(currCol)), max(currCol))

        IF currCol = 3 THEN
            x$ = UCASE$(back$)
            IF x$ = "A" OR x$ = "L" OR x$ = "" OR x$ = " " THEN
                ok = TRUE
                edit$(currRow, currCol) = x$
            ELSE
                BEEP
            END IF
        ELSE
            ok = TRUE
        END IF
        
    LOOP UNTIL ok
    RETURN

EditAccountsHideCursor:
    COLOR colors(7, monitorType), colors(4, monitorType)
    LOCATE currRow + 5, col(currCol)
    PRINT SPACE$(vis(currCol));
    LOCATE currRow + 5, col(currCol)
    PRINT LEFT$(edit$(currRow, currCol), vis(currCol));
    printHelpLine help$(currCol)
    RETURN


EditAccountsSaveData:
    FOR a = 1 TO 18
        account(a).title = edit$(a, 1)
        account(a).desc = edit$(a, 2)
        account(a).aType = edit$(a, 3)
    NEXT a
    SaveState
    RETURN

END SUB

SUB EditTrans (item)

    REDIM help$(6), col(6), vis(6), max(6), currString$(3), currFig#(5)
    REDIM balance#(1000)

    file$ = "money." + Cvit$(item)
    OPEN file$ FOR RANDOM AS #1 LEN = 84
    FIELD #1, 8 AS ioDate$, 10 AS ioRef$, 50 AS ioDesc$, 8 AS ioFig1$, 8 AS ioFig2$
    FIELD #1, 11 AS valid$, 5 AS ioMaxRecord$, 8 AS ioBalance$

    currString$(1) = ""
    currString$(2) = ""
    currString$(3) = ""
    currFig#(4) = 0
    currFig#(5) = 0

    GET #1, 1
    IF valid$ <> "THISISVALID" THEN
        LSET ioDate$ = ""
        LSET ioRef$ = ""
        LSET ioDesc$ = ""
        LSET ioFig1$ = MKD$(0)
        LSET ioFig2$ = MKD$(0)
        PUT #1, 2
        LSET valid$ = "THISISVALID"
        LSET ioMaxRecord$ = "1"
        LSET ioBalance$ = MKD$(0)
        PUT #1, 1
    END IF

    maxRecord = VAL(ioMaxRecord$)

    balance#(0) = 0
    a = 1
    WHILE a <= maxRecord
        GET #1, a + 1
        balance#(a) = balance#(a - 1) + CVD(ioFig1$) - CVD(ioFig2$)
        a = a + 1
    WEND
    GOSUB EditTransWriteBalance

    COLOR colors(7, monitorType), colors(4, monitorType)
    box 2, 1, 24, 80

    COLOR colors(5, monitorType), colors(4, monitorType)
    x$ = Trim$(account(item).title)
    LOCATE 1, 80 - LEN(x$)
    PRINT x$;

    help$(1) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    help$(2) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    help$(3) = "<F2> Save & Exit    <F9> Insert Transaction     <F10> Delete Transaction"
    help$(4) = "Transactions that Increase value of asset, or increase debt owed for liability"
    help$(5) = "Transactions that decrease value of asset, or decrease debt owed for liability"

    col(1) = 2
    col(2) = 11
    col(3) = 19
    col(4) = 50
    col(5) = 60

    vis(1) = 8
    vis(2) = 7
    vis(3) = 30
    vis(4) = 9
    vis(5) = 9

    max(1) = 8
    max(2) = 10
    max(3) = 50
    max(4) = 8
    max(5) = 8

    COLOR colors(7, monitorType), colors(4, monitorType)
    LOCATE 3, 2: PRINT "  Date  ≥ Ref#  ≥Description                   ≥Increase ≥Decrease ≥ Balance  "
    LOCATE 4, 2: PRINT "ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ"

     u$ = "\      \≥\     \≥\                            \≥"
    u1$ = "        ≥       ≥                              ≥         ≥         ≥          "
    u1x$ = "ﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂ≥ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ"
    u2$ = "##,###.##"
    u3$ = "###,###.##"
    u4$ = "         "

    currTopline = 1
    GOSUB EditTransPrintWholeScreen

    currRow = 1
    currCol = 1
    GOSUB EditTransGetLine

    finished = FALSE
    DO
        GOSUB EditTransShowCursor
        DO: kbd$ = INKEY$: LOOP UNTIL kbd$ <> ""
        GOSUB EditTransHideCursor

        IF kbd$ >= " " AND kbd$ < "~" THEN
            GOSUB EditTransEditItem
        END IF

        SELECT CASE kbd$
            CASE CHR$(0) + "H"  'up
                GOSUB EditTransMoveUp
            CASE CHR$(0) + "P"
                GOSUB EditTransMoveDown
            CASE CHR$(0) + "K", CHR$(0) + CHR$(15)
                currCol = (currCol + 3) MOD 5 + 1
            CASE CHR$(0) + "M", CHR$(9)
                currCol = (currCol) MOD 5 + 1
            CASE CHR$(0) + "G"
                currCol = 1
            CASE CHR$(0) + "O"
                currCol = 5
            CASE CHR$(0) + "I"
                currRow = 1
                currTopline = currTopline - 19
                IF currTopline < 1 THEN
                    currTopline = 1
                END IF
                GOSUB EditTransPrintWholeScreen
                currRow = 1
                GOSUB EditTransGetLine
            CASE CHR$(0) + "Q"
                currRow = 1
                currTopline = currTopline + 19
                IF currTopline > maxRecord THEN
                    currTopline = maxRecord
                END IF
                GOSUB EditTransPrintWholeScreen
                currRow = 1
                GOSUB EditTransGetLine
            CASE CHR$(0) + "<"
                finished = TRUE
            CASE CHR$(0) + "C"
                GOSUB EditTransAddRecord
            CASE CHR$(0) + "D"
                GOSUB EditTransDeleteRecord
            CASE CHR$(13)
            CASE ELSE
                'BEEP
        END SELECT
    LOOP UNTIL finished

    CLOSE

    EXIT SUB


EditTransShowCursor:
    COLOR colors(8, monitorType), colors(9, monitorType)
    LOCATE currRow + 4, col(currCol)
    PRINT SPACE$(vis(currCol));
    LOCATE currRow + 4, col(currCol)
    SELECT CASE currCol
        CASE 1, 2, 3
            PRINT LEFT$(currString$(currCol), vis(currCol));
        CASE 4
            IF currFig#(4) <> 0 OR currFig#(5) = 0 THEN
                PRINT USING u2$; currFig#(4);
            END IF
        CASE 5
            IF currFig#(5) <> 0 THEN
                PRINT USING u2$; currFig#(5);
            END IF
    END SELECT
    printHelpLine help$(currCol)
    RETURN


EditTransHideCursor:
    COLOR colors(7, monitorType), colors(4, monitorType)
    LOCATE currRow + 4, col(currCol)
    PRINT SPACE$(vis(currCol));
    LOCATE currRow + 4, col(currCol)
    SELECT CASE currCol
        CASE 1, 2, 3
            PRINT LEFT$(currString$(currCol), vis(currCol));
        CASE 4
            IF currFig#(4) <> 0 OR currFig#(5) = 0 THEN
                PRINT USING u2$; currFig#(4);
            END IF
        CASE 5
            IF currFig#(5) <> 0 THEN
                PRINT USING u2$; currFig#(5);
            END IF
    END SELECT
    RETURN


EditTransEditItem:

    currRecord = currTopline + currRow - 1
    COLOR colors(8, monitorType), colors(9, monitorType)

    SELECT CASE currCol
        CASE 1, 2, 3
            kbd$ = GetString$(currRow + 4, col(currCol), kbd$, new$, vis(currCol), max(currCol))
            currString$(currCol) = new$
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
        CASE 4
            DO
                kbd$ = GetString$(currRow + 4, col(4), kbd$, new$, vis(4), max(4))
                new4# = VAL(new$)
            LOOP WHILE new4# > 99999.99 OR new4# < 0

            a = currRecord
            WHILE a <= maxRecord
                balance#(a) = balance#(a) + new4# - currFig#(4) + currFig#(5)
                a = a + 1
            WEND
            currFig#(4) = new4#
            currFig#(5) = 0
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
            GOSUB EditTransPrintBalances
            GOSUB EditTransWriteBalance
        CASE 5
            DO
                kbd$ = GetString$(currRow + 4, col(5), kbd$, new$, vis(5), max(5))
                new5# = VAL(new$)
            LOOP WHILE new4# > 99999.99 OR new5# < 0

            a = currRecord
            WHILE a <= maxRecord
                balance#(a) = balance#(a) - new5# + currFig#(5) - currFig#(4)
                a = a + 1
            WEND
            currFig#(4) = 0
            currFig#(5) = new5#
            GOSUB EditTransPutLine
            GOSUB EditTransGetLine
            GOSUB EditTransPrintBalances
            GOSUB EditTransWriteBalance
        CASE ELSE
    END SELECT
    GOSUB EditTransPrintLine
    RETURN

EditTransMoveUp:
    IF currRow = 1 THEN
        IF currTopline = 1 THEN
            BEEP
        ELSE
            ScrollDown
            currTopline = currTopline - 1
            GOSUB EditTransGetLine
            GOSUB EditTransPrintLine
        END IF
    ELSE
        currRow = currRow - 1
        GOSUB EditTransGetLine
    END IF
    RETURN

EditTransMoveDown:
    IF (currRow + currTopline - 1) >= maxRecord THEN
        BEEP
    ELSE
        IF currRow = 19 THEN
            ScrollUp
            currTopline = currTopline + 1
            GOSUB EditTransGetLine
            GOSUB EditTransPrintLine
        ELSE
            currRow = currRow + 1
            GOSUB EditTransGetLine
        END IF
    END IF


EditTransPrintLine:
    COLOR colors(7, monitorType), colors(4, monitorType)
    currRecord = currTopline + currRow - 1
    LOCATE currRow + 4, 2
    IF currRecord = maxRecord + 1 THEN
        LOCATE currRow + 4, 2
        PRINT u1x$;
    ELSEIF currRecord > maxRecord THEN
        LOCATE currRow + 4, 2
        PRINT u1$;
    ELSE
        PRINT USING u$; currString$(1); currString$(2); currString$(3);
        IF currFig#(5) = 0 THEN
            PRINT USING u2$ + "≥" + u4$ + "≥" + u3$; currFig#(4); balance#(currRecord)
        ELSE
            PRINT USING u4$ + "≥" + u2$ + "≥" + u3$; currFig#(5); balance#(currRecord)
        END IF
    END IF
    RETURN

EditTransPrintBalances:
    COLOR colors(7, monitorType), colors(4, monitorType)
    FOR a = 1 TO 19
        currRecord = currTopline + a - 1
        IF currRecord <= maxRecord THEN
            LOCATE 4 + a, 70
            PRINT USING u3$; balance#(currTopline + a - 1);
        END IF
    NEXT a
    RETURN

EditTransDeleteRecord:
    IF maxRecord = 1 THEN
        BEEP
    ELSE
        currRecord = currTopline + currRow - 1
        maxRecord = maxRecord - 1
        a = currRecord
        WHILE a <= maxRecord
            GET #1, a + 2
            PUT #1, a + 1
            balance#(a) = balance#(a + 1) - currFig#(4) + currFig#(5)
            a = a + 1
        WEND
        
        LSET valid$ = "THISISVALID"
        LSET ioMaxRecord$ = Cvit$(maxRecord)
        PUT #1, 1
        GOSUB EditTransPrintWholeScreen
        currRecord = currTopline + currRow - 1
        IF currRecord > maxRecord THEN
            GOSUB EditTransMoveUp
        END IF
        GOSUB EditTransGetLine
        GOSUB EditTransWriteBalance
    END IF
    RETURN

EditTransAddRecord:
    currRecord = currTopline + currRow - 1
    a = maxRecord
    WHILE a > currRecord
        GET #1, a + 1
        PUT #1, a + 2
        balance#(a + 1) = balance#(a)
        a = a - 1
    WEND
    balance#(currRecord + 1) = balance#(currRecord)
    maxRecord = maxRecord + 1
    LSET ioDate$ = ""
    LSET ioRef$ = ""
    LSET ioDesc$ = ""
    LSET ioFig1$ = MKD$(0)
    LSET ioFig2$ = MKD$(0)
    PUT #1, currRecord + 2

    LSET valid$ = "THISISVALID"
    LSET ioMaxRecord$ = Cvit$(maxRecord)
    PUT #1, 1
    GOSUB EditTransPrintWholeScreen
    GOSUB EditTransGetLine
    RETURN

EditTransPrintWholeScreen:
    temp = currRow
    FOR currRow = 1 TO 19
        currRecord = currTopline + currRow - 1
        IF currRecord <= maxRecord THEN
            GOSUB EditTransGetLine
        END IF
        GOSUB EditTransPrintLine
    NEXT currRow
    currRow = temp
    RETURN

EditTransWriteBalance:
    GET #1, 1
    LSET ioBalance$ = MKD$(balance#(maxRecord))
    PUT #1, 1
    RETURN

EditTransPutLine:
    currRecord = currTopline + currRow - 1
    LSET ioDate$ = currString$(1)
    LSET ioRef$ = currString$(2)
    LSET ioDesc$ = currString$(3)
    LSET ioFig1$ = MKD$(currFig#(4))
    LSET ioFig2$ = MKD$(currFig#(5))
    PUT #1, currRecord + 1
    RETURN

EditTransGetLine:
    currRecord = currTopline + currRow - 1
    GET #1, currRecord + 1
    currString$(1) = ioDate$
    currString$(2) = ioRef$
    currString$(3) = ioDesc$
    currFig#(4) = CVD(ioFig1$)
    currFig#(5) = CVD(ioFig2$)
    RETURN
END SUB

SUB fancyCls (dots, background)

    VIEW PRINT 2 TO 24
    COLOR dots, background
    CLS 2

    FOR a = 95 TO 1820 STEP 45
        row = a / 80 + 1
        col = a MOD 80 + 1
        LOCATE row, col
        PRINT CHR$(250);
    NEXT a

    VIEW PRINT

END SUB

FUNCTION GetString$ (row, col, start$, end$, vis, max)

    
    curr$ = Trim$(LEFT$(start$, max))

    LOCATE , , 1

    finished = FALSE
    DO
        GOSUB GetStringShowText
        GOSUB GetStringGetKey

        IF LEN(kbd$) > 1 THEN
            finished = TRUE
            GetString$ = kbd$
        ELSE
            SELECT CASE kbd$
                CASE CHR$(13), CHR$(27), CHR$(9)
                    finished = TRUE
                    GetString$ = kbd$
                
                CASE CHR$(8)
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                    END IF

                CASE " " TO "}"
                    IF LEN(curr$) < max THEN
                        curr$ = curr$ + kbd$
                    END IF

                CASE ELSE
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL finished

    end$ = curr$

    EXIT FUNCTION
    

GetStringShowText:
    LOCATE row, col
    IF LEN(curr$) > vis THEN
        PRINT RIGHT$(curr$, vis);
    ELSE
        PRINT curr$; SPACE$(vis - LEN(curr$));
        LOCATE row, col + LEN(curr$)
    END IF
    RETURN

GetStringGetKey:
    kbd$ = ""
    WHILE kbd$ = ""
        kbd$ = INKEY$
    WEND
    RETURN



END FUNCTION

SUB Initialize

    VIEW PRINT

    FOR colorset = 1 TO 4
        FOR x = 1 TO 10
            READ colors(x, colorset)
        NEXT x
    NEXT colorset

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

SUB lcenter (text$)
    PRINT TAB(41 - LEN(text$) / 2); text$
END SUB

SUB LoadState

    OPEN "money.dat" FOR INPUT AS #1
    INPUT #1, monitorType

    FOR a = 1 TO 18
        LINE INPUT #1, account(a).title
        LINE INPUT #1, account(a).aType
        LINE INPUT #1, account(a).desc
    NEXT a
    
    CLOSE

END SUB

SUB MakeBackup
END SUB

FUNCTION Menu (currChoiceX, maxChoice, choice$(), itemRow(), itemCol(), help$(), barMode)
    currChoice = currChoiceX

    'Color in Help Bar
    COLOR colors(5, monitorType), colors(4, monitorType)
    LOCATE 25, 1
    PRINT SPACE$(80);

    'if in bar mode, color in menu bar, else color box/shadow
    IF barMode THEN
        COLOR colors(7, monitorType), colors(4, monitorType)
        LOCATE 1, 1
        PRINT SPACE$(80);
    ELSE
        COLOR colors(7, monitorType), colors(4, monitorType)
        box itemRow(1) - 1, itemCol(1) - 1, itemRow(maxChoice) + 1, itemCol(1) + LEN(choice$(1)) + 1
        
        COLOR colors(10, monitorType), colors(6, monitorType)
        FOR a = 1 TO maxChoice + 1
            LOCATE itemRow(1) + a - 1, itemCol(1) + LEN(choice$(1)) + 2
            PRINT CHR$(178); CHR$(178);
        NEXT a
        LOCATE itemRow(maxChoice) + 2, itemCol(maxChoice) + 2
        PRINT STRING$(LEN(choice$(maxChoice)) + 2, 178);
    END IF
    
    'print the choices
    COLOR colors(7, monitorType), colors(4, monitorType)
    FOR a = 1 TO maxChoice
        LOCATE itemRow(a), itemCol(a)
        PRINT choice$(a);
    NEXT a

    finished = FALSE

    WHILE NOT finished
        
        GOSUB MenuShowCursor
        GOSUB MenuGetKey
        GOSUB MenuHideCursor

        SELECT CASE kbd$
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
    finished = TRUE
    RETURN

MenuEscape:
    currChoice = 0
    finished = TRUE
    RETURN

MenuUp:
    IF barMode THEN
        BEEP
    ELSE
        currChoice = (currChoice + maxChoice - 2) MOD maxChoice + 1
    END IF
    RETURN

MenuLeft:
    IF barMode THEN
        currChoice = (currChoice + maxChoice - 2) MOD maxChoice + 1
    ELSE
        currChoice = -2
        finished = TRUE
    END IF
    RETURN

MenuRight:
    IF barMode THEN
        currChoice = (currChoice) MOD maxChoice + 1
    ELSE
        currChoice = -3
        finished = TRUE
    END IF
    RETURN

MenuDown:
    IF barMode THEN
        finished = TRUE
    ELSE
        currChoice = (currChoice) MOD maxChoice + 1
    END IF
    RETURN

MenuShowCursor:
    COLOR colors(8, monitorType), colors(9, monitorType)
    LOCATE itemRow(currChoice), itemCol(currChoice)
    PRINT choice$(currChoice);
    printHelpLine help$(currChoice)
    RETURN

MenuGetKey:
    kbd$ = ""
    WHILE kbd$ = ""
        kbd$ = INKEY$
    WEND
    RETURN

MenuHideCursor:
    COLOR colors(7, monitorType), colors(4, monitorType)
    LOCATE itemRow(currChoice), itemCol(currChoice)
    PRINT choice$(currChoice);
    RETURN


END FUNCTION

SUB MenuSystem

    DIM choice$(20), menuRow(20), menuCol(20), help$(20)

    choice = 1
    finished = FALSE

    WHILE NOT finished
        fancyCls colors(2, monitorType), colors(1, monitorType)
        GOSUB MenuSystemMain

        subchoice = -1
        WHILE subchoice < 0
            SELECT CASE choice
                CASE 1: GOSUB MenuSystemFile
                CASE 2: GOSUB MenuSystemEdit
                CASE 3: GOSUB MenuSystemAccount
                CASE 4: GOSUB MenuSystemReport
                CASE 5: GOSUB MenuSystemColors
            END SELECT
            fancyCls colors(2, monitorType), colors(1, monitorType)

            SELECT CASE subchoice
                CASE -2: choice = (choice + 3) MOD 5 + 1
                CASE -3: choice = (choice) MOD 5 + 1
            END SELECT
        WEND
    WEND
    EXIT SUB


MenuSystemMain:
    choice$(1) = " File "
    choice$(2) = " Accounts "
    choice$(3) = " Transactions "
    choice$(4) = " Reports "
    choice$(5) = " Colors "

    menuRow(1) = 1: menuCol(1) = 2
    menuRow(2) = 1: menuCol(2) = 8
    menuRow(3) = 1: menuCol(3) = 18
    menuRow(4) = 1: menuCol(4) = 32
    menuRow(5) = 1: menuCol(5) = 41
    
    help$(1) = "Make backups, Restore from Backup, Exit the Money Manager"
    help$(2) = "Add New Accounts, Delete old Accounts"
    help$(3) = "Select an account to work with"
    help$(4) = "Select a report to print"
    help$(5) = "Select your favorite screen colors"
    
    DO
        NewChoice = Menu((choice), 5, choice$(), menuRow(), menuCol(), help$(), TRUE)
    LOOP WHILE NewChoice = 0
    choice = NewChoice
    RETURN

MenuSystemFile:
    choice$(1) = " Exit           "

    menuRow(1) = 3: menuCol(1) = 2

    help$(1) = "Exit the Money Manager"

    subchoice = Menu(1, 1, choice$(), menuRow(), menuCol(), help$(), FALSE)

    SELECT CASE subchoice
        CASE 1: finished = TRUE
        CASE ELSE
    END SELECT
    RETURN


MenuSystemEdit:
    choice$(1) = " Edit Account Titles "

    menuRow(1) = 3: menuCol(1) = 8
    
    help$(1) = "Add/Edit/Delete Accounts."

    subchoice = Menu(1, 1, choice$(), menuRow(), menuCol(), help$(), FALSE)

    SELECT CASE subchoice
        CASE 1: EditAccounts
        CASE ELSE
    END SELECT
    RETURN


MenuSystemAccount:

    FOR a = 1 TO 18
        IF Trim$(account(a).title) = "" THEN
            choice$(a) = RIGHT$(STR$(a), 2) + ". ------------------- "
        ELSE
            choice$(a) = RIGHT$(STR$(a), 2) + ". " + account(a).title
        END IF
        menuRow(a) = a + 2
        menuCol(a) = 18
        help$(a) = account(a).desc
    NEXT a

    subchoice = Menu(1, 18, choice$(), menuRow(), menuCol(), help$(), FALSE)

    IF subchoice > 0 THEN
        EditTrans (subchoice)
    END IF
    RETURN


MenuSystemReport:
    choice$(1) = " Net Worth Report       "
    menuRow(1) = 3: menuCol(1) = 32
    help$(1) = "Computes your net worth."

    FOR a = 1 TO 18
        IF Trim$(account(a).title) = "" THEN
            choice$(a + 1) = RIGHT$(STR$(a), 2) + ". ------------------- "
        ELSE
            choice$(a + 1) = RIGHT$(STR$(a), 2) + ". " + account(a).title
        END IF
        menuRow(a + 1) = a + 3
        menuCol(a + 1) = 32
        help$(a + 1) = account(a).desc
    NEXT a

    subchoice = Menu(1, 19, choice$(), menuRow(), menuCol(), help$(), FALSE)

    SELECT CASE subchoice
        CASE 1
            NetWorthReport
        CASE 2 TO 19
            TransactionSummary (subchoice - 1)
        CASE ELSE
    END SELECT
    RETURN

MenuSystemColors:
    choice$(1) = " Monochrome Scheme "
    choice$(2) = " Blue/Cyan Scheme  "
    choice$(3) = " Cyan/Blue Scheme  "
    choice$(4) = " Grey/Red Scheme   "

    menuRow(1) = 3: menuCol(1) = 41
    menuRow(2) = 4: menuCol(2) = 41
    menuRow(3) = 5: menuCol(3) = 41
    menuRow(4) = 6: menuCol(4) = 41

    help$(1) = "Proper color scheme for monochrome and LCD displays"
    help$(2) = "Color scheme featuring cyan"
    help$(3) = "Color scheme featuring blue"
    help$(4) = "Color scheme featuring red"

    subchoice = Menu(1, 4, choice$(), menuRow(), menuCol(), help$(), FALSE)

    SELECT CASE subchoice
        CASE 1 TO 4
            monitorType = subchoice
            SaveState
        CASE ELSE
    END SELECT
    RETURN


END SUB

SUB NetWorthReport

    DIM assetIndex(18), liabilityIndex(18)

    maxAsset = 0
    maxLiability = 0

    FOR a = 1 TO 18
        IF account(a).aType = "A" THEN
            maxAsset = maxAsset + 1
            assetIndex(maxAsset) = a
        ELSEIF account(a).aType = "L" THEN
            maxLiability = maxLiability + 1
            liabilityIndex(maxLiability) = a
        END IF
    NEXT a

    fancyCls colors(2, monitorType), colors(1, monitorType)
    printHelpLine ""
    COLOR colors(7, monitorType), colors(4, monitorType)
    box 8, 20, 14, 62
    center 10, "Prepare printer for Report!"
    center 12, "Hit <Enter> to print, or <Esc> to abort"

    DO: kbd$ = INKEY$: LOOP WHILE kbd$ <> CHR$(13) AND kbd$ <> CHR$(27)

    IF kbd$ = CHR$(13) THEN
        CLS
        PRINT "THIS IS A TEMPORARY PRINT ROUTINE"
       
        u0$ = "                     \                  \ "
        u1$ = "                          \                  \  +$$,###.##"
        u2$ = "                                                ----------"
        u3$ = "                                                =========="
        PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
        lcenter "T H E   M O N E Y   M A N A G E R"
        PRINT : PRINT
        lcenter "NET WORTH REPORT -- " + DATE$
        lcenter "-------------------------------------------"
        PRINT USING u0$; "ASSETS:"
        assetTotal# = 0
        a = 1
        WHILE a <= maxAsset
            file$ = "money." + Cvit$(assetIndex(a))
            OPEN file$ FOR RANDOM AS #1 LEN = 84
            FIELD #1, 11 AS valid$, 5 AS ioMaxRecord$, 8 AS ioBalance$
            GET #1, 1
            IF valid$ = "THISISVALID" THEN
                PRINT USING u1$; account(assetIndex(a)).title; CVD(ioBalance$)
                assetTotal# = assetTotal# + CVD(ioBalance$)
            END IF
            CLOSE
            a = a + 1
        WEND
        PRINT u2$
        PRINT USING u1$; "Total assets"; assetTotal#
        PRINT
        PRINT
        PRINT USING u0$; "LIABILITIES:"
        liabilityTotal# = 0
        a = 1
        WHILE a <= maxLiability
            file$ = "money." + Cvit$(liabilityIndex(a))
            OPEN file$ FOR RANDOM AS #1 LEN = 84
            FIELD #1, 11 AS valid$, 5 AS ioMaxRecord$, 8 AS ioBalance$
            GET #1, 1
            IF valid$ = "THISISVALID" THEN
                PRINT USING u1$; account(liabilityIndex(a)).title; CVD(ioBalance$)
                liabilityTotal# = liabilityTotal# + CVD(ioBalance$)
            END IF
            CLOSE
            a = a + 1
        WEND
        PRINT u2$
        PRINT USING u1$; "Total liability"; liabilityTotal#
        PRINT

        PRINT
        PRINT u2$
        PRINT USING u1$; "NET WORTH"; assetTotal# - liabilityTotal#
        PRINT u3$
    END IF

END SUB

SUB pause

    WHILE INKEY$ <> "": WEND
    WHILE INKEY$ = "": WEND

END SUB

SUB printHelpLine (help$)

    COLOR colors(5, monitorType), colors(4, monitorType)
    LOCATE 25, 1
    PRINT SPACE$(80);
    center 25, help$

END SUB

SUB RestoreBackup
END SUB

SUB SaveState

    OPEN "money.dat" FOR OUTPUT AS #2
    PRINT #2, monitorType
    
    FOR a = 1 TO 18
        PRINT #2, account(a).title
        PRINT #2, account(a).aType
        PRINT #2, account(a).desc
    NEXT a
    
    CLOSE #2

END SUB

SUB ScrollDown

    DEF SEG = VARSEG(ScrollDownAsm(1))
    CALL Absolute(VARPTR(ScrollDownAsm(1)))
    DEF SEG

END SUB

SUB ScrollUp

    DEF SEG = VARSEG(ScrollUpAsm(1))
    CALL Absolute(VARPTR(ScrollUpAsm(1)))
    DEF SEG

END SUB

SUB Sparkle

    a$ = "$    $    $    $    $    $    $    $    $    $    $    $    $    $    $    $    $    "

    WHILE INKEY$ = ""
        FOR a = 1 TO 5
            LOCATE 1, 1
            PRINT MID$(a$, a, 80);
            LOCATE 20, 1
            PRINT MID$(a$, 6 - a, 80);

            FOR b = 2 TO 19
                c = (a + b) MOD 5
                IF c = 1 THEN
                    LOCATE b, 80
                    PRINT "$";
                    LOCATE 21 - b, 1
                    PRINT "$";
                ELSE
                    LOCATE b, 80
                    PRINT " ";
                    LOCATE 21 - b, 1
                    PRINT " ";
                END IF
            NEXT b
        NEXT a
    WEND

END SUB

SUB TransactionSummary (item)

    fancyCls colors(2, monitorType), colors(1, monitorType)
    printHelpLine ""
    COLOR colors(7, monitorType), colors(4, monitorType)
    box 8, 20, 14, 62
    center 10, "Prepare printer for Report!"
    center 12, "Hit <Enter> to print, or <Esc> to abort"

    DO: kbd$ = INKEY$: LOOP WHILE kbd$ <> CHR$(13) AND kbd$ <> CHR$(27)

    IF kbd$ = CHR$(13) THEN
        CLS
        PRINT "THIS IS A TEMPORARY PRINT ROUTINE"
        PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
        lcenter "T H E   M O N E Y   M A N A G E R"
        PRINT
        lcenter "TRANSACTION SUMMARY -- " + DATE$
        PRINT : PRINT
        lcenter Trim$(account(item).title)
        PRINT
        u5$ = "--------|-------|------------------------------|---------|---------|----------"
        PRINT u5$
        PRINT "  Date  | Ref#  |Description                   |Increase |Decrease | Balance  "
        PRINT u5$
        u0$ = "\      \|\     \|\                            \|"
        u2$ = "##,###.##"
        u3$ = "###,###.##"
        u4$ = "         "

        file$ = "money." + Cvit$(item)
        OPEN file$ FOR RANDOM AS #1 LEN = 84
        FIELD #1, 8 AS ioDate$, 10 AS ioRef$, 50 AS ioDesc$, 8 AS ioFig1$, 8 AS ioFig2$
        FIELD #1, 11 AS valid$, 5 AS ioMaxRecord$, 8 AS ioBalance$
        GET #1, 1
        IF valid$ = "THISISVALID" THEN
            balance# = 0
            maxRecord = VAL(ioMaxRecord$)
            currRecord = 1
            WHILE currRecord <= maxRecord

                GET #1, currRecord + 1
                fig1# = CVD(ioFig1$)
                fig2# = CVD(ioFig2$)

                PRINT USING u0$; ioDate$; ioRef$; ioDesc$;
                IF fig2# = 0 THEN
                    balance# = balance# + fig1#
                    PRINT USING u2$ + "|" + u4$ + "|" + u3$; fig1#; balance#
                ELSE
                    balance# = balance# - fig2#
                    PRINT USING u4$ + "|" + u2$ + "|" + u3$; fig2#; balance#
                END IF
                currRecord = currRecord + 1
            WEND
            PRINT u5$
        END IF
        CLOSE
    END IF
END SUB

FUNCTION Trim$ (x$)

    IF x$ = "" THEN
        Trim$ = ""
    ELSE
        lastChar = 0
        FOR a = 1 TO LEN(x$)
            y$ = MID$(x$, a, 1)
            IF y$ <> CHR$(0) AND y$ <> " " THEN
                lastChar = a
            END IF
        NEXT a
        Trim$ = LEFT$(x$, lastChar)
    END IF
    
END FUNCTION

