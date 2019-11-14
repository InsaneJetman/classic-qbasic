
'For faster operation
DEFINT A-Z

'Declaration of sub programs
DECLARE SUB ScrollDown ()
DECLARE SUB Initialize ()
DECLARE SUB PhoneEditor ()
DECLARE FUNCTION GetString$ (row, col, start$, end$, vis, max)

'Declaration of constants
CONST TRUE = -1
CONST false = NOT TRUE

'declaration of global arrays
DIM SHARED ScrollUpAsm(1 TO 7)          'Stores asm routine to scroll up
DIM SHARED ScrollDownAsm(1 TO 7)        'Stores asm routine to scroll down

'Program
    Initialize
    PhoneEditor

'assembly routines
DATA &HB8,&H01,&H06,&HB9,&H00,&H02,&HBA,&H4F,&H17,&HB7,&H00,&HCD,&H10,&HCB
DATA &HB8,&H01,&H07,&HB9,&H00,&H02,&HBA,&H4F,&H17,&HB7,&H00,&HCD,&H10,&HCB

'                              Max Chars   Max
'       Title$     Column      Visible     Chars
DATA    "Name",     2,         19,         30
DATA    "Phone",   22,         19,         20
DATA    "Address", 42,         19,         30
DATA    "City",    62,          9,         20
DATA    "St",      72,          2,          2
DATA    "Zip",     75,          5,          5

'This funtion gets string input at location row,col.  The string to
'input is at most MAX characters long, and no more than VIS chars may be
'displayed.  The initial string is sent in START$, and the resulting
'string is returned as END$
'The function itself returns the keystroke that was used to exit this
'routine, for example, arrow keys, function keys, return, escape, etc...
'
FUNCTION GetString$ (row, col, start$, end$, vis, max)

    'establish current string
    curr$ = LTRIM$(LEFT$(start$, max))

    'force cursor to exist
    LOCATE , , 1

    finished = false
    DO
        GOSUB GetStringShowText
        GOSUB GetStringGetKey

        IF LEN(kbd$) > 1 THEN                       'Exit if special key
            finished = TRUE
            GetString$ = kbd$
        ELSE
            SELECT CASE kbd$
                CASE CHR$(13), CHR$(27), CHR$(9)    'Exit if special key
                    finished = TRUE
                    GetString$ = kbd$
                
                CASE CHR$(8)                        'Handle backspace
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                    END IF

                CASE " " TO "}"                     'Handle any text key
                    IF LEN(curr$) < max THEN
                        curr$ = curr$ + kbd$
                    END IF

                CASE ELSE                           'Beep for anything else
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL finished

    end$ = curr$
    EXIT FUNCTION
    

GetStringShowText:                                  'Show at most VIS chars
    LOCATE row, col                                 'of string
    IF LEN(curr$) > vis THEN
        PRINT RIGHT$(curr$, vis);
    ELSE
        PRINT curr$; SPACE$(vis - LEN(curr$));
        LOCATE row, col + LEN(curr$)
    END IF
    RETURN


GetStringGetKey:                                    'Standard inkey loop
    DO
        kbd$ = INKEY$
    LOOP WHILE kbd$ = ""
    RETURN

END FUNCTION

SUB Initialize

    'set up screen
    WIDTH , 25
    VIEW PRINT
    LOCATE , , 1

    'read in first asm routine
    P = VARPTR(ScrollUpAsm(1))
    DEF SEG = VARSEG(ScrollUpAsm(1))
    FOR i = 0 TO 13
       READ j
       POKE (P + i), j
    NEXT i

    'read in second asm routine
    P = VARPTR(ScrollDownAsm(1))
    DEF SEG = VARSEG(ScrollDownAsm(1))
    FOR i = 0 TO 13
       READ j
       POKE (P + i), j
    NEXT i

    'return segment to normal
    DEF SEG

END SUB

SUB PhoneEditor

    'Dimension arrays
    DIM edit$(6)        'Current line being edited
    DIM Title$(6)       'Column titles
    DIM col(6)          'Column locations
    DIM vis(6)          'Maximum visible chars
    DIM max(6)          'Maximum chars allowes
    DIM io$(6)          'Used to RANDOM access file I/O
    DIM name1$(6)       'Temp storage to print names
    DIM name2$(6)       'Temp storage to print names
    DIM index(2000)     'Tells the editor where to find name on disk
    DIM key$(2000)      'Used for sorting

    'prepare screen
    FOR a = 1 TO 6
        READ Title$(a), col(a), vis(a), max(a)
    NEXT a

    u1$ = "�\                 \�\                 \�\                 \�\       \�\\�\   \�"
    u2$ = "쳐컴컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴탠컴컴컴컴탠컵컴컴캑"
    u3$ = "읕컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴좔컴컴컴컴좔컨컴컴켸"
        
    COLOR 14, 1
    CLS
    
    PRINT USING u1$; Title$(1); Title$(2); Title$(3); Title$(4); Title$(5); Title$(6);
    PRINT u2$
    LOCATE 25, 1
    PRINT "          <F2=Exit>  <F5=Sort>  <F6=Print>  <F9=Insert>  <F10=Delete>";

    'Open random access file
    OPEN "Phone.dat" FOR RANDOM AS #1 LEN = 107
    FIELD #1, 30 AS io$(1), 20 AS io$(2), 30 AS io$(3), 15 AS io$(4), 2 AS io$(5), 5 AS io$(6)
    FIELD #1, 11 AS ioValid$, 5 AS ioMaxRecord$

    'get first record, if it's a new file, it will initialize it.
    'Record #1 contains the code word "THISISVALID" indicating the file is
    'properly initialized, and the number of records in the file
    GET #1, 1
    IF ioValid$ <> "THISISVALID" THEN
        FOR a = 1 TO 6
            LSET io$(a) = ""
        NEXT a
        PUT #1, 2
        LSET ioValid$ = "THISISVALID"
        RSET ioMaxRecord$ = "1"
        PUT #1, 1
    END IF

    maxRecord = VAL(ioMaxRecord$)

    'Set up initial index
    FOR a = 1 TO maxRecord
        index(a) = a
    NEXT a


    'Initialize editor variables
    currTopLine = 1
    GOSUB printWholeScreen

    currRow = 1
    currCol = 1
    GOSUB GetLine

    finished = false

    'Main editor loop
    DO
        GOSUB ShowCursor
        GOSUB EditItem
        GOSUB HideCursor

        'Respond to user keystroke
        SELECT CASE kbd$
            CASE CHR$(0) + "H"                      'up
                GOSUB MoveUp
            CASE CHR$(0) + "P"                      'down
                GOSUB MoveDown
            CASE CHR$(0) + "K", CHR$(0) + CHR$(15)  'left or backtab
                currCol = (currCol + 4) MOD 6 + 1
            CASE CHR$(0) + "M", CHR$(9)             'right, or tab
                currCol = (currCol) MOD 6 + 1
            CASE CHR$(0) + "G"                      'Home
                currCol = 1
            CASE CHR$(0) + "O"                      'End
                currCol = 6
            CASE CHR$(0) + "I"                      'Page up
                currRow = 1
                currTopLine = currTopLine - 22
                IF currTopLine < 1 THEN
                    currTopLine = 1
                END IF
                GOSUB printWholeScreen
                currRow = 1
                GOSUB GetLine
            CASE CHR$(0) + "Q"                      'Page Down
                currRow = 1
                currTopLine = currTopLine + 22
                IF currTopLine > maxRecord THEN
                    currTopLine = maxRecord
                END IF
                GOSUB printWholeScreen
                currRow = 1
                GOSUB GetLine
            CASE CHR$(0) + "<"                      'F2
                finished = TRUE
            CASE CHR$(0) + "?"                      'F5
                GOSUB sort
            CASE CHR$(0) + "@":                     'F6
                GOSUB PrintPhoneBook
            CASE CHR$(0) + "C"                      'F9
                GOSUB AddRecord
            CASE CHR$(0) + "D"
                GOSUB DeleteRecord                  'F10
            CASE CHR$(13)                           'Enter
            CASE ELSE
                BEEP
        END SELECT
    LOOP UNTIL finished
    CLOSE
    EXIT SUB


MoveUp:
    IF currRow = 1 THEN
        IF currTopLine = 1 THEN
            BEEP
        ELSE
            ScrollDown
            currTopLine = currTopLine - 1
            GOSUB GetLine
            GOSUB PrintLine
        END IF
    ELSE
        currRow = currRow - 1
        GOSUB GetLine
    END IF
    RETURN

MoveDown:
    IF (currRow + currTopLine - 1) >= maxRecord THEN
        BEEP
    ELSE
        IF currRow = 22 THEN
            ScrollDown
            currTopLine = currTopLine + 1
            GOSUB GetLine
            GOSUB PrintLine
        ELSE
            currRow = currRow + 1
            GOSUB GetLine
        END IF
    END IF
    RETURN

PrintPhoneBook:
    CLS
    PRINT "THIS IS A TEMPORARY PRINT ROUTINE"

    FOR a = 1 TO maxRecord STEP 2
        IF a <= maxRecord THEN
            GET #1, index(a) + 1
            FOR b = 1 TO 6
                name1$(b) = RTRIM$(io$(b))
            NEXT b
        ELSE
            FOR b = 1 TO 6
                name1$(b) = ""
            NEXT b
        END IF
        IF a + 1 <= maxRecord THEN
            GET #1, index(a + 1) + 1
            FOR b = 1 TO 6
                name2$(b) = RTRIM$(io$(b))
            NEXT b
        ELSE
            FOR b = 1 TO 6
                name2$(b) = ""
            NEXT b
        END IF
        u$ = "\" + SPACE$(35) + "\\" + SPACE$(38) + "\"
        PRINT USING u$; name1$(1); name2$(1)
        PRINT USING u$; name1$(3); name2$(3)
        PRINT USING u$; name1$(4) + "," + name1$(5) + "  " + name1$(6); name2$(4) + "," + name2$(5) + "  " + name2$(6)
        PRINT USING u$; name1$(2); name2$(2)
        PRINT
        PRINT
        INPUT a$
    NEXT a
    
    GOSUB printWholeScreen
    RETURN

ShowCursor:
    COLOR 0, 7
    LOCATE currRow + 2, col(currCol)
    PRINT LEFT$(edit$(currCol) + SPACE$(vis(currCol)), vis(currCol));
    RETURN

HideCursor:
    COLOR 14, 1
    LOCATE currRow + 2, col(currCol)
    PRINT LEFT$(edit$(currCol) + SPACE$(vis(currCol)), vis(currCol));
    RETURN

EditItem:
    'Wait for a keystroke
    COLOR 0, 7
    DO
        kbd$ = INKEY$
    LOOP UNTIL kbd$ <> ""

    'if a text char, edit the line, else return
    IF kbd$ >= " " AND kbd$ <= "~" THEN
        kbd$ = GetString$(currRow + 2, col(currCol), kbd$, back$, vis(currCol), max(currCol))
        edit$(currCol) = back$
        GOSUB putLine
    END IF
    RETURN


PrintLine:
    COLOR 14, 1
    currRecord = currTopLine + currRow - 1
    LOCATE currRow + 2, 1
    IF currRecord = maxRecord + 1 THEN
        PRINT u3$;
    ELSEIF currRecord > maxRecord THEN
        PRINT SPACE$(80);
    ELSE
        PRINT USING u1$; edit$(1); edit$(2); edit$(3); edit$(4); edit$(5); edit$(6);
    END IF
    RETURN


DeleteRecord:
    IF maxRecord = 1 THEN
        BEEP
    ELSE
        currRecord = currTopLine + currRow - 1      'init currRecord
        maxRecord = maxRecord - 1                   'Decrement maxRecord
        theRecord = index(currRecord)               'Save pointer to currRecord

        'Removing a name leaves a hole.  So...
        'Squeeze actual physical data records on disk
        FOR a = index(currRecord) TO maxRecord
            GET #1, a + 2
            PUT #1, a + 1
        NEXT a

        'Squeeze the index stored in memory.
        FOR a = currRecord TO maxRecord
            index(a) = index(a + 1)
        NEXT a

        'Now that the actuall records were moved on disk, we need to
        'decrement the value of every pointer in the index array
        'that pointed to a name that was moved.
        FOR a = 1 TO maxRecord
            IF index(a) > theRecord THEN
                index(a) = index(a) - 1
            END IF
        NEXT a

        'Update record#1
        LSET ioValid$ = "THISISVALID"
        RSET ioMaxRecord$ = STR$(maxRecord)
        PUT #1, 1

        'Reprint screen, restablish cursor position
        GOSUB printWholeScreen
        currRecord = currTopLine + currRow - 1
        IF currRecord > maxRecord THEN
            GOSUB MoveUp
        END IF
        GOSUB GetLine
    END IF
    RETURN


AddRecord:
    IF maxRecord < 2000 THEN
        currRecord = currTopLine + currRow - 1      'Establish current record#
        maxRecord = maxRecord + 1                   'Increment maxRecord
        FOR a = 1 TO 6                              'Clear IO buffer
            LSET io$(a) = ""
        NEXT a
        PUT #1, maxRecord + 1                       'Insert into last pos in file

        'We just added the new record to the physical file, now we
        'need to make room in the index
        a = maxRecord
        WHILE a > currRecord
            index(a + 1) = index(a)
            a = a - 1
        WEND
    
        'Assign new spot in index to the new record
        index(currRecord + 1) = maxRecord

        'Update first record
        LSET ioValid$ = "THISISVALID"
        RSET ioMaxRecord$ = STR$(maxRecord)
        PUT #1, 1

        GOSUB printWholeScreen
        GOSUB GetLine
    ELSE
        BEEP
    END IF
    RETURN



printWholeScreen:
    temp = currRow
    FOR currRow = 1 TO 22
        currRecord = currTopLine + currRow - 1
        IF currRecord <= maxRecord THEN
            GOSUB GetLine
        END IF
        GOSUB PrintLine
    NEXT currRow
    currRow = temp
    RETURN


putLine:
    currRecord = currTopLine + currRow - 1
    FOR a = 1 TO 6
        LSET io$(a) = edit$(a)
    NEXT a
    PUT #1, index(currRecord) + 1
    RETURN

GetLine:
    currRecord = currTopLine + currRow - 1
    GET #1, index(currRecord) + 1
    FOR a = 1 TO 6
        edit$(a) = io$(a)
    NEXT a
    RETURN


sort:
    'Scan database, collect the strings from the current col in key$()
    FOR a = 1 TO maxRecord
        GET #1, index(a) + 1
        key$(a) = io$(currCol)
    NEXT a

    'do the bubble sort
    DO
        swapFlag = false
        FOR i = 1 TO maxRecord - 1
            IF key$(i) > key$(i + 1) THEN
                SWAP key$(i), key$(i + 1)
                SWAP index(i), index(i + 1)
                swapFlag = TRUE
            END IF
        NEXT i
    LOOP UNTIL NOT swapFlag
        
    'Reprint the screen
    currTopLine = 1
    GOSUB printWholeScreen
    currRow = 1
    GOSUB GetLine
    RETURN

END SUB

SUB ScrollDown

    'Call the asm routine stored in the array
    DEF SEG = VARSEG(ScrollDownAsm(1))
    CALL Absolute(VARPTR(ScrollDownAsm(1)))
    DEF SEG

END SUB

SUB ScrollUp

    'call the asm routnie stored in the array
    DEF SEG = VARSEG(ScrollUpAsm(1))
    CALL Absolute(VARPTR(ScrollUpAsm(1)))
    DEF SEG

END SUB

