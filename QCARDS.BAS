'* QCards - A simple database using a cardfile user interface.
'* Each record in the database is represented by a card. The user
'* can scroll through the cards using normal scrolling keys.
'* Other commands allow the user to edit, add, sort, find, or
'* delete cards.
'*
'* Input:  Keyboard - user commands and entries
'*         File - database records
'*
'* Output: Screen - card display and help
'*         File - database records
'*

' The module-level code begins here.

'*************** Declarations and definitions begin here ********************

DEFINT A-Z     'Resets the default data type from single precision to integer

' Define names similar to keyboard names with their equivalent key codes.
CONST SPACE = 32, ESC = 27, ENTER = 13, TABKEY = 9
CONST DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
CONST HOME = 71, ENDK = 79, PGDN = 81, PGUP = 73
CONST INS = 82, DEL = 83, NULL = 0
CONST CTRLD = 4, CTRLG = 7, CTRLH = 8, CTRLS = 19, CTRLV = 22

' Define English names for color-specification numbers. Add BRIGHT to
' any color to get bright version.
CONST BLACK = 0, BLUE = 1, GREEN = 2, CYAN = 3, RED = 4, MAGENTA = 5
CONST YELLOW = 6, WHITE = 7, BRIGHT = 8

' Assign colors to different kinds of text. By changing the color assigned,
' you can change the color of the QCARDS display. The initial colors are
' chosen because they work for color or black-and-white displays.
CONST BACKGROUND = BLACK, NORMAL = WHITE, HILITE = WHITE + BRIGHT
' Codes for normal and highlight (used in data statements)
CONST CNORMAL = 0, CHILITE = 1

' Screen positions - Initialized for 25 rows. Screen positions can be
' modified for 43-row mode if you have an EGA or VGA adapter.
CONST HELPTOP = 15, HELPBOT = 23, HELPLEFT = 60, HELPWID = 20
CONST CARDSPERSCREEN = 7, LASTROW = 25

' Miscellaneous symbolic constants
CONST FALSE = 0, TRUE = NOT FALSE
CONST CURSORON = 1, CURSOROFF = 0

' File names
CONST TMPFILE$ = "$$$87y$.$5$"       ' Unlikely file name

' Field names
CONST NPERSON = 0, NNOTE = 1, NMONTH = 2, NDAY = 3, NYEAR = 4, NPHONE = 5
CONST NSTREET = 6, NCITY = 7, NSTATE = 8, NZIP = 9, NFIELDS = NZIP + 1

' Declare user-defined type (a data structure) for random-access file records.
TYPE PERSON
    CardNum     AS INTEGER          'First element is card number
    Names       AS STRING * 37      'Names (in order for alphabetical sort)
    Note        AS STRING * 31      'Note about person
    Month       AS INTEGER          'Birth month
    Day         AS INTEGER          'Birth day
    Year        AS INTEGER          'Birth year
    Phone       AS STRING * 12      'Phone number
    Street      AS STRING * 29      'Street address
    City        AS STRING * 13      'City
    State       AS STRING * 2       'State
    Zip         AS STRING * 5       'Zip code
END TYPE

' SUB procedure declarations begin here.

' This space reserved

DECLARE SUB AsciiKey (Choice$, TopCard%, LastCard%)
DECLARE SUB CleanUp (LastCard%)
DECLARE SUB ClearHelp ()
DECLARE SUB DrawCards ()
DECLARE SUB EditCard (Card AS PERSON)
DECLARE SUB InitIndex (LastCard%)
DECLARE SUB PrintLabel (Card AS PERSON)
DECLARE SUB SortIndex (SortField%, LastCard%)
DECLARE SUB ShowViewHelp ()
DECLARE SUB ShowTopCard (WorkCard AS PERSON)
DECLARE SUB ShowEditHelp ()
DECLARE SUB ShowCmdLine ()
DECLARE SUB ShowCards (TopCard%, LastCard%)

' FUNCTION procedure declarations begin here.
DECLARE FUNCTION EditString$ (InString$, Length%, NextField%)
DECLARE FUNCTION FindCard% (TopCard%, LastCard%)
DECLARE FUNCTION Prompt$ (Msg$, Row%, Column%, Length%)
DECLARE FUNCTION SelectField% ()

' Procedure declarations end here.

' Define temporary Index() array to illustrate QCARDS screen.
REDIM SHARED Index(1) AS PERSON

' Define a dummy record as a work card.
DIM Card AS PERSON

'*************** Declarations and definitions end here ********************

' The execution-sequence logic of QCARDS begins here.

' Open data file QCARDS.DAT for random access using file #1



' To count records in file, divide the length of the file by the
' length of a single record; use integer division (\) instead of
' normal division (/). Assign the resulting value to LastCard.



' Redefine the Index array to hold the records in the file plus
' 20 extra (the extra records allow the user to add cards).
' This array is dynamic - this means the number of elements
' in Index() varies depending on the size of the file.
' Also, Index() is a shared procedure, so it is available to
' all SUB and FUNCTION procedures in the program.
'
' Note that an error trap lets QCARDS terminate with an error
' message if the memory available is not sufficient. If no
' error is detected, the error trap is turned off following the
' REDIM statement.



' Use the block IF...THEN...ELSE statement to decide whether
' to load the records from the disk file QCARDS.DAT into the
' array of records called Index() declared earlier. In the IF
' part, you will check to see if there are actually records
' in the file. If there are, LastCard will be greater than 0,
' and you can call the InitIndex procedure to load the records
' into Index(). LastCard is 0 if there are no records in the
' file yet. If there are no records in the file, the ELSE
' clause is executed. The code between ELSE and END IF starts
' the Index() array at card 1.




' Use the DrawCards procedure to initialize the screen
' and draw the cards. Then, set the first card as the top
' card. Finally, pass the variables TopCard and LastCard
' as arguments to the ShowCards procedure. The call to
' ShowCards places all the data for TopCard on the front
' card on the screen, then it places the top-line
' information (the person's name) on the remaining cards.




' Keep the picture on the screen forever with an unconditional
' DO...LOOP statement. The DO part of the statement goes on
' the next code line. The LOOP part goes just before the END
' statement. This loop encloses the central logic that lets
' a user interact with QCARDS.




' Get user keystroke with a conditional DO...LOOP statement.
' Within the loop, use the INKEY$ function to capture a user
' keystroke, which is then assigned to a string variable. The
' WHILE part of the LOOP line keeps testing the string
' variable. Until a key is pressed, INKEY$ keeps returning a
' null (that is a zero-length) string, represented by "".
' When a key is pressed, INKEY$ returns a string with a
' length greater than zero, and the loop terminates.




' Use the LEN function to find out whether Choice$ is greater
' than a single character (i.e. a single byte). If Choice$ is
' a single character (that is, it is less than 2 bytes long),
' the key pressed was an ordinary "typewriter keyboard"
' character (these are usually called ASCII keys because they
' are part of the ASCII character set). When the user enters
' an ASCII character, it indicates a choice of one of the QCARDS
' commands from the command line at the bottom of the screen.
' If the user did press an ASCII key, use the LCASE$ function
' to convert it to lower case (in the event the capital letter
' was entered).
'
' The ELSE clause is only executed if Choice$ is longer than a
' single character (and therefore not a command-line key).
' If Choice$ is not an ASCII key, it represents an "extended"
' key. (The extended keys include the DIRECTION keys on the
' numeric keypad, which is why QCARDS looks for them.) The
' RIGHT$ function is then used trim away the extra byte,
' leaving a value that may correspond to one of the DIRECTION
' keys. Use a SELECT CASE construction to respond to those key-
' presses that represent numeric-keypad DIRECTION keys.



' Adjust the cards according to the key pressed by the user,
' then call the ShowCards procedure to show adjusted stack.



' This is the bottom of the unconditional DO loop.


END

' The execution sequence of the module-level code ends here.
' The program may terminate elsewhere for legitimate reasons,
' but the normal execution sequence ends here. Statements
' beyond the END statement are executed only in response to
' other statements.
                           
' This first label, MemoryErr, is an error handler.

MemoryErr:
    PRINT "Not enough memory. Can't read file."
    END

' Data statements for screen output - initialized for 25 rows. Can be
' modified for 43-row mode if you have an EGA or VGA adapter.

CardScreen:
DATA "                  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
DATA "                  ³                                       ³"
DATA "               ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ"
DATA "               ³                                       ³  ³"
DATA "            ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ  ³"
DATA "            ³                                       ³  ³  ³"
DATA "         ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ  ³  ³"
DATA "         ³                                       ³  ³  ³  ³"
DATA "      ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ  ³  ³  ³"
DATA "      ³                                       ³  ³  ³  ³  ³"
DATA "   ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ  ³  ³  ÃÄÄÙ"
DATA "   ³                                       ³  ³  ³  ³  ³"
DATA "ÚÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÍÍµ  ³  ³  ÃÄÄÙ"
DATA "³ _____________________________________ ³  ³  ³  ³  ³"
DATA "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ  ³  ³  ÃÄÄÙ"
DATA "³ Note: _______________________________ ³  ³  ³  ³"
DATA "³                                       ³  ³  ÃÄÄÙ"
DATA "³ Birth: __/__/__   Phone: ___-___-____ ³  ³  ³"
DATA "³                                       ³  ÃÄÄÙ"
DATA "³ Street: _____________________________ ³  ³"
DATA "³                                       ÃÄÄÙ"
DATA "³ City: ____________ ST: __  Zip: _____ ³"
DATA "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

' Color codes and strings for view-mode help

ViewHelp:
DATA 0,  "Select card with:"
DATA 1, "      UP"
DATA 1, "      DOWN"
DATA 1, "      PGUP"
DATA 1, "      PGDN"
DATA 1, "      HOME"
DATA 1, "      END"
DATA 1, ""
DATA 1, ""

' Color codes and strings for edit-mode help

EditHelp:
DATA 0,  "Next field:"
DATA 1, "      TAB"
DATA 0,  "Accept card:"
DATA 1, "      ENTER"
DATA 0,  "Edit field:"
DATA 1, "      DEL     BKSP"
DATA 1, "      RIGHT   LEFT"
DATA 1, "      HOME    END"
DATA 1, "      INS     ESC"

' Row, column, and length of each field

FieldPositions:
DATA 14, 6, 37                      : ' Names
DATA 16, 12, 31                     : ' Note
DATA 18, 13, 2                      : ' Month
DATA 18, 16, 2                      : ' Day
DATA 18, 19, 2                      : ' Year
DATA 18, 31, 12                     : ' Phone
DATA 20, 14, 29                     : ' Street
DATA 22, 12, 13                     : ' City
DATA 22, 29, 2                      : ' State
DATA 22, 38, 5                      : ' Zip
DATA 0, 0, 0

'*
'* AsciiKey - Handles ASCII keys. You can add new commands by
'* assigning keys and actions here and adding them to the command
'* line displayed by the ShowCmdLine SUB. For example, you could add
'* L (for Load new file) to prompt the user for a new database file.
'*
'* Params: UserChoice$ - key pressed by the user
'*         TopCard - the number of the current record
'*         LastCard - the number of records
'*
SUB AsciiKey (UserChoice$, TopCard%, LastCard%)
DIM WorkCard AS PERSON

    SELECT CASE UserChoice$
        ' Edit the current card.
        CASE "e"
            CALL ShowEditHelp
            Tmp$ = Prompt$("Editing Card...", LASTROW, 1, 0)
            CALL EditCard(Index(TopCard))
            PUT #1, Index(TopCard).CardNum, Index(TopCard)
            LOCATE , , CURSOROFF
            CALL ShowViewHelp

        ' Add and edit a blank or duplicate card.
        CASE "a", "c"
            IF UserChoice$ = "c" THEN
                WorkCard = Index(TopCard)   ' Duplicate of top card
            ELSE
                WorkCard.CardNum = 0        ' Initialize new card.
                WorkCard.Names = ""
                WorkCard.Note = ""
                WorkCard.Month = 0
                WorkCard.Day = 0
                WorkCard.Year = 0
                WorkCard.Phone = ""
                WorkCard.Street = ""
                WorkCard.City = ""
                WorkCard.State = ""
                WorkCard.Zip = ""
            END IF
            TopCard = LastCard + 1
            LastCard = TopCard
            Index(TopCard) = WorkCard
            Index(TopCard).CardNum = TopCard
            CALL ShowCards(TopCard, LastCard)
            CALL ShowEditHelp
            Tmp$ = Prompt$("Editing Card...", LASTROW, 1, 0)
            CALL EditCard(Index(TopCard))
            PUT #1, Index(TopCard).CardNum, Index(TopCard)
            LOCATE , , CURSOROFF
            CALL ShowViewHelp

        ' Move deleted card to end and adjust last card.
        CASE "d"
            FOR Card = TopCard TO LastCard - 1
                SWAP Index(Card + 1), Index(Card)
            NEXT Card
            LastCard = LastCard - 1

        ' Find a specified card.
        CASE "f"
            CALL ShowEditHelp
            Tmp$ = "Enter fields for search (blank fields are ignored)"
            Tmp$ = Prompt$(Tmp$, LASTROW, 1, 0)
            Card = FindCard(TopCard, LastCard)
            IF Card THEN
                TopCard = Card
            ELSE
                BEEP
                CALL ClearHelp
                Tmp$ = "Can't find card. Press any key..."
                Tmp$ = Prompt$(Tmp$, LASTROW, 1, 1)
            END IF
            LOCATE , , CURSOROFF
            CALL ShowViewHelp

        ' Sorts cards by a specified field.
        CASE "s"
            CALL ClearHelp
            Tmp$ = "TAB to desired sort field, then press ENTER"
            Tmp$ = Prompt$(Tmp$, LASTROW, 1, 0)
            CALL SortIndex(SelectField, LastCard)
            TopCard = 1
            CALL ShowViewHelp

        ' Prints address of top card on printer.
        CASE "p"
            CALL PrintLabel(Index(TopCard))

        ' Terminates the program.
        CASE "q", CHR$(ESC)
            CALL CleanUp(LastCard)
            LOCATE , , CURSORON
            CLS
            END
        CASE ELSE
            BEEP
    END SELECT

END SUB

'*
'* CleanUp - Writes all records from memory to a file. Deleted
'* records (past the last card) will not be written. The valid records
'* are written to a temporary file. The old file is deleted, and the
'* new file is given the old name.
'*
'* Params: LastCard - the number of valid records
'*
'* Output: Valid records to "QCARDS.DAT" through TMPFILE$
'*
SUB CleanUp (LastCard)

    ' Write records to temporary file in their current sort order.
    OPEN TMPFILE$ FOR RANDOM AS #2 LEN = LEN(Index(1))
    FOR Card = 1 TO LastCard
        PUT #2, Card, Index(Card)
    NEXT

    ' Delete old file and replace it with new version.
    CLOSE
    KILL "QCARDS.DAT"
    NAME TMPFILE$ AS "QCARDS.DAT"

END SUB

'*
'* ClearHelp - Writes spaces to the help area of the screen.
'*
'* Params: None
'*
'* Output: Blanks to the screen
'*
SUB ClearHelp

    ' Clear key help
    COLOR NORMAL, BACKGROUND
    FOR Row = HELPTOP TO HELPBOT
        LOCATE Row, HELPLEFT
        PRINT SPACE$(HELPWID)
    NEXT

    ' Clear command line
    LOCATE LASTROW, 1
    PRINT SPACE$(80);

END SUB

'*
'* DrawCards - Initializes screen by setting the color, setting the width
'* and height, clearing the screen, and hiding the cursor. Then writes card
'* text and view-mode help to the screen.
'*
'* Params: None
'*
'* Output: Text to the screen
'*
SUB DrawCards

    ' Clear screen to current color.
    WIDTH 80, LASTROW
    COLOR NORMAL, BACKGROUND
    CLS
    LOCATE , , CURSOROFF, 0, 7

    ' Display line characters that form cards.
    RESTORE CardScreen
    FOR Row = 1 TO 23
        LOCATE Row, 4
        READ Tmp$
        PRINT Tmp$;
    NEXT

    ' Display help.
    CALL ShowViewHelp

END SUB

'*
'* EditCard - Edits each field of a specified record.
'*
'* Params: Card - the record to be edited
'*
'* Return: Since Card is passed by reference, the edited version is
'*         effectively returned.
'*
SUB EditCard (Card AS PERSON)

    ' Set NextFlag and continue editing each field.
    ' NextFlag is cleared when the user presses ENTER.

    NextFlag = TRUE
    DO

        RESTORE FieldPositions

        ' Start with first field.
        READ Row, Column, Length
        LOCATE Row, Column
        ' Edit string fields directly.
        Card.Names = EditString(RTRIM$(Card.Names), Length, NextFlag)
        ' Result of edit determines whether to continue.
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.Note = EditString(RTRIM$(Card.Note), Length, NextFlag)
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        ' Convert numeric fields to strings for editing.
        Tmp$ = LTRIM$(STR$(Card.Month))
        Tmp$ = EditString(Tmp$, Length, NextFlag)
        ' Convert result back to number.
        Card.Month = VAL(Tmp$)
        LOCATE Row, Column
        PRINT USING "##_/"; Card.Month;
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Tmp$ = LTRIM$(STR$(Card.Day))
        Tmp$ = EditString(Tmp$, Length, NextFlag)
        Card.Day = VAL(Tmp$)
        LOCATE Row, Column
        PRINT USING "##_/"; Card.Day;
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Tmp$ = LTRIM$(STR$(Card.Year))
        Tmp$ = EditString(Tmp$, Length, NextFlag)
        Card.Year = VAL(Tmp$)
        LOCATE Row, Column
        PRINT USING "##"; Card.Year;
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.Phone = EditString(RTRIM$(Card.Phone), Length, NextFlag)
        RSET Card.Phone = Card.Phone
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.Street = EditString(RTRIM$(Card.Street), Length, NextFlag)
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.City = EditString(RTRIM$(Card.City), Length, NextFlag)
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.State = EditString(RTRIM$(Card.State), Length, NextFlag)
        IF NextFlag = FALSE THEN EXIT SUB

        READ Row, Column, Length
        LOCATE Row, Column
        Card.Zip = EditString(RTRIM$(Card.Zip), Length, NextFlag)
        IF NextFlag = FALSE THEN EXIT SUB

    LOOP

END SUB

'*
'* EditString$ - Edits a specified string. This function
'* implements a subset of editing functions used in the QuickBASIC
'* environment and in Windows. Common editing keys are recognized,
'* including direction keys, DEL, BKSP, INS (for insert and overwrite
'* modes), ESC, and ENTER. TAB is recognized only if the NextField
'* flag is set. CTRL-key equivalents are recognized for most keys.
'* A null string can be specified if no initial value is desired.
'* You could modify this function to handle additional QB edit
'* commands, such as CTRL+A (word back) and CTRL+F (word forward).
'*
'* Params: InString$ - The input string (can be null)
'*         Length - Maximum length of string (the function beeps and
'*           refuses additional keys if the user tries to enter more)
'*         NextField - Flag indicating on entry whether to accept TAB
'*           key; on exit, indicates whether the user pressed TAB (TRUE)
'*           or ENTER (FALSE)
'*
'* Input:  Keyboard
'* Ouput:  Screen - Noncontrol keys are echoed.
'*         Speaker - beep if key is invalid or string is too long
'*
'* Return: The edited string
'*
FUNCTION EditString$ (InString$, Length, NextField)
STATIC Insert

    ' Initialize variables and clear field to its maximum length.
    Work$ = InString$
    Row = CSRLIN: Column = POS(0)
    FirstTime = TRUE
    P = LEN(Work$): MaxP = P
    PRINT SPACE$(Length);

    ' Since Insert is STATIC, its value is maintained from one
    ' call to the next. Insert is 0 (FALSE) the first time the
    ' function is called.
    IF Insert THEN
        LOCATE Row, Column, CURSORON, 6, 7
    ELSE
        LOCATE Row, Column, CURSORON, 0, 7
    END IF

    ' Reverse video on entry.
    COLOR BACKGROUND, NORMAL
    PRINT Work$;

    ' Process keys until either TAB or ENTER is pressed.
    DO

        ' Get a key -- either a one-byte ASCII code or a two-byte
        ' extended code.
        DO
            Choice$ = INKEY$
        LOOP WHILE Choice$ = ""

        ' Translate two-byte extended codes to the one meaningful byte.
        IF LEN(Choice$) = 2 THEN
            Choice$ = RIGHT$(Choice$, 1)
            SELECT CASE Choice$

                ' Translate extended codes to ASCII control codes.
                CASE CHR$(LEFT)
                    Choice$ = CHR$(CTRLS)
                CASE CHR$(RIGHT)
                    Choice$ = CHR$(CTRLD)
                CASE CHR$(INS)
                    Choice$ = CHR$(CTRLV)
                CASE CHR$(DEL)
                    Choice$ = CHR$(CTRLG)

                ' Handle HOME and END keys, since they don't have control
                ' codes. Send NULL as a signal to ignore.
                CASE CHR$(HOME)
                    P = 0
                    Choice$ = CHR$(NULL)
                CASE CHR$(ENDK)
                    P = MaxP
                    Choice$ = CHR$(NULL)

                ' Make other key choices invalid.
                CASE ELSE
                    Choice$ = CHR$(1)
            END SELECT
        END IF

        ' Handle one-byte ASCII codes.
        SELECT CASE ASC(Choice$)

            ' If it is null, ignore it.
            CASE NULL

            ' Accept field (and card if NextField is used).
            CASE ENTER
                NextField = FALSE
                EXIT DO

            ' Accept the field unless NextField is used. If NextField
            ' is cleared, TAB is invalid.
            CASE TABKEY
                IF NextField THEN
                    EXIT DO
                ELSE
                    BEEP
                END IF

            ' Restore the original string.
            CASE ESC
                Work$ = InString$
                LOCATE Row, Column, CURSOROFF
                PRINT SPACE$(MaxP)
                EXIT DO

            ' CTRL+S or LEFT arrow moves cursor to left.
            CASE CTRLS
                IF P > 0 THEN
                    P = P - 1
                    LOCATE , P + Column
                ELSE
                    BEEP
                END IF

            ' CTRL+D or RIGHT arrow moves cursor to right.
            CASE CTRLD
                IF P < MaxP THEN
                    P = P + 1
                    LOCATE , P + Column
                ELSE
                    BEEP
                END IF

            ' CTRL+G or DEL deletes character under cursor.
            CASE CTRLG
                IF P < MaxP THEN
                    Work$ = LEFT$(Work$, P) + RIGHT$(Work$, MaxP - P - 1)
                    MaxP = MaxP - 1
                ELSE
                    BEEP
                END IF

            ' CTRL+H or BKSP deletes character to left of cursor.
            CASE CTRLH, 127
                IF P > 0 THEN
                    Work$ = LEFT$(Work$, P - 1) + RIGHT$(Work$, MaxP - P)
                    P = P - 1
                    MaxP = MaxP - 1
                END IF

            ' CTRL+V or INS toggles between insert and overwrite modes.
            CASE CTRLV
                Insert = NOT Insert
                IF Insert THEN
                    LOCATE , , , 6, 7
                ELSE
                    LOCATE , , , 0, 7
                END IF

            ' Echo ASCII characters to screen.
            CASE IS >= SPACE

                ' Clear the field if this is first keystroke, then
                ' start from the beginning.
                IF FirstTime THEN
                    LOCATE , Column
                    COLOR NORMAL, BACKGROUND
                    PRINT SPACE$(MaxP);
                    LOCATE , Column
                    P = 0: MaxP = P
                    Work$ = ""
                END IF

                ' If insert mode and cursor not beyond end, insert character.
                IF Insert THEN
                    IF MaxP < Length THEN
                        Work$ = LEFT$(Work$, P) + Choice$ + RIGHT$(Work$, MaxP - P)
                        MaxP = MaxP + 1
                        P = P + 1
                    ELSE
                        BEEP
                    END IF

                ELSE
                    ' If overwrite mode and cursor at end (but not beyond),
                    ' insert character.
                    IF P = MaxP THEN
                        IF MaxP < Length THEN
                            Work$ = Work$ + Choice$
                            MaxP = MaxP + 1
                            P = P + 1
                        ELSE
                            BEEP
                        END IF

                    ' If overwrite mode and before end, overwrite character.
                    ELSE
                        MID$(Work$, P + 1, 1) = Choice$
                        P = P + 1
                    END IF
                END IF

            ' Consider other key choices invalid.
            CASE ELSE
                BEEP
        END SELECT
       
        ' Print the modified string.
        COLOR NORMAL, BACKGROUND
        LOCATE , Column, CURSOROFF
        PRINT Work$ + " ";
        LOCATE , Column + P, CURSORON
        FirstTime = FALSE

    LOOP

    ' Print the final string and assign it to function name.
    COLOR NORMAL, BACKGROUND
    LOCATE Row, Column, CURSOROFF
    PRINT Work$;
    EditString$ = Work$
    LOCATE Row, Column

END FUNCTION

'*
'* FindCard - Finds a specified record. The user specifies as many
'* fields to search for as desired. The search begins at the card
'* after the current card and proceeds until the specified record or
'* the current card is reached. Specified records are retained between
'* calls to make repeat searching easier. This SUB could be enhanced
'* to find partial matches of string fields.
'*
'* Params: TopCard - number of top card
'*         LastCard - number of last card
'*
'* Params: None
'*
'* Return: Number (zero-based) of the selected field
'*
FUNCTION FindCard% (TopCard%, LastCard%)

STATIC TmpCard AS PERSON, NotFirst

    ' Initialize string fields to null on the first call. (Note that the
    ' variables TmpCard and NotFirst, declared STATIC above, retain their
    ' values between subsequent calls.)
    IF NotFirst = FALSE THEN
        TmpCard.Names = ""
        TmpCard.Note = ""
        TmpCard.Phone = ""
        TmpCard.Street = ""
        TmpCard.City = ""
        TmpCard.State = ""
        TmpCard.Zip = ""
        NotFirst = TRUE
    END IF

    ' Show top card, then use EditCardFunction to specify fields
    ' for search.
    CALL ShowTopCard(TmpCard)
    CALL EditCard(TmpCard)

    ' Search until a match is found or all cards have been checked.
    Card = TopCard
    DO
        Card = Card + 1
        IF Card > LastCard THEN Card = 1
        Found = 0

        ' Test name to see if it's a match.
        SELECT CASE RTRIM$(UCASE$(TmpCard.Names))
            CASE "", RTRIM$(UCASE$(Index(Card).Names))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                   
        ' Test note text.
        SELECT CASE RTRIM$(UCASE$(TmpCard.Note))
            CASE "", RTRIM$(UCASE$(Index(Card).Note))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                   
        ' Test month.
        SELECT CASE TmpCard.Month
            CASE 0, Index(Card).Month
                Found = Found + 1
            CASE ELSE
        END SELECT
                                 
        ' Test day.
        SELECT CASE TmpCard.Day
            CASE 0, Index(Card).Day
                Found = Found + 1
            CASE ELSE
        END SELECT
                                 
        ' Test year.
        SELECT CASE TmpCard.Year
            CASE 0, Index(Card).Year
                Found = Found + 1
            CASE ELSE
        END SELECT
                                 
        ' Test phone number.
        SELECT CASE RTRIM$(UCASE$(TmpCard.Phone))
            CASE "", RTRIM$(UCASE$(Index(Card).Phone))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                 
        ' Test street address.
        SELECT CASE RTRIM$(UCASE$(TmpCard.Street))
            CASE "", RTRIM$(UCASE$(Index(Card).Street))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                  
        ' Test city.
        SELECT CASE RTRIM$(UCASE$(TmpCard.City))
            CASE "", RTRIM$(UCASE$(Index(Card).City))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                  
        ' Test state.
        SELECT CASE RTRIM$(UCASE$(TmpCard.State))
            CASE "", RTRIM$(UCASE$(Index(Card).State))
                Found = Found + 1
            CASE ELSE
        END SELECT
                                  
        ' Test zip code.
        SELECT CASE TmpCard.Zip
            CASE "", RTRIM$(UCASE$(Index(Card).Zip))
                Found = Found + 1
            CASE ELSE
        END SELECT

        ' If match is found, set function value and quit, else next card.
        IF Found = NFIELDS - 1 THEN
            FindCard% = Card
            EXIT FUNCTION
        END IF

    LOOP UNTIL Card = TopCard

    ' Return FALSE when no match is found.
    FindCard% = FALSE

END FUNCTION

'*
'* InitIndex - Reads records from file and assigns each value to
'* array records. Index values are set to the actual order of the
'* records in the file. The order of records in the array may change
'* because of sorting or additions, but the CardNum field always
'* has the position in which the record actually occurs in the file.
'*
'* Params: LastCard - number of records in array
'*
'* Input:  File "QCARDS.DAT"
'*
SUB InitIndex (LastCard) STATIC
DIM Card AS PERSON

    FOR Record = 1 TO LastCard
    
        ' Read a record from the file and put each field in the array.
        GET #1, Record, Card
        Index(Record).CardNum = Record
        Index(Record).Names = Card.Names
        Index(Record).Note = Card.Note
        Index(Record).Month = Card.Month
        Index(Record).Day = Card.Day
        Index(Record).Year = Card.Year
        Index(Record).Phone = Card.Phone
        Index(Record).Street = Card.Street
        Index(Record).City = Card.City
        Index(Record).State = Card.State
        Index(Record).Zip = Card.Zip

    NEXT Record

END SUB

'*
'* PrintLabel - Prints the name, address, city, state, and zip code from
'* a card. This SUB could easily be modified to print a return address
'* or center the address on an envelope.
'*
'* Params: Card - all the data about a person
'*
'* Output: Printer
'*
SUB PrintLabel (Card AS PERSON)

    LPRINT Card.Names
    LPRINT Card.Street
    LPRINT Card.City; ", "; Card.State; Card.Zip
    LPRINT

END SUB

'*
'* Prompt$ - Prints a prompt at a specified location on the screen and
'* (optionally) gets a user response. This function can take one of three
'* different actions depending on the length parameter.
'*
'* Params: Msg$ - message or prompt (can be "" for no message)
'*         Row
'*         Column
'*         Length - one of the following:
'*           <1 - Don't wait for response
'*            1 - Get character response
'*           >1 - Get string response up to length
'*
'* Output: Keyboard
'* Output: Screen - noncontrol characters echoed
'*
'* Return: String entered by user
'*
FUNCTION Prompt$ (Msg$, Row, Column, Length)

    LOCATE Row, Column
    PRINT Msg$;
    
    SELECT CASE Length
        CASE IS <= 0    ' No return
            Prompt$ = ""
        CASE 1          ' Character return
            LOCATE , , CURSORON
            Prompt$ = INPUT$(1)
        CASE ELSE       ' String return
            Prompt$ = EditString("", Length, FALSE)
    END SELECT

END FUNCTION

'*
'* SelectField - Enables a user to select a field using TAB key.
'* TAB moves to the next field. ENTER selects the current field.
'*
'* Params: None
'*
'* Return: Number (zero-based) of the selected field
'*
FUNCTION SelectField%

    ' Get first cursor position and set first FieldNum.
    RESTORE FieldPositions
    READ Row, Column, Length
    FieldNum = 0

    ' Rotate cursor through fields.
    DO

        ' Set cursor on current field.
        LOCATE Row, Column, CURSORON

        ' Get a TAB or ENTER.
        DO
            Ky = ASC(INPUT$(1))
        LOOP UNTIL (Ky = ENTER) OR (Ky = TABKEY)

        ' If ENTER pressed, turn off cursor and return field.
        IF Ky = ENTER THEN
            
            LOCATE , , CURSOROFF
            SelectField% = FieldNum
            EXIT FUNCTION

        ' Otherwise, it was TAB, so advance to next field.
        ELSE

            FieldNum = FieldNum + 1
            READ Row, Column, Length
            IF Row = 0 THEN
                RESTORE FieldPositions
                READ Row, Column, Length
                FieldNum = 0
            END IF

        END IF

    LOOP

END FUNCTION

'*
'* ShowCards - Shows all the fields of the top card and the top
'* field of the other visible cards.
'*
'* Params: TopCard - number of top card
'*         LastCard - number of last card
'*
'* Output: Screen
'*
SUB ShowCards (TopCard, LastCard)

    ' Show each field of top card.
    CALL ShowTopCard(Index(TopCard))

    ' Show the Names field for other visible cards.
    Card = TopCard
    RESTORE FieldPositions
    READ Row, Column, Length
    FOR Count = 2 TO CARDSPERSCREEN

        ' Show location and card number for next highest card.
        Row = Row - 2: Column = Column + 3
        Card = Card + 1
        IF Card > LastCard THEN Card = 1

        LOCATE Row, Column
        PRINT SPACE$(Length)

        LOCATE Row, Column
        PRINT Index(Card).Names

    NEXT Count

END SUB

'*
'* ShowCmdLine - Puts command line on screen with highlighted key
'* characters. Modify this SUB if you add additional commands.
'*
'* Params: None
'*
'* Output: Screen
'*
SUB ShowCmdLine

    LOCATE LASTROW, 1
    COLOR HILITE, BACKGROUND: PRINT " E";
    COLOR NORMAL, BACKGROUND: PRINT "dit Top   ";
    COLOR HILITE, BACKGROUND: PRINT "A";
    COLOR NORMAL, BACKGROUND: PRINT "dd New   ";
    COLOR HILITE, BACKGROUND: PRINT "C";
    COLOR NORMAL, BACKGROUND: PRINT "opy to New   ";
    COLOR HILITE, BACKGROUND: PRINT "D";
    COLOR NORMAL, BACKGROUND: PRINT "elete   ";
    COLOR HILITE, BACKGROUND: PRINT "F";
    COLOR NORMAL, BACKGROUND: PRINT "ind   ";
    COLOR HILITE, BACKGROUND: PRINT "S";
    COLOR NORMAL, BACKGROUND: PRINT "ort   ";
    COLOR HILITE, BACKGROUND: PRINT "P";
    COLOR NORMAL, BACKGROUND: PRINT "rint   ";
    COLOR HILITE, BACKGROUND: PRINT "Q";
    COLOR NORMAL, BACKGROUND: PRINT "uit ";

END SUB

'*
'* ShowEditHelp - Reads colors and strings for edit-mode help and
'* puts them on screen.
'*
'* Params: None
'*
'* Output: Screen
'*
SUB ShowEditHelp

    ' Clear old help and display new.
    ClearHelp
    RESTORE EditHelp
    FOR Row = HELPTOP TO HELPBOT
        READ Clr
        IF Clr = CNORMAL THEN
            COLOR NORMAL, BACKGROUND
        ELSE
            COLOR HILITE, BACKGROUND
        END IF
        LOCATE Row, HELPLEFT
        READ Tmp$
        PRINT Tmp$;
    NEXT

    ' Restore normal color.
    COLOR NORMAL, BACKGROUND

END SUB

'*
'* ShowTopCard - Shows all the fields of the top card.
'*
'* Params: WorkCard - record to be displayed as top card
'*
'* Output: Screen
'*
SUB ShowTopCard (WorkCard AS PERSON)

    ' Display each field of current card.
    RESTORE FieldPositions
    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.Names;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.Note;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT USING "##_/"; WorkCard.Month; WorkCard.Day;
    PRINT USING "##"; WorkCard.Year;
    READ Row, Column, Length, Row, Column, Length

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.Phone;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.Street;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.City;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length);
    LOCATE Row, Column
    PRINT WorkCard.State;

    READ Row, Column, Length
    LOCATE Row, Column
    PRINT SPACE$(Length)
    LOCATE Row, Column
    PRINT WorkCard.Zip;

END SUB

'*
'* ShowViewHelp - Reads colors and strings for view-mode help and
'* puts them on screen.
'*
'* Params: None
'*
'* Output: Screen
'*
SUB ShowViewHelp

    ' Clear old help and display new.
    ClearHelp
    RESTORE ViewHelp
    FOR Row = HELPTOP TO HELPBOT
        READ Clr
        IF Clr = CNORMAL THEN
            COLOR NORMAL, BACKGROUND
        ELSE
            COLOR HILITE, BACKGROUND
        END IF
        LOCATE Row, HELPLEFT
        READ Tmp$
        PRINT Tmp$;
    NEXT

    ' Restore color and show command line.
    COLOR NORMAL, BACKGROUND
    ShowCmdLine

END SUB

'*
'* SortIndex - Sorts all records in memory according to a specified
'* field. After the sort, the first record in memory becomes the top
'* card. Note that although the order is changed in memory, the order
'* remains the same in the file. The true file order is shown by the
'* CardNum field of each record. This SUB uses the Shell sort algorithm.
'*
'* Params: SortField - 0-based number of the field to sort on
'*         LastCard - number of last card
'*
SUB SortIndex (SortField, LastCard)

    ' Set comparison offset to half the number of records.
    Offset = LastCard \ 2

    ' Loop until offset gets to zero.
    DO WHILE Offset > 0

        Limit = LastCard - Offset

        DO

            ' Assume no switches at this offset.
            Switch = FALSE

            ' Compare elements for the specified field and switch
            ' any that are out of order.
            FOR i = 1 TO Limit
                SELECT CASE SortField
                    CASE NPERSON
                        IF Index(i).Names > Index(i + Offset).Names THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NNOTE
                        IF Index(i).Note > Index(i + Offset).Note THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NMONTH
                        IF Index(i).Month > Index(i + Offset).Month THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NDAY
                        IF Index(i).Day > Index(i + Offset).Day THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NYEAR
                        IF Index(i).Year > Index(i + Offset).Year THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NPHONE
                        IF Index(i).Phone > Index(i + Offset).Phone THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NSTREET
                        IF Index(i).Street > Index(i + Offset).Street THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NCITY
                        IF Index(i).City > Index(i + Offset).City THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NSTATE
                        IF Index(i).State > Index(i + Offset).State THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                    CASE NZIP
                        IF Index(i).Zip > Index(i + Offset).Zip THEN
                            SWAP Index(i), Index(i + Offset)
                            Switch = i
                        END IF
                END SELECT

            NEXT i

            ' Sort on next pass only to location where last switch was made.
            Limit = Switch

        LOOP WHILE Switch

        ' No switches at last offset. Try an offset half as big.
        Offset = Offset \ 2
    LOOP

END SUB

