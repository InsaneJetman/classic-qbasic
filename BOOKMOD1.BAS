'***********************************************************************
'*      This is module level code for BOOKMOD2.BAS, and contains screen*
'*      drawing and user interface maintenance routines. This module   *
'*      doesn't contain ISAM statements.                               *
'***********************************************************************

DEFINT A-Z
'$INCLUDE: 'booklook.bi'
KeysBox:
  DATA "ษออออออออออออออออออออออออออออออออออออออป"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "บ                                      บ"
  DATA "ศอต Keys for Database Viewing/Editing ฦผ"

HelpKeys1:
  DATA ""
  DATA "N = Next Record      P = Previous   "
  DATA "R = Reorder Records  F = Find Record"
  DATA "W = When Due Back    B = Borrower   "
  DATA "      V = View Other Table          "
  DATA "A = Add Record       D = Drop Record"
  DATA "E = Edit Record      Q = Quit       "
  DATA "O = Check Book Out   I = Check In   "
  DATA ""

HelpKeys2:
  DATA ""
  DATA "N = Next Record      P = Previous   "
  DATA "R = Reorder Records  F = Find Record"
  DATA "      B = Books Outstanding         "
  DATA "      V = View Other Table          "
  DATA "A = Add Record       D = Drop Record"
  DATA "E = Edit Record      Q = Quit       "
  DATA "                                    "
  DATA ""

Indexbox1:
  DATA "ษอออออออออออออออออออออออออออป"
  DATA "บ By Titles                 บ"
  DATA "บ By Authors                บ"
  DATA "บ By Publishers             บ"
  DATA "บ By ID numbers             บ"
  DATA "บ By Title + Author + ID    บ"
  DATA "บ Default = Insertion order บ"
  DATA "บ                           บ"
  DATA "ศอต Current Sorting Order ฦอผ"
Indexbox2:
  DATA "ษอออออออออออออออออออออออออออป"
  DATA "บ By Name                   บ"
  DATA "บ By State                  บ"
  DATA "บ By Zip code               บ"
  DATA "บ By Card number            บ"
  DATA "บ                           บ"
  DATA "บ Default = Insertion order บ"
  DATA "บ                           บ"
  DATA "ศอต Current Sorting Order ฦอผ"


BooksTable:
DATA "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
DATA "บ                                                                    บ"
DATA "บ    Title:                                                          บ"
DATA "บ                                                                    บ"
DATA "บ    Author:                                                         บ"
DATA "บ                                                                    บ"
DATA "บ    Publisher:                                                      บ"
DATA "บ                                                                    บ"
DATA "บ    Edition:                                                        บ"
DATA "บ                                                                    บ"
DATA "บ    Price:                                                          บ"
DATA "บ                                                                    บ"
DATA "บ    ID number:                                                      บ"
DATA "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"


LendeesTable:
DATA "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
DATA "บ                                                                    บ"
DATA "บ   Name:                                                            บ"
DATA "บ                                                                    บ"
DATA "บ   Street:                                                          บ"
DATA "บ                                                                    บ"
DATA "บ   City:                                                            บ"
DATA "บ                                                                    บ"
DATA "บ   State:                                                           บ"
DATA "บ                                                                    บ"
DATA "บ   Zipcode:                                                         บ"
DATA "บ                                                                    บ"
DATA "บ   Card number:                                                     บ"
DATA "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"

OperandBox:
DATA "ษอออออออออออออออออออออออออออป"
DATA "บ                           บ"
DATA "บ Greater Than              บ"
DATA "บ or                        บ"
DATA "บ Equal To     Value Enteredบ"
DATA "บ or                        บ"
DATA "บ Less Than                 บ"
DATA "บ                           บ"
DATA "ศออต Relationship to Key ฦออผ"

EditMessage:
DATA "ษอออออออออออออออออออออออออออป"
DATA "บ A log is being kept while บ"
DATA "บ you edit fields in this   บ"
DATA "บ record. Press U to undo   บ"
DATA "บ each preceding edit, or   บ"
DATA "บ CTRL+U to undo all of the บ"
DATA "บ pending edits as a group. บ"
DATA "บ                           บ"
DATA "ศอออออต To Undo Edits ฦอออออผ"

'***************************************************************************
'*  The ClearEm SUB erases the parts of the screen where table record col- *
'*  umn information is displayed, depending on which fields are specified. *
'*                                Parameters                               *
'*  TableNum    Integer specifying the table being displayed               *
'*  Field?      Boolean values specifying which fields to erase            *
'***************************************************************************
SUB ClearEm (TableNum%, Field1%, Field2%, Field3%, Field4%, Field5%, Field6%)

  DIM ToClear(10) AS INTEGER

  ToClear(0) = Field1: ToClear(1) = Field2: ToClear(2) = Field3
  ToClear(3) = Field4: ToClear(4) = Field5: ToClear(5) = Field6
  
  COLOR FOREGROUND, BACKGROUND

      FOR Index = 0 TO 5
        IF ToClear(Index) THEN
          SELECT CASE Index
            CASE 0
              LOCATE TITLEFIELD, 18
              PRINT "                                                    "
            CASE 1
              LOCATE AUTHORFIELD, 18
              PRINT "                                                    "
            CASE 2
              LOCATE PUBFIELD, 18
              PRINT "                                                    "
            CASE 3
              LOCATE EDFIELD, 18
              PRINT "                                                    "
            CASE 4
              IF TableNum% = cCardHoldersTableNum THEN
                LOCATE PRICEFIELD, 18
                PRINT "                                                    "
              ELSE
                LOCATE PRICEFIELD, 19
                PRINT "                                                   "
              END IF
            CASE 5
              LOCATE IDFIELD, 18
              PRINT "                                                    "
          END SELECT
        END IF
      NEXT Index
END SUB

'**************************************************************************
'*  The ConfirmEntry FUNCTION echoes the user's input and processes his   *
'*  response to make sure the proper action is taken.                     *
'*                                 Parameters                             *
'*  Letter$   Contains the input that the user has just entered.          *
'**************************************************************************
FUNCTION ConfirmEntry% (Letter$)
  Alert$ = "Press ENTER to confirm choice, type value, or TAB to move on"
  CALL ShowMessage(Alert$, 1)
  DO
  Answer$ = INKEY$
  LOOP WHILE Answer$ = EMPTYSTRING
  Reply% = ASC(Answer$)

  SELECT CASE Reply%
    CASE ENTER
      ConfirmEntry% = -1
      Letter$ = ""
    CASE TABKEY
      ConfirmEntry% = 0
      Letter$ = Answer$
    CASE ASC(" ") TO ASC("~")
      Letter$ = Answer$
      ConfirmEntry = -1
    CASE ELSE
      ConfirmEntry% = 0
      Letter$ = "eScApE"
      CALL ShowMessage("Invalid key --- Try again", 0)
   END SELECT
END FUNCTION

'***************************************************************************
'*    The DrawHelpBoox SUB draws the menu box that links a key to a task.  *
'***************************************************************************
SUB DrawHelpBox
  COLOR FOREGROUND, BACKGROUND
  RESTORE KeysBox
    FOR Row = BOXTOP TO BOXEND
      LOCATE Row, 1
      READ Temp$
      PRINT Temp$
      IF Row = BOXEND THEN
        COLOR BACKGROUND, FOREGROUND + BRIGHT
        LOCATE Row, HELPCOL + 3
        PRINT " Keys for Database Viewing/Editing "
        COLOR FOREGROUND, BACKGROUND
      END IF
    NEXT Row
  COLOR FOREGROUND, BACKGROUND
END SUB

'***************************************************************************
'*    The DrawHelpKeys SUB refills the menu box that links a key to a task.*
'*                                Parameters                               *
'*    TableNum    Integer identifying the table being displayed            *
'***************************************************************************
SUB DrawHelpKeys (TableNum AS INTEGER)

COLOR FOREGROUND, BACKGROUND
IF TableNum = cBookStockTableNum THEN RESTORE HelpKeys1 ELSE RESTORE HelpKeys2
FOR Row = BOXTOP TO BOXEND
  LOCATE Row, HELPCOL + 2
  READ Temp$
  PRINT Temp$
  IF Row = BOXEND THEN
    COLOR BACKGROUND, FOREGROUND + BRIGHT
    LOCATE Row, HELPCOL + 3
    PRINT " Keys for Database Viewing/Editing "
    COLOR FOREGROUND, BACKGROUND
    END IF
NEXT Row
COLOR FOREGROUND, BACKGROUND

END SUB

'***************************************************************************
'*  The DrawIndexBox procedure draws the appropriate index box, depending  *
'*  the table being displayed. If the task is EDITRECORD, the index box    *
'*  information is replaced with information about Undo and Undo All       *
'*                               Parameters                                *
'*  TableNum    Integer identifying the table being displayed              *
'*  Task        Integer identifying the task the user is involved in       *
'***************************************************************************
SUB DrawIndexBox (TableNum AS INTEGER, Task%)

COLOR FOREGROUND, BACKGROUND

IF Task = EDITRECORD THEN
  RESTORE EditMessage
ELSE
  IF TableNum = 1 THEN RESTORE Indexbox1 ELSE RESTORE Indexbox2
END IF

FOR Row = BOXTOP TO BOXEND
  LOCATE Row, 42
  READ Temp$
  PRINT Temp$
  IF Row = BOXEND THEN
    IF Task = EDITRECORD THEN
      COLOR FOREGROUND + BRIGHT, BACKGROUND
      LOCATE 19, INDBOX + 16
      PRINT "U"
      LOCATE 21, INDBOX + 2
      PRINT "CTRL+U"
      LOCATE Row, INDBOX + 7
      PRINT " To Undo Edits "
      COLOR FOREGROUND, BACKGROUND
    ELSE
      COLOR BACKGROUND, FOREGROUND + BRIGHT
      LOCATE Row, INDBOX + 3
      PRINT " Current Sorting Order "
      COLOR FOREGROUND, BACKGROUND
    END IF
  END IF
NEXT Row
COLOR FOREGROUND, BACKGROUND

END SUB

'***************************************************************************
'*  The DrawScreen SUB calls other procedures to draw the appropriate parts*
'*  of the screen for the table to be displayed.                           *
'*                                Parameters                               *
'*  TableNum    Integer telling which table is to be shown                 *
'***************************************************************************
SUB DrawScreen (TableNum AS INTEGER)
  CALL DrawTable(TableNum)
  CALL DrawHelpBox
  CALL DrawHelpKeys(TableNum)
  CALL DrawIndexBox(TableNum, Task)
  CALL ShowMessage("", 0)
  COLOR FOREGROUND, BACKGROUND
END SUB

'***************************************************************************
'*  The DrawTable SUB draws and lables the table being displayed.          *
'*                                Parameters                               *
'*  TableNum    The number of the table currently being displayed          *
'***************************************************************************
SUB DrawTable (TableNum AS INTEGER)
CALL ClearEm(TableNum, 1, 1, 1, 1, 1, 1)
VIEW PRINT
COLOR FOREGROUND, BACKGROUND
SELECT CASE TableNum
  CASE 1
    TableName$ = " Book Stock Table "
  CASE 2
    TableName$ = " Card Holders Table "
END SELECT

HowLong = LEN(TableName$)
NameSpace$ = "ต" + STRING$(HowLong, 32) + "ฦ"
PlaceName = (72 \ 2) - (HowLong \ 2)

IF TableNum = 1 THEN RESTORE BooksTable ELSE RESTORE LendeesTable

COLOR FOREGROUND, BACKGROUND

FOR Row = TABLETOP TO TABLEEND
  LOCATE Row, 1
  READ Temp$
  PRINT Temp$
  IF Row = TABLETOP THEN
    LOCATE TABLETOP, PlaceName
    PRINT NameSpace$
    COLOR BACKGROUND, BRIGHT + FOREGROUND
    LOCATE 1, PlaceName + 1
    PRINT TableName$
    COLOR FOREGROUND, BACKGROUND
  END IF
NEXT Row
COLOR FOREGROUND, BACKGROUND

END SUB

'***************************************************************************
'*  The EraseMessage SUB erases the message in the message box between the *
'*  displayed table and the menus at the bottom of the screen. It replaces *
'*  the corners of the table and menus that may have been overwritten      *
'***************************************************************************
SUB EraseMessage
  COLOR FOREGROUND, BACKGROUND
       LOCATE MESBOXTOP, 1
       PRINT "ศ"; STRING$(68, CHR$(205)); "ผ"
       LOCATE MESFIELD, 1
       PRINT SPACE$(70)
       LOCATE MESBOXEND, 1
       PRINT "ษ"; STRING$(38, CHR$(205)); "ป ษ"; STRING$(27, CHR$(205)); "ป"

END SUB

'**************************** MakeString FUNCTION **************************
'*                                                                         *
'* The MakeString FUNCTION provides a minimal editor to operate in the     *
'* BOOKLOOK message box. A prompt is shown. The user can enter numbers,    *
'* letters, punctuation, the ENTER, BACKSPACE and ESC keys.                *
'*                                                                         *
'*                            Parameters:                                  *
'*   FilterTrap   Brings in a keystroke or letter by ASCII value           *
'*   ThisString   Prompt passed in depends on calling function             *
'*                                                                         *
'***************************************************************************
FUNCTION MakeString$ (FilterTrap AS INTEGER, ThisString$)

MessageLen = LEN(ThisString$)                   ' Save length of the prompt
IF FilterTrap THEN                              ' then, if a letter was
  ThisString$ = ThisString$ + CHR$(FilterTrap)  ' passed in, add it to the
  NewString$ = CHR$(FilterTrap)                 ' prompt and use it to start
END IF                                          ' string to be returned.
CALL ShowMessage(ThisString$, 1)                ' Show the string and turn
DO                                              ' on cursor at end.
  DO
  Answer$ = INKEY$
  LOOP WHILE Answer$ = EMPTYSTRING
      SELECT CASE Answer$
        CASE CHR$(ESCAPE)
          FilterTrap = ESCAPE
          CALL ShowMessage(KEYSMESSAGE, 0)
          EXIT FUNCTION
        CASE " " TO "~"
          NewString$ = NewString$ + Answer$
          ThisString$ = ThisString$ + Answer$
          CALL ShowMessage(ThisString$, 1)
        CASE CHR$(BACKSPACE)
          ShortLen = LEN(ThisString$) - 1
          ThisString$ = MID$(ThisString$, 1, ShortLen)
          NewString$ = MID$(ThisString$, MessageLen + 1)
          CALL ShowMessage(ThisString$, 1)
        CASE CHR$(ENTER)
          LOCATE , , 0
          MakeString$ = LTRIM$(RTRIM$(NewString$))
          EXIT FUNCTION
        CASE ELSE
          BEEP
          CALL ShowMessage("Not a valid key --- press Space bar", 0)
      END SELECT
LOOP
END FUNCTION

'***************************************************************************
'*  The ReturnKey$ FUNCTION gets a key from the user and returns its value *
'***************************************************************************
FUNCTION ReturnKey$
  DO
    Answer$ = INKEY$
  LOOP WHILE Answer$ = EMPTYSTRING
  ReturnKey$ = Answer$
END FUNCTION

'******************************** ShowIt SUB ******************************
'*                                                                        *
'*    After the user enters a value to search for in a specific index,    *
'*    this SUB places the value in the proper element of the temporary    *
'*    record variable, then displays the value in the field. Finally,     *
'*    the user is prompted to choose the relationship the indexed value   *
'*    should have to the key that has been entered.                       *
'*                            Parameters:                                 *
'*    TabesRec:       A temporary recordvariable - same as BigRec         *
'*    WhichIndex:     Tells name of Index on which key should be sought   *
'*    WhichTable:     The number of the table currently being displayed   *
'*    StringTo Show:  Value user wants to search for in index             *
'*                                                                        *
'**************************************************************************
SUB ShowIt (TablesRec AS RecStruct, WhichIndex$, WhichTable%, StringToShow$)
  TablesRec.TableNum = WhichTable
  TablesRec.WhichIndex = WhichIndex$
  COLOR BRIGHT + FOREGROUND, BACKGROUND
      SELECT CASE WhichIndex$
        CASE "TitleIndexBS"
          TablesRec.Inventory.Title = StringToShow$
        CASE "AuthorIndexBS"
          TablesRec.Inventory.Author = StringToShow$
        CASE "PubIndexBS"
          TablesRec.Inventory.Publisher = StringToShow$
        CASE "IDIndex"
          TablesRec.Inventory.IDnum = VAL(StringToShow$)
        CASE "NameIndexCH"
          TablesRec.Lendee.TheName = StringToShow$
        CASE "StateIndexCH"
          TablesRec.Lendee.State = StringToShow$
        CASE "ZipIndexCH"
          TablesRec.Lendee.Zip = VAL(StringToShow$)
        CASE "CardNumIndexCH"
          TablesRec.Lendee.CardNum = VAL(StringToShow$)
      END SELECT
    CALL ShowRecord(TablesRec)
  COLOR FOREGROUND, BACKGROUND
END SUB

'***************************************************************************
'*  The ShowKeys SUB presents the key the user should press for a desired  *
'*  operation associated with a description of the task.                   *
'*                               Parameters                                *
'*  TablesRec   RecStruct type variable containing table information       *
'*  ForeGrnd    Integer indicating whether key is highlighted or not       *
'*  TableDone   1 for No Next Record, 0 otherwise (usually DimN)           *
'*  TableStart  1 for No Previous Record, 0 otherwise (usually DimP)       *
'***************************************************************************
SUB ShowKeys (TablesRec AS RecStruct, ForeGrnd%, TableDone%, TableStart%)
  COLOR ForeGrnd, BACKGROUND                    'foreground bright
  LOCATE NLINE, 3
  PRINT "N"
  LOCATE NLINE, 24
  PRINT "P"
  LOCATE RLINE, 3
  PRINT "R"
  LOCATE RLINE, 24
  PRINT "F"
  IF TablesRec.TableNum = cBookStockTableNum THEN
    LOCATE WLINE, 3
    PRINT "W"
    LOCATE WLINE, 24
    PRINT "B"
  ELSE
    LOCATE WLINE, 9
    PRINT "B"
  END IF
  LOCATE VLINE, 9
  PRINT "V"
  LOCATE ALINE, 3
  PRINT "A"
  LOCATE ALINE, 24
  PRINT "D"
  LOCATE ELINE, 3
  PRINT "E"
  LOCATE ELINE, 24
  PRINT "Q"
  IF TablesRec.TableNum = cBookStockTableNum THEN
    LOCATE CLINE, 3
    PRINT "O"
    LOCATE CLINE, 24
    PRINT "I"
  END IF
  IF TableDone = TRUE THEN
  
    LOCATE NLINE, 3
    PRINT " No Next Record"
  ELSE
    LOCATE NLINE, 3
    PRINT "N "
    COLOR FOREGROUND, BACKGROUND
    LOCATE NLINE, 5
    PRINT "= "
    LOCATE NLINE, 6
    PRINT " Next Record"
  END IF
  IF TableStart = TRUE THEN
    COLOR ForeGrnd, BACKGROUND
    LOCATE NLINE, 20
    PRINT " No Previous Record"
  ELSE
    COLOR ForeGrnd, BACKGROUND
    LOCATE NLINE, 20
    PRINT "    P "
    COLOR FOREGROUND, BACKGROUND
    LOCATE NLINE, 26
    PRINT "= "
    LOCATE NLINE, 27
    PRINT " Previous   "
    END IF
  COLOR FOREGROUND, BACKGROUND
END SUB

'**************************************************************************
'*  The ShowMessage SUB displays the message string passed in the message *
'*  box between the displayed table and the menus. If the Cursor parameter*
'*  is 0, no cursor appears in the box; if it is 1, a cursor is displaed. *
'*                                 Parameters                             *
'*  Message$    Prompt or message to display                              *
'*  Cursor      Boolean value telling whether or not to show a cursor     *
'**************************************************************************
SUB ShowMessage (Message$, Cursor)
  CALL EraseMessage
  IF (LEN(Message$) MOD 2) THEN
        Borderlen = 1
  END IF
  MesLen = LEN(Message$)
  SELECT CASE Cursor                          ' No cursor request means to
  CASE FALSE                                  ' center the message in box
    HalfMes = (MesLen \ 2) + 1                ' and display without cursor
    Start = (SCREENWIDTH \ 2) - HalfMes
  CASE ELSE
    Start = 4                                 ' Message is part of an edit
  END SELECT                                  ' so display flush left, and
    LOCATE MESBOXTOP, 2                       ' keep cursor visible
    PRINT "ษ"; STRING$(66, CHR$(205)); "ป"
    LOCATE MESFIELD, 2
    PRINT "บ"; SPACE$(66); "บ"
    LOCATE MESBOXEND, 2
    PRINT "ศ"; STRING$(37, CHR$(205)); "ห"; "อห"; STRING$(26, CHR$(205)); "ผ"
    COLOR BRIGHT + FOREGROUND, BACKGROUND
    LOCATE MESFIELD, Start, Cursor
    PRINT Message$;
    LOCATE MESFIELD, Start + MesLen, Cursor
    PRINT "";
    COLOR FOREGROUND, BACKGROUND
END SUB

'**************************************************************************
'*  The ShowRecord SUB displays the columns of the current record of the  *
'*  table being displayed. Numerics are only displayed if they are <> 0.  *
'*                                Parameters                              *
'*  TablesRec   RecStruct type variable containing table information      *
'**************************************************************************
SUB ShowRecord (TablesRec AS RecStruct)
COLOR FOREGROUND, BACKGROUND
  SELECT CASE TablesRec.TableNum
    CASE cBookStockTableNum
      LOCATE TITLEFIELD, 18: PRINT TablesRec.Inventory.Title
      LOCATE AUTHORFIELD, 18: PRINT TablesRec.Inventory.Author
      LOCATE PUBFIELD, 18: PRINT TablesRec.Inventory.Publisher
      IF TablesRec.Inventory.Edition <> 0 THEN LOCATE EDFIELD, 17: PRINT STR$(TablesRec.Inventory.Edition)
      IF TablesRec.Inventory.Price <> 0 THEN LOCATE PRICEFIELD, 17: PRINT " $"; STR$(TablesRec.Inventory.Price)
      IF TablesRec.Inventory.IDnum <> 0 THEN LOCATE IDFIELD, 17: PRINT STR$(TablesRec.Inventory.IDnum)
    CASE cCardHoldersTableNum
      LOCATE NAMEFIELD, 18: PRINT TablesRec.Lendee.TheName
      LOCATE STREETFIELD, 18: PRINT TablesRec.Lendee.Street
      LOCATE CITYFIELD, 18: PRINT TablesRec.Lendee.City
      LOCATE STATEFIELD, 18: PRINT TablesRec.Lendee.State
      IF TablesRec.Lendee.Zip <> 0 THEN LOCATE ZIPFIELD, 17: PRINT STR$(TablesRec.Lendee.Zip)
      IF TablesRec.Lendee.CardNum <> 0 THEN LOCATE CARDNUMFIELD, 17: PRINT STR$(TablesRec.Lendee.CardNum)
    CASE ELSE
       CALL ShowMessage("There are no other forms defined", 0)
  END SELECT
END SUB

'**************************************************************************
'*  The UserChoice SUB is used to echo back to the user the most recent   *
'*  menu selection he has made. Not all menu choices are echoed back.     *
'*                                Parameters                              *
'*  BigRec    RecStruct type variable containing table information        *
'*  Row       Row on which to put the Feedback$                           *
'*  Column    Column at which to start the Feedback$                      *
'*  Feedback$ Menu-choice string to highlight                             *
'**************************************************************************
SUB UserChoice (BigRec AS RecStruct, Row, Column, Feedback$)
    CALL DrawHelpKeys(BigRec.TableNum)
    CALL ShowKeys(BigRec, BRIGHT + FOREGROUND, DimN, DimP)
    COLOR FOREGROUND + BRIGHT, BACKGROUND
    LOCATE Row, Column
    PRINT Feedback$
    COLOR FOREGROUND, BACKGROUND
END SUB

