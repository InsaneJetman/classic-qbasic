'***********************************************************************
'*  This is module level code for BOOKMOD2.BAS. It contains procedures *
'*  that use ISAM statements as well as procedures that support them.  *
'*  It is the third module of the BOOKLOOK program.                    *
'***********************************************************************
DEFINT A-Z
'$INCLUDE: 'booklook.bi'

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

'************************************************************************
'*                                                                      *
'*  This SUB checks the real current index after a try to set an index. *
'*  If the index was successfully set, it's name is displayed, other-   *
'*  wise the current index is displayed. IndexBox is called to update   *
'*  Current Sorting Order box on the screen.                            *
'*                                                                      *
'************************************************************************
SUB AdjustIndex (TablesRec AS RecStruct)
  RealIndexName$ = GETINDEX$(TablesRec.TableNum)
  CALL Indexbox(TablesRec, CheckIndex%(TablesRec, 0))
  IF RealIndexName$ <> EMPTYSTRING THEN
    Alert$ = "Records are now ordered by the index called " + RealIndexName$
  ELSE
    Alert$ = "Records now ordered by the default (NULL) index"
  END IF
  CALL ShowMessage(Alert$, 0)
END SUB

'***************************************************************************
'*  The ChangeRecord FUNCTION gets the new field value with MakeString. It *
'*  then assigns the value (converted if necessary) to its proper element  *
'*  in the recordvariable (TablesRec) used to update the table.            *
'*                                Parameters                               *
'*  FirstLetter   If the user has started typing, this contains a letter   *
'*  Argument      Tells what field the cursor is currently in              *
'*  TablesRec     RecStruct type variable holding all table information    *
'*  Task          Tells which operation is being performed                 *
'***************************************************************************
FUNCTION ChangeRecord (FirstLetter$, Argument, TablesRec AS RecStruct, Task AS INTEGER)
  STATIC SaveTitle AS STRING
  Prompt$ = "New Field Value: "

  IF Task <> SEEKFIELD THEN            ' Adjust the Argument --- It is in-
    IF Argument = TITLEFIELD THEN      ' cremented as part of PlaceCursor.
      Argument = IDFIELD               ' But it needs the user's original
    ELSE                               ' choice in this function.
       Argument = Argument - 2
    END IF
  END IF

  Filter% = ASC(FirstLetter$)                ' Convert FirstLetter$ to ascii
  Remainder$ = MakeString$(Filter%, Prompt$) ' number to pass to MakeString.
  IF Filter% = ESCAPE THEN                   ' This lets the user press ESC
    ChangeRecord = 0                         ' to abandon function.
    CALL ShowRecord(TablesRec)
    EXIT FUNCTION
  END IF
                                           ' Select for proper assignment of
  SELECT CASE Argument                     ' string user makes with MakeString
    CASE TITLEFIELD, NAMEFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD OR Task = SEEKFIELD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          TablesRec.Inventory.Title = Remainder$
        ELSE
          TablesRec.Lendee.TheName = Remainder$
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE AUTHORFIELD, STREETFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          TablesRec.Inventory.Author = Remainder$
        ELSE
          TablesRec.Lendee.Street = Remainder$
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE PUBFIELD, CITYFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          TablesRec.Inventory.Publisher = Remainder$
        ELSE
          TablesRec.Lendee.City = Remainder$
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE EDFIELD, STATEFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          TablesRec.Inventory.Edition = VAL(Remainder$)
        ELSE
          TablesRec.Lendee.State = Remainder$
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE PRICEFIELD, ZIPFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          TablesRec.Inventory.Price = VAL(Remainder$)
        ELSE
          TablesRec.Lendee.Zip = VAL(Remainder$)
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE IDFIELD, CARDNUMFIELD
      IF Task = EDITRECORD OR Task = ADDRECORD THEN
        IF TablesRec.TableNum = cBookStockTableNum THEN
          size = LEN(Remainder$)
          FOR counter = 1 TO size
            IF ASC(MID$(Remainder$, counter, 1)) = 0 THEN
              Remainder$ = MID$(Remainder$, (counter + 1), size)
            END IF
          NEXT counter
          TablesRec.Inventory.IDnum = VAL(LTRIM$(RTRIM$(Remainder$)))
        ELSE
          TablesRec.Lendee.CardNum = VAL(Remainder$)
        END IF
      END IF
      COLOR FOREGROUND, BACKGROUND
    CASE ELSE
        CALL ShowMessage("  Can't change that field ", 0)
        BEEP
        SLEEP 1
END SELECT
 ChangeRecord = 1
END FUNCTION

'***************************************************************************
'*  The CheckIndex uses the GETINDEX function to find the current index.   *
'*  Since only some displayed fields correspond to indexes, the number     *
'*  returned is a code indicating what to do, not the index name           *
'*                                Parameters                               *
'*  TablesRec   RecStuct type variable holding all table information       *
'*  FirstTime   If first time is TRUE, Index is NULL index                 *
'***************************************************************************
FUNCTION CheckIndex% (TablesRec AS RecStruct, FirstTime)
  Check$ = GETINDEX$(TablesRec.TableNum)
  SELECT CASE Check$
    CASE "TitleIndexBS", "NameIndexCH"
      CheckIndex% = 0
    CASE "AuthorIndexBS"
      CheckIndex% = 1
    CASE "PubIndexBS"
      CheckIndex% = 2
    CASE "StateIndexCH"
      CheckIndex% = 3
    CASE "ZipIndexCH"
      CheckIndex% = 4
    CASE "IDIndex", "CardNumIndexCH"
      CheckIndex% = 5
    CASE "BigIndex"                 ' There's no combined index on
      CheckIndex% = 6               ' CardHolders table
    CASE ""
      CheckIndex% = 7               ' This is a special case for the
                                    ' Blank line in CardHolders table
    IF FirstTime% THEN
      CALL Indexbox(TablesRec, 7)
    END IF
  END SELECT
END FUNCTION

'***************************************************************************
'*  The EdAddCursor function is used to place the cursor in the proper     *
'*  when the task is to Edit or Add a record.  Note when printing numeric  *
'*  fields LOCATE 1 column left to compensate  for the implicit "+" sign.  *
'*                                Parameters                               *
'*  NextField   Tells which field is to be highlighted next                *
'*  Job         Tells operation user wants to engage in                    *
'*  TablesRec   RecStruct type variable holding all table information      *
'*  FirstShot   Nonzero value indicates this is first time through         *
'***************************************************************************
FUNCTION EdAddCursor (NextField%, Job%, TablesRec AS RecStruct, FirstShot%)
  SELECT CASE TablesRec.TableNum
    CASE cBookStockTableNum                       ' BookStock table is 1
      SELECT CASE NextField
        CASE TITLEFIELD, NAMEFIELD
          LOCATE IDFIELD, 17
          IF FirstShot THEN COLOR FOREGROUND, BACKGROUND
          PRINT TablesRec.Inventory.IDnum
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE TITLEFIELD, 18
          PRINT TablesRec.Inventory.Title
          NextField% = AUTHORFIELD
        CASE AUTHORFIELD, STREETFIELD
          LOCATE TITLEFIELD, 18
          PRINT TablesRec.Inventory.Title
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE AUTHORFIELD, 18
          PRINT TablesRec.Inventory.Author
          NextField% = PUBFIELD
        CASE PUBFIELD, CITYFIELD
          LOCATE AUTHORFIELD, 18
          PRINT TablesRec.Inventory.Author
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE PUBFIELD, 18
            PRINT TablesRec.Inventory.Publisher
            NextField% = EDFIELD
        CASE EDFIELD, STATEFIELD
          LOCATE PUBFIELD, 18
          PRINT TablesRec.Inventory.Publisher
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE EDFIELD, 17
          PRINT TablesRec.Inventory.Edition
          NextField% = PRICEFIELD
        CASE PRICEFIELD, ZIPFIELD
          LOCATE EDFIELD, 17
          PRINT TablesRec.Inventory.Edition
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE PRICEFIELD, 19
          PRINT ; TablesRec.Inventory.Price
          NextField% = IDFIELD
        CASE IDFIELD, CARDNUMFIELD
          LOCATE PRICEFIELD, 18
          PRINT "$"; TablesRec.Inventory.Price
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE IDFIELD, 17
          PRINT TablesRec.Inventory.IDnum
          NextField% = TITLEFIELD
      END SELECT
    CASE cCardHoldersTableNum                       ' CardHolders table is 2
      SELECT CASE NextField
        CASE NAMEFIELD
          LOCATE CARDNUMFIELD, 17
          IF FirstShot THEN COLOR FOREGROUND, BACKGROUND
          PRINT TablesRec.Lendee.CardNum
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE NAMEFIELD, 18
          PRINT TablesRec.Lendee.TheName
          NextField% = STREETFIELD
        CASE STREETFIELD
          LOCATE NAMEFIELD, 18
          PRINT TablesRec.Lendee.TheName
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE STREETFIELD, 18
          PRINT TablesRec.Lendee.Street
          NextField% = CITYFIELD
        CASE CITYFIELD
          LOCATE STREETFIELD, 18
          PRINT TablesRec.Lendee.Street
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE CITYFIELD, 18
          PRINT TablesRec.Lendee.City
          NextField% = STATEFIELD
        CASE STATEFIELD
          LOCATE CITYFIELD, 18
          PRINT TablesRec.Lendee.City
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE STATEFIELD, 18
          PRINT TablesRec.Lendee.State
          NextField% = PRICEFIELD
        CASE ZIPFIELD
          LOCATE STATEFIELD, 18
          PRINT TablesRec.Lendee.State
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE ZIPFIELD, 17
          PRINT TablesRec.Lendee.Zip
          NextField% = IDFIELD
        CASE CARDNUMFIELD
          LOCATE ZIPFIELD, 17
          PRINT TablesRec.Lendee.Zip
          COLOR BACKGROUND, BRIGHT + FOREGROUND
          LOCATE CARDNUMFIELD, 17
          PRINT TablesRec.Lendee.CardNum
          NextField% = TITLEFIELD
      END SELECT
  END SELECT
  COLOR FOREGROUND, BACKGROUND
END FUNCTION

'***************************************************************************
'*  The EditField function lets the user choose whether or not to actually *
'*  change the current field (by calling ChangeRecord) or move on to the   *
'*  next field. It also displays a message telling how to Undo edits. If   *
'*  EditField returns TRUE, a SAVEPOINT is set at module level. If the task*
'*  is ADDRECORD, the user is taken through the fields one at a time until *
'*  they have all been entered.                                            *
'*                              Parameters                                 *
'*  Argument    Tells which field is currently being dealt with            *
'*  TablesRec   RecStruct type variable holding current table information  *
'*  FirstLetter If the user has started typing, the letter is passed in    *
'*  Task        Tells what type of operation the user is performing        *
'*  Answer      Same as Task, but passed to ChangeRecord
'***************************************************************************
FUNCTION EditField (Argument%, TablesRec AS RecStruct, FirstLetter$, Task%, Answer%)
  ' Show the transaction block message dealing with undoing edits:
  IF Task = EDITRECORD THEN CALL DrawIndexBox(1, Task)

  STATIC NextField
  FirstLetter$ = ""
  IF Task = EDITRECORD THEN CALL ShowMessage("Edit this field or TAB to another", 0)
  Argument = TITLEFIELD
  Value = PlaceCursor(Argument, TablesRec, FirstLetter$, 1, Task%)
  IF Argument THEN
    IF Task = EDITRECORD THEN CALL ShowMessage("Edit this field or TAB to another", 0)
    COLOR FOREGROUND, BACKGROUND
    WasFieldChanged = ChangeRecord(FirstLetter$, Argument, TablesRec, Answer)

    IF Task = EDITRECORD AND WasFieldChanged <> 0 THEN
      CALL ShowMessage("Press E to Edit another field ", 0)
      EditField = TRUE            ' If True is returned, a SAVEPOINT is set
    ELSEIF Task = EDITRECORD AND WasFieldChanged = 0 THEN
      CALL ShowRecord(TablesRec)
      CALL ShowMessage("Please try again...", 0)
      EditField = FALSE     'Don't set SAVEPOINT if user escapes from edit
    ELSEIF Task = SEEKFIELD THEN
      EditField = FALSE: EXIT FUNCTION
    END IF
    IF Task = ADDRECORD THEN
      NextField = 1
      DO WHILE NextField <> 0 AND Argument <> 0
        CALL ShowMessage("Enter value for field or ESC to abandon addition ", 0)
        SELECT CASE NextField
          CASE 1
            Argument = AUTHORFIELD
            FieldsDone = FieldsDone + 1
          CASE 2
            Argument = PUBFIELD
            FieldsDone = FieldsDone + 1
          CASE 3
            Argument = EDFIELD
            FieldsDone = FieldsDone + 1
          CASE 4
            Argument = PRICEFIELD
            FieldsDone = FieldsDone + 1
          CASE 5
            Argument = IDFIELD
            FieldsDone = FieldsDone + 1
            NextField = 0
          CASE ELSE
            CALL ShowMessage("Problem in the CASE assignments to Argument", 0): SLEEP
        END SELECT
        FirstLetter$ = ""
        Value = PlaceCursor(Argument, TablesRec, FirstLetter$, 1, Task%)
        IF Argument THEN
          COLOR FOREGROUND, BACKGROUND
          WasFieldChanged = ChangeRecord(FirstLetter$, Argument, TablesRec, Answer)
          NextField = NextField + 1
          IF FieldsDone = 5 THEN EditField% = 1: EXIT FUNCTION
        END IF
      LOOP
      EditField = FALSE 'No need for SAVEPOINT with ADDRECORD
    END IF
  ELSE
  CALL ShowRecord(TablesRec)
  CALL ShowMessage("Please try again...", 0)
  SLEEP: CALL EraseMessage
  CALL DrawIndexBox(TablesRec.TableNum, 0)' Replace Edit stuff with Index stuff
  EditField = FALSE     'Don't set SAVEPOINT if user escapes from edit
  END IF

END FUNCTION

'***************************************************************************
'*  The GetKeyVals SUB gathers the Keys for searching on a combined index. *
'*  It shows the fields as they are entered.                               *
'*                                Parameters                               *
'*  TablesRec   Contains all the information for the tables                *
'*  Key1        Represents the Title field of BookStock table              *
'*  Key2        Represents the Author field of BookStock table             *
'*  Key3        Represents the IDnum field of BookStock table              *
'*  Letter      Holds the first letter the user tries to enter at prompt   *
'***************************************************************************
SUB GetKeyVals (TablesRec AS RecStruct, Key1$, Key2$, Key3#, Letter$)
  WhichTable = TablesRec.TableNum
  Prompt$ = "Value to Seek: "

  CALL DrawScreen(WhichTable)
  DO
    ' Have the user ENTER the Title value to search for
    COLOR BACKGROUND, FOREGROUND
    LOCATE TITLEFIELD, 18
    PRINT "Please enter the Title to find"
    Key1$ = MakeString$(ASC(Letter$), Prompt$)
    CALL ShowIt(TablesRec, "TitleIndexBS", WhichTable, Key1$)
  LOOP UNTIL Key1$ <> ""

  Letter$ = " "    ' Set it to a blank space for typing

    ' Have the user ENTER the Author value to search for
  DO
    COLOR BACKGROUND, FOREGROUND
    LOCATE AUTHORFIELD, 18
    PRINT "Please enter the Author name to find"
    Key2$ = MakeString$(ASC(Letter$), Prompt$)
    ' Show it just shows the input user has entered, not a record from file
    CALL ShowIt(TablesRec, "AuthorIndexBS", WhichTable, Key2$)
  LOOP UNTIL Key2$ <> ""

  Letter$ = " "    ' Set it to a blank space for typing
    ' Have the user ENTER the ID number value to search for
  DO
    COLOR BACKGROUND, FOREGROUND
    LOCATE IDFIELD, 18
    PRINT "Please enter the ID number to find"
    ValueToSeek$ = MakeString$(ASC(Letter$), Prompt$)
    Key3# = CDBL(VAL(ValueToSeek$))       ' CURRENCY field
    CALL ShowIt(TablesRec, "IDIndex", WhichTable, ValueToSeek$)
LOOP UNTIL Key3# <> 0
END SUB

'****************************** GetOperand FUNCTION ************************
'* The GetOperand FUNCTION displays a choice of operators to allow user a  *
'* choice in how a SEEKoperand search will be conducted. If the user makes *
'* a valid choice, it is assigned to HoldOperand. An invalid choice or a   *
'* choice of ESC results in "<>" being passed back. This permits an exit   *
'* from the function (which is recursive). Otherwise, the user's choice is *
'* trapped in HoldOperand when ENTER is pressed.                           *
'* Note that this function is recursive so use the calls menu to keep      *
'* track of the nesting depth when stepping through it. Unlike PlaceCursor *
'* GetOperand doesn't keep track of the stack - the stack set should be OK.*
'*                              Parameters                                 *
'*   HoldOperand    Contains operand to check each time function calls     *
'*                  itself; Let's user ESC from function if desired.       *
'***************************************************************************
FUNCTION GetOperand% (HoldOperand$)
  STATIC WhichOne     ' Keep track of which case from call to call

  ' If user has chose ESC then exit back to caller
  IF HoldOperand$ = "<>" THEN WhichOne = 0: EXIT FUNCTION

  ' if this is the first time through the function then
  ' Replace the Sort Order box with box of operand choices
  IF WhichOne = 0 THEN
    RESTORE OperandBox
    FOR Row = BOXTOP TO BOXEND
      LOCATE Row, 42
      READ Temp$
      PRINT Temp$
      IF Row = BOXEND THEN
        COLOR FOREGROUND + BRIGHT, BACKGROUND
        LOCATE Row, INDBOX + 5
        PRINT "Relationship to Key"
      END IF
    NEXT Row
    LOCATE VLINE, 44
    PRINT "Equal To     Value Entered"     ' This is default --- if user
    COLOR FOREGROUND, BACKGROUND           ' presses ENTER without tabbing,
  END IF                                   ' SeekRecord sets the operand
                                           ' to =    Note: a more flexible
                                           ' default choice might be >=

  Alert$ = "Now press TAB to select how search should be conducted"
  CALL ShowMessage(Alert$, 0)
  DO
  Answer$ = INKEY$
  LOOP WHILE Answer$ <> CHR$(TABKEY) AND Answer$ <> CHR$(ENTER) AND Answer$ <> CHR$(ESCAPE)

  IF LEN(Answer$) = 1 THEN
    SELECT CASE ASC(Answer$)
      CASE TABKEY
        SELECT CASE WhichOne
          CASE 0
            COLOR FOREGROUND, BACKGROUND
            LOCATE VLINE, 44
            PRINT "Equal To"
            COLOR BRIGHT + FOREGROUND, BACKGROUND
            LOCATE RLINE, 44
            PRINT "Greater Than"
            WhichOne = WhichOne + 1
            HoldOperand$ = ">"
          CASE 1
            COLOR BRIGHT + FOREGROUND, BACKGROUND
            LOCATE VLINE, 44
            PRINT "Equal To"
            LOCATE WLINE, 44
            PRINT "or"
            WhichOne = WhichOne + 1
            HoldOperand$ = ">="
          CASE 2
            COLOR FOREGROUND, BACKGROUND
            LOCATE RLINE, 44
            PRINT "Greater Than"
            LOCATE WLINE, 44
            PRINT "or"
            COLOR BRIGHT + FOREGROUND, BACKGROUND
            LOCATE ALINE, 44
            PRINT "or"
            LOCATE ELINE, 44
            PRINT "Less Than"
            WhichOne = WhichOne + 1
            HoldOperand$ = "<="
          CASE 3
            COLOR FOREGROUND, BACKGROUND
            LOCATE VLINE, 44
            PRINT "Equal To"
            LOCATE ALINE, 44
            PRINT "or"
            WhichOne = WhichOne + 1
            HoldOperand$ = "<"
            SLEEP
          CASE 4
            COLOR FOREGROUND, BACKGROUND
            LOCATE ELINE, 44
            PRINT "Less Than"
            COLOR BRIGHT + FOREGROUND, BACKGROUND
            LOCATE VLINE, 44
            PRINT "Equal To     Value Entered"
            WhichOne = WhichOne + 1
            HoldOperand$ = "="
          CASE ELSE
        END SELECT                          ' If no choice was made, call
        IF WhichOne > 4 THEN WhichOne = 0   ' GetOperand again
        COLOR FOREGROUND, BACKGROUND
        OK = GetOperand%(HoldOperand$)
      CASE ENTER
        WhichOne = 0
        EXIT FUNCTION
    CASE ESCAPE                 ' If user chooses ESC, signal the function
      HoldOperand$ = "<>"       ' to exit and keep exiting back through
      GetOperand% = 0           ' all levels of recursion
      WhichOne = 0
    CASE ELSE                   ' If user chooses invalid key, try again
      BEEP
      CALL ShowMessage("Use TAB to select relationship to search for...", 0)
      COLOR white, BACKGROUND
      OK = GetOperand%(HoldOperand$)
  END SELECT
ELSE
END IF

END FUNCTION

'***************************************************************************
'*  The IndexBox SUB highlights the proper index name in the Current Index *
'*  box at the bottom right section of the screen.                         *
'                                                                          *
'*  TablesRec   RecStruct type variable containing all table information   *
'*  MoveDown    Integer representing line on which index name resides      *
'***************************************************************************
SUB Indexbox (TablesRec AS RecStruct, MoveDown)
   Table = TablesRec.TableNum
   COLOR BRIGHT + FOREGROUND, BACKGROUND
   LOCATE 17 + MoveDown, 44
   SELECT CASE MoveDown
     CASE 0
      IF Table = cBookStockTableNum THEN PRINT "By Titles   " ELSE PRINT "By Name    "
      COLOR FOREGROUND, BACKGROUND
      LOCATE ELINE, 44
      PRINT "Default = Insertion Order"
     CASE 1
      IF Table = cBookStockTableNum THEN PRINT "By Authors   "
      COLOR FOREGROUND, BACKGROUND
      LOCATE NLINE, 44
      IF Table = cBookStockTableNum THEN PRINT "By Titles   " ELSE PRINT "By Name     "
     CASE 2
      IF Table = cBookStockTableNum THEN PRINT "By Publishers   "
      COLOR FOREGROUND, BACKGROUND
      LOCATE RLINE, 44
      IF Table = cBookStockTableNum THEN PRINT "By Authors    "
     CASE 3
      IF Table = cCardHoldersTableNum THEN
        LOCATE RLINE, 44
        PRINT "By States     "
        COLOR FOREGROUND, BACKGROUND
        LOCATE NLINE, 44
        PRINT "By Names     "
      ELSE
        COLOR FOREGROUND, BACKGROUND
        LOCATE WLINE, 44
        PRINT "By Publishers"
      END IF
     CASE 4
      IF Table = cCardHoldersTableNum THEN
        LOCATE WLINE, 44
        PRINT "By Zipcodes   "
        COLOR FOREGROUND, BACKGROUND
        LOCATE RLINE, 44
        PRINT "By States     "
      END IF
     CASE 5
      LOCATE VLINE, 44
      IF Table = cBookStockTableNum THEN
        PRINT "By ID Numbers   "
        COLOR FOREGROUND, BACKGROUND
      ELSE
        PRINT "By Card numbers   "
        COLOR FOREGROUND, BACKGROUND
        LOCATE WLINE, 44
        PRINT "By Zipcodes    "
      END IF
     CASE 6
      IF Table = cBookStockTableNum THEN
        LOCATE ALINE, 44
        PRINT "By Title + Author + ID"
        COLOR FOREGROUND, BACKGROUND
        LOCATE VLINE, 44
        PRINT "By ID Numbers"
      ELSE
        LOCATE VLINE, 44
        COLOR FOREGROUND, BACKGROUND
        PRINT "By Card numbers   "
      END IF
     COLOR FOREGROUND, BACKGROUND
     CASE 7
      LOCATE ELINE, 44
      PRINT "Default = Insertion Order"
      COLOR FOREGROUND, BACKGROUND
      IF Table = cBookStockTableNum THEN
        LOCATE ALINE, 44
        PRINT "By Title + Author + ID"
      ELSE
        LOCATE VLINE, 44
        PRINT "By Card numbers"
      END IF
    END SELECT
   IF MoveDown < 7 THEN
    MoveDown = MoveDown + 1
   ELSE
    MoveDown = 0
   END IF
COLOR FOREGROUND, BACKGROUND
END SUB

'***************************************************************************
'* The OrderCursor FUNCTION returns TRUE or FALSE for user index choice.   *
'* Each time the user places the cursor on an Index to sort on, this       *
'* function displays an instruction message in the field(s) corresponding  *
'* to the Index, It then associates the highlighted index name (in the     *
'* Sorting Order box) with the name it is known by in the program, and     *
'* places that name in the .WhichIndex element of a structured variable of *
'* RecStruct type.                                                         *
'*                                   Parameters:                           *
'* Index       Integer telling which index user has highlighted            *
'* NextField   Manifest Constant telling big cursor field position         *
'* Job         Manifest Constant indicating task being performed           *
'* TablesRec   Variable of RecStruct type, whose .WhichInded element is    *
'*             used to return the index name to be used by SETINDEX.       *
'***************************************************************************
FUNCTION OrderCursor (Index%, NextField%, Job%, TablesRec AS RecStruct, Letter$)
  OrderCursor = FALSE
  CALL Indexbox(TablesRec, Index)         ' Light up the new index
  COLOR BACKGROUND, BRIGHT + FOREGROUND   ' in Sorting Order box
  LOCATE NextField, 18
  IF Job = REORDER THEN         ' Tell the user what is expected of him

    IF TablesRec.TableNum = cBookStockTableNum THEN
      IF NextField <> PRICEFIELD AND NextField <> EDFIELD THEN
        PRINT "Press enter to resort, or TAB to move on"
      ELSE
        LOCATE NextField, 20 '19
        PRINT "Sorry, cannot sort on an unindexed field"
      END IF
    ELSE
      IF NextField <> STREETFIELD AND NextField <> CITYFIELD THEN
        PRINT "Press enter to resort, or TAB to move on"
      ELSE
        PRINT "Sorry, cannot sort on an unindexed field"
      END IF
    END IF
   END IF

        ' The following places the name of the index to sort on in the
        ' WhichIndex element of the structured variable TablesRec --- it
        ' retrieved at the module-level code

        LOCATE NextField, 18
        SELECT CASE NextField
          CASE TITLEFIELD, NAMEFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                PRINT "Type Title to search for, or press TAB to move on"
              ELSE
                PRINT "Type Name to search for, or press TAB to move on"
              END IF
            END IF
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                TablesRec.WhichIndex = "TitleIndexBS"
              ELSE
                TablesRec.WhichIndex = "NameIndexCH"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = AUTHORFIELD
            END IF
          CASE AUTHORFIELD, STREETFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                PRINT "Type Author name to search for, or TAB to move on"
              ELSE
                PRINT "Sorry, can't search on an unindexed field"
              END IF
            END IF
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                TablesRec.WhichIndex = "AuthorIndexBS"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = PUBFIELD
            END IF
          CASE PUBFIELD, CITYFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                PRINT "Type Publisher name to search for, or TAB to move on"
              ELSE
                PRINT "Sorry, can't search on an unindexed field"
              END IF
            END IF
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                TablesRec.WhichIndex = "PubIndexBS"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = EDFIELD
            END IF
          CASE EDFIELD, STATEFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cCardHoldersTableNum THEN
                PRINT "Type State (2 letters), or TAB to move on"
              ELSE
                PRINT "Sorry, can't search on an unindexed field"
              END IF
            END IF
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cCardHoldersTableNum THEN
                TablesRec.WhichIndex = "StateIndexCH"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = PRICEFIELD
            END IF
          CASE PRICEFIELD, ZIPFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cCardHoldersTableNum THEN
                PRINT "Type Zipcode to search for, or TAB to move on"
              ELSE
                LOCATE PRICEFIELD, 20
                PRINT "Sorry, can't search on an unindexed field"
              END IF
            END IF
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cCardHoldersTableNum THEN
                TablesRec.WhichIndex = "ZipIndexCH"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = IDFIELD
            END IF
          CASE IDFIELD, CARDNUMFIELD
            IF Job = SEEKFIELD THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                PRINT "Type ID number to search for, or TAB to move on"
              ELSE
                PRINT "Type Card number to seek, or press TAB to move on"
              END IF
            END IF
            ' Setting Letter$ to "" may be unnecessary now
            Letter$ = ""
            IF ConfirmEntry%(Letter$) THEN
              IF TablesRec.TableNum = cBookStockTableNum THEN
                TablesRec.WhichIndex = "IDIndex"
              ELSE
                TablesRec.WhichIndex = "CardNumIndexCH"
              END IF
              OrderCursor = TRUE
              EXIT FUNCTION
            ELSE
              OrderCursor = FALSE
              NextField% = BIGINDEX
            END IF
        END SELECT
 IF Letter$ = "eScApE" THEN OrderCursor = 3: FirstLetter$ = ""
END FUNCTION

'***************************************************************************
'*  The PlaceCursor FUNCTION lets the user tab around on the displayed form*
'*  when performing field-specific operations on the table. Since this     *
'*  function is recursive it keeps track of available stack space. The two *
'*  major possibilities are SEEKs/REORDERs (for which OrderCursor is then  *
'*  called) and EDIT/ADD records (for which EdAddCursor is called. Note    *
'*  the combined index (BigIndex) and the default index are handled as     *
'*  special cases, since they don't correspond to a single field.Recursive *
'*  construction lets the user cycle through the fields as long as         *
'*  sufficient stack remains to keep calling PlaceCursor. Note that since  *
'*  it is recursive, it may take while to step out while stepping with F8. *
'*                                Parameters                               *
'*  WhichField    Integer identifier specifying current field on form      *
'*  TablesRec     Variable of type RecStruct holding all table information *
'*  FirstLetter$  Carries user response to initial prompt shown            *
'*  FirstTime     Boolean telling whether this is first cal or recursion   *
'*  Task          Tells operation being performed                          *
'***************************************************************************
'
FUNCTION PlaceCursor% (WhichField, TablesRec AS RecStruct, FirstLetter$, FirstTime AS INTEGER, Task AS INTEGER)
STATIC ReturnValue, InitialLetter$, GetOut, counter, WhichOne
WhichTable = TablesRec.TableNum
IF ExitFlag THEN EXIT FUNCTION

ReturnValue = WhichField
' Keep tabs on the stack and exit and reset it if it gets too low
IF FRE(-2) < 400 THEN
  WhichField = 0
  PlaceCursor = 0
  GetOut = -1
  EXIT FUNCTION
END IF

' Set up for each of the possible operations that use PlaceCursor
IF Task = REORDER THEN
   COLOR FOREGROUND, BACKGROUND
   CALL ShowMessage("Press TAB to choose field to sort on, ESC to escape", 0)
   IF WhichField = TITLEFIELD THEN WhichOne = 0
ELSEIF Task = SEEKFIELD THEN
   CALL ShowMessage("TAB to a field, then enter a value to search", 0)
ELSEIF Task = ADDRECORD THEN
  IF FirstTime THEN FirstLetter$ = CHR$(TABKEY) ELSE FirstLetter$ = ""
END IF

' The following IF... lets function handle either an entered letter or TAB
IF FirstLetter$ <> "" THEN
    Answer$ = FirstLetter$
ELSEIF FirstTime THEN
  IF Task = EDITRECORD THEN
    Answer$ = CHR$(TABKEY)
  END IF
ELSE
  DO
  Answer$ = INKEY$
  LOOP WHILE Answer$ = EMPTYSTRING
END IF

IF LEN(Answer$) = 1 THEN

' Clear the fields for the appropriate messages
IF Task <> EDITRECORD AND Task <> ADDRECORD THEN
CALL ClearEm(TablesRec.TableNum, 1, 1, 1, 1, 1, 1)
END IF

   SELECT CASE ASC(Answer$)
    CASE IS = TABKEY, ENTER
           SELECT CASE WhichField
            CASE TITLEFIELD, AUTHORFIELD, PUBFIELD, EDFIELD, PRICEFIELD, IDFIELD
              IF Task = REORDER OR Task = SEEKFIELD THEN
                RetVal = OrderCursor(WhichOne, WhichField, Task, TablesRec, FirstLetter$)
                IF RetVal THEN
                  ' trap a magic value for an escape here then call the Draw stuff
                  IF RetVal <> 3 THEN
                    WhichOne = 0: EXIT FUNCTION
                  ELSE
                    WhichOne = 0
                    WhichField = 0
                    PlaceCursor = 0
                    CALL ShowRecord(TablesRec)
                    CALL ShowMessage("You've escaped! Try again", 0)
                    CALL DrawTable(WhichTable)
                    CALL DrawHelpKeys(WhichTable)
                    CALL ShowKeys(TablesRec, FOREGROUND + BRIGHT, 0, 0)
                    EXIT FUNCTION
                  END IF
                END IF
              ELSEIF Task = EDITRECORD OR Task = ADDRECORD THEN
                Placed = EdAddCursor(WhichField, Task, TablesRec, FirstTime)
              END IF
           
            CASE BIGINDEX
                CALL Indexbox(TablesRec, WhichOne)
                IF WhichTable = cBookStockTableNum THEN
                  COLOR BACKGROUND, BRIGHT + FOREGROUND
                  IF Task = REORDER THEN
                    LOCATE TITLEFIELD, 18
                    PRINT "Press ENTER to sort first by Title..."
                    LOCATE AUTHORFIELD, 18
                    PRINT "... then subsort by Author..."
                    LOCATE IDFIELD, 18
                    PRINT "... then subsort again by ID "
                    SLEEP
                  ELSEIF Task = SEEKFIELD THEN
                    LOCATE TITLEFIELD, 18
                    PRINT "First, type in the Title to search for,"
                    LOCATE AUTHORFIELD, 18
                    PRINT "... then type in the Author's name"
                    LOCATE IDFIELD, 18
                    PRINT "... then type in the ID number "
                    CALL ShowMessage("Typing in a value for a combined index is tricky...", 0)
                    SLEEP
                  END IF
                  COLOR FOREGROUND, BACKGROUND
                  IF ConfirmEntry%(FirstLetter$) THEN
                    TablesRec.WhichIndex = "BigIndex"
                    IF Task = SEEKFIELD THEN
                      WhichOne = 0
                      WhichField = TITLEFIELD
                    END IF
                    EXIT FUNCTION
                  END IF
                END IF
                CALL ClearEm(TablesRec.TableNum, 1, 1, 0, 0, 1, 0)
                WhichField = NULLINDEX   ' TITLEFIELD

            CASE NULLINDEX
                CALL Indexbox(TablesRec, WhichOne)
                IF Task = SEEKFIELD THEN
                  CALL ShowMessage("Can't SEEK on the default index", 0)
                  DO
                    KeyIn$ = INKEY$
                    IF KeyIn$ <> "" THEN
                      IF ASC(KeyIn$) = ESCAPE THEN EXIT FUNCTION
                    END IF
                  LOOP WHILE KeyIn$ = ""
                  'SLEEP
                '  EXIT FUNCTION
                'END IF
                ELSEIF ConfirmEntry%(FirstLetter$) THEN
                  TablesRec.WhichIndex = "NULL"
                  EXIT FUNCTION
                END IF
                WhichField = TITLEFIELD
              
            CASE ELSE
                EraseMessage
                 CALL ShowMessage("Not a valid key --- press Space bar", 0)
                EXIT FUNCTION
          END SELECT
        ' Placecursor calls itself for next user response
        Value = PlaceCursor(WhichField, TablesRec, FirstLetter$, 0, Task)

    CASE ESCAPE
      WhichOne = 0
      WhichField = 0
      PlaceCursor = 0
      CALL ShowRecord(TablesRec)
      CALL ShowMessage("You've escaped! Try again", 0)
      CALL DrawTable(WhichTable)
      CALL DrawHelpKeys(WhichTable)
      CALL ShowKeys(TablesRec, FOREGROUND + BRIGHT, 0, 0)
      EXIT FUNCTION
    CASE 32 TO 127                        ' Acceptable ASCII characters
     InitialLetter$ = Answer$
     FirstLetter$ = InitialLetter$
     EXIT FUNCTION
    CASE ELSE
        BEEP
        EraseMessage
         CALL ShowMessage("Not a valid key --- press Space bar", 0)
        WhichField = 0
        PlaceCursor = 0
        EXIT FUNCTION
    END SELECT
ELSEIF Answer$ <> CHR$(9) THEN
  EraseMessage
  CALL ShowMessage("Not a valid key --- press Space bar", 0)
  WhichField = 0
  EXIT FUNCTION
ELSE
     CALL ShowMessage("  Press TAB key or ENTER  ", 0)
END IF

IF GetOut THEN
  counter = counter + 1
  IF counter < 15 THEN
    WhichField = 0
    WhichOne = 0
    EXIT FUNCTION
  ELSE
    GetOut = 0
    counter = 0
 END IF
END IF

END FUNCTION

'***************************************************************************
'*  The TransposeName FUNCTION takes a  string and decideds whether it is  *
'*  a comma-delimited, last-name-first name, a first-name-first name or a  *
'*  single word name. In the last case, the string is returned unchanged.  *
'*  In either of the other cases, the string is translated to the comple-  *
'*  mentary format.                                                        *
'*                              Parameters                                 *
'*  TheName   A string representing a CardHolders table TheName element,   *
'*            or a BookStock table Author Element                          *
'***************************************************************************
FUNCTION TransposeName$ (TheName AS STRING)
SubStrLen = (INSTR(TheName, ","))
IF SubStrLen = 0 THEN
  SubStrLen = INSTR(TheName, " ")
  IF SubStrLen = 0 THEN TransposeName$ = TheName: EXIT FUNCTION
END IF
TheName = LTRIM$(RTRIM$(TheName))
  IF INSTR(TheName, ",") THEN
    LastNameLen = INSTR(TheName, ",")
    LastName$ = LTRIM$(RTRIM$(LEFT$(TheName, LastNameLen - 1)))
    FirstName$ = LTRIM$(RTRIM$(MID$(TheName, LastNameLen + 1)))
    TransposeName$ = LTRIM$(RTRIM$(FirstName$ + " " + LastName$))
  ELSE
    FirstNameLen = INSTR(TheName, " ")
    IF FirstNameLen THEN
      FirstName$ = LTRIM$(RTRIM$(LEFT$(TheName, FirstNameLen - 1)))
      LastName$ = LTRIM$(RTRIM$(MID$(TheName, FirstNameLen + 1)))
    ELSE
      LastName$ = LTRIM$(RTRIM$(TheName))
    END IF
    TransposeName$ = LTRIM$(RTRIM$(LastName$ + ", " + FirstName$))
  END IF
END FUNCTION

'****************************** ValuesOK FUNCTION **************************
'* The ValuesOK FUNCTION checks the values input by the user for various   *
'* purposes. The checking is very minimal and checks the format of what is *
'* entered. For example, the IDnum field needs a double value, but the form*
'* (5 digits, followed by a decimal point, followed by 4 digits) is more   *
'* important than the data type.                                           *
'*                                Parameters:                              *
'*   Big Rec      User-defined type containing all table information       *
'*   Key1, Key2   Represent strings to check                               *
'*   ValueToSeek  Represents the final value of a combined index           *
'***************************************************************************
FUNCTION ValuesOK (BigRec AS RecStruct, Key1$, Key2$, ValueToSeek$)
  IndexName$ = BigRec.WhichIndex
  ValueToSeek$ = LTRIM$(RTRIM$(ValueToSeek$))
  SELECT CASE RTRIM$(LTRIM$(IndexName$))
    CASE "TitleIndexBS", "PubIndexBS"       ' LEN <= 50
      IF LEN(Key1$) > 50 THEN ValuesOK = FALSE: EXIT FUNCTION

    CASE "AuthorIndexBS", "NameIndexCH"     ' LEN <= 36
      IF LEN(Key1$) > 36 THEN ValuesOK = FALSE: EXIT FUNCTION

    CASE "StateIndexCH"                     ' LEN = 2
      IF LEN(Key1$) > 2 THEN ValuesOK = FALSE: EXIT FUNCTION

    CASE "IDIndex", "IDIndexBO"             ' 5 digits befor d.p., 4 after
      IF LEN(ValueToSeek$) <> 10 THEN ValuesOK = FALSE: EXIT FUNCTION
      IF MID$(ValueToSeek$, 6, 1) <> "." THEN
        ValuesOK = FALSE: EXIT FUNCTION
      END IF
    CASE "CardNumIndexCH", "CardNumIndexBO" ' 5 digits, value <= LONG
      IF LEN(ValueToSeek$) <> 5 THEN ValuesOK = FALSE: EXIT FUNCTION

    CASE "ZipIndexCH"                       ' 5 digits, value <= LONG
      IF LEN(ValueToSeek$) <> 5 THEN ValuesOK = FALSE: EXIT FUNCTION

    CASE "BigIndex"                         ' Key1$ <= 50, Key2$ <= 36
      IF LEN(Key1$) > 50 THEN ValuesOK = FALSE: EXIT FUNCTION
      IF LEN(Key2$) > 36 THEN ValuesOK = FALSE: EXIT FUNCTION
      IF MID$(ValueToSeek$, 6, 1) <> "." THEN
        ValuesOK = FALSE: EXIT FUNCTION
      END IF
  END SELECT
  ValuesOK = TRUE
END FUNCTION

