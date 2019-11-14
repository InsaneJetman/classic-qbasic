'****************************** Main  Module *******************************
'* This window contains the module-level code of BOOKLOOK.BAS, a program   *
'* used to manage the database of a hypothethical library (BOOKS.MDB). The *
'* program requires the following additional modules: BOOKMOD1.BAS,        *
'* BOOKMOD2.BAS, and BOOKMOD3.BAS, all named in the file BOOKLOOK.MAK. The *
'* include file BOOKLOOK.BI and the database file BOOKS.MDB must also be   *
'* accessible. The program is discussed in Chapter 10, Database Programming*
'* with ISAM in the BASIC Programmer's Guide.				   *
'*                                                                         *
'* If you do NOT have expanded memory available, you should have invoked   *
'* the PROISAM.EXE TSR as PROISAM /Ib:n, where n can be between 10-20.     *
'* The /Ib: option specifies the number of buffers ISAM needs. Higher n    *
'* values improve performance. Too few buffers, and the program will fail  *
'* with an "Insufficient ISAM buffers" error. If /Ib: is set too high,     *
'* there may not be enough memory to load and run the program. If you have *
'* expanded memory, ISAM automatically uses up to 1.2 megabytes, even if   *
'* you set Ib: to a low value. With a program the size of BOOKLOOK, use the*
'* /Ie: option to reserve some expanded memory for QBX. This indirectly    *
'* limits the amount of expanded memory ISAM uses, but make sure ISAM gets *
'* enough EMS for at least 15 buffers (each buffer = 2K). As a last resort,*
'* you can start QBX with the /NOF switch to make more memory available.   *
'*                                                                         *
'* BOOKLOOK manages 3 tables, BookStock, CardHolders, and BooksOut. The    *
'* data in the BookStock and CardHolders tables is displayed as forms on   *
'* screen. The user can switch between table displays by pressing "V" (for *
'* View Other Table). Each table is defined as a separate structure. The   *
'* structure for BookStock is Books, for CardHolders it is Borrowers, and  *
'* for BooksOut it is BookStatus. Each of these is incorporated as an      *
'* element of the structure RecStruct. RecStruct also has an element of    *
'* INTEGER type called TableNum (to keep track of which table is being     *
'* displayed), and a STRING element called WhichIndex that holds the name  *
'* of the index by which the user chooses to order presentation of records.*
'* Press F2 to see a list of procedures called by the program.             *
'***************************************************************************

DEFINT A-Z
'$INCLUDE: 'BOOKLOOK.BI'
SCREEN 0
CLS                         ' TempRec is for editing and adding records
DIM TempRec AS RecStruct    ' Used only to blank out a TempRec
DIM EmptyRec AS RecStruct   ' See BOOKLOOK.BI for declaration of
DIM BigRec AS RecStruct     ' this structure and its elements
DIM Marker(25) AS INTEGER   ' Array to hold SAVEPOINT returns

' Open the database and the BookStock, CardHolders, and BooksOut tables

ON ERROR GOTO MainHandler
OPEN "BOOKS.MDB" FOR ISAM Books "BookStock" AS cBookStockTableNum
OPEN "BOOKS.MDB" FOR ISAM Borrowers "CardHolders" AS cCardHoldersTableNum
OPEN "BOOKS.MDB" FOR ISAM BookStatus "BooksOut" AS cBooksOutTableNum
ON ERROR GOTO 0

BigRec.TableNum = cBookStockTableNum   ' Decide which table to show first
 
   ' Since the database has multiple tables, this outer DO loop is used to
   ' reset the number associated with the table the user wants to
   ' to access, then draw the screen appropriate to that table, etc.
DO
  EraseMessage                          ' Show the interface
  CALL DrawScreen(BigRec.TableNum)
  Checked = CheckIndex%(BigRec, TRUE)   ' Show current index
  CALL Retriever(BigRec, DimN, DimP, answer) ' Retrieve and show a record
  CALL ShowMessage(" Press V to View other table", 0)
  CALL ShowStatus(" Total records in table: ", CDBL(LOF(BigRec.TableNum)))
    
  ' This loop lets the user traverse BigRec.TableNum and insert, delete,
  ' or modify records.
  DO                                           ' At start of each loop, show
                                               ' the user valid operations
    CALL Retriever(BigRec, DimN, DimP, answer) ' and display current record

    STACK 4000                          ' Set large stack for recursions-it
                                        ' also resets FRE(-2) to stack 4000.

    answer% = GetInput%(BigRec)         ' Find out what the user wants to do

    IF answer < UNDO THEN               ' Excludes UNDOALL & INVALIDKEY too
      CALL EditCheck(PendingFlag, answer, BigRec)
    END IF

    SELECT CASE answer         ' Process valid user requests
      CASE QUIT
        CALL ShowMessage(" You chose Quit. So long! ", 0)
        END

                               ' If user picks "N" (Next Record), MOVENEXT.
                               ' CheckPosition handles end-of-file (i.e. the
      CASE GOAHEAD, ENDK       ' position just past the last record). If EOF
                               ' or BOF = TRUE, CheckPosition holds position
        MOVENEXT BigRec.TableNum
        CALL CheckPosition(BigRec, answer, DimN, DimP)

                               ' Same logic as GOAHEAD, but reversed
      CASE GOBACK, HOME

        MOVEPREVIOUS BigRec.TableNum
        CALL CheckPosition(BigRec, answer, DimN, DimP)

                               ' If user chooses "E", let him edit a field.
                               ' Assign the value returned by SAVEPOINT to
                               ' an array element, then update the table and
                               ' show the changed field. Trap any "duplicate
      CASE EDITRECORD          ' value for unique index" (error 86) and
                               ' handle it. The value returned by SAVEPOINT
                               ' allows rollbacks so the user can undo edits

        IF LOF(BigRec.TableNum) THEN
          IF EditField(Argument%, BigRec, Letter$, EDITRECORD, answer%) THEN

              ' You save a sequence of savepoint identifiers in an array so
              ' you can let the user roll the state of the file back to a
              ' specific point. The returns from SAVEPOINT aren't guaranteed
              ' to be sequential.
            n = n + 1              ' Increment counter first so savepoint
            Marker(n) = SAVEPOINT  ' is synced with array-element subscript

            Alert$ = "Setting Savepoint number " + STR$(Marker(n))
            CALL ShowMessage(Alert$, 0)
            ON ERROR GOTO MainHandler
            SELECT CASE BigRec.TableNum   ' Update the table being displayed
              CASE cBookStockTableNum
                UPDATE BigRec.TableNum, BigRec.Inventory
              CASE cCardHoldersTableNum
                UPDATE BigRec.TableNum, BigRec.Lendee
            END SELECT
            ON ERROR GOTO 0
          ELSE
            COMMITTRANS               ' Use COMMITTRANS abort transaction if
            PendingFlag = FALSE       ' the user presses ESC
            n = 0                     ' Reset array counter
          END IF
        ELSE
          CALL ShowMessage("Sorry, no records in this table to edit", 0): SLEEP
        END IF
                          ' If choice is "A", get the values the user wants
                          ' in each of the fields (with AddOne). If there
                          ' is no ESCAPE from the edit, INSERT the record.
                          ' Trap "Duplicate value for unique index" errors
                          ' and handle them in MainHandler (error 86).
      CASE ADDRECORD
        added = AddOne(BigRec, EmptyRec, TempRec, answer%)
        IF added THEN
          Alert$ = "A new record assumes proper place in current index"
          CALL ShowMessage(Alert$, 0)
          ON ERROR GOTO MainHandler
          SELECT CASE BigRec.TableNum     ' Insert into table being shown
            CASE cBookStockTableNum
              INSERT BigRec.TableNum, TempRec.Inventory
            CASE cCardHoldersTableNum
              INSERT BigRec.TableNum, TempRec.Lendee
          END SELECT
          ON ERROR GOTO 0
        END IF
        TempRec = EmptyRec
        
                              ' If choice is "D" --- prompt for confirmation.
                              ' If so, delete it and show new current record.
      CASE TOSSRECORD
        AnyRecords = LOF(BigRec.TableNum)
        IF BigRec.TableNum = cBookStockTableNum THEN CheckedOut = GetStatus(BigRec, 0#)
        IF BigRec.TableNum = cCardHoldersTableNum THEN
          SETINDEX cBooksOutTableNum, "CardNumIndexBO"
          SEEKEQ cBooksOutTableNum, BigRec.Lendee.CardNum
          IF NOT EOF(cBooksOutTableNum) THEN CheckedOut = TRUE
        END IF
        IF AnyRecords AND CheckedOut = FALSE THEN
          Alert$ = "Press D again to Delete this record, ESC to escape"
          CALL ShowMessage(Alert$, 0)
          DeleteIt% = GetInput%(BigRec)
          IF DeleteIt% = TOSSRECORD THEN   ' Delete currently-displayed record
            DELETE BigRec.TableNum
            CALL ShowMessage("Record deleted...Press a key to continue", 0)
          ELSE
            CALL ShowMessage("Record not deleted. Press a key to continue", 0)
            CALL ShowRecord(BigRec)
          END IF
          ' The following code checks whether the record deleted was the last
          ' record in the index, then makes the new last record current
          IF EOF(BigRec.TableNum) THEN
            MOVELAST BigRec.TableNum
          END IF
        ELSE
          IF BigRec.TableNum = cBookStockTableNum THEN
            IF AnyRecords = 0 THEN Alert$ = "Sorry, no records in this table to delete"
            IF CheckedOut THEN Alert$ = "Can't delete --- this book currently checked out!"
          ELSEIF BigRec.TableNum = cCardHoldersTableNum THEN
            IF AnyRecords = 0 THEN Alert$ = "Sorry, no records in this table to delete"
            IF CheckedOut THEN Alert$ = "Can't delete --- this cardholder still has books out!"
          END IF
          CALL ShowMessage(Alert$, 0): SLEEP
        END IF
        CheckedOut = FALSE

                               ' If user chooses "R", walk the fields so he
                               ' can choose new index to order presentation
      CASE REORDER
        Letter$ = CHR$(TABKEY)
        GotOne = ChooseOrder(BigRec, EmptyRec, TempRec, Letter$, REORDER)

                               ' If a choice of indexes was made, retrieve
                               ' the index name, set an error trap, and try
                               ' to set the index, then display new index.
        IF GotOne THEN
          IndexName$ = LTRIM$(RTRIM$(TempRec.WhichIndex))
          ON ERROR GOTO MainHandler
          IF IndexName$ <> "NULL" THEN            ' This string is placed in
            SETINDEX BigRec.TableNum, IndexName$  ' TempRec.WhichIndex if
          ELSE                                    ' user chooses "Default."
            SETINDEX BigRec.TableNum, ""          ' "" is valid index name
          END IF                                  'representing NULL index
          ON ERROR GOTO 0                         '(i.e. the default order)
          CALL AdjustIndex(BigRec)
          LSET TempRec = EmptyRec
        END IF

                          ' If choice is "F", first set current index
      CASE SEEKFIELD      ' using same procedure as REORDER. Then do seek.

        Letter$ = CHR$(TABKEY)        ' Pass TABKEY for PlaceCursor
        GotOne = ChooseOrder(BigRec, EmptyRec, TempRec, Letter$, SEEKFIELD)

        IF GotOne AND TEXTCOMP(TempRec.WhichIndex, "NULL") THEN
          CALL SeekRecord(BigRec, TempRec, Letter$)
          FirstLetter$ = ""
          DimN = EOF(BigRec.TableNum): DimP = BOF(BigRec.TableNum)
        END IF

                          ' STATUS gets the due date of a book & displays it
      CASE STATUS
        IF BigRec.TableNum = cBookStockTableNum THEN
          CALL ShowStatus("", 0#)                  ' Explicitly type the 0
          GotIt = GetStatus(BigRec, DateToShow#)   ' to avoid type mismatch
          IF GotIt THEN
            Alert$ = "Press B for information on Borrower of this book"
            CALL ShowMessage(Alert$, 0)
            CALL ShowStatus("Due Date: ", DateToShow#)
          END IF
        END IF

                         ' LendeeProfile displays borrower of displayed book
      CASE BORROWER
        CALL LendeeProfile(BigRec)

                         ' BooksBorrowed shows books borrowed by CardHolder
      CASE WHICHBOOKS
        IF Borrowed THEN CALL BooksBorrowed(BigRec)

                         ' If user hits "V" cycle through displayable tables
      CASE OTHERTABLE
        IF BigRec.TableNum < cDisplayedTables THEN
          BigRec.TableNum = BigRec.TableNum + 1
        ELSE
          BigRec.TableNum = 1
        END IF
        EXIT DO
                         ' If user picks "I" to check current book back in,
                         ' make sure it is out, then check it back in
      CASE CHECKIN
        IF Borrowed THEN
          GotIt = GetStatus(BigRec, DateToShow#)
          IF DateToShow# THEN
            CALL ReturnBook(BigRec, DateToShow#)
          END IF
        END IF
                         ' If user picks "O" to check current book out,
                         ' make sure it is available, then check it out
      CASE CHECKOUT
        GotIt = GetStatus(BigRec, DateToShow#)
          IF DateToShow# = 0# THEN
             CALL BorrowBook(BigRec)
          ELSE
             CALL ShowMessage("Sorry, this book is already checked out...", 0)
          END IF

                        ' If user wants to Undo all or some of a series of
                        ' uncommitted edits, make sure there is a pending
                        ' transaction to undo, then restore the state of the
                        ' file one step at a time, or altogether, depending
                        ' on whether U or ^U was entered.
      CASE UNDO, UNDOALL
        IF PendingFlag = TRUE THEN
          IF n < 1 THEN
            CALL ShowMessage("No pending edits left to Undo...", 0)
          ELSE
            IF answer = UNDO THEN
              Alert$ = "Restoring back to Savepoint # " + STR$(Marker(n))
              CALL ShowMessage(Alert$, 0)
              ROLLBACK Marker(n)
              n = n - 1
            ELSE                    ' If it's not UNDO, it must be UNDOALL
              CALL ShowMessage("Undoing the whole last series of edits", 0)
              ROLLBACK ALL
              n = 0
            END IF
         END IF
       ELSE
         CALL ShowMessage("There are no pending edits left to Undo...", 0)
       END IF

      CASE INVALIDKEY              ' Alert user if wrong key is pressed
        CALL ShowMessage(KEYSMESSAGE, 0)
        IF PendingFlag = TRUE THEN CALL DrawIndexBox(BigRec.TableNum, EDITRECORD)
    END SELECT
    CALL DrawHelpKeys(BigRec.TableNum)
    CALL ShowKeys(BigRec, BRIGHT + FOREGROUND, DimN, DimP)
  LOOP
LOOP
CLOSE
END

' This error handler takes care of the most common ISAM errors

MainHandler:

IF ERR = 73 THEN        ' 73 = Feature unavailable
  CALL ShowMessage("You forgot to load the ISAM TSR program", 0)
  END
ELSEIF ERR = 88 THEN        ' 88 = Database inconsistent
  ' If you have text files corresponding to each of the tables, then
  ' MakeOver prompts for their names and creates an ISAM file from them.
  CALL MakeOver(BigRec)
  RESUME NEXT

ELSEIF ERR = 83 THEN        ' 83 = Index not found
    CALL DrawScreen(BigRec.TableNum)
    CALL ShowMessage("Unable to set the index. Need more buffers?", 0)
    RESUME NEXT
ELSEIF ERR = 86 THEN        ' 86 = Duplicate value for unique index
    ' Trap errors when a user tries to enter a value for the Card Number or
    ' ID fields that duplicates a value already in the table
    IF answer = ADDRECORD THEN CALL DupeFixer(TempRec) ELSE CALL DupeFixer(BigRec)
    RESUME
ELSE
  Alert$ = "Sorry, not able to handle this error in BOOKLOOK: " + STR$(ERR)
  CALL ShowMessage(Alert$, 0)
  END
END IF

'***************************************************************************
'*  The AddOne FUNCTION is called once for each field when the user wants  *
'*  to add a record to the displayed table.                                *
'*                                Parameters                               *
'*  BigRec    RecStruct variable containing information on all tables      *
'*  EmptyRec  Empty record of same type as BigRec                          *
'*  TempRec   Temporary record record of same type as BigRec               *
'*  Answer    Integer passed through to EditField; tells task to perform   *
'***************************************************************************
FUNCTION AddOne (BigRec AS RecStruct, EmptyRec AS RecStruct, TempRec AS RecStruct, answer%)
  CALL ClearEm(BigRec.TableNum, 1, 1, 1, 1, 1, 1)
  LSET TempRec = EmptyRec
  CALL ShowMessage("Enter the first field of the new record", 0)
  TempRec.TableNum = BigRec.TableNum
  Edited = EditField(Argument%, TempRec, FirstLetter$, ADDRECORD, answer%)
  IF Edited THEN
    AddOne = -1
  ELSE
    AddOne = 0
  END IF
  COLOR FOREGROUND, BACKGROUND
END FUNCTION

'***************************************************************************
'* The CheckPosition SUB checks the table position after the requested user*
'* action is completed. If EOF follows a MOVENEXT or the user has chosen   *
'* MOVELAST, the Keys for Database Viewing/Editing box is updated to say   *
'* "No Next Record." If BOF  follows a MOVEPREVIOUS or user has chosen a   *
'* MOVEFIRST, "No Previous Record" is displayed.                           *
'* In either case, the position is held by executing MOVELAST or MOVEFIRST.*
'*                            Parameters:                                  *
'*   Big Rec      User-defined type containing all table information       *
'*   Answer       Tells what operation retrieve results from               *
'*   DimN & DimP  Flags telling which menu items should be dimmed/changed  *
'***************************************************************************
SUB CheckPosition (BigRec AS RecStruct, answer, DimN%, DimP%)
  SELECT CASE answer
    CASE GOAHEAD, ENDK
      IF EOF(BigRec.TableNum) OR (answer = ENDK) THEN
        CALL ShowMessage("This is the last record in this index", 0)
        DimN = TRUE: DimP = FALSE
        MOVELAST BigRec.TableNum
      ELSE                    ' If not EOF, turn on N
        DimN = FALSE: DimP = FALSE
        CALL EraseMessage
      END IF
    CASE GOBACK, HOME
      IF BOF(BigRec.TableNum) OR (answer = HOME) THEN
        CALL ShowMessage("This is the first record in this index", 0)
        DimP = TRUE: DimN = FALSE
        MOVEFIRST BigRec.TableNum
      ELSE
        DimP = FALSE: DimN = FALSE
        CALL EraseMessage
      END IF
  END SELECT
END SUB

'***************************************************************************
'* The ChooseOrder FUNCTION calls PlaceCursor so the user can move around  *
'* the form to pick the index to set.                                      *
'*                                  Parameters                             *
'*  BigRec       BigRec has all the table information in updated form      *
'*  EmptyRec     EmptyRec is same template as BigRec, but fields are empty *
'*  TempRec      Holds intermediate and temporary data                     *
'*  FirstLetter  Catches letter if user starts typing during SEEKFIELD     *
'*  Task         Either REORDER or SEEKFIELD - passed on to PlaceCursor    *
'***************************************************************************
FUNCTION ChooseOrder (BigRec AS RecStruct, EmptyRec AS RecStruct, TempRec AS RecStruct, FirstLetter$, Task%)
  CALL DrawTable(BigRec.TableNum)
  CALL DrawIndexBox(BigRec.TableNum, Task)
  Argument = TITLEFIELD                    ' Always start with first field
  TempRec = EmptyRec: TempRec.TableNum = BigRec.TableNum

  ' Pass temporary RecStruct variable so user can't trash BigRec
  value = PlaceCursor(Argument, TempRec, FirstLetter$, 1, Task)

  ' If the user chooses ESC, redraw everything, then exit to module level
  IF ASC(TempRec.WhichIndex) = 0 THEN
    CALL DrawIndexBox(BigRec.TableNum, Task)
    CALL ShowRecord(BigRec)
    CALL ShowMessage(KEYSMESSAGE, 0)
    ChooseOrder = 0
    EXIT FUNCTION
  ELSE                                ' Otherwise, if user makes a choice
    ChooseOrder = -1                  ' of Indexes, signal success to the
  END IF                              ' module-level code
END FUNCTION

'***************************************************************************
'*                                                                         *
'*  The DupeFixer SUB is called when the tries to enter a duplicate value  *
'*  for the BookStock table's IDnum column or the the CardHolders table's  *
'*  CardNum column, because their indexes are Unique. The procedure prompts*
'*  the user to enter a new value.                                         *
'***************************************************************************
SUB DupeFixer (BigRec AS RecStruct)
    IF BigRec.TableNum = cBookStockTableNum THEN
      DO
        Alert$ = STR$(BigRec.Inventory.IDnum) + " is not unique. "
        CALL ShowMessage(Alert$, 1)
        COLOR YELLOW + BRIGHT, BACKGROUND
        INPUT "Try another number: ", TempString$
        BigRec.Inventory.IDnum = VAL(TempString$)
      LOOP UNTIL BigRec.Inventory.IDnum
    ELSEIF BigRec.TableNum = cCardHoldersTableNum THEN
      DO
        Alert$ = STR$(BigRec.Lendee.CardNum) + " is not unique. "
        CALL ShowMessage(Alert$, 1)
        COLOR YELLOW + BRIGHT, BACKGROUND
        INPUT "Try another number: ", TempString$
        BigRec.Lendee.CardNum = VAL(TempString$)
      LOOP UNTIL BigRec.Lendee.CardNum
    END IF
    COLOR FOREGROUND, BACKGROUND
END SUB

'********************************* EditCheck SUB ***************************
'*                                                                         *
'* The EditCheck procedure monitors what the user wants to do, and if the  *
'* choice is EDITRECORD, makes sure that a transaction is begun, or if it  *
'* already has begun, continues it. If a transaction has been pending, and *
'* the user chooses anything except EDITRECORD, then the transaction is    *
'* committed.                                                              *
'*                                                                         *
'*                            Parameters:                                  *
'*   Pending      A flag that indicates whether transaction is pending     *
'*   Task         Tells what operation the user wants to perform now       *
'*   TablesRec    Structure containing information about the tables        *
'*                                                                         *
'***************************************************************************
SUB EditCheck (Pending, Task, TablesRec AS RecStruct)
  ' First, decide if this is a new or pending transaction, or not one at all
  ' The only transaction in this program keeps edits to the current record
  ' pending until the user moves on to a new record or a new operation
  ' (for example a Reorder).
SHARED n                          ' n is index to array of savepoint ids

      IF Task = EDITRECORD THEN
        IF Pending = FALSE THEN
          BEGINTRANS
          Pending = TRUE
        END IF
      ELSEIF Pending = TRUE THEN  ' Equivalent to Task<>EDITRECORD AND
        COMMITTRANS               ' Pending=TRUE
        Pending = FALSE
        n = 0                     ' Reset array index for savepoint ids
        CALL DrawIndexBox(TablesRec.TableNum, 0)
      END IF
END SUB

'***************************************************************************
'*  The GetInput FUNCTION takes the keystroke input by the user and returns*
'*  a constant indicating what the user wants to do. If the keystroke rep- *
'*  resents a valid operation, the choice is echoed to the screen.         *
'***************************************************************************
FUNCTION GetInput% (BigRec AS RecStruct)
DO
  answer$ = INKEY$
LOOP WHILE answer$ = EMPTYSTRING
  IF LEN(answer$) > 1 THEN
    RightSide = HighKeys%(answer$)
    GetInput = RightSide
  ELSE
    SELECT CASE answer$
      CASE "A", "a"
        CALL UserChoice(BigRec, ALINE, 7, "Add Record")
        GetInput% = ADDRECORD
      CASE "B", "b"
        IF BigRec.TableNum = cBookStockTableNum THEN
          CALL UserChoice(BigRec, WLINE, 28, "Borrower")
          GetInput% = BORROWER
        ELSE
          CALL UserChoice(BigRec, WLINE, 13, "Books Outstanding")
          GetInput% = WHICHBOOKS
        END IF
      CASE "O", "o"
        CALL UserChoice(BigRec, CLINE, 7, "Check Book Out")
        GetInput% = CHECKOUT
      CASE "I", "i"
        CALL UserChoice(BigRec, CLINE, 28, "Check In")
        GetInput% = CHECKIN
      CASE "D", "d"
        CALL UserChoice(BigRec, ALINE, 28, "Drop Record")
        GetInput% = TOSSRECORD
      CASE "N", "n"
        GetInput% = GOAHEAD
      CASE "P", "p"
        GetInput% = GOBACK
      CASE "Q", "q"
        CALL UserChoice(BigRec, ELINE, 28, "Quit")
        GetInput% = QUIT
      CASE "E", "e"
        CALL UserChoice(BigRec, ELINE, 7, "Edit Record")
        GetInput% = EDITRECORD
      CASE "F", "f"
        CALL UserChoice(BigRec, RLINE, 28, "Find Record")
        GetInput% = SEEKFIELD
      CASE "R", "r"
        CALL UserChoice(BigRec, RLINE, 7, "Reorder Records")
        GetInput% = REORDER
      CASE "V", "v"
        GetInput% = OTHERTABLE
      CASE "W", "w"
        CALL UserChoice(BigRec, WLINE, 7, "When Due Back")
        GetInput% = STATUS
      CASE CHR$(ESCAPE)
        GetInput% = ESCAPE
      CASE "U", "u"
        GetInput = UNDO       ' U signals rollback request after editing
      CASE CHR$(CTRLU)        ' ^U = rollback a whole series of edits
        GetInput = UNDOALL
      CASE ELSE
        GetInput% = INVALIDKEY
        BEEP
    END SELECT
  END IF
END FUNCTION

'**************************************************************************
'*  The HighKeys FUNCTION handles common two-byte keys input by the user. *
'*  The Answer parameter is the keystroke entered by the user.            *                                                          *
'**************************************************************************
FUNCTION HighKeys (answer AS STRING)
  SELECT CASE ASC(RIGHT$(answer$, 1))     ' Look at code for right byte
    CASE UP
      HighKeys = GOBACK                   ' UP is the up-arrow key
    CASE DOWN
      HighKeys = GOAHEAD                  ' DOWN is the down-arrow key
    CASE HOME
      HighKeys = HOME                     ' etc.
    CASE ENDK
      HighKeys = ENDK
      CASE LEFT
      HighKeys = OTHERTABLE
    CASE RIGHT
      HighKeys = OTHERTABLE
    CASE PGUP
      CALL ShowMessage("You could program so PGUP moves back n records", 0): SLEEP
      HighKeys = INVALIDKEY
    CASE PGDN
      CALL ShowMessage("You could program so PGDN moves forward n records", 0): SLEEP
      HighKeys = INVALIDKEY
    CASE ELSE
      CALL ShowMessage("Sorry, that key isn't handled yet.", 0): SLEEP
      HighKeys = INVALIDKEY
  END SELECT
END FUNCTION

'****************************** Retriever SUB ******************************
'* The Retriever SUB retrieves records from the database file and puts     *
'* them into the appropriate recordvariable for the table being displayed. *
'* An error trap is set in case the retrieve fails, in which case a message*
'* is displayed. Note that if a preceding SEEKoperand fails, EOF is TRUE.  *
'* In that case, position is set to the last record, which is retrieved.   *
'*                            Parameters:                                  *
'*   Big Rec      User-defined type containing all table information       *
'*   DimN & DimP  Flags telling which menu items should be dimmed/changed  *
'*   Task         Tells what operation retrieve results from               *
'***************************************************************************
SUB Retriever (BigRec AS RecStruct, DimN, DimP, Task)
  STATIC PeekFlag         ' Set this if user is just peeking at other table
  LOCATE , , 0            ' Turn off the cursor
  ' Show the user which choice was made, and whether EOF or BOF
  CALL ShowKeys(BigRec, FOREGROUND + BRIGHT, DimN, DimP)
  ' If table is empty, don't try to retrieve anything
  IF LOF(BigRec.TableNum) = 0 THEN
    DrawTable (BigRec.TableNum)
    CALL ShowMessage("There are no records in this table", 0): EXIT SUB
  END IF

  IF Task <> ENDK AND Task <> HOME THEN
    IF Task < EDITRECORD THEN                         ' Edit needs its
      CALL Indexbox(BigRec, CheckIndex%(BigRec, 0))   ' own prompts. Show
    ELSEIF Task > INVALIDKEY THEN                     ' indexbox otherwise
      IF Task <> ESC THEN CALL DrawIndexBox(BigRec.TableNum, 0)
      CALL Indexbox(BigRec, CheckIndex%(BigRec, 0))
    END IF
  END IF
  IF BOF(BigRec.TableNum) THEN MOVEFIRST (BigRec.TableNum)
  ON LOCAL ERROR GOTO LocalHandler           ' Trap errors on the retrieve.
  IF NOT EOF(BigRec.TableNum) THEN           ' Retrieve current record
    SELECT CASE BigRec.TableNum              ' from table being displayed
      CASE cBookStockTableNum                ' if EOF is not true
        RETRIEVE BigRec.TableNum, BigRec.Inventory
      CASE cCardHoldersTableNum
        RETRIEVE BigRec.TableNum, BigRec.Lendee
    END SELECT
  ELSE                                       ' If EOF is true, set position
    MOVELAST BigRec.TableNum                 ' to the last record in table,
    SELECT CASE BigRec.TableNum              ' then retrieve the record
      CASE cBookStockTableNum
        RETRIEVE BigRec.TableNum, BigRec.Inventory
      CASE cCardHoldersTableNum
        RETRIEVE BigRec.TableNum, BigRec.Lendee
    END SELECT
    DimN = TRUE
  END IF
  ON LOCAL ERROR GOTO 0                             ' Turn off error trap
  CALL ClearEm(BigRec.TableNum, 1, 1, 1, 1, 1, 1)
  CALL ShowRecord(BigRec)
  IF Task = OTHERTABLE THEN   ' If user is just peeking at the other table
    IF PeekFlag = 0 THEN      ' remind him how to get back to first table
      CALL ShowMessage("Press V to return to the other table", 0)
      PeekFlag = 1
    END IF
  ELSE
    PeekFlag = 0
  END IF
EXIT SUB

LocalHandler:
  IF ERR = 85 THEN
    CALL ShowMessage("Unable to retrieve your record...", 0)
  END IF
  RESUME NEXT
END SUB

'********************************* SeekRecord SUB *************************
'*  SeekRecord takes the name of the user's chosen index, sets it as the  *
'*  current index, then prompts the user to enter the value to seek. A    *
'*  minimal editor, MakeString, gets user input. If the SEEK is on a com- *
'*  bined index, GetKeyVals is called to get the input. Input is checked  *
'*  for minimal acceptability by ValuesOK. If it is OK, GetOperand is     *
'*  called to let the user specify how to conduct the SEEK.               *
'*                              Parameters:                               *
'*      TablesRec  Contains current record information for all tables     *
'*      TempRec    Contains the name of the index on which to seek (in    *
'*                 TempRec.WhichIndex element)                            *
'*      Letter$    If the user starts typing instead of pressing ENTER    *
'*                 Letter$ catches the keystroke, passes it to MakeString *
'**************************************************************************
SUB SeekRecord (TablesRec AS RecStruct, TempRec AS RecStruct, Letter$)
  DIM EmptyRec AS RecStruct             ' Make an empty record.
  IF LEFT$(Letter$, 1) < " " THEN       ' Exit if value is not a valid
                                        ' character, then redraw
    CALL DrawIndexBox(TablesRec.TableNum, SEEKFIELD)
    CALL Indexbox(TablesRec, CheckIndex%(TablesRec, TRUE))
    CALL ShowMessage("You must enter a valid string or numeric value", 0)
    EXIT SUB
  END IF
  TheTable = TablesRec.TableNum
  IndexName$ = RTRIM$(TempRec.WhichIndex)
  IF GETINDEX$(TheTable) <> IndexName$ THEN  ' If index to seek on is not
    ON LOCAL ERROR GOTO SeekHandler          ' current, set it now. Trap
    SETINDEX TheTable, IndexName$            ' possible failure of SETINDEX
    ON LOCAL ERROR GOTO 0                    ' then turn off error trap.
  END IF
  CALL AdjustIndex(TablesRec)                ' Show the current index
  TablesRec.WhichIndex = TempRec.WhichIndex
  TempRec = EmptyRec                         ' Clear TempRec for data
  TempRec.TableNum = TablesRec.TableNum
  ' Get the value to SEEK for from the user. The data type you assign the
  ' input to must be the same as the data in the database, so get it as a
  ' string with MakeString, then convert it to proper type for index. If
  ' the index is the combined index BigIndex, use GetKeyVals for input...

 SELECT CASE RTRIM$(LTRIM$(IndexName$))
   CASE "TitleIndexBS", "AuthorIndexBS", "PubIndexBS", "NameIndexCH", "StateIndexCH"
    Prompt$ = "Value To Seek: "
    Key1$ = MakeString$(ASC(Letter$), Prompt$): IF Key1$ = "" THEN EXIT SUB
   CASE "IDIndex", "CardNumIndexCH", "ZipIndexCH"
    ValueToSeek$ = MakeString$(ASC(Letter$), Prompt$)
    IF ValueToSeek$ = "" THEN EXIT SUB
    IF IndexName$ = "IDIndex" THEN
      NumberToSeek# = VAL(ValueToSeek$)
      Key1$ = ValueToSeek$
    ELSE
      NumberToSeek& = VAL(ValueToSeek$)
      Key1$ = ValueToSeek$
    END IF
   CASE "BigIndex"
    CALL GetKeyVals(TempRec, Key1$, Key2$, Key3#, Letter$)
    ValueToSeek$ = STR$(Key3#)
   CASE ""
     Alert$ = "Sorry, can't search for field values on the default index"
     CALL ShowMessage(Alert$, 0)
   CASE ELSE
  END SELECT

  ' Make sure the input values are minimally acceptable

  IF NOT ValuesOK(TablesRec, Key1$, Key2$, ValueToSeek$) THEN
    CALL ShowMessage("Sorry, problem with your entry. Try again!", 0)
    EXIT SUB
  END IF

  ' Show the user the values he entered in their appropriate fields
  CALL ClearEm(TablesRec.TableNum, 1, 1, 1, 1, 1, 1)
  CALL ShowIt(TempRec, IndexName$, TheTable, Key1$)

  ' GetOperand lets user specify the way the SEEK is to be conducted ---
  ' either  =, >, >=, <, or <= the value that was entered above

  DidIt = GetOperand%(Operand$)

  ' The actual SEEK has to be done according to two factors, the Index on
  ' which it is conducted, and the condition chosen in GetOperand. In the
  ' next section, case on the Operand returned, then IF and ELSEIF on the
  ' basis of the index on which the search is being conducted

  IF Operand$ <> "<>" THEN                ' "<>" represents user ESC choice

   SELECT CASE Operand$
    CASE "", "="                        ' If operand ="" or "=", use =
      IF IndexName$ = "BigIndex" THEN
        IF INSTR(Key2$, ",") = 0 THEN Key2$ = TransposeName(Key2$) ' a name
        SEEKEQ TheTable, Key1$, Key2$, Key3#
      ELSEIF IndexName$ = "NameIndexCH" OR IndexName$ = "AuthorIndexBS" THEN
        IF INSTR(Key1$, ",") = 0 THEN Key1$ = TransposeName(Key1$) ' a name
        SEEKEQ TheTable, LTRIM$(RTRIM$(Key1$))
      ELSEIF IndexName$ = "IDIndex" THEN
        SEEKEQ TheTable, NumberToSeek#
      ELSEIF IndexName$ = "CardNumIndexCH" OR IndexName$ = "ZipIndexCH" THEN
        SEEKEQ TheTable, NumberToSeek&
      ELSE
        SEEKEQ TheTable, Key1$
      END IF
    CASE ">="                      ' at least gets them close
      IF IndexName$ = "BigIndex" THEN
        IF INSTR(Key2$, ",") = 0 THEN Key2$ = TransposeName(Key2$) ' a name
        SEEKGE TheTable, Key1$, Key2$, Key3#
      ELSEIF IndexName$ = "NameIndexCH" OR IndexName$ = "AuthorIndexBS" THEN
        IF INSTR(Key1$, ",") = 0 THEN Key1$ = TransposeName(Key1$)
        SEEKGE TheTable, Key1$
      ELSEIF IndexName$ = "IDIndex" THEN
        SEEKGE TheTable, NumberToSeek#
      ELSEIF IndexName$ = "CardNumIndexCH" OR IndexName$ = "ZipIndexCH" THEN
        SEEKGE TheTable, NumberToSeek&
      ELSE
        SEEKGE TheTable, Key1$
      END IF
    CASE ">"
      IF IndexName$ = "BigIndex" THEN
        IF INSTR(Key2$, ",") = 0 THEN Key2$ = TransposeName(Key2$)
        SEEKGT TheTable, Key1$, Key2$, Key3#
      ELSEIF IndexName$ = "NameIndexCH" OR IndexName$ = "AuthorIndexBS" THEN
        IF INSTR(Key1$, ",") = 0 THEN Key1$ = TransposeName(Key1$)
        SEEKGT TheTable, Key1$
      ELSEIF IndexName$ = "IDIndex" THEN
        SEEKGT TheTable, NumberToSeek#
      ELSEIF IndexName$ = "CardNumIndexCH" OR IndexName$ = "ZipIndexCH" THEN
        SEEKGT TheTable, NumberToSeek&
      ELSE
        SEEKGT TheTable, Key1$
      END IF
    CASE "<="
      IF IndexName$ = "BigIndex" THEN
        IF INSTR(Key2$, ",") = 0 THEN Key2$ = TransposeName(Key2$)
        SEEKGT TheTable, Key1$, Key2$, Key3#
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "NameIndexCH" OR IndexName$ = "AuthorIndexBS" THEN
        IF INSTR(Key1$, ",") = 0 THEN Key1$ = TransposeName(Key1$)
        SEEKGT TheTable, Key1$
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "IDIndex" THEN
        SEEKGT TheTable, NumberToSeek#
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "CardNumIndexCH" OR IndexName$ = "ZipIndexCH" THEN
        SEEKGT TheTable, NumberToSeek&
        MOVEPREVIOUS TheTable
      ELSE
        SEEKGT TheTable, Key1$
        MOVEPREVIOUS TheTable
      END IF
    CASE "<"
      IF IndexName$ = "BigIndex" THEN
        IF INSTR(Key2$, ",") = 0 THEN Key2$ = TransposeName(Key2$)
        SEEKGE TheTable, Key1$, Key2$, Key3#
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "NameIndexCH" OR IndexName$ = "AuthorIndexBS" THEN
        IF INSTR(Key1$, ",") = 0 THEN Key1$ = TransposeName(Key1$)
        SEEKGE TheTable, Key1$
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "IDIndex" THEN
        SEEKGE TheTable, NumberToSeek#
        MOVEPREVIOUS TheTable
      ELSEIF IndexName$ = "CardNumIndexCH" OR IndexName$ = "ZipIndexCH" THEN
        SEEKGE TheTable, NumberToSeek&
        MOVEPREVIOUS TheTable
      ELSE
        SEEKGE TheTable, Key1$
        MOVEPREVIOUS TheTable
      END IF
    CASE ELSE
      Alert$ = "The returned operand was " + Operand$
      CALL ShowMessage(Alert$, 0)
      SLEEP
  END SELECT
 ELSE                        ' If they choose ESC, go back to module level
   CALL DrawScreen(TheTable)
   CALL ShowRecord(TablesRec)
   Alert$ = "You've escaped. " + KEYSMESSAGE
   CALL ShowMessage(Alert$, 0)
   SLEEP
   Operand$ = ""
 END IF
  CALL EraseMessage
  CALL DrawScreen(TheTable)
  CALL Indexbox(TablesRec, CheckIndex%(TablesRec, FALSE))
 IF EOF(TablesRec.TableNum) THEN
  Alert$ = "Sorry,  unable to match value you entered with any field value"
  CALL ShowMessage(Alert$, 0):  ' SLEEP: EraseMessage
 END IF

EXIT SUB

SeekHandler:
 IF ERR = 83 THEN                          ' 83 = Index not found
    CALL DrawScreen(TablesRec.TableNum)
    Alert$ = "SETINDEX for " + IndexName$ + " failed. Need more buffers?"
    CALL ShowMessage(Alert$, 0)
    EXIT SUB
 END IF

END SUB   ' End of SeekRecord procedure
