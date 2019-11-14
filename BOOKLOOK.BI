DEFINT A-Z
' Define constants for TRUE and FALSE
CONST FALSE = 0, TRUE = NOT FALSE
 
' Define constants for the database file & table names
CONST cBookStockTableNum = 1, cCardHoldersTableNum = 2, cBooksOutTableNum = 3
CONST cDisplayedTables = 2

' Define names similar to keyboard names with their equivalent key codes.
CONST SPACE = 32, ESC = 27, ENTER = 13, TABKEY = 9, ESCAPE = 27, BACKSPACE = 8
CONST DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
CONST HOME = 71, ENDK = 79, PGDN = 81, PGUP = 73
CONST INS = 82, DEL = 83, NULL = 0, EMPTYSTRING = ""
CONST CTRLU = 21

' Define English names for color-specification numbers. Add BRIGHT to
' any color to get bright version.
CONST BLACK = 0, BLUE = 1, GREEN = 2, CYAN = 3, RED = 4, MAGENTA = 5
CONST YELLOW = 6, WHITE = 7, BRIGHT = 8

' Assign colors to different kinds of text. By changing the color assigned,
' you can change the color of the display. The initial colors are
' chosen because they work for color or black-and-white displays.
CONST BACKGROUND = BLACK, NORMAL = WHITE, HILITE = WHITE + BRIGHT
CONST FOREGROUND = CYAN

' Screen positions - Initialized for 25 rows. Screen positions can be
' modified for 43-row mode if you have an EGA or VGA adapter.

CONST TABLETOP = 1, TABLEEND = 14
CONST BOXTOP = 16, NLINE = 17, RLINE = 18, WLINE = 19, VLINE = 20, ALINE = 21
CONST ELINE = 22, CLINE = 23, BOXEND = 24, INDBOX = 42, HELPCOL = 1
                                  
CONST SCREENWIDTH = 74

CONST MESBOXTOP = TABLEEND, MESFIELD = TABLEEND + 1
CONST MESBOXEND = BOXTOP

CONST DATATOP = 2, DATAEND = 13, NAMEFIELD = 3, TITLEFIELD = 3
CONST STREETFIELD = 5, AUTHORFIELD = 5, CITYFIELD = 7, PUBFIELD = 7
CONST STATEFIELD = 9, EDFIELD = 9, ZIPFIELD = 11, PRICEFIELD = 11
CONST CARDNUMFIELD = 13, IDFIELD = 13

CONST QUIT = 0, GOAHEAD = 1, GOBACK = 2, BORROWER = 3, WHICHBOOKS = 4
CONST SEEKFIELD = 5, REORDER = 6, STATUS = 7, OTHERTABLE = 8, TOSSRECORD = 9
CONST ADDRECORD = 10, CHECKOUT = 11, CHECKIN = 12
CONST EDITRECORD = 13, UNDO = 14, UNDOALL = 15, INVALIDKEY = 16

CONST BIGINDEX = 20, NULLINDEX = 21

CONST KEYSMESSAGE = " Press one of the Viewing/Editing keys  "

TYPE BookStatus
  IDnum     AS DOUBLE
  CardNum   AS LONG
  DueDate   AS DOUBLE
END TYPE
TYPE Borrowers
  CardNum         AS LONG
  Zip             AS LONG
  TheName         AS STRING * 36
  City            AS STRING * 26
  Street          AS STRING * 50
  State           AS STRING * 2
END TYPE

TYPE Books
  IDnum     AS DOUBLE
  Price     AS CURRENCY
  Edition   AS INTEGER
  Title     AS STRING * 50
  Publisher AS STRING * 50
  Author    AS STRING * 36
END TYPE

TYPE RecStruct                  ' This structure contains each of the other
  TableNum    AS INTEGER        ' other table structures. When you pass a
  WhichIndex  AS STRING * 40    ' reference to this structure to procedures
  Inventory   AS Books          ' the procedure decodes the TableNum
  Lendee      AS Borrowers      ' element, then deals with the proper table.
  OutBooks    AS BookStatus     ' WhichIndex is used to communicate the index
END TYPE                        ' the user wants to set.

DECLARE SUB BooksBorrowed (TablesRec AS ANY)
DECLARE SUB ShowRecord (TablesRec AS ANY)
DECLARE SUB ShowIt (TablesRec AS RecStruct, WhichIndex$, WhichTable%, StringToShow$)
DECLARE SUB UserChoice (BigRec AS RecStruct, Row%, Column%, Feedback$)
DECLARE SUB DrawHelpBox ()
DECLARE SUB BooksOutBox (OutBookNames() AS STRING, Header$, Footer$, BiggestYet%, Num%)
DECLARE SUB CheckPosition (BigRec AS RecStruct, Answer%, DimN%, DimP%)
DECLARE SUB EditCheck (Pending%, Task%, TablesRec AS RecStruct)
DECLARE SUB LendeeProfile (TablesRec AS RecStruct)
DECLARE SUB Retriever (BigRec AS RecStruct, DimN%, DimP%, Task%)
DECLARE SUB ClearEm (TableNum%, Field1%, Field2%, Field3%, Field4%, Field5%, Field6%)
DECLARE SUB EraseMessage ()
DECLARE SUB Indexbox (TablesRec AS RecStruct, MoveDown%)
DECLARE SUB MakeOver (BigRec AS RecStruct)
DECLARE SUB ShowKeys (TablesRec AS RecStruct, ForeGrnd%, TableDone%, TableStart%)
DECLARE SUB ShowMessage (Message$, Cursor%)
DECLARE SUB GetKeyVals (TablesRec AS ANY, Key1$, Key2$, Key3#, Letter$)
DECLARE SUB GetKeyVals (TablesRec AS ANY, Key1$, Key2$, Key3#, Letter$)
DECLARE SUB TossKey ()
DECLARE SUB DrawHelpKeys (TableNum AS INTEGER)
DECLARE SUB DrawIndexBox (TableNum AS INTEGER, Task%)
DECLARE SUB DrawScreen (TableNum AS INTEGER)
DECLARE SUB DrawTable (TableNum AS INTEGER)
DECLARE SUB AdjustIndex (TablesRec AS RecStruct)
DECLARE SUB SeekRecord (TablesRec AS RecStruct, TempRec AS RecStruct, Letter$)
DECLARE SUB ShowStatus (Stat$, ValueToShow AS DOUBLE)
DECLARE SUB ReturnBook (TablesRec AS RecStruct, DueDate#)
DECLARE SUB BorrowBook (TablesRec AS RecStruct)
DECLARE SUB PeekWindow (OutBookNames() AS STRING, Header$, Footer$, BiggestYet%)
DECLARE SUB DupeFixer (BigRec AS ANY)
DECLARE FUNCTION CatchKey% ()
DECLARE FUNCTION ValuesOK% (BigRec AS ANY, Key1$, Key2$, ValueToSeek$)
DECLARE FUNCTION RetrieveFailure% (ErrorNum%, Origin$)
DECLARE FUNCTION TransposeName$ (TheName AS STRING)
DECLARE FUNCTION ReturnKey$ ()
DECLARE FUNCTION GetStatus% (TablesRec AS RecStruct, DateToShow#)
DECLARE FUNCTION ChangeRecord% (FirstLetter$, Argument%, TablesRec AS RecStruct, Task AS INTEGER)
DECLARE FUNCTION CheckIndex% (TablesRec AS RecStruct, FirstTime%)
DECLARE FUNCTION ConfirmEntry% (Letter$)
DECLARE FUNCTION EdAddCursor% (NextField%, Job%, TablesRec AS RecStruct, FirstShot%)
DECLARE FUNCTION EditField% (Argument%, TablesRec AS RecStruct, FirstLetter$, Task%, Answer%)
DECLARE FUNCTION GetInput% (BigRec AS RecStruct)
DECLARE FUNCTION GetOperand% (HoldOperand$)
DECLARE FUNCTION HighKeys% (Answer AS STRING)
DECLARE FUNCTION MakeString$ (FilterTrap AS INTEGER, Prompt$)
DECLARE FUNCTION OrderCursor% (Index%, NextField%, Job%, TablesRec AS RecStruct, Letter$)
DECLARE FUNCTION PlaceCursor% (WhichField%, TablesRec AS RecStruct, FirstLetter$, FirstTime AS INTEGER, Task AS INTEGER)
DECLARE FUNCTION Reader% (BigRec AS RecStruct, SeqFile%)
DECLARE FUNCTION AddOne% (BigRec AS RecStruct, EmptyRec AS RecStruct, TempRec AS RecStruct, Answer%)
DECLARE FUNCTION ChooseOrder% (BigRec AS RecStruct, EmptyRec AS RecStruct, TempRec AS RecStruct, FirstLetter$, Task%)
DECLARE FUNCTION ReturnKey$ ()
DECLARE FUNCTION Borrowed ()

