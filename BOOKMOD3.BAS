'***************************************************************************
'* This is module level code for BOOKMOD3.BAS, the fourth                  *
'* module of BOOKLOOK.BAS.                                                 *
'*                                                                         *
'* The module contains a procedure, MakeOver, you can use to convert text  *
'* files containing the right format and type of information for the tables*
'* used by the BOOKLOOK program to a .MDB file. However, you need to call  *
'* MakeOver from the Immediate Window, and in order for it to work, you    *
'* must use the PROISAMD version of the TSR, because MakeOver needs the    *
'* data dictionary functionality for creating indexes, etc.                *
'* If you use the DTFMTER.QLB library functions you must include the files *
'* DATIM.BI and FORMAT.BI at this level, using syntax as shown below.      *
'***************************************************************************
DEFINT A-Z
'$INCLUDE: 'booklook.bi'

'***************************************************************************
'*  The BooksBorrowed SUB takes the CardNum in BooksOut associated with the*
'*  currently displayed CardHolder, then looks up each book in BooksOut    *
'*  assigned to that CardNum. Note that you can use SEEKoperand to find the*
'*  first matching record, but thereafter you need to MOVENEXT and check   *
'*  each succeeding record to see if the CardNum matches. When a match is  *
'*  made, look up the IDnum in the BooksOut table and retrieve the title.  *
'*  Put all the titles in the Titles array, then display with PeekWindow.  *
'*                                   Parameters                            *
'*  TablesRec   Structure containing information on all database tables    *
'***************************************************************************
SUB BooksBorrowed (TablesRec AS RecStruct)
	DIM Titles(50) AS STRING
	' First, get the card number of the current record in Bookstock - then
	' at the end of this procedure, restore that book
	IF LOF(cBooksOutTableNum) = 0 THEN EXIT SUB
	IF GETINDEX$(cBooksOutTableNum) <> "CardNumIndexBO" THEN
		SETINDEX cBooksOutTableNum, "CardNumIndexBO"
	END IF
	RevName$ = TransposeName$(TablesRec.Lendee.TheName)
	SEEKEQ cBooksOutTableNum, TablesRec.Lendee.CardNum
	 IF NOT EOF(cBooksOutTableNum) THEN
		DO
			RETRIEVE cBooksOutTableNum, TablesRec.OutBooks
			 IF TablesRec.OutBooks.CardNum = TablesRec.Lendee.CardNum THEN
					IF GETINDEX$(cBookStockTableNum) <> "IDIndex" THEN
						SETINDEX cBookStockTableNum, "IDIndex"
					END IF
					SEEKEQ cBookStockTableNum, TablesRec.OutBooks.IDnum
					IF NOT EOF(cBookStockTableNum) THEN
						RETRIEVE cBookStockTableNum, TablesRec.Inventory
						Titles(Index) = RTRIM$(TablesRec.Inventory.Title)
						ThisSize = LEN(RTRIM$(Titles(Index)))
							IF ThisSize > Biggest THEN
								Biggest = ThisSize
							END IF
					 Index = Index + 1
					END IF
				END IF
		MOVENEXT cBooksOutTableNum
		LOOP UNTIL EOF(cBooksOutTableNum)
	ELSE
		Alert$ = RevName$ + " currently has no books checked out"
		CALL ShowMessage(Alert$, 0)
	END IF
	IF Index <> 0 THEN
		HeadMessage$ = " Books borrowed by " + RevName$ + " "
		FootMessage$ = " Press a key to continue "
		CALL PeekWindow(Titles(), HeadMessage$, FootMessage$, Biggest)
		CALL DrawTable(TablesRec.TableNum)
		CALL ShowMessage(KEYSMESSAGE, 0)
	END IF
END SUB

'***************************************************************************
'*  The BorrowBook SUB prompts the user to enter the name of the Cardholder*
'*  who wants to borrow the book, then updates all the other tables accord-*
'*  ingly. The name or cardnumber can be entered --- if conversion to a    *
'*  number fails, the user entered a name. If the name isn't of the right  *
'*  format, it is transposed to last-first, comma delimited. If no exact   *
'*  match is found, the next best match is attempted and presented for the *
'*  approval of the user.
'*                                  Parameter                              *
'*  TablesRec   RecStruct type variable holding current table information  *
'***************************************************************************
SUB BorrowBook (TablesRec AS RecStruct)

DIM SaveBook AS RecStruct
DIM PeekString(10) AS STRING

Prompt$ = "Name or Card Number to Seek: "
SaveBook = TablesRec                          ' Save book information
	' Prompt user and catch keystroke
CALL ShowMessage("Enter borrower cardnumber or name: ", 1)
FirstChar = ASC(ReturnKey$)                   ' ReturnKey$ is a function
IF FirstChar = ESCAPE THEN CALL ShowMessage("", 0): EXIT SUB
Answer$ = MakeString$(FirstChar, Prompt$)
IF FirstChar = ESCAPE THEN CALL ShowMessage("", 0): EXIT SUB
NumToCheck& = VAL(Answer$)
IF NumToCheck& = 0 THEN
	IF INSTR(Answer$, ",") = 0 THEN
		StraightName$ = Answer$
		Answer$ = TransposeName$(Answer$)
	ELSE
		StraightName$ = TransposeName$(Answer$)
	END IF

	SETINDEX cCardHoldersTableNum, "NameIndexCH"
	SEEKEQ cCardHoldersTableNum, Answer$
	IF EOF(cCardHoldersTableNum) THEN
		MOVEFIRST cCardHoldersTableNum
		SEEKGE cCardHoldersTableNum, Answer$     ' If EQ fails, try GE
		IF EOF(cCardHoldersTableNum) THEN
			Alert$ = "Sorry, couldn't find " + StraightName$ + " in CardHolders table..."
			CALL ShowMessage(Alert$, 0)
			EXIT SUB
		END IF
	END IF
	IF NOT EOF(cCardHoldersTableNum) THEN
		RETRIEVE cCardHoldersTableNum, SaveBook.Lendee
		IF TEXTCOMP(LEFT$(SaveBook.Lendee.TheName, 2), LEFT$(Answer$, 2)) = 0 THEN
			NumToCheck& = SaveBook.Lendee.CardNum
		ELSE
			Alert$ = "Sorry, couldn't match " + StraightName$ + " in CardHolders table..."
			CALL ShowMessage(Alert$, 0): ' SLEEP: EraseMessage
			EXIT SUB
		END IF
	END IF
ELSE
	SETINDEX cCardHoldersTableNum, "CardNumIndexCH"
	SEEKEQ cCardHoldersTableNum, NumToCheck&
	IF EOF(cCardHoldersTableNum) THEN
		Alert$ = "Sorry, could not match " + Answer$
		CALL ShowMessage(Alert$, 0):  ' SLEEP: EraseMessage
		EXIT SUB
	ELSE
		RETRIEVE cCardHoldersTableNum, SaveBook.Lendee
		NumToCheck& = SaveBook.Lendee.CardNum
	END IF
END IF
												' You can replace this phoney date with a call to
DateDue# = 32950#     ' the Date/Time library as shown on these 2 lines:
'DateDue# = Now# + 30#
'DateDue$ = STR$(Month&(DateDue#)) + "/" + LTRIM$(STR$(Day&(DateDue#))) + "/" + LTRIM$(STR$(Year&(DateDue#)))

' Show the information on the Cardholder you found...
DO
	PeekString(0) = " This book will be checked out to: "
	PeekString(1) = ""
	PeekString(2) = RTRIM$(SaveBook.Lendee.TheName)
	PeekString(3) = RTRIM$(SaveBook.Lendee.Street)
	PeekString(4) = RTRIM$(SaveBook.Lendee.City) + ", " + RTRIM$(SaveBook.Lendee.State)
	PeekString(5) = "Card number: " + STR$(SaveBook.Lendee.CardNum)
	PeekString(6) = ""
	PeekString(7) = "The Due Date will be " + STR$(DateDue# + 30)
	IF LEN(DateDue$) THEN PeekString(7) = "The Due Date will be " + DateDue$
	FOR Index = 0 TO 8
		ThisSize = LEN(RTRIM$(PeekString(Index)))
		IF ThisSize > Biggest THEN
			Biggest = ThisSize
		END IF
	NEXT Index

	HeadMessage$ = " Cardholder checking out this book "
	FootMessage$ = " Press ENTER to confirm this checkout "
	Alert$ = "Press N seek next similar match, ESC to abort checkout"
	CALL ShowMessage(Alert$, 0)
	CALL PeekWindow(PeekString(), HeadMessage$, FootMessage$, Biggest)

	' Let the user press "N" to see the next best match, ESC to abort checkout
	' anything else to confirm this as person to whom to check book out to

	Reply$ = ReturnKey$
	SELECT CASE Reply$
		CASE CHR$(ESCAPE)
			DoneFlag = TRUE
		CASE "N", "n"
			MOVENEXT cCardHoldersTableNum
			IF EOF(cCardHoldersTableNum) THEN
				DoneFlag = TRUE
			ELSE
				RETRIEVE cCardHoldersTableNum, SaveBook.Lendee
				NumToCheck& = SaveBook.Lendee.CardNum
				IF LEFT$(SaveBook.Lendee.TheName, 2) <> LEFT$(Answer$, 2) THEN
					DoneFlag = TRUE
				END IF
			END IF
		CASE ELSE
				TablesRec.OutBooks.CardNum = NumToCheck&
				TablesRec.OutBooks.IDnum = SaveBook.Inventory.IDnum
				TablesRec.OutBooks.DueDate = DateDue#
				DoneFlag = TRUE
				MOVEFIRST (cBooksOutTableNum)
				INSERT cBooksOutTableNum, TablesRec.OutBooks
				CALL ShowMessage("", 0)
	END SELECT
LOOP UNTIL DoneFlag

CALL DrawTable(TablesRec.TableNum)
CALL ShowMessage(KEYSMESSAGE, 0)
	
END SUB

'**************************************************************************
'*  The Borrowed FUNCTION simply makes sure there are records in the      *
'*  BooksOut table. If there are none, a message is displayed             *
'**************************************************************************
FUNCTION Borrowed
	IF LOF(cBooksOutTableNum) = 0 THEN
		CALL ShowMessage("Sorry, no records in the BooksOut table", 0): SLEEP
		Borrowed = FALSE
	ELSE
		Borrowed = TRUE
	END IF
END FUNCTION

'***************************************************************************
'* The CatchKey function gets a keystroke and returns TRUE if it was ENTER,*
'* otherwise it returns FALSE.                                             *
'***************************************************************************
FUNCTION CatchKey%
	DO
	Answer$ = INKEY$
	LOOP WHILE Answer$ = ""
	SELECT CASE ASC(Answer$)
		CASE ENTER
			CatchKey% = -1
		CASE ELSE
			CatchKey% = 0
	END SELECT
END FUNCTION

'***************************************************************************
'*  The GetStatus FUNCTION looks up the status of a book in the BooksOut   *
'*  table. If the SEEK fails it means the book isn't checked out, and that *
'*  message is displayed. Otherwise, it is placed in DateToShow parameter. *
'*  The final message about retrieving borrow info relates to LendeeProfile*
'*                                   Parameters                            *
'*  TablesRec     Structure containing the information about all the tables*
'*  DateToShow    The due date to show in the ShowStatus SUB               *
'***************************************************************************
FUNCTION GetStatus (TablesRec AS RecStruct, DateToShow#)
		IF GETINDEX$(cBooksOutTableNum) <> "IDIndexBO" THEN
			SETINDEX cBooksOutTableNum, "IDIndexBO"
		END IF
		SEEKEQ cBooksOutTableNum, TablesRec.Inventory.IDnum
		IF NOT EOF(cBooksOutTableNum) THEN
			RETRIEVE cBooksOutTableNum, TablesRec.OutBooks
		ELSE
			Alert$ = "This book is not checked out"   ' the book wasn't in BooksOut
			CALL ShowMessage(Alert$, 0)               ' table, so it wasn't out
			DateToShow# = 0: GetStatus = FALSE
			EXIT FUNCTION
		END IF
		DateToShow# = TablesRec.OutBooks.DueDate#
		GetStatus = TRUE
END FUNCTION

'***************************************************************************
'*  The LendeeProfile takes the IDnum of the currently displayed book, then*
'*  looks that up in the BooksOut table and fetches the CardHolder record  *
'*  that corresponds to the CardNum entry in BooksOut. Then the CardNum is *
'*  looked up in the CardHolders table and the borrower information shown. *
'*                                Parameters                               *
'*  TablesRec   Contains information on all the tables in the database     *
'***************************************************************************
SUB LendeeProfile (TablesRec AS RecStruct)
	' Make sure the CardHolders table actually has records
	IF LOF(cCardHoldersTableNum) = 0 THEN
		CALL ShowMessage("Sorry, there are no cardholder records", 0): SLEEP
		EXIT SUB
	END IF
	' Create an array to hold information from CardHolders table
	DIM LendeeInfo(10)  AS STRING
	' Set the index if it is not the one you want
	IF GETINDEX$(cBooksOutTableNum) <> "IDIndexBO" THEN
		SETINDEX cBooksOutTableNum, "IDIndexBO"
	END IF
	SEEKEQ cBooksOutTableNum, TablesRec.Inventory.IDnum    ' Seek the record.
	IF EOF(cBooksOutTableNum) THEN                         ' If you find it,
		CALL ShowMessage("This book is not checked out", 0)  ' the book is out,
		EXIT SUB                                             ' otherwise not.
	ELSE                                                   ' If it's there,
		RETRIEVE cBooksOutTableNum, TablesRec.OutBooks       ' fetch it.

		' If the CardNum exists, set an index in CardHolders and SEEK the
		' CardNum. If SEEK fails, print a warning; if it succeeds, get the
		' information about the borrower, and display it using PeekWindow

		IF TablesRec.OutBooks.CardNum <> 0 THEN
			IF GETINDEX$(cCardHoldersTableNum) <> "CardNumIndexCH" THEN
				SETINDEX cCardHoldersTableNum, "CardNumIndexCH"
			END IF
			SEEKEQ cCardHoldersTableNum, TablesRec.OutBooks.CardNum
			IF EOF(cBooksOutTableNum) THEN
				Alert$ = "Cardholder number associated with book ID is not valid"
				CALL ShowMessage(Alert$, 0)
				EXIT SUB
			ELSE
				RETRIEVE cCardHoldersTableNum, TablesRec.Lendee
				LendeeInfo(0) = RTRIM$(TablesRec.Lendee.TheName)
				LendeeInfo(1) = ""
				LendeeInfo(2) = RTRIM$(TablesRec.Lendee.Street)
				LendeeInfo(3) = RTRIM$(TablesRec.Lendee.City)
				LendeeInfo(4) = RTRIM$(TablesRec.Lendee.State)
				LendeeInfo(5) = LTRIM$(STR$(TablesRec.Lendee.Zip))
				LendeeInfo(7) = STR$(TablesRec.Lendee.CardNum)
				LendeeInfo(6) = ""
				LendeeInfo(7) = "Card number: " + LendeeInfo(7)
				LendeeInfo(8) = ""
				FOR Index = 1 TO 6
					ThisBig = LEN(LendeeInfo(Index))
					IF ThisBig > BiggestYet THEN
						BiggestYet = ThisBig
					END IF
				NEXT Index
				Alert$ = "Press V to access the record for this cardholder"
				CALL ShowMessage(Alert$, 0)
				HeadMessage$ = "Borrower of this Book"
				FootMessage$ = "Press a key to clear box"
				CALL ClearEm(TablesRec.TableNum, 1, 1, 1, 1, 1, 1)
				CALL PeekWindow(LendeeInfo(), HeadMessage$, FootMessage$, BiggestYet)
				CALL DrawTable(TablesRec.TableNum)
				CALL ShowMessage(KEYSMESSAGE, 0)
			END IF
		END IF
	END IF
END SUB

'***************************************************************************
'* The MakeOver SUB lets the user input the names of properly formatted    *
'* text files, then creates a database file of the same type as BOOKS.MDB. *
'* There is also a prompt for the new database name. The text files must   *
'* contain comma-delimited fields, with strings within double quote marks. *
'* The last part of this SUB demonstrates how indexes are created. You need*
'* to have loaded PROISAMD.EXE to run this procedure.                      *
'*                            Parameters:                                  *
'*   Big Rec      User-defined type containing all table information       *
'***************************************************************************
'
SUB MakeOver (BigRec AS RecStruct)
	CLOSE
	Alert$ = "Type name of file containing Cardholders table data: "
	CALL ShowMessage(Alert$, 1)
	INPUT "", CardFile$
	Alert$ = "Type name of file containing BooksOut table data: "
	CALL ShowMessage(Alert$, 1)
	INPUT "", OutBooks$
	Alert$ = "Type name of file containing BookStock table data: "
	CALL ShowMessage(Alert$, 1)
	INPUT "", BookFile$
	Alert$ = "Type name of ISAM file to create: "
	CALL ShowMessage(Alert$, 1)
	INPUT "", IsamFile$
	IF UCASE$(IsamFile$) = "BOOKS.MDB" THEN KILL "BOOKS.MDB"
	CALL ShowMessage("Loading database...", 0)

	CLOSE
	ON LOCAL ERROR GOTO FileHandler
	LenFileNo% = 10
	OPEN CardFile$ FOR INPUT AS LenFileNo%
	OutFileNo% = 11
	OPEN OutBooks$ FOR INPUT AS OutFileNo%
	RecFileNo% = 12
	OPEN BookFile$ FOR INPUT AS RecFileNo%
	ON ERROR GOTO 0
	
	' Open the database and the BookStock table
	OPEN IsamFile$ FOR ISAM Books "BookStock" AS cBookStockTableNum
	OPEN IsamFile$ FOR ISAM Borrowers "CardHolders" AS cCardHoldersTableNum
	OPEN IsamFile$ FOR ISAM BookStatus "BooksOut" AS cBooksOutTableNum
	CALL ShowMessage(" Opened all isam tables", 0)

	SeqFile% = LenFileNo
	DO WHILE (Reader%(BigRec, SeqFile%))
	 INSERT cCardHoldersTableNum, BigRec.Lendee
	LOOP
	SeqFile% = OutFileNo
	DO WHILE (Reader%(BigRec, SeqFile))
	 INSERT cBooksOutTableNum, BigRec.OutBooks
	LOOP
	SeqFile = RecFileNo
	DO WHILE (Reader%(BigRec, SeqFile))
	 INSERT cBookStockTableNum, BigRec.Inventory
	LOOP
	CALL ShowMessage("Finished reading in records---Indexes next", 0)
' These indexes are already in the BOOKS.MDB database --- the following
' is the syntax that was used to create them

	ON LOCAL ERROR GOTO FileHandler
	CREATEINDEX cBookStockTableNum, "TitleIndexBS", 0, "Title"
	CREATEINDEX cBookStockTableNum, "AuthorIndexBS", 0, "Author"
	CREATEINDEX cBookStockTableNum, "PubIndexBS", 0, "Publisher"
	CREATEINDEX cBookStockTableNum, "IDIndex", 1, "IDnum"     ' Note unique index
	CREATEINDEX cBookStockTableNum, "BigIndex", 0, "Title", "Author", "IDnum"

	CREATEINDEX cBooksOutTableNum, "IDIndexBO", 0, "IDnum"
	CREATEINDEX cBooksOutTableNum, "CardNumIndexBO", 0, "CardNum"

	CREATEINDEX cCardHoldersTableNum, "NameIndexCH", 0, "TheName"
	CREATEINDEX cCardHoldersTableNum, "StateIndexCH", 0, "State"
	CREATEINDEX cCardHoldersTableNum, "ZipIndexCH", 0, "Zip"
	CREATEINDEX cCardHoldersTableNum, "CardNumIndexCH", 1, "CardNum"  ' Unique index
	ON ERROR GOTO 0
	CALL ShowMessage(" All done with indexes...", 0)
	'CLOSE

	EXIT SUB

FileHandler:
	IF ERR = 73 THEN
		CALL ShowMessage("You need to Exit QBX and load PROISAMD /Ib:24 /Ii:16", 0)
	ELSEIF ERR = 10 THEN
		Alert$ = "Finished appending the records to " + IsamFile$
		CALL ShowMessage(Alert$, 0)
		END
	ELSEIF ERR = 86 THEN
		Alert$ = "Tried to add record with duplicate value on a unique index"
		CALL ShowMessage(Alert$, 0)
		ELSE
		CALL ShowMessage("Can't find textfiles needed to make the database", 0)
	END IF
	END
END SUB

'***************************************************************************
'*  The PeekWindow SUB displays the elements of the OutBookNames array in  *
'*  a window on top of the currently displayed table.                      *
'*                                Parameters                               *
'*  OutBookNames    Array of strings containing lines displayed in window  *
'*  Header$         String to show at top of window                        *
'*  Footer$         String to show at bottom of window                     *
'*  BiggestYet      Length of the longest string to be shown               *
'***************************************************************************
SUB PeekWindow (OutBookNames() AS STRING, Header$, Footer$, BiggestYet%)
HeadLen = LEN(Header$)        ' + 4
FootLen = LEN(Footer$)        ' + 4
IF HeadLen > FootLen THEN Bigger = HeadLen ELSE Bigger = FootLen
IF Bigger > BiggestYet THEN BiggestYet = Bigger
																					 
InnerBox = 9          ' InnerBox is total number of lines allowed inside box
first = 0: last = 8
DO

	' Calculate header and footer placement

		IF (HeadLen MOD 2) THEN
			HeadStart = ((BiggestYet - HeadLen) \ 2) + 13
		ELSE
			HeadStart = ((BiggestYet - HeadLen) \ 2) + 12
		END IF
		IF (FootLen MOD 2) THEN
			FootStart = ((BiggestYet - FootLen) \ 2) + 13
		ELSE
			FootStart = ((BiggestYet - FootLen) \ 2) + 12
		END IF

		' Print a box and fill it with titles
		Inset = TABLETOP + 2

		Lines = Inset + 1
		IF MoreBoxes = FALSE THEN
			LOCATE Inset, 3
			PRINT "       É"; STRING$(BiggestYet + 2, CHR$(205)); "»"
		END IF
		FOR PrintEm = first TO last
			LOCATE Lines + NextSpace, 3
			PRINT "       º "; OutBookNames(Total); SPACE$(BiggestYet - LEN((OutBookNames(Total)))); " º"
			Total = Total + 1: NextSpace = NextSpace + 1
		NEXT PrintEm
		IF MoreBoxes = FALSE THEN                       ' Means first group
			LOCATE Lines + NextSpace, 3
			PRINT "       È"; STRING$(BiggestYet + 2, CHR$(205)); "¼"
			COLOR BACKGROUND, FOREGROUND + BRIGHT
			LOCATE Inset, HeadStart
			PRINT Header$;                          '"µ "; Header$; " Æ"
			LOCATE Lines + NextSpace, FootStart
			PRINT Footer$                           '"µ "; Footer$; " Æ"
			COLOR FOREGROUND, BACKGROUND
		END IF
		SLEEP
	first = first + InnerBox: last = last + InnerBox
	NextSpace = 0: HowMany = 0

	MoreBoxes = TRUE

LOOP UNTIL LEN(RTRIM$(OutBookNames(Total))) = 0

END SUB

'***************************************************************************
'*  The Reader FUNCTION reads specified text files and returns each line   *
'*  as a separate record for the corresponding table.                      *
'*                               Parameters                                *
'*  BigRec    RecStruct variable containing information on tables          *
'*  SeqFile   File number used to open the text file to be read
'***************************************************************************
FUNCTION Reader% (BigRec AS RecStruct, SeqFile%)
	SELECT CASE SeqFile
		CASE 10
			IF NOT EOF(SeqFile) THEN
			 INPUT #SeqFile, BigRec.Lendee.CardNum, BigRec.Lendee.Zip, BigRec.Lendee.TheName, BigRec.Lendee.City, BigRec.Lendee.Street, BigRec.Lendee.State
			 Reader = -1
			ELSE
				Reader = 0
			END IF
		CASE 11
			IF NOT EOF(SeqFile) THEN
			 INPUT #SeqFile, BigRec.OutBooks.IDnum, BigRec.OutBooks.CardNum, BigRec.OutBooks.DueDate
			 Reader = -1
			ELSE
			 Reader = 0
			END IF
		CASE 12
			IF NOT EOF(SeqFile) THEN
				INPUT #SeqFile, BigRec.Inventory.IDnum, BigRec.Inventory.Price, BigRec.Inventory.Edition, BigRec.Inventory.Title, BigRec.Inventory.Author, BigRec.Inventory.Publisher
				Reader = -1
			 ELSE
				Reader = 0
			 END IF
	END SELECT
END FUNCTION

'***************************************************************************
'*  The ReturnBook SUB checks the book currently being displayed back into *
'*  the library --- that is, it eliminates the appropriate entry from the  *
'*  BooksOut table. It checks to see if the book is overdue, and if so, it *
'*  displays the amount of the fine to be paid.                            *
'*                                Parameters                               *
'*  TablesRec   RecStruct type variable holding current table information  *
'***************************************************************************
SUB ReturnBook (TablesRec AS RecStruct, DueDate#)

DIM ReturnLines(10) AS STRING

Alert$ = "Press ENTER to check current book in, N to abort checkin..."
CALL ShowMessage(Alert$, 0)

SETINDEX cBooksOutTableNum, "IDIndexBO"
SEEKEQ cBooksOutTableNum, TablesRec.Inventory.IDnum
IF NOT EOF(cBooksOutTableNum) THEN
	RETRIEVE cBooksOutTableNum, TablesRec.OutBooks
END IF
SETINDEX cCardHoldersTableNum, "CardNumIndexCH"
SEEKEQ cCardHoldersTableNum, TablesRec.OutBooks.CardNum

IF NOT EOF(cBooksOutTableNum) THEN
	IF LOF(cCardHoldersTableNum) THEN
		RETRIEVE cCardHoldersTableNum, TablesRec.Lendee
	END IF
END IF

Today# = 32000    'Replace this with call to DTFMTER.QLB library routine
									'as shown on the next 2 lines
'Today# = Now#
'ShowDate$ = STR$(Month&(Today#)) + "/" + LTRIM$(STR$(Day&(Today#))) + "/" + LTRIM$(STR$(Year&(Today#)))
IF Today# > TablesRec.OutBooks.DueDate THEN
	Fine = Today# - TablesRec.OutBooks.DueDate
END IF

DateDue# = (TablesRec.OutBooks.DueDate)
' If you have DTFMTER.QLB loaded, use in to get date to display
' DateDue$ = STR$(Month&(DateDue#)) + "/" + LTRIM$(STR$(Day&(DateDue#))) + "/" + LTRIM$(STR$(Year&(DateDue#)))
ReturnLines(0) = ""
ReturnLines(1) = RTRIM$(TablesRec.Inventory.Title)
ReturnLines(2) = "is checked out to card number: " + STR$(TablesRec.OutBooks.CardNum)
ReturnLines(3) = RTRIM$(TablesRec.Lendee.TheName)
ReturnLines(4) = ""
ReturnLines(5) = "Today's Date:     " + STR$(Today#) + " - A phoney date"
IF LEN(ShowDate$) THEN ReturnLines(5) = "Today's Date:     " + ShowDate$
ReturnLines(6) = "Due Date of Book: " + STR$(TablesRec.OutBooks.DueDate)
IF LEN(DateDue$) THEN ReturnLines(6) = "Due Date of Book: " + DateDue$
ReturnLines(7) = "Fine Payable:     $" + STR$(ABS(Fine / 100))
ReturnLines(8) = ""
ReturnLines(9) = ""
FOR Index = 0 TO 10
	ThisOne = LEN(ReturnLines(Index))
	IF ThisOne > BiggestYet THEN BiggestYet = ThisOne
NEXT Index
Header$ = "Press ENTER to check book in..."
Footer$ = "Press N or n to abort checkin..."
CALL PeekWindow(ReturnLines(), Header$, Footer$, BiggestYet%)

IF CatchKey THEN                              ' If user confirms, delete
	IF LOF(cBooksOutTableNum) <> 0 THEN         ' the entry to BooksOut table
		DELETE cBooksOutTableNum
	END IF
END IF
CALL DrawTable(TablesRec.TableNum)
CALL EraseMessage

END SUB

'***************************************************************************
'* The ShowStatus SUB uses the due date associated with the book IDnum from*
'* of the BooksOut table. This date is in serial form which is not decoded *
'* here, but can be decoded with the date/time function library supplied   *
'* with BASIC 7.1. The due date is displayed centered on the top line of   *
'* the ShowMessage box.                                                    *
'*                                Parameters                               *
'*  Stat$       Message introducing the due date when displayed in its box *
'*  ValueToShow The due date of the book from the BooksOut table           *
'***************************************************************************
SUB ShowStatus (Stat$, ValueToShow AS DOUBLE)

COLOR FOREGROUND, BACKGROUND
DataEndLine$ = STRING$(60, 205)       'redraw the bottom line

StringToShow$ = Stat$       ' Figure out where to locate the text
IF ValueToShow = 0 THEN
	LOCATE TABLEEND, 4
	PRINT DataEndLine$
	EXIT SUB
ELSE
	' The dates in the file are in serial form. Use the DTFMTER.QLB library
	' to decode serial dates for normal display. In the code below, the
	' calls to the library are commented out.

	'TheDate$ = STR$(Month&(ValueToShow)) + "/" + LTRIM$(STR$(Day&(ValueToShow))) + "/" + LTRIM$(STR$(Year&(ValueToShow)))
	IF Stat$ = " Total records in table: " OR LEN(TheDate$) = 0 THEN
		StringToShow$ = StringToShow$ + " " + STR$(ValueToShow)
	ELSE
		StringToShow$ = StringToShow$ + " " + TheDate$
	END IF
	HowLong = LEN(StringToShow$)
	PlaceStatus = (73 \ 2) - (HowLong \ 2)
	StatusSpace$ = CHR$(181) + STRING$(HowLong, 32) + CHR$(198)
END IF
LOCATE TABLEEND, PlaceStatus
PRINT StatusSpace$
COLOR BACKGROUND, BRIGHT + FOREGROUND
LOCATE TABLEEND, PlaceStatus + 1
PRINT StringToShow$
COLOR FOREGROUND, BACKGROUND

END SUB
