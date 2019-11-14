'       ISAMDEM1.BAS - second module of the ISAM demonstration program.
'
'               Copyright (C) 1989-1990, Microsoft Corporation
'
'   Main module - ISAMDEMO.BAS
'   Include files - ISAMDEMO.BI
'
'
'$INCLUDE: 'isamdemo.bi'
		
DECLARE SUB SetUpBackground (bColor AS INTEGER)
DECLARE SUB SetUpMenu ()

DEFINT A-Z
' ClearCustRecord
' Clears global customer record
'
SUB ClearCustRecord
	CustRec.AcctNo = ""
	CustRec.Company = ""
	CustRec.Street = ""
	CustRec.City = ""
	CustRec.state = ""
	CustRec.Zip = ""
	CustRec.Phone1 = ""
	CustRec.Phone2 = ""
	CustRec.Contact = ""
	CustRec.Opened = ""
END SUB

' ClearInveRecord
' Clears global inventory record
'
SUB ClearInveRecord
	InventRec.ItemNo = ""
	InventRec.Descrip = ""
	InventRec.stock = 0
	InventRec.Cost = 0
	InventRec.Retail = 0
	InventRec.Vendor = ""
END SUB

' ClearInvoRecord
' Clears global invoice record
'
SUB ClearInvoRecord
	GetNextInvoice
	InvoiceRec.AcctNo = ""
	InvoiceRec.Date = ""
	InvoiceRec.Total = 0
	InvoiceRec.TaxRate = 0
END SUB

' ClearTranRecord
' Clears global transaction record
'
SUB ClearTranRecord
	TransRec.TransNo = 0
	TransRec.InvoiceNo = ""
	TransRec.ItemNo = ""
	TransRec.Quantity = 0
	TransRec.Price = 0
END SUB

' CreateListBox
' Creates a list box within the current window
'
' text$() - the list
' cBox    - the ListBox
' func    - function flag for DrawList
'
SUB CreateListBox (text$(), cBox AS ListBox, func)
DIM oBox AS ListBox

	' get displayable length
	IF cBox.listLen < cBox.bLen THEN
	cBox.maxLen = cBox.listLen
	ELSE
	cBox.maxLen = cBox.bLen
	END IF

	' create box
	WindowBox cBox.topRow, cBox.leftCol, cBox.topRow + cBox.bLen + 1, cBox.leftCol + cBox.bWid + 1

	' add scroll bar if necessary
	IF cBox.listLen <> cBox.maxLen THEN
	ButtonOpen cBox.sBut, 1, "", cBox.topRow + 1, cBox.leftCol + cBox.bWid + 1, cBox.topRow + cBox.bLen, cBox.leftCol + cBox.bWid + 1, 6
	END IF

	' open area button
	ButtonOpen cBox.aBut, 1, "", cBox.topRow + 1, cBox.leftCol + 1, cBox.topRow + cBox.bLen, cBox.leftCol + cBox.bWid, 4

	' set current list element relative to list box top
	IF cBox.listPos <= cBox.maxLen THEN
		cBox.curTop = 1
		cBox.curPos = cBox.listPos
	ELSEIF cBox.listPos + cBox.maxLen > cBox.listLen + 1 THEN
		cBox.curTop = cBox.listLen - cBox.maxLen + 1
		cBox.curPos = cBox.listPos - cBox.curTop + 1
	ELSE
		cBox.curTop = cBox.listPos
		cBox.curPos = 1
	END IF

	' Display list within the box
	oBox.curTop = cBox.curTop + 1
	DrawList text$(), cBox, func, oBox
END SUB

' DispCustWin
' Creates and displays customer record form
SUB DispCustWin (handle)
	IF level = 0 THEN
	topRow = 4
	ELSE
	topRow = 5
	END IF
	lefCol = 3
	WindowOpen handle, topRow, lefCol, topRow + 19, lefCol + 73, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Customer Record"
	WindowBox 1, 2, 20, 54
	WindowLocate 3, 4
	WindowPrint -2, "Acct No:"
	WindowBox 2, 12, 4, 19
	WindowLocate 3, 32
	WindowPrint -2, "Date Opened: "
	WindowLocate 6, 4
	WindowPrint -2, "Company:"
	WindowBox 5, 12, 7, 52
	WindowLocate 9, 4
	WindowPrint -2, "Street:"
	WindowBox 8, 12, 10, 52
	WindowLocate 12, 4
	WindowPrint -2, "City:"
	WindowBox 11, 12, 13, 30
	WindowLocate 12, 32
	WindowPrint -2, "ST:"
	WindowBox 11, 35, 13, 39
	WindowLocate 12, 41
	WindowPrint -2, "Zip:"
	WindowBox 11, 45, 13, 52
	WindowLocate 15, 4
	WindowPrint -2, "Phone:"
	WindowBox 14, 12, 16, 28
	WindowLocate 15, 30
	WindowPrint -2, "Phone:"
	WindowBox 14, 36, 16, 52
	WindowLocate 18, 4
	WindowPrint -2, "Contact:"
	WindowBox 17, 12, 19, 52
	WindowBox 1, 56, 3, 73
	WindowBox 4, 56, 9, 73
	WindowBox 10, 56, 13, 73
	WindowBox 14, 56, 20, 73
END SUB

' DispInveWin
' Creates and displays inventory record form
'
SUB DispInveWin (handle)
	WindowOpen handle, 5, 3, 21, 76, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Inventory Sheet"
	WindowBox 1, 2, 17, 57
	WindowLocate 3, 4
	WindowPrint -2, "Item No:"
	WindowBox 2, 12, 4, 19
	WindowLocate 6, 4
	WindowPrint -2, "Descr:"
	WindowBox 5, 12, 7, 55
	WindowLocate 9, 4
	WindowPrint -2, "Cost:"
	WindowBox 8, 12, 10, 24
	WindowLocate 9, 35
	WindowPrint -2, "Retail:"
	WindowBox 8, 43, 10, 55
	WindowLocate 12, 4
	WindowPrint -2, "Stock:"
	WindowBox 11, 12, 13, 21
	WindowLocate 15, 4
	WindowPrint -2, "Vendor:"
	WindowBox 14, 12, 16, 55
	WindowBox 1, 59, 3, 73
	WindowBox 4, 59, 9, 73
	WindowBox 10, 59, 17, 73
END SUB

' DispInvoWin
' Creates and displays purchase order form
'
SUB DispInvoWin (handle)
	WindowOpen handle, 4, 3, 24, 76, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Company Invoice"
	WindowBox 1, 2, 21, 57
	WindowLocate 1, 4
	WindowPrint -2, "Invoice No: "
	WindowLocate 1, 42
	WindowPrint -2, "Date: "
	WindowLocate 3, 4
	WindowPrint -2, "Acct No:"
	WindowBox 2, 12, 4, 19
	WindowLocate 6, 4
	WindowPrint -2, "Company:"
	WindowBox 5, 12, 7, 55
	WindowLocate 9, 4
	WindowPrint -2, "Item No:"
	WindowBox 8, 12, 10, 19
	WindowLocate 9, 22
	WindowPrint -2, "Price:"
	WindowBox 8, 28, 10, 40
	WindowLocate 9, 43
	WindowPrint -2, "Qty:"
	WindowBox 8, 47, 10, 55
	WindowLocate 11, 4
	WindowPrint -2, "    Item No.       Price    Quantity         Total"
	WindowLocate 18, 32
	WindowPrint -2, "SubTotal:"
	WindowLocate 19, 32
	WindowPrint -2, "Tax:"
	WindowLocate 19, 17
	WindowPrint -2, "Tax %"
	WindowBox 18, 22, 20, 30
	WindowLocate 20, 32
	WindowPrint -2, "Total:"
	WindowBox 1, 59, 4, 73
	WindowLocate 1, 61
	WindowPrint -2, "Transaction"
	WindowBox 5, 59, 8, 73
	WindowLocate 5, 61
	WindowPrint -2, "Records"
	WindowBox 9, 59, 21, 73
	WindowLocate 9, 61
	WindowPrint -2, "Invoice"
END SUB

' DrawList
' Displays a list in a list box
'
SUB DrawList (text$(), cBox AS ListBox, listtype, oBox AS ListBox)
STATIC barpos AS INTEGER

	' handle special operations
	IF listtype = 0 THEN
		barpos = 0
		index = cBox.curTop - 1
	ELSE
		index = 0
		SELECT CASE listtype
		CASE 1                                  'create a list of transactions
			CreateTranList text$(), cBox
			barpos = 0
		CASE 2                                  'create a list of inventory records
			CreateInveList text$(), cBox, oBox, barpos
		CASE 3                                  'create a list of customer records
			CreateCustList text$(), cBox, oBox, barpos
		CASE 4                                  'create a list of customer invoice records
			CreateCInvList text$(), cBox, oBox, barpos
		CASE 5                                  'create a list of invoice records
			CreateInvoList text$(), cBox, oBox, barpos
		END SELECT
	END IF

	' Draw each element of list that should currently appear in box
	FOR i = 1 TO cBox.bLen
		IF i = cBox.curPos THEN                 ' highlight current list element
			WindowColor 7, 0
		ELSE
			WindowColor 0, 7
		END IF

		'display list elements
		WindowLocate cBox.topRow + i, cBox.leftCol + 1
		IF i <= cBox.maxLen THEN
			WindowPrint -1, LEFT$(text$(index + i) + STRING$(cBox.bWid, " "), cBox.bWid)
		ELSE
			WindowPrint -1, STRING$(cBox.bWid, " ")
		END IF
	NEXT i

	' update scrollbar position indicator if scrollbar present
	IF cBox.listLen <> cBox.maxLen AND barpos = 0 THEN
		position = (cBox.curTop + cBox.curPos - 1) * (cBox.maxLen - 2) / cBox.listLen
		IF position < 1 THEN
			position = 1
		ELSEIF position > cBox.maxLen - 2 THEN
			position = cBox.maxLen - 2
		END IF
		ButtonSetState cBox.sBut, position
	ELSE
		ButtonSetState cBox.sBut, barpos
	END IF

	' Reset color in case current element was last to be drawn
	WindowColor 0, 7

	' update current position in case list has been scrolled
	cBox.listPos = cBox.curTop + cBox.curPos - 1
END SUB

' GetBoundaries
' Gets character or date boundaries for displaying invoice or
' customer lists.  Returns OK if operation a success else returns CANCEL.
'
' handle - window handle
' func - type of boundary to get (date or character)
'
FUNCTION GetBoundaries% (handle, func)
	GOSUB ShowForm
												'display form
	curBut = 0
	currEditField = 1

	' window control loop
	finished = FALSE
	WHILE finished = FALSE
	WindowDo curBut, currEditField              ' wait for event
	SELECT CASE Dialog(0)
		CASE 1                                  ' button pressed
			curBut = Dialog(1)
			finished = TRUE
		CASE 2                                  ' edit field
			curBut = 0
			currEditField = Dialog(2)
		CASE 6                                  ' enter
			curBut = 3 - ButtonInquire(1)
			finished = TRUE
		CASE 7                                  ' tab
			SELECT CASE curBut
				CASE 0
				SELECT CASE currEditField
					CASE 1
						currEditField = 2
					CASE 2
						currEditField = 0
						ButtonSetState 2, 1
						curBut = 1
						ButtonSetState 1, 2
				END SELECT
				CASE 1
					ButtonToggle 1
					curBut = 2
					ButtonToggle 2
				CASE 2
					currEditField = 1
					curBut = 0
					ButtonSetState 2, 1
					ButtonSetState 1, 2
			END SELECT
		CASE 8                                  ' back tab
			SELECT CASE curBut
				CASE 0
				SELECT CASE currEditField
					CASE 1
						currEditField = 0
						ButtonSetState 1, 1
						curBut = 2
						ButtonSetState 2, 2
					CASE 2
						currEditField = 1
				END SELECT
				CASE 1
					currEditField = 2
					curBut = 0
				CASE 2
					ButtonToggle 2
					curBut = 1
					ButtonToggle 1
			END SELECT
		CASE 9                                  ' escape
			curBut = 2
			finished = TRUE
		CASE 14                                 ' space bar
			IF curBut > 0 THEN finished = TRUE
	 END SELECT
	 'error checking for user input
	 IF finished = TRUE AND curBut = 1 THEN
		IF UCASE$(LTRIM$(EditFieldInquire$(1))) > UCASE$(LTRIM$(EditFieldInquire$(2))) THEN
			PrintError "Ending condition must be greater than or equal to beginning condition."
			finished = FALSE
		END IF
	END IF
	WEND

	' get input boundaries
	IF curBut = 1 THEN
		SELECT CASE func
			CASE 2                              'customer name boundaries
				Bound1$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
				Bound2$ = UCASE$(LTRIM$(EditFieldInquire$(2)))
				IF Bound1$ = "" THEN Bound1$ = "A"
				IF Bound2$ = "" THEN Bound2$ = "Z"
				Bound2$ = MID$(Bound2$, 1, LEN(Bound2$) - 1) + CHR$(ASC(MID$(Bound2$, LEN(Bound2$), 1)) + 1)
			CASE 3                              'date boundaries
				temp$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
				IF temp$ = "" THEN
					Bound1$ = "0"
				ELSE
					Bound1$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
				END IF
				temp$ = UCASE$(LTRIM$(EditFieldInquire$(2)))
				IF temp$ = "" THEN
					Bound2$ = "999998"
				ELSE
					Bound2$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
				END IF
				Bound2$ = LTRIM$(STR$(VAL(Bound2$) + 1))
		END SELECT
		GetBoundaries = OK
	ELSE
		GetBoundaries = CANCEL
	END IF

	WindowClose handle
	EXIT FUNCTION

ShowForm:
	SELECT CASE func
		CASE 1                                  'no boundaries needed, exit
			Bound1$ = ""
			Bound2$ = ""
			GetBoundaries = CANCEL
			EXIT FUNCTION
		CASE 2                                  'display form for customer name boundaries
			WindowOpen handle, 7, 25, 15, 51, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, ""
			WindowLocate 1, 2
			WindowPrint -2, "List companies with names"
			WindowLocate 3, 2
			WindowPrint -2, "beginning with"
			WindowBox 2, 17, 4, 27
			EditFieldOpen 1, "A", 3, 18, 0, 7, 9, 8
			WindowLocate 6, 2
			WindowPrint -2, "through"
			WindowBox 5, 17, 7, 27
			EditFieldOpen 2, "Z", 6, 18, 0, 7, 9, 8
			WindowLine 8
			ButtonOpen 1, 2, "OK", 9, 4, 0, 0, 1
			ButtonOpen 2, 1, "Cancel", 9, 14, 0, 0, 1
		CASE 3                                  'display form for date boundaries
			WindowOpen handle, 7, 25, 15, 53, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, ""
			WindowLocate 1, 2
			WindowPrint -2, "List invoices with dates"
			WindowLocate 3, 2
			WindowPrint -2, "from"
			WindowBox 2, 7, 4, 17
			EditFieldOpen 1, "", 3, 8, 0, 7, 9, 8
			WindowLocate 3, 19
			WindowPrint -2, "(mm-dd-yy)"
			WindowLocate 6, 2
			WindowPrint -2, "to"
			WindowBox 5, 7, 7, 17
			EditFieldOpen 2, "", 6, 8, 0, 7, 9, 8
			WindowLocate 6, 19
			WindowPrint -2, "(mm-dd-yy)"
			WindowLine 8
			ButtonOpen 1, 2, "OK", 9, 4, 0, 0, 1
			ButtonOpen 2, 1, "Cancel", 9, 14, 0, 0, 1
	END SELECT
RETURN

END FUNCTION

' GetFileCount
' Returns number of DOS files matching a given file spec
'
' fileSpec$ - DOS file spec  (i.e. "*.*")
'
FUNCTION GetFileCount% (fileSpec$)
	count = 0

	FileName$ = DIR$(fileSpec$)             ' Get first match if any

	DO WHILE FileName$ <> ""                ' continue until no more matches
	count = count + 1
	FileName$ = DIR$
	LOOP

	GetFileCount = count                    ' return count
END FUNCTION

' GetNextInvoice
' Sets Global invoice number to next available invoice number.
'
SUB GetNextInvoice
	ON LOCAL ERROR RESUME NEXT

	' if empty table start with first number
	IF LOF(InvoTabNum) = 0 THEN
		InvoiceRec.InvoiceNo = LTRIM$(STR$(100001))
	' else start at one greater than last number used
	ELSE
		SETINDEX InvoTabNum, "InvoiceIndex"
		MOVELAST InvoTabNum
		RETRIEVE InvoTabNum, InvoiceRec
		InvoiceRec.InvoiceNo = LTRIM$(STR$(VAL(InvoiceRec.InvoiceNo) + 1))
	END IF
END SUB

' InitAll
' Performs all initialization for the program
'
SUB InitAll
	origdir$ = CURDIR$
	ISAMfile$ = ""

	CustTabNum = 0
	InveTabNum = 0
	InvoTabNum = 0
	TranTabNum = 0

	SCREEN 0                                ' init screen
	WIDTH 80, 25

	MenuInit                                ' init menu routines
	WindowInit                              ' init window routines
	MouseInit                               ' init mouse routines
	SetUpMenu                               ' Set up menu bar
	SetUpBackground 0                       ' Set up screen background
	MouseShow                               ' display mouse

	' display program introduction
	a$ = "Amazing Ray's Amusement Park Supply Company|"
	a$ = a$ + "Database Transaction System||"
	a$ = a$ + "An ISAM Demo|"
	a$ = a$ + "for|"
	a$ = a$ + "Microsoft BASIC 7.1 Professional Development System|"
	a$ = a$ + "Copyright (c) 1989-1990 Microsoft Corporation|"
	IF Alert(4, a$, 8, 12, 16, 68, "Color", "Monochrome", "") = 1 THEN
		MenuColor 0, 7, 4, 8, 0, 4, 7
		SetUpBackground 1
	ELSE
		MenuColor 0, 7, 15, 8, 7, 0, 15
	END IF

	MenuPreProcess
	MenuShow
END SUB

' Max
' Returns the maximum of two numbers
'
' num1, num2 - numbers to compare
'
FUNCTION Max% (num1, num2)
	IF num1 > num2 THEN
		Max% = num1
	ELSE
		Max% = num2
	END IF
END FUNCTION

' Min
' Compares two numbers and returns the smallest
'
' num1, num2 - numbers to compare
'
FUNCTION Min% (num1, num2)
	IF num1 < num2 THEN
		Min% = num1
	ELSE
		Min% = num2
	END IF
END FUNCTION

' NewDB
' Prompts user for a file name and creates a new ISAM file with proper indexes and tables.
'
SUB NewDB
	ON LOCAL ERROR GOTO NewErr

	NewCustTabNum = 0
	NewInveTabNum = 0
	NewInvoTabNum = 0
	NewTranTabNum = 0

	' Open window for display
	WindowOpen 1, 8, 20, 12, 58, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "New DataBase"
	WindowLocate 2, 2
	WindowPrint 2, "File Name:"
	WindowBox 1, 13, 3, 38
	WindowLine 4

	' open edit field for file name
	EditFieldOpen 1, FileName$, 2, 14, 0, 7, 24, 70

	' open command buttons
	ButtonOpen 1, 2, "OK", 5, 6, 0, 0, 1
	ButtonOpen 2, 1, "Cancel", 5, 25, 0, 0, 1

	' start with cursor in edit field
	curBut = 0
	currEditField = 1
	pushButton = 1

	' control loop for window
	finished = FALSE
	WHILE NOT finished
		WindowDo curBut, currEditField                  ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' Button pressed
				pushButton = Dialog(1)
				finished = TRUE
			CASE 2                                      ' Edit Field
				curBut = 0
				currEditField = 1
			CASE 6                                      ' enter
				finished = TRUE
			CASE 7                                      ' tab
				SELECT CASE curBut
					CASE 0, 1
						ButtonSetState curBut, 1
						curBut = curBut + 1
						pushButton = curBut
						ButtonSetState pushButton, 2
						currEditField = 0
					CASE 2
						curBut = 0
						pushButton = 1
						currEditField = 1
						ButtonSetState 1, 2
						ButtonSetState 2, 1
				END SELECT
			CASE 8                                      ' back tab
				SELECT CASE curBut
					CASE 0
						curBut = 2
						pushButton = 2
						currEditField = 0
						ButtonSetState 1, 1
						ButtonSetState 2, 2
					CASE 1
						curBut = 0
						currEditField = 1
					CASE 2
						curBut = 1
						pushButton = 1
						ButtonSetState 1, 2
						ButtonSetState 2, 1
				END SELECT
			CASE 9                                      ' escape
				pushButton = 2
				finished = TRUE
			CASE 14                                     ' space bar
				IF curBut <> 0 THEN finished = TRUE
		END SELECT

		' error checking before finishing
		IF finished = TRUE AND pushButton = 1 THEN
			temp$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
			' must specify a file
			IF temp$ = "" THEN
				PrintError "Must specify a name."
				finished = FALSE
			' check if file is valid and can be opened
			ELSE
				create = 1
				IF temp$ = ISAMfile$ THEN
					a$ = "| " + "Database is currently open.  Overwrite?"
					IF Alert(4, a$, 8, 20, 12, 60, "Yes", "No", "") = 1 THEN
						CLOSE CustTabNum, InveTabNum, InvoTabNum, TranTabNum
						CustTabNum = 0
						InveTabNum = 0
						InvoTabNum = 0
						TranTabNum = 0

						KILL temp$
					ELSE
						create = 0
					END IF
				ELSEIF DIR$(temp$) = temp$ THEN
					a$ = "| " + "Database already exists.  Overwrite?"
					IF Alert(4, a$, 8, 20, 12, 60, "Yes", "No", "") = 1 THEN
						KILL temp$
					ELSE
						create = 0
					END IF
				END IF

				' open file
				IF create = 1 THEN
					WindowOpen 2, 9, 18, 10, 60, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, ""
					WindowLocate 1, 5
					WindowPrint -2, "Creating tables..."
					NewCustTabNum = FREEFILE
					OPEN temp$ FOR ISAM CustRecord "CustTable" AS NewCustTabNum
					NewInveTabNum = FREEFILE
					OPEN temp$ FOR ISAM InventRecord "InventTable" AS NewInveTabNum
					NewInvoTabNum = FREEFILE
					OPEN temp$ FOR ISAM InvoiceRecord "InvoiceTable" AS NewInvoTabNum
					NewTranTabNum = FREEFILE
					OPEN temp$ FOR ISAM TransRecord "TransTable" AS NewTranTabNum
				ELSE
					finished = FALSE
				END IF
NewErrReturn:
			END IF
		END IF
	WEND

	' close old file and store new file name
	IF pushButton = 1 THEN
		ISAMfile$ = temp$

		CLOSE CustTabNum, InveTabNum, InvoTabNum, TranTabNum
		CustTabNum = NewCustTabNum
		InveTabNum = NewInveTabNum
		InvoTabNum = NewInvoTabNum
		TranTabNum = NewTranTabNum

		' create indexes on new table
		CreateIndexes
	END IF

	WindowClose 2
	WindowClose 1

	EXIT SUB

' local error handler
NewErr:
	finished = FALSE                              ' dont exit until valid file specified

	CLOSE NewCustTabNum, NewInveTabNum, NewInvoTabNum, NewTranTabNum

	ShowError ERR                                 ' display error
	WindowClose 2
	IF ERR = 73 THEN                              ' ISAM TSR not running
		WindowClose 1
		EXIT SUB
	END IF
RESUME NewErrReturn

END SUB

' NextRecord
' Gets the next record in a table that matches the search criteria
'
' TableNum - ISAM table
' key1$ - current search criteria
' origkey1$ - original search criteria
' lastkey1$ - last search criteria
' key2$ - current search criteria (second search criteria for customer table)
' origkey2$ - original search criteria
' lastkey2$ - last search criteria
'
FUNCTION NextRecord% (TableNum, key1$, origkey1$, lastkey1$, key2$, origkey2$, lastkey2$)
	IF LOF(TableNum) THEN
		IF TableNum = InveTabNum THEN SETINDEX TableNum, "ItemIndex"
		IF key1$ = "" AND key2$ = "" THEN
			IF TableNum = CustTabNum THEN SETINDEX TableNum, "CompanyIndex"
			MOVEFIRST TableNum
			origkey1$ = ""
			origkey2$ = ""
		ELSEIF key1$ <> lastkey1$ OR key2$ <> lastkey2$ THEN
			origkey1$ = key1$
			origkey2$ = key2$
			IF key1$ = "" THEN
				SETINDEX TableNum, "AcctIndex"
				SEEKGE TableNum, key2$
			ELSE
				IF TableNum = CustTabNum THEN
					SETINDEX TableNum, "CompanyIndex"
					SEEKGE TableNum, key1$, key2$
				ELSE
					SEEKGE TableNum, key1$
				END IF
			END IF
			IF EOF(TableNum) THEN
				NextRecord% = 2
				EXIT FUNCTION
			END IF
		ELSE
			IF origkey1$ = "" AND origkey2$ <> "" THEN
				SETINDEX TableNum, "AcctIndex"
				SEEKEQ TableNum, lastkey2$
			ELSE
				IF TableNum = CustTabNum THEN
					SETINDEX TableNum, "CompanyIndex"
					SEEKEQ TableNum, lastkey1$, lastkey2$
				ELSE
					SEEKEQ TableNum, lastkey1$
				END IF
			END IF
			MOVENEXT TableNum
			IF EOF(TableNum) THEN
				IF origkey1$ = "" AND origkey2$ <> "" THEN
					SEEKGE TableNum, origkey2$
				ELSEIF TableNum = CustTabNum THEN
					SEEKGE TableNum, origkey1$, origkey2$
				ELSE
					SEEKGE TableNum, origkey1$
				END IF
			END IF
		END IF
		NextRecord% = 1
	ELSE
		NextRecord% = 3
	END IF
END FUNCTION

' OpenDB
' Prompts user for an isam file to open.
'
SUB OpenDB
DIM fileList$(1 TO 10)
DIM fileBox AS ListBox

	ON LOCAL ERROR GOTO OpenErr

	NewCustTabNum = 0
	NewInveTabNum = 0
	NewInvoTabNum = 0
	NewTranTabNum = 0

	fileSpec$ = "*.MDB"                         ' default file spec

	origPos = 0                                 ' no file list element selected

	GOSUB ShowOpenWindow                        ' display form

	' start with cursor in edit field
	curBut = 0
	currEditField = 1
	pushButton = 3

	' control loop
	finished = FALSE
	WHILE finished = FALSE
		WindowDo curBut, currEditField                  ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' button pressed
				curBut = Dialog(1)
				SELECT CASE curBut
					CASE 1, 2
						currEditField = 0
						ScrollList fileList$(), fileBox, curBut, 0, winRow, winCol
						curBut = 2
					CASE 3, 4
						pushButton = curBut
						finished = TRUE
				END SELECT
			CASE 2                                      ' edit Field
				curBut = 0
				currEditField = 1
			CASE 6                                      ' enter
				IF INSTR(EditFieldInquire$(1), "*") = 0 THEN finished = TRUE
			CASE 7                                      ' tab
				SELECT CASE curBut
					CASE 0
						curBut = 2
						currEditField = 0
					CASE 1, 2
						curBut = 3
						ButtonSetState pushButton, 1
						ButtonSetState curBut, 2
						pushButton = 3
					CASE 3
						curBut = 4
						ButtonSetState pushButton, 1
						ButtonSetState curBut, 2
						pushButton = 4
					CASE 4
						curBut = 0
						currEditField = 1
						ButtonSetState pushButton, 1
						pushButton = 3
						ButtonSetState pushButton, 2
				END SELECT
			CASE 8                                      ' back tab
				SELECT CASE curBut
					CASE 0
						curBut = 4
						currEditField = 0
						ButtonSetState pushButton, 1
						ButtonSetState curBut, 2
						pushButton = 4
					CASE 1, 2
						curBut = 0
						currEditField = 1
					CASE 3
						curBut = 2
					CASE 4
						curBut = 3
						ButtonSetState pushButton, 1
						ButtonSetState curBut, 2
						pushButton = 3
				END SELECT
			CASE 9                                      ' escape
				pushButton = 4
				finished = TRUE
			CASE 10, 12                                 ' up, left arrow
				ScrollList fileList$(), fileBox, 3, 0, winRow, winCol
			CASE 11, 13                                 'down, right arrow
				ScrollList fileList$(), fileBox, 4, 0, winRow, winCol
			CASE 14                                     ' space bar
				IF curBut > 2 THEN
					pushButton = curBut
					finished = TRUE
				END IF
		END SELECT

		temp$ = EditFieldInquire$(1)

		' simple error checking before finishing
		IF finished AND pushButton <> 4 THEN
			' invalid file specified
			IF INSTR(temp$, "*") THEN
				PrintError "Invalid file specification."
				finished = FALSE
			'no file specified
			ELSEIF LEN(temp$) = 0 THEN
				PrintError "Must specify a name."
				finished = FALSE
			'okay - open file
			ELSE
				fileSpec$ = temp$
				NewCustTabNum = FREEFILE
				OPEN fileSpec$ FOR ISAM CustRecord "CustTable" AS NewCustTabNum
				NewInveTabNum = FREEFILE
				OPEN fileSpec$ FOR ISAM InventRecord "InventTable" AS NewInveTabNum
				NewInvoTabNum = FREEFILE
				OPEN fileSpec$ FOR ISAM InvoiceRecord "InvoiceTable" AS NewInvoTabNum
				NewTranTabNum = FREEFILE
				OPEN fileSpec$ FOR ISAM TransRecord "TransTable" AS NewTranTabNum

				'test if open database file contains necessary table and index
				SETINDEX NewInveTabNum, "ItemIndex"

OpenErrReturn:
			END IF
		END IF

		' more processing to do
		IF NOT finished THEN
			' update edit field display based on list box selection
			IF fileBox.listPos <> origPos THEN
				fileSpec$ = fileList$(fileBox.listPos)
				origPos = fileBox.listPos
				EditFieldClose 1
				EditFieldOpen 1, fileSpec$, 2, 14, 0, 7, 13, 70
			' update list box contents based on new edit field contents
			ELSEIF LTRIM$(RTRIM$(fileSpec$)) <> LTRIM$(RTRIM$(temp$)) THEN
				fileSpec$ = UCASE$(temp$)
				IF fileSpec$ <> "" THEN
					IF MID$(fileSpec$, 2, 1) = ":" THEN
						CHDRIVE MID$(fileSpec$, 1, 2)
						fileSpec$ = MID$(fileSpec$, 3, LEN(fileSpec$))
					END IF
					position = 0
					WHILE INSTR(position + 1, fileSpec$, "\") <> 0
						position = INSTR(position + 1, fileSpec$, "\")
					WEND
					IF position = 1 THEN
						CHDIR "\"
					ELSEIF position > 0 THEN
						CHDIR LEFT$(fileSpec$, position - 1)
					END IF
					fileSpec$ = MID$(fileSpec$, position + 1, LEN(fileSpec$))
					WindowLocate 5, 2
					IF LEN(CURDIR$) > 26 THEN
						direct$ = LEFT$(CURDIR$, 26)
					ELSE
						direct$ = CURDIR$
					END IF
					WindowPrint -1, direct$ + STRING$(26 - LEN(direct$), " ")

					fileCount = GetFileCount(fileSpec$)
				ELSE
					fileCount = 0
				END IF

				EditFieldClose 1
				EditFieldOpen 1, fileSpec$, 2, 14, 0, 7, 13, 70

				origPos = 0
				fileBox.listLen = fileCount
				fileBox.listPos = 0

				' get new file list
				IF fileCount = 0 THEN
					REDIM fileList$(10)
				ELSE
					REDIM fileList$(fileCount)
					fileList$(1) = DIR$(fileSpec$)
					FOR i% = 2 TO fileCount
						fileList$(i%) = DIR$
					NEXT i%
				END IF
				CreateListBox fileList$(), fileBox, 0
			END IF
		END IF
	WEND

	' store file name and return file number
	IF pushButton = 3 THEN
		ISAMfile$ = fileSpec$
		
		CLOSE CustTabNum, InveTabNum, InvoTabNum, TranTabNum
		CustTabNum = NewCustTabNum
		InveTabNum = NewInveTabNum
		InvoTabNum = NewInvoTabNum
		TranTabNum = NewTranTabNum
	ELSE
		CHDRIVE MID$(origdir$, 1, 2)
		CHDIR MID$(origdir$, 3, LEN(origdir$) - 2)
	END IF

	WindowClose 1

	EXIT SUB

' handle any file opening errors
OpenErr:
	finished = FALSE                            ' dont allow exit until valid file chosen

	CLOSE NewCustTabNum, NewInveTabNum, NewInvoTabNum, NewTranTabNum

	ShowError ERR                               ' display error message
	IF ERR = 73 OR ERR = 89 THEN                ' ISAM TSR not running or out of ISAM buffers.
		WindowClose 1
		EXIT SUB
	END IF
RESUME OpenErrReturn

' display open dialog
ShowOpenWindow:
	origdir$ = CURDIR$

	' get list of files matching spec
	fileCount = GetFileCount(fileSpec$)
	IF fileCount THEN
		REDIM fileList$(fileCount)
	END IF
	fileList$(1) = DIR$(fileSpec$)
	FOR i% = 2 TO fileCount
		fileList$(i%) = DIR$
	NEXT i%

	' set up list box for file list
	fileBox.sBut = 1
	fileBox.aBut = 2
	fileBox.listLen = fileCount
	fileBox.topRow = 7
	fileBox.bLen = 5
	fileBox.leftCol = 7
	fileBox.bWid = 14
	fileBox.listPos = origPos

	' create window for display
	winRow = 6
	winCol = 25
	WindowOpen 1, winRow, winCol, 20, 52, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Open Database"
	WindowLocate 2, 2
	WindowPrint 2, "File Name:"
	WindowBox 1, 13, 3, 27
	WindowLocate 5, 2
	WindowPrint 2, origdir$
	WindowLocate 6, 11
	WindowPrint 2, "Files"
	WindowLine 14

	' create list box for file list
	CreateListBox fileList$(), fileBox, 0

	' open edit field for file spec
	EditFieldOpen 1, fileSpec$, 2, 14, 0, 7, 13, 70

	' open command buttons
	ButtonOpen 3, 2, "OK", 15, 5, 0, 0, 1
	ButtonOpen 4, 1, "Cancel", 15, 15, 0, 0, 1
RETURN

END SUB

' PrintError
' Prints error messages on the screen in an Alert box.
'
' text$ - error message
'
SUB PrintError (text$)
	textLen = LEN(text$) + 2
	lefCol = ((80 - textLen) / 2) - 1
	a$ = "| " + text$
	junk = Alert(4, a$, 8, lefCol, 12, textLen + lefCol, "", "", "")
END SUB

' Quit
' Exits the program.
'
SUB Quit
SHARED finished AS INTEGER
	'return user to startup directory
	CHDRIVE MID$(origdir$, 1, 2)
	CHDIR MID$(origdir$, 3, LEN(origdir$) - 2)

	'parting message
	a$ = " Exiting Amazing Ray's database transaction system.||"
	a$ = a$ + "Have an AMAZING day!"
	junk = Alert(4, a$, 8, 14, 12, 65, "", "", "")
	
	'clean up
	finished = TRUE
	CLOSE CustTabNum, InveTabNum, InvoTabNum, TranTabNum
	MouseHide
	COLOR 15, 0
	CLS
END SUB

' RunList
' Controls lists created from the various ISAM tables.
'
SUB RunList (handle, func)
DIM recBox AS ListBox, oBox AS ListBox, recList$(1 TO 5)

	ON LOCAL ERROR GOTO ListErr

	GOSUB ShowList                                      'display list form

	curBut = 2

	' window control loop
	finished = FALSE
	WHILE finished = FALSE
		WindowDo curBut, 0                            ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' button pressed
				curBut = Dialog(1)
				SELECT CASE curBut
					CASE 1, 2
						ScrollList recList$(), recBox, curBut, func, topRow, lefCol
						curBut = 2
					CASE 3
						ButtonSetState 4, 1
						ButtonSetState 3, 2
						GOSUB ViewRecord
					CASE 4
						finished = TRUE
				END SELECT
			CASE 6                                      ' enter
				IF ButtonInquire(3) = 2 THEN
					GOSUB ViewRecord
				ELSE
					finished = TRUE
				END IF
			CASE 7                                      ' tab
				SELECT CASE curBut
					CASE 1, 2
						ButtonSetState 3, 2
						ButtonSetState 4, 1
						curBut = 3
					CASE 3
						ButtonToggle 3
						curBut = 4
						ButtonToggle 4
					CASE 4
						ButtonToggle 4
						curBut = 2
						ButtonToggle 3
				END SELECT
			CASE 8                                      ' back tab
				SELECT CASE curBut
					CASE 1, 2
						ButtonSetState 3, 1
						ButtonSetState 4, 2
						curBut = 4
					CASE 4
						ButtonToggle 4
						curBut = 3
						ButtonToggle 3
					CASE 3
						curBut = 2
				END SELECT
			CASE 9                                      ' escape
				finished = TRUE
			CASE 10, 12                                 ' up, left arrow
				ScrollList recList$(), recBox, 3, func, topRow, lefCol
			CASE 11, 13                                 'down, right arrow
				ScrollList recList$(), recBox, 4, func, topRow, lefCol
			CASE 14                                     ' space bar
				IF curBut = 3 THEN
					GOSUB ViewRecord
				ELSEIF curBut = 4 THEN
					finished = TRUE
				END IF
		END SELECT
	WEND

	WindowClose handle
	ClearInvoRecord

	EXIT SUB

ListErr:
	SELECT CASE ERR
		CASE 52
			PrintError "A database file must be opened before list can be displayed."
			EXIT SUB
		CASE ELSE
			ShowError ERR
			IF ERR = 85 OR ERR = 89 THEN                'out of ISAM buffers or no matching records
				WindowClose handle
				EXIT SUB
			END IF
	END SELECT
RESUME NEXT
													 
ViewRecord:                                       'display list
	SELECT CASE func
		CASE 2                                        'inventory list
			IF recBox.curPos < 1 THEN
				PrintError "No matches found."
			ELSE
				SEEKEQ InveTabNum, MID$(recList$(recBox.curPos), 2, 5)
				RETRIEVE InveTabNum, InventRec
				RunInventRec WindowNext
				SETINDEX InveTabNum, "ItemIndex"
				SEEKEQ InveTabNum, MID$(recList$(2), 2, 5)
				oBox = recBox
				oBox.curTop = oBox.curTop + 1
				DrawList recList$(), recBox, func, oBox
			END IF
		CASE 3                                        'customer list
			IF recBox.curPos < 1 THEN
				PrintError "No matches found."
			ELSE
				SETINDEX CustTabNum, "AcctIndex"
				SEEKEQ CustTabNum, MID$(recList$(recBox.curPos), 27, 5)
				RETRIEVE CustTabNum, CustRec
				temp1$ = Bound1$
				temp2$ = Bound2$
				RunCustRec WindowNext
				Bound1$ = temp1$
				Bound2$ = temp2$
				SETINDEX CustTabNum, "CompanyIndex"
				SEEKGE CustTabNum, MID$(recList$(2), 2, 23), MID$(recList$(2), 27, 5)
				oBox = recBox
				oBox.curTop = oBox.curTop + 1
				DrawList recList$(), recBox, func, oBox
			END IF
		CASE 4                                        'customer invoice list
			SETINDEX InvoTabNum, "InvoiceIndex"
			SEEKEQ InvoTabNum, MID$(recList$(recBox.curPos), 15, 6)
			RETRIEVE InvoTabNum, InvoiceRec
			RunInvoice WindowNext
			SETINDEX InvoTabNum, "InvAcctIndex"
			temp$ = MID$(recList$(1), 2, 8)
			temp$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
			SEEKEQ InvoTabNum, CustRec.AcctNo, temp$, MID$(recList$(1), 15, 6)
		CASE 5                                        'invoice list
			SETINDEX InvoTabNum, "InvoiceIndex"
			SEEKEQ InvoTabNum, MID$(recList$(recBox.curPos), 15, 6)
			RETRIEVE InvoTabNum, InvoiceRec
			RunInvoice WindowNext
			SETINDEX InvoTabNum, "DateIndex"
			temp$ = MID$(recList$(1), 2, 8)
			temp$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
			SEEKEQ InvoTabNum, temp$, MID$(recList$(1), 15, 6)
	END SELECT
RETURN

ShowList:                                         'display entire form
	SELECT CASE func
		CASE 2                                        'inventory list form
			IF LOF(InveTabNum) = 0 THEN
				PrintError "Inventory database is empty."
				EXIT SUB
			END IF

			junk = GetBoundaries(handle, 1)
			SETINDEX InveTabNum, "ItemIndex"
			title$ = "Inventory List"
			header$ = "Item No.        Description         Stock         Cost         Retail"
			recBox.bWid = 67
			topRow = 8
			lefCol = 5
		CASE 3                                        'customer list form
			IF LOF(CustTabNum) = 0 THEN
				PrintError "Customer database is empty."
				EXIT SUB
			END IF
			IF GetBoundaries(handle, 2) <> OK THEN EXIT SUB
			SETINDEX CustTabNum, "CompanyIndex"
			title$ = "Customer List"
			header$ = "Company                   Acct No.     Date Opened"
			recBox.bWid = 48
			topRow = 8
			lefCol = 17
		CASE 4                                        'customer invoice list form
			IF LTRIM$(CustRec.AcctNo) = "" THEN
				PrintError "Must specify an account number before invoice list can be displayed."
				EXIT SUB
			END IF
			SETINDEX InvoTabNum, "InvAcctIndex"
			SEEKGE InvoTabNum, CustRec.AcctNo, "0", "0"
			IF EOF(InvoTabNum) THEN
				PrintError "No invoices exist for account number " + CustRec.AcctNo + "."
				EXIT SUB
			END IF
			IF GetBoundaries(handle, 3) <> OK THEN EXIT SUB
			title$ = "Invoice List"
			header$ = "Date         Invoice No.    Invoice Total"
			recBox.bWid = 39
			topRow = 8
			lefCol = 18
		CASE 5                                        'invoice list form
			IF LOF(InvoTabNum) = 0 THEN
				PrintError "Invoice database is empty."
				EXIT SUB
			END IF
			IF GetBoundaries(handle, 3) <> OK THEN EXIT SUB
			SETINDEX InvoTabNum, "DateIndex"
			title$ = "Invoice List"
			header$ = "Date         Invoice No.    Acct No.    Invoice Total"
			recBox.bWid = 51
			topRow = 8
			lefCol = 14
	END SELECT

	recBox.sBut = 1
	recBox.aBut = 2
	recBox.topRow = 2
	recBox.bLen = 5
	recBox.leftCol = 2
	recBox.listLen = 9999
	recBox.listPos = 1

	WindowOpen handle, topRow, lefCol, topRow + 9, lefCol + recBox.bWid + 3, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, title$
	WindowLocate 1, 2
	WindowPrint -2, header$
	CreateListBox recList$(), recBox, func
	WindowLine 9

	ButtonOpen 3, 2, "View Record", 10, 8, 0, 0, 1
	ButtonOpen 4, 1, "Cancel", 10, recBox.bWid - 12, 0, 0, 1
RETURN

END SUB

' ScrollList
' Handles scrolling for a list box.
'
' text$() - list
' cBox - list box
' curBut - current button
' status - to determine if button was pressed, or up or down arrow keys were used
' func - for special operations (passed to DrawList)
' winRow - top absolute row of current window
' winCol - left absolute column of current window
'
SUB ScrollList (text$(), cBox AS ListBox, status, func, winRow, winCol)
DIM tempBox AS ListBox

	tempBox = cBox

	' scroll using scroll buttons
	IF status = 1 THEN
		SELECT CASE Dialog(19)
			' scroll up
			CASE -1
			IF cBox.curTop > 1 THEN
				cBox.curTop = cBox.curTop - 1
				cBox.curPos = cBox.curPos + 1
				IF cBox.curPos > cBox.maxLen THEN cBox.curPos = cBox.maxLen
			END IF
			' scroll down
			CASE -2
			IF cBox.curTop + cBox.maxLen <= cBox.listLen THEN
				cBox.curTop = cBox.curTop + 1
				cBox.curPos = cBox.curPos - 1
				IF cBox.curPos < 1 THEN cBox.curPos = 1
			END IF
			' scroll to position
			CASE ELSE
			position = Dialog(19)
			IF position > 1 THEN
				position = position * (cBox.listLen) / (cBox.bLen - 2)
				IF position < 1 THEN
				positon = 1
				ELSEIF position > cBox.listLen THEN
				position = cBox.listLen
				END IF
			END IF

			IF cBox.curTop <= position AND cBox.curTop + cBox.maxLen > position THEN
				cBox.curPos = position - cBox.curTop + 1
			ELSEIF position <= cBox.maxLen THEN
				cBox.curTop = 1
				cBox.curPos = position
			ELSE
				cBox.curTop = position - cBox.maxLen + 1
				cBox.curPos = position - cBox.curTop + 1
			END IF
		END SELECT

	' area button chosen
	ELSEIF status = 2 THEN
		' make selected position the current position
		IF Dialog(17) <= cBox.maxLen THEN
			cBox.curPos = Dialog(17)
			DrawList text$(), cBox, func, tempBox
		END IF

		' poll for repeated scrolling while mouse button is down
		DO
			tempBox = cBox
			X! = TIMER
			MousePoll r, C, lb, rb              ' poll mouse
			IF lb = TRUE THEN
			' if below list box then scroll down
			IF r > cBox.topRow + cBox.bLen + winRow - 1 THEN
				GOSUB Down1
			' if above list box then scroll up
			ELSEIF r < cBox.topRow + winRow THEN
				GOSUB Up1
			' if to right of list box then scroll down
			ELSEIF C > cBox.leftCol + cBox.bWid + winCol - 1 THEN
				GOSUB Down1
			' if to left of list box then scroll up
			ELSEIF C < cBox.leftCol + winCol THEN
				GOSUB Up1
			' inside box
			ELSEIF r - winRow - cBox.topRow + 1 <= cBox.maxLen THEN
				cBox.curPos = r - winRow - cBox.topRow + 1
			END IF
		
			' draw list
			DrawList text$(), cBox, func, tempBox
			ELSE
			EXIT DO
			END IF
			WHILE TIMER < X! + .05: WEND
		LOOP

		' up arrow key hit
	ELSEIF status = 3 THEN
		GOSUB Up1

	' down arrow key hit
	ELSEIF status = 4 THEN
		GOSUB Down1
	END IF

	DrawList text$(), cBox, func, tempBox                ' redraw list

	EXIT SUB

' scroll list up one
Up1:
	IF cBox.curPos > 1 THEN
	cBox.curPos = cBox.curPos - 1
	ELSEIF cBox.curTop > 1 THEN
	cBox.curTop = cBox.curTop - 1
	END IF
RETURN

' scroll list down one
Down1:
	IF cBox.curPos < cBox.maxLen THEN
	cBox.curPos = cBox.curPos + 1
	ELSEIF cBox.curTop + cBox.maxLen <= cBox.listLen THEN
	cBox.curTop = cBox.curTop + 1
	END IF
RETURN

END SUB

' SetUpBackground
' Creates and displays the background screen pattern for the demo.
'
SUB SetUpBackground (bColor)
	MouseHide

	WIDTH , 25
	COLOR 7, bColor
	CLS
	COLOR 7, bColor
	LOCATE 3, 61
	PRINT "Amazing Ray's Amusem"

	start = 1
	fin = 24
	FOR j = 1 TO 3
	FOR i = start TO fin STEP 4
		LOCATE i, i * 3 - 2 + (j - 1) * 27
		PRINT MID$("Amazing Ray's Amusements", 1, 83 - (i * 3 + (j - 1) * 27));
	NEXT i
	start = start + 3
	fin = fin - 8
	NEXT j

	start = 0
	fin = 23
	FOR j = 1 TO 3
	FOR i = start TO fin STEP 4
		Y = i * 3 - 24 * j - (j - 1) * 3
		IF Y < 0 THEN
		temp$ = MID$("Amazing Ray's Amusements", ABS(Y) + 1, 24 - ABS(Y))
		LOCATE i + 2, 1
		ELSE
		temp$ = MID$("Amazing Ray's Amusements", 1, 24)
		LOCATE i + 2, Y + 1
		END IF
		PRINT temp$;
	NEXT i
	start = start + 9
	NEXT j

	MouseShow
END SUB

' SetUpMenu
' Creates the menu bar for the program
'
SUB SetUpMenu
	' file menu title
	MenuSet FILETITLE, 0, 1, "File", 1
	MenuSet FILETITLE, 1, 1, "New ...", 1
	MenuSet FILETITLE, 2, 1, "Open ...", 1
	MenuSet FILETITLE, 3, 1, "-", 1
	MenuSet FILETITLE, 4, 1, "Create Sample Database ...", 1
	MenuSet FILETITLE, 5, 1, "-", 1
	MenuSet FILETITLE, 6, 1, "Exit", 2

	' transaction menu title
	MenuSet TRANSTITLE, 0, 1, "Transaction", 1
	MenuSet TRANSTITLE, 1, 1, "Purchase Order ...", 1
	MenuSet TRANSTITLE, 2, 1, "Invoice List ...", 1

	' customer menu title
	MenuSet CUSTTITLE, 0, 1, "Customer", 1
	MenuSet CUSTTITLE, 1, 1, "Record ...", 1
	MenuSet CUSTTITLE, 2, 1, "List ...", 1

	' inventory menu title
	MenuSet INVTITLE, 0, 1, "Inventory", 1
	MenuSet INVTITLE, 1, 1, "Record ...", 1
	MenuSet INVTITLE, 2, 1, "List ...", 1
END SUB

' ShowError
' Displays an appropriate error message for the given error
'
' errorNum - error number
'
SUB ShowError (errorNum)
	SELECT CASE errorNum
		CASE 10, 55                            ' file open already
			PrintError "Database is already open."
		CASE 52, 54, 84                        ' invalid file
			PrintError "Specified file is not a valid database file."
		CASE 53                                ' file not found
			PrintError "File not found."
		CASE 64                                ' bad file name
			PrintError "Invalid file name."
		CASE 68                                ' device unavailable
			PrintError "Selected device unavailable."
		CASE 71                                ' disk drive not ready
			PrintError "Disk not ready."
		CASE 75                                ' path access error
			PrintError "Invalid path."
		CASE 76                                ' path not found
			PrintError "Path not found."
		CASE 73                                ' ISAM TSR not started
			PrintError "ISAM TSR not running.  Exit and run PROISAMD.EXE at DOS prompt."
		CASE 83                                ' Index not found (i.e. invalid database)
			PrintError "Invalid ISAM table and index format for this application."
		CASE 85                                ' no current record found
			PrintError "No matches found."
		CASE 86                                ' duplicate value for unique index
			PrintError "Duplicate value for a unique index specified."
		CASE 88                                ' database inconsistent
			PrintError "Database has been corrupted.  Use REPAIR facility to correct it."
		CASE 89                                ' out of ISAM buffers
			PrintError "Out of ISAM buffers.  Specify more using PROISAMD /Ib:# option."
		CASE ELSE                               ' catch all
			PrintError "BASIC error #" + LTRIM$(STR$(ERR)) + " occurred."
	END SELECT
END SUB

' TotalInvoice
' Totals a purchase order and returns OK if no errors occurred else
' returns CANCEL.
'
FUNCTION TotalInvoice%
DIM SubTotal AS CURRENCY, TaxTotal AS CURRENCY

	tax$ = EditFieldInquire$(6)
	IF tax$ = "" THEN
		PrintError "Must specify a tax rate before invoice can be totalled."
		TotalInvoice% = FALSE
		EXIT FUNCTION
	ELSE
		InvoiceRec.TaxRate = VAL(tax$)
		IF InvoiceRec.TaxRate = 0 AND LTRIM$(tax$) <> "0" THEN
			PrintError "Tax rate must be a numeric value."
			TotalInvoice% = FALSE
			EXIT FUNCTION
		END IF
	END IF

	SETINDEX TranTabNum, "TransInvIndex"
	SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, 1
	IF EOF(TranTabNum) THEN
		PrintError "No transactions exist to total."
		TotalInvoice% = FALSE
		EXIT FUNCTION
	END IF

	SubTotal = 0
	RETRIEVE TranTabNum, TransRec
	WHILE NOT EOF(TranTabNum) AND TransRec.InvoiceNo = InvoiceRec.InvoiceNo
		RETRIEVE TranTabNum, TransRec
		SubTotal = SubTotal + TransRec.Price * TransRec.Quantity
		MOVENEXT TranTabNum
	WEND

	InvoiceRec.Total = SubTotal + SubTotal * InvoiceRec.TaxRate / 100

	WindowLocate 18, 42
	WindowPrint -2, RIGHT$("          " + FormatS$(SubTotal, "$#,##0.00"), 14)
	WindowLocate 19, 42
	WindowPrint -2, RIGHT$("          " + FormatS$(SubTotal * InvoiceRec.TaxRate / 100, "$#,##0.00"), 14)
	WindowLocate 20, 42
	WindowPrint -2, RIGHT$("          " + FormatS$(InvoiceRec.Total, "$#,##0.00"), 14)

	SETINDEX InvoTabNum, "InvoiceIndex"
	SEEKEQ InvoTabNum, InvoiceRec.InvoiceNo
	IF EOF(InvoTabNum) THEN
		INSERT InvoTabNum, InvoiceRec
	ELSE
		UPDATE InvoTabNum, InvoiceRec
	END IF

	TotalInvoice% = OK
END FUNCTION

' UpdateInventory
' Updates inventory items current stock when a purchase order is made.
'
SUB UpdateInventory (ItemNo AS STRING, Quantity)
	SETINDEX InveTabNum, "ItemIndex"
	SEEKEQ InveTabNum, ItemNo
	RETRIEVE InveTabNum, InventRec

	IF InventRec.stock = 0 AND Quantity > 0 THEN
		PrintError "This item is currently out of stock"
		Quantity = 0
	ELSEIF InventRec.stock < Quantity AND Quantity > 0 THEN
		a$ = "|"
		a$ = a$ + "Current stock  of item number " + ItemNo + " is|"
		a$ = a$ + "less than quantity ordered.           |"
		a$ = a$ + "   ordered: " + STR$(Quantity) + "       |"
		a$ = a$ + "   stock:   " + STR$(InventRec.stock) + "       |"
		a$ = a$ + " Take remaining stock?                   "
		reply = Alert(4, a$, 8, 19, 16, 61, "Yes", "No", "")

		IF reply = 1 THEN
			Quantity = InventRec.stock
			InventRec.stock = 0
			UPDATE InveTabNum, InventRec
		ELSE
			Quantity = 0
		END IF
	ELSE
		InventRec.stock = InventRec.stock - Quantity
		UPDATE InveTabNum, InventRec
	END IF
END SUB

