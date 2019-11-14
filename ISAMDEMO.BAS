'       ISAMDEMO.BAS - Main module of ISAM demonstration program
'
'           Copyright (C) 1989-1990, Microsoft Corporation
'     
'   This demo program uses BASIC 7.1's ISAM features along with its
'   User Interface toolbox to implement a transaction database system.
'   It consists of three modules (ISAMDEMO.BAS, ISAMDEM1.BAS and ISAMDEM2.BAS)
'   and one include file (ISAMDEMO.BI).
'
'   A sample data file "AMAZRAYS.DAT" is provided with this demo.  An ISAM
'   database file must be created from it.  This is done by selecting
'   the "Create Sample Database" option from the File menu.  The sample
'   data file must be located in the current directory (i.e. the directory
'   appearing in the File.Open dialog).
'   NOTE: The sample data provided with this demo is entirely fictitious.
'
'   The PROISAMD.EXE TSR must be installed before the ISAMDEMO can be
'   be run.  If you do NOT have expanded memory available, you should
'   invoke PROISAMD.EXE with the /Ib:n option, where n should be between
'   15-20.  The /Ib: option specifies the number of buffers ISAM needs.
'   Higher n values improve performance. Too few buffers, and the program
'   will fail with an "Insufficient ISAM buffers" error.  If you do HAVE
'   expanded memory, ISAM automatically uses up to 1.2 megabytes, even if
'   you set Ib: to a low value.
'
'   EMS is needed to load and run the demo under QBX.  If you do not
'   have EMS, refer to the command line compile instructions below which
'   will allow you to run the demo from the DOS prompt.  To run the
'   demo under QBX requires a combination QuickLib consisting of
'   UITBEFR.LIB and DTFMTER.LIB.  This is created as follows:
'           LIB isamdem.lib + uitbefr.lib + dtfmter.lib;
'           LINK /Q isamdem.lib, isamdem.qlb,,qbxqlb.lib;
'   Once created, just start QBX with this QuickLib and load the
'   demo's modules (isamdemo.bas, isamdem1.bas and isamdem2.bas).
'
'   To create a compiled version of the isam demo perform the
'   following steps:
'       LIB isamdem.lib + uitbefr.lib + dtfmter.lib;
'       BC /O/X/FS isamdemo.bas;
'       BC /O/X/FS isamdem1.bas;
'       BC /O/X/FS isamdem2.bas;
'       LINK /EX isamdemo isamdem1 isamdem2, isamdemo.exe,, isamdem.lib;
'   "ISAMDEMO" can now be run from the command line.

DEFINT A-Z
'$INCLUDE: 'isamdemo.bi'

DECLARE SUB HandleMenuEvent ()
DECLARE SUB CreateTranList (text$(), cBox AS ANY)
DECLARE SUB BuildSampDB ()

'definitions for UI toolbox
DIM GloTitle(MAXMENU)           AS MenuTitleType
DIM GloItem(MAXMENU, MAXITEM)   AS MenuItemType
DIM GloWindow(MAXWINDOW)        AS windowType
DIM GloButton(MAXBUTTON)        AS buttonType
DIM GloEdit(30)                 AS EditFieldType
DIM GloWindowStack(MAXWINDOW)   AS INTEGER
DIM GloBuffer$(MAXWINDOW + 1, 2)

	STACK 10000                            'increase stack size

	ON ERROR GOTO GlobErr                  'global error handling

	InitAll                                'initialize program

	finished = FALSE                       'main control loop
	WHILE NOT finished
		kbd$ = MenuInkey$
		WHILE MenuCheck(2)
			HandleMenuEvent                    'handle menu selection
		WEND
	WEND

	END

GlobErr:
	ShowError ERR                          'display error message
RESUME NEXT

' BuildSampDB
' Builds sample database from AMAZRAYS.DAT data file provided on the
' distribution disks.  Data file must be located in current directory
' for operation to succeed.
'
SUB BuildSampDB
	ON LOCAL ERROR GOTO BuildErr

	temp$ = "amazrays.mdb"                'sample database file to create
	IF LEN(DIR$(temp$)) = 0 THEN          'if file not exists, then create it
		CLOSE
		filenum = FREEFILE
		temp2$ = "amazrays.dat"             'sample data file
		OPEN temp2$ FOR INPUT AS filenum

		'inform user that database is being built
		WindowOpen 1, 9, 18, 10, 60, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, ""
		WindowLocate 1, 5
		WindowPrint -2, "Creating ISAM file 'AMAZRAYS.MDB'"
		WindowLocate 2, 5
		WindowPrint -2, "Creating tables..."

		'create ISAM tables
		CustTabNum = FREEFILE
		OPEN temp$ FOR ISAM CustRecord "CustTable" AS CustTabNum
		InveTabNum = FREEFILE
		OPEN temp$ FOR ISAM InventRecord "InventTable" AS InveTabNum
		InvoTabNum = FREEFILE
		OPEN temp$ FOR ISAM InvoiceRecord "InvoiceTable" AS InvoTabNum
		TranTabNum = FREEFILE
		OPEN temp$ FOR ISAM TransRecord "TransTable" AS TranTabNum

		CreateIndexes                       'create table indexes

		'read information in from data file
		WindowLocate 2, 5
		WindowPrint -2, "Reading data...           "
		INPUT #filenum, num
		FOR i = 1 TO num
			INPUT #filenum, CustRec.AcctNo, CustRec.Company, CustRec.Street, CustRec.City, CustRec.state, CustRec.Zip, CustRec.Phone1, CustRec.Phone2, CustRec.Contact, CustRec.Opened
			INSERT CustTabNum, CustRec
		NEXT i

		INPUT #filenum, num
		FOR i = 1 TO num
			INPUT #filenum, InventRec.ItemNo, InventRec.Descrip, InventRec.Cost, InventRec.Retail, InventRec.stock, InventRec.Vendor
			INSERT InveTabNum, InventRec
		NEXT i

		INPUT #filenum, num
		FOR i = 1 TO num
			INPUT #filenum, InvoiceRec.InvoiceNo, InvoiceRec.Date, InvoiceRec.AcctNo, InvoiceRec.TaxRate, InvoiceRec.Total
			INSERT InvoTabNum, InvoiceRec
		NEXT i

		INPUT #filenum, num
		FOR i = 1 TO num
			INPUT #filenum, TransRec.TransNo, TransRec.InvoiceNo, TransRec.ItemNo, TransRec.Quantity, TransRec.Price
			INSERT TranTabNum, TransRec
		NEXT i

		CLOSE
		WindowClose 1
		CustTabNum = 0
		InveTabNum = 0
		InvoTabNum = 0
		TranTabNum = 0
	ELSE
		PrintError "AMAZRAYS.MDB already exists in current directory."
	END IF

	EXIT SUB

BuildErr:
	IF ERR = 53 THEN
		temp$ = "Sample data file AMAZRAYS.DAT not found in " + CURDIR$
		PrintError temp$
		EXIT SUB
	ELSE
		ShowError ERR
		IF ERR = 55 OR ERR = 10 OR ERR = 73 THEN
			WindowClose 1
			EXIT SUB
		END IF
	END IF
RESUME NEXT
END SUB

' CreateCInvList
' Creates list of a customer's invoices to be displayed within a list box
'
' text$() - list
' cBox - list box
' oBox - original list box
' barpos - scroll bar position
'
SUB CreateCInvList (text$(), cBox AS ListBox, oBox AS ListBox, barpos)

	IF cBox.curTop <> oBox.curTop THEN          'if list contents changed
		GOSUB GetFirstCDateRecord

		First$ = text$(1)
		FOR i = 1 TO cBox.bLen                  'get new list contents
			IF EOF(InvoTabNum) THEN
				FOR j = i TO cBox.bLen
						text$(j) = ""
					NEXT j
					EXIT FOR
			END IF
			RETRIEVE InvoTabNum, InvoiceRec
			IF UCASE$(InvoiceRec.Date) >= Bound2$ OR InvoiceRec.AcctNo <> CustRec.AcctNo THEN
				MOVELAST InvoTabNum
				text$(i) = ""
			ELSE
				text$(i) = " " + MID$(InvoiceRec.Date, 3, 2) + "-" + MID$(InvoiceRec.Date, 5, 2) + "-" + MID$(InvoiceRec.Date, 1, 2)
				text$(i) = text$(i) + "     " + InvoiceRec.InvoiceNo + "    " + RIGHT$("             " + FormatS$(InvoiceRec.Total, "$#,##0.00"), 14)
			END IF
			MOVENEXT InvoTabNum
		NEXT i

		IF text$(cBox.bLen) = "" AND cBox.curTop > 1 THEN
			barpos = cBox.bLen - 2
			cBox.curTop = oBox.curTop
			cBox.curPos = oBox.curPos
			FOR i = cBox.bLen TO 2 STEP -1
				text$(i) = text$(i - 1)
			NEXT i
			text$(1) = First$
		END IF
		temp$ = MID$(text$(1), 2, 8)
		temp$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
		SEEKEQ InvoTabNum, CustRec.AcctNo, temp$, MID$(text$(1), 15, 6)
	ELSEIF text$(cBox.bLen) = "" THEN           'if list smaller than box
		IF cBox.curTop = 1 THEN barpos = 1
		FOR i = 1 TO cBox.bLen                  'adjust position
			IF text$(i) = "" AND cBox.curPos >= i THEN
				cBox.curPos = i - 1
				EXIT FOR
			END IF
		NEXT i
	END IF

	EXIT SUB

'get first record to display in list
GetFirstCDateRecord:
	IF cBox.curTop = 1 THEN                     'if top of list then first record
		barpos = 1
		SEEKGE InvoTabNum, CustRec.AcctNo, Bound1$, "0"
	ELSEIF cBox.curTop + cBox.curPos - 1 = 9999 THEN    ' if bottom of list then start with
														' last record and work up
		SEEKGE InvoTabNum, CustRec.AcctNo, Bound2$, "0"
		cBox.curPos = cBox.bLen
		FOR i = 1 TO cBox.bLen
			MOVEPREVIOUS InvoTabNum
			IF BOF(InvoTabNum) THEN
				MOVEFIRST InvoTabNum
				cBox.curTop = 1
				cBox.curPos = i - 1
				EXIT FOR
			ELSE
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) < Bound1$ OR InvoiceRec.AcctNo <> CustRec.AcctNo$ THEN
					cBox.curTop = 1
					cBox.curPos = i - 1
					MOVENEXT InvoTabNum
					EXIT FOR
				END IF
			END IF
		NEXT i
		barpos = cBox.bLen - 2
	ELSE                                        'determine position in database
		IF cBox.curTop < oBox.curTop THEN
			MOVEPREVIOUS InvoTabNum
			IF BOF(InvoTabNum) THEN
				barpos = 1
				cBox.curTop = 1
				MOVEFIRST InvoTabNum
			ELSE
				barpos = Max(barpos - 1, 1)
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) < Bound1$ OR InvoiceRec.AcctNo <> CustRec.AcctNo THEN
					MOVENEXT InvoTabNum
				END IF
			END IF
		ELSEIF cBox.curTop > oBox.curTop THEN
			MOVENEXT InvoTabNum
			IF EOF(InvoTabNum) THEN
				barpos = cBox.bLen - 2
				cBox.curTop = oBox.curTop
				MOVELAST InvoTabNum
			ELSE
				barpos = Min(barpos + 1, cBox.bLen - 2)
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) > Bound2$ OR InvoiceRec.AcctNo <> CustRec.AcctNo THEN
					MOVEPREVIOUS InvoTabNum
				END IF
			END IF
		END IF
	END IF

RETURN

END SUB

' CreateCustList
' Creates list of a customers to be displayed within a list box
'
' text$() - list
' cBox - list box
' oBox - original list box
' barpos - scroll bar position
'
SUB CreateCustList (text$(), cBox AS ListBox, oBox AS ListBox, barpos)
	IF cBox.curTop <> oBox.curTop THEN          'if list contents changed
		GOSUB GetFirstAlphaRecord

		First$ = text$(1)
		FOR i = 1 TO cBox.bLen                  'get new list contents
			IF EOF(CustTabNum) OR LOF(CustTabNum) = 0 THEN
				FOR j = i TO cBox.bLen
					text$(j) = ""
				NEXT j
				IF LOF(CustTabNum) = 0 THEN cBox.curPos = 0
				EXIT FOR
			END IF
			RETRIEVE CustTabNum, CustRec
			IF UCASE$(MID$(CustRec.Company, 1, LEN(Bound2$))) >= Bound2$ THEN
				MOVELAST CustTabNum
				text$(i) = ""
			ELSE
				text$(i) = " " + MID$(CustRec.Company, 1, 23) + "  " + CustRec.AcctNo + "        "
				text$(i) = text$(i) + MID$(CustRec.Opened, 3, 2) + "-" + MID$(CustRec.Opened, 5, 2) + "-" + MID$(CustRec.Opened, 1, 2)
			END IF
			MOVENEXT CustTabNum
		NEXT i

		IF text$(cBox.bLen) = "" AND cBox.curTop > 1 THEN
			barpos = cBox.bLen - 2
			cBox.curTop = oBox.curTop
			cBox.curPos = oBox.curPos
			FOR i = cBox.bLen TO 2 STEP -1
				text$(i) = text$(i - 1)
			NEXT i
			text$(1) = First$
		END IF
		SEEKGE CustTabNum, MID$(text$(1), 2, 23), MID$(text$(1), 27, 5)
	ELSEIF text$(cBox.bLen) = "" THEN
		IF cBox.curTop = 1 THEN barpos = 1
		FOR i = 1 TO cBox.bLen
			IF text$(i) = "" AND cBox.curPos >= i THEN
				cBox.curPos = i - 1
				EXIT FOR
			END IF
		NEXT i
	END IF
	EXIT SUB

'get first record to display in list
GetFirstAlphaRecord:               
	IF cBox.curTop = 1 THEN                     'if top of list then first record
		SEEKGE CustTabNum, Bound1$, "0"
		barpos = 1
	ELSEIF cBox.curTop + cBox.curPos - 1 = 9999 THEN  'if bottom of list then start with
														'last record and work back
		IF Bound2$ = "[" THEN
			MOVELAST CustTabNum
			MOVENEXT CustTabNum
		ELSE
			SEEKGE CustTabNum, Bound2$, "0"
		END IF
		cBox.curPos = cBox.bLen
		FOR i = 1 TO cBox.bLen
			MOVEPREVIOUS CustTabNum
			IF BOF(CustTabNum) THEN
				MOVEFIRST CustTabNum
				cBox.curTop = 1
				cBox.curPos = i - 1
				EXIT FOR
			ELSE
				RETRIEVE CustTabNum, CustRec
				IF UCASE$(MID$(CustRec.Company, 1, LEN(Bound1$))) < Bound1$ THEN
					cBox.curTop = 1
					cBox.curPos = i - 1
					MOVENEXT CustTabNum
					EXIT FOR
				END IF
			END IF
		NEXT i
		barpos = cBox.bLen - 2
	ELSE                                        'determine position in database
		IF cBox.curTop < oBox.curTop THEN
			MOVEPREVIOUS CustTabNum
			IF BOF(CustTabNum) THEN
				barpos = 1
				cBox.curTop = 1
				MOVEFIRST CustTabNum
			ELSE
				barpos = Max(barpos - 1, 1)
				RETRIEVE CustTabNum, CustRec
				IF UCASE$(MID$(CustRec.Company, 1, LEN(Bound1$))) < Bound1$ THEN
					MOVENEXT CustTabNum
				END IF
			END IF
		ELSEIF cBox.curTop > oBox.curTop THEN
			MOVENEXT CustTabNum
			IF EOF(CustTabNum) THEN
				barpos = cBox.bLen - 2
				cBox.curTop = oBox.curTop
				MOVELAST CustTabNum
			ELSE
				barpos = Min(cBox.bLen - 2, barpos + 1)
				RETRIEVE CustTabNum, CustRec
				IF UCASE$(MID$(CustRec.Company, 1, LEN(Bound2$))) > Bound2$ THEN
					MOVEPREVIOUS CustTabNum
				END IF
			END IF
		END IF
	END IF

RETURN

END SUB

' CreateIndexes
' Creates indexes for any new ISAM tables created in the demo
'
SUB CreateIndexes
	WindowLocate 2, 5
	WindowPrint -2, "Creating table indexes..."
	CREATEINDEX CustTabNum, "AcctIndex", TRUE, "AcctNo"
	CREATEINDEX CustTabNum, "CompanyIndex", TRUE, "Company", "AcctNo"
	CREATEINDEX InveTabNum, "ItemIndex", TRUE, "ItemNo"
	CREATEINDEX InvoTabNum, "InvoiceIndex", TRUE, "InvoiceNo"
	CREATEINDEX InvoTabNum, "DateIndex", TRUE, "Date", "InvoiceNo"
	CREATEINDEX InvoTabNum, "InvAcctIndex", TRUE, "AcctNo", "Date", "InvoiceNo"
	CREATEINDEX TranTabNum, "TransInvIndex", TRUE, "InvoiceNo", "TransNo"
END SUB

' CreateInveList
' Creates list of a inventory items to be displayed within a list box
'
' text$() - list
' cBox - list box
' oBox - original list box
' barpos - scroll bar position
'
SUB CreateInveList (text$(), cBox AS ListBox, oBox AS ListBox, barpos)
	IF cBox.curTop <> oBox.curTop THEN          'if list contents changed
		GOSUB GetFirstPhysRecord

		First$ = text$(1)
		FOR i = 1 TO cBox.bLen                  'get new list contents
			IF EOF(InveTabNum) OR LOF(InveTabNum) = 0 THEN
				FOR j = i TO cBox.bLen
					text$(j) = ""
				NEXT j
				IF LOF(InveTabNum) = 0 THEN cBox.curPos = 0
				EXIT FOR
			END IF
			RETRIEVE InveTabNum, InventRec
			text$(i) = " " + InventRec.ItemNo + "   " + MID$(InventRec.Descrip, 1, 24) + "  " + RIGHT$("     " + STR$(InventRec.stock), 5)
			text$(i) = text$(i) + "  " + RIGHT$("          " + FormatS$(InventRec.Cost, "$#,##0.00"), 11) + "  " + RIGHT$("          " + FormatS$(InventRec.Retail, "$#,##0.00"), 11)
			MOVENEXT InveTabNum
		NEXT i

		IF text$(cBox.bLen) = "" AND cBox.curTop > 1 THEN
			barpos = cBox.bLen - 2
			cBox.curPos = oBox.curPos
			cBox.curTop = oBox.curTop
			FOR i = cBox.bLen TO 2 STEP -1
				text$(i) = text$(i - 1)
			NEXT i
			text$(1) = First$
		END IF
		SEEKEQ InveTabNum, MID$(text$(1), 2, 5)
	ELSEIF text$(cBox.bLen) = "" THEN
		IF cBox.curTop = 1 THEN barpos = 1
		FOR i = 1 TO cBox.bLen
			IF text$(i) = "" AND cBox.curPos >= i THEN
				cBox.curPos = i - 1
				EXIT FOR
			END IF
		NEXT i
	END IF

	EXIT SUB

'get first record to display in list
GetFirstPhysRecord:
	IF cBox.curTop = 1 THEN                     'if top of list then first record
		MOVEFIRST InveTabNum
		barpos = 1
	ELSEIF cBox.curTop + cBox.curPos - 1 = 9999 THEN  'if bottom of list then start with
														'last record and work back
		MOVELAST InveTabNum
		cBox.curPos = cBox.bLen
		FOR i = 1 TO cBox.bLen - 1
			MOVEPREVIOUS InveTabNum
			IF BOF(InveTabNum) THEN
				MOVEFIRST InveTabNum
				cBox.curTop = 1
				cBox.curPos = i
				EXIT FOR
			END IF
		NEXT i
		barpos = cBox.bLen - 2
	ELSE                                        'determine position in database
		IF cBox.curTop < oBox.curTop THEN
			barpos = Max(barpos - 1, 1)
			MOVEPREVIOUS InveTabNum
			IF BOF(InveTabNum) THEN
				cBox.curTop = 1
				MOVEFIRST InveTabNum
				barpos = 1
			END IF
		ELSEIF cBox.curTop > oBox.curTop THEN
			barpos = Min(barpos + 1, cBox.bLen - 2)
			MOVENEXT InveTabNum
		END IF
	END IF
RETURN

END SUB

' CreateInvoList
' Creates list of invoices to be displayed within a list box
'
' text$() - list
' cBox - list box
' oBox - original list box
' barpos - scroll bar position
'
SUB CreateInvoList (text$(), cBox AS ListBox, oBox AS ListBox, barpos)
	IF cBox.curTop <> oBox.curTop THEN          'if list contents changed
		GOSUB GetFirstDateRecord

		First$ = text$(1)
		FOR i = 1 TO cBox.bLen                  'get new list contents
			IF EOF(InvoTabNum) THEN
				FOR j = i TO cBox.bLen
					text$(j) = ""
				NEXT j
				EXIT FOR
			END IF
			RETRIEVE InvoTabNum, InvoiceRec
			IF UCASE$(InvoiceRec.Date) >= Bound2$ THEN
				MOVELAST InvoTabNum
				text$(i) = ""
			ELSE
				text$(i) = " " + MID$(InvoiceRec.Date, 3, 2) + "-" + MID$(InvoiceRec.Date, 5, 2) + "-" + MID$(InvoiceRec.Date, 1, 2)
				text$(i) = text$(i) + "     " + InvoiceRec.InvoiceNo + "        " + InvoiceRec.AcctNo + "   " + RIGHT$("             " + FormatS$(InvoiceRec.Total, "$#,##0.00"), 14)
			END IF
			MOVENEXT InvoTabNum
		NEXT i

		IF text$(cBox.bLen) = "" AND cBox.curTop > 1 THEN
			barpos = cBox.bLen - 2
			cBox.curTop = oBox.curTop
			cBox.curPos = oBox.curPos
			FOR i = cBox.bLen TO 2 STEP -1
				text$(i) = text$(i - 1)
			NEXT i
			text$(1) = First$
		END IF
		temp$ = MID$(text$(1), 2, 8)
		temp$ = MID$(temp$, 7, 2) + MID$(temp$, 1, 2) + MID$(temp$, 4, 2)
		SEEKEQ InvoTabNum, temp$, MID$(text$(1), 15, 6)
	ELSEIF text$(cBox.bLen) = "" THEN
		IF cBox.curTop = 1 THEN barpos = 1
		FOR i = 1 TO cBox.bLen
			IF text$(i) = "" AND cBox.curPos >= i THEN
				cBox.curPos = i - 1
				EXIT FOR
				END IF
		NEXT i
	END IF

	EXIT SUB

'get first record to display in list
GetFirstDateRecord:
	IF cBox.curTop = 1 THEN                     'if top of list then first record
		barpos = 1
		SEEKGE InvoTabNum, Bound1$, "0"
	ELSEIF cBox.curTop + cBox.curPos - 1 = 9999 THEN  'if bottom of list then start with
														'last record and work back
		SEEKGE InvoTabNum, Bound2$, "0"
		cBox.curPos = cBox.bLen
		FOR i = 1 TO cBox.bLen
			MOVEPREVIOUS InvoTabNum
			IF BOF(InvoTabNum) THEN
				MOVEFIRST InvoTabNum
				cBox.curTop = 1
				cBox.curPos = i - 1
				EXIT FOR
			ELSE
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) < Bound1$ THEN
					cBox.curTop = 1
					cBox.curPos = i - 1
					MOVENEXT InvoTabNum
					EXIT FOR
				END IF
			END IF
		NEXT i
		barpos = cBox.bLen - 2                  'determine position in database
	ELSE
		IF cBox.curTop < oBox.curTop THEN
			MOVEPREVIOUS InvoTabNum
			IF BOF(InvoTabNum) THEN
				barpos = 1
				cBox.curTop = 1
				MOVEFIRST InvoTabNum
			ELSE
				barpos = Max(barpos - 1, 1)
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) < Bound1$ THEN
					MOVENEXT InvoTabNum
				END IF
			END IF
		ELSEIF cBox.curTop > oBox.curTop THEN
			MOVENEXT InvoTabNum
			IF EOF(InvoTabNum) THEN
				barpos = cBox.bLen - 2
				cBox.curTop = oBox.curTop
				MOVELAST InvoTabNum
			ELSE
				barpos = Min(barpos + 1, cBox.bLen - 2)
				RETRIEVE InvoTabNum, InvoiceRec
				IF UCASE$(InvoiceRec.Date) > Bound2$ THEN
					MOVEPREVIOUS InvoTabNum
				END IF
			END IF
		END IF
	END IF

RETURN

END SUB

' CreateTranList
' Creates list of an invoice's transactions to be displayed within a list box
'
' text$() - list
' cBox - list box
' oBox - original list box
' barpos - scroll bar position
'
SUB CreateTranList (text$(), cBox AS ListBox)
	SETINDEX TranTabNum, "TransInvIndex"
	SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, cBox.curTop
	FOR i = 1 TO cBox.bLen
		IF cBox.curTop + i - 1 < 10 THEN
			sp$ = " "
		ELSE
			sp$ = ""
		END IF
		IF EOF(TranTabNum) THEN
			text$(i) = sp$ + LTRIM$(STR$(cBox.curTop + i - 1)) + "."
		ELSE
			RETRIEVE TranTabNum, TransRec
			IF TransRec.InvoiceNo <> InvoiceRec.InvoiceNo THEN
				text$(i) = sp$ + LTRIM$(STR$(cBox.curTop + i - 1)) + "."
			ELSE
				text$(i) = sp$ + LTRIM$(STR$(cBox.curTop + i - 1)) + ". " + TransRec.ItemNo + "   " + RIGHT$("         " + FormatS$(TransRec.Price, "$#,##0.00"), 11) + "     "
				text$(i) = text$(i) + RIGHT$("   " + STR$(TransRec.Quantity), 5) + "  " + RIGHT$("         " + FormatS$(TransRec.Price * TransRec.Quantity, "$#,##0.00"), 14)
			END IF
			MOVENEXT TranTabNum
		END IF
	NEXT i

END SUB

' HandleMenuEvent
' Determines the action to perform when user makes a menu selection.
'
SUB HandleMenuEvent
	ON LOCAL ERROR GOTO HandleErr

	menu = MenuCheck(0)
	item = MenuCheck(1)

	' clear global records
	ClearInveRecord
	ClearCustRecord
	ClearInvoRecord
	ClearTranRecord

	SELECT CASE menu
		' file menu selection
		CASE FILETITLE
			SELECT CASE item
				' create new ISAM file
				CASE 1: NewDB
				' open existing ISAM file
				CASE 2: OpenDB
				' build sample database
				CASE 4: BuildSampDB
				' exit program
				CASE 6: Quit
			END SELECT

		' transaction menu selection
		CASE TRANSTITLE
			BEGINTRANS
			SELECT CASE item
				' purchase order
				CASE 1
					RunInvoice 1
				' list of invoices
				CASE 2
					RunList 1, 5
			END SELECT
			COMMITTRANS

		' customer menu selection
		CASE CUSTTITLE
			BEGINTRANS
			SELECT CASE item
				' customer record
				CASE 1
					level = 0
					RunCustRec 1
				' list of customers
				CASE 2
					level = 0
					RunList 1, 3
			END SELECT
			COMMITTRANS

		' inventory menu selection
		CASE INVTITLE
			BEGINTRANS
			SELECT CASE item
				' inventory record
				CASE 1
					level = 0
					RunInventRec 1
				' list of inventory items
				CASE 2
					level = 0
					RunList 1, 2
			END SELECT
			COMMITTRANS
	END SELECT

	EXIT SUB

HandleErr:
	ShowError ERR
	EXIT SUB
RESUME NEXT

END SUB

