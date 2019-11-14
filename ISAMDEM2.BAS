'       ISAMDEM2.BAS - third module of the ISAM demonstration program.
'
'               Copyright (C) 1989-1990, Microsoft Corporation
'
'   Main module - ISAMDEMO.BAS
'   Include files - ISAMDEMO.BI
'
'
'$INCLUDE: 'isamdemo.bi'

DEFINT A-Z
' RunCustRec
' Controls customer record form.  Provides viewing, updating, adding and
' deleting of customers.
'
' Handle - window handle
'
SUB RunCustRec (handle)
	ON LOCAL ERROR GOTO CustErr

	IF ISAMfile$ = "" THEN                          'isam file must be opened
		PrintError "A database file must be opened before records can be viewed."
		EXIT SUB
	END IF

	savePosition = SAVEPOINT                        'ISAM savepoint
	ErrFlag = FALSE

	GOSUB ShowCustRec

	' window control loop
	finished = FALSE
	WHILE finished = FALSE
		WindowDo curBut, currEditField                ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' button pressed
				curBut = Dialog(1)
				currEditField = 0
				ButtonSetState pushButton, 1
				ButtonSetState curBut, 2
				pushButton = curBut
				GOSUB CustRecButton
			CASE 2                                      ' edit field
				IF level < 2 THEN
					curBut = 0
					currEditField = Dialog(2)
				END IF
			CASE 6                                      ' enter
				curBut = pushButton
				GOSUB CustRecButton
			CASE 7, 11                                  ' tab, down arrow
				IF level < 2 THEN
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE IS < 9
								currEditField = currEditField + 1
							CASE 9
								currEditField = 0
								curBut = 1
						END SELECT
					CASE 2
						curBut = 4 + RecCurrent
					CASE 4
						curBut = 8 + 3 * RecCurrent
					CASE 5
						IF NOT RecCurrent THEN
							curBut = 8
						ELSEIF level = 0 THEN
							curBut = 6
						ELSEIF HasInvoice THEN
							curBut = 7
						ELSE
							curBut = 8
						END IF
					CASE 6
						curBut = 8 + HasInvoice
					CASE 9
						currEditField = 1
						curBut = 0
						ButtonSetState pushButton, 1
						pushButton = 1
						ButtonSetState pushButton, 2
					CASE ELSE
						curBut = curBut + 1
				END SELECT
				IF curBut <> 0 THEN
					ButtonSetState pushButton, 1
					ButtonSetState curBut, 2
					pushButton = curBut
				END IF
				END IF
			CASE 8, 10                                  ' back tab, up arrow
				IF level < 2 THEN
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE 1
								currEditField = 0
								curBut = 9
							CASE ELSE
								currEditField = currEditField - 1
						END SELECT
					CASE 1
						currEditField = 9
						curBut = 0
					CASE 4
						curBut = 2 - RecCurent
					CASE 7
						curBut = 6 - level
					CASE 8
						IF NOT RecCurrent THEN
							curBut = 4
						ELSEIF HasInvoice THEN
							curBut = 7
						ELSEIF level = 0 THEN
							curBut = 6
						ELSE
							curBut = 5
						END IF
					CASE ELSE
						curBut = curBut - 1
				END SELECT
				IF curBut <> 0 THEN
						ButtonSetState pushButton, 1
						pushButton = curBut
						ButtonSetState pushButton, 2
				END IF
				END IF
			CASE 9                                      ' escape
				finished = CANCEL
			CASE 14                                     ' space bar
				IF curBut > 0 THEN GOSUB CustRecButton
		 END SELECT
	WEND

	IF finished = OK THEN
		CHECKPOINT
	ELSE
		ROLLBACK savePosition
	END IF

	WindowClose handle
	EXIT SUB

CustErr:
	SELECT CASE ERR
		CASE 52                                       'open database before continuing
			PrintError "A database file must be opened before customer records can be displayed."
			EXIT SUB
		CASE ELSE
			ShowError ERR
			IF ERR = 86 THEN                            'can't add duplicate records
				RESUME CustErrReturn
			ELSEIF ERR = 89 THEN                        'not enough ISAM buffers to continue
				WindowClose handle
				ROLLBACK savePosition
				EXIT SUB
			END IF
	END SELECT
	
RESUME NEXT

CustRecButton:                                    'process button selection
	IF level > 1 THEN
		finished = CANCEL
	ELSE
	SELECT CASE curBut
		CASE 1                                        'get next record
			GOSUB RetrieveCustRecord
		CASE 2                                        'clear form for new record
			GOSUB ClearCustEditFields
		CASE 3                                        'update current form
			GOSUB RetrieveCustEditFields
			IF NOT ErrFlag THEN
				UPDATE CustTabNum, CustRec
			ELSE
				ErrFlag = FALSE
			END IF
		CASE 4                                       'add new record
			GOSUB RetrieveCustEditFields
			IF NOT ErrFlag THEN
				CustRec.Opened = MID$(DATE$, 9, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 4, 2)
				INSERT CustTabNum, CustRec
				GOSUB ClearCustEditFields
			ELSE
				ErrFlag = FALSE
			END IF
		CASE 5                                        'delete record
			DELETE CustTabNum
			GOSUB ClearCustEditFields
		CASE 6                                        'purchase order
			ClearInveRecord
			ClearInvoRecord
			RunInvoice WindowNext
			GOSUB CheckInvoice
			GOSUB UpdateButtons
			level = 0
		CASE 7                                        'display list of invoices
			oldlevel = level
			RunList WindowNext, 4
			level = oldlevel
		CASE 8
			finished = OK
		CASE 9
			finished = CANCEL
	END SELECT
	END IF
CustErrReturn:
RETURN

RetrieveCustRecord:
	key2$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
	key1$ = LTRIM$(EditFieldInquire$(2))
	status = NextRecord%(CustTabNum, key1$, origkey1$, lastkey1$, key2$, origkey2$, lastkey2$)

	IF status = 1 THEN
		RETRIEVE CustTabNum, CustRec
		IF LEFT$(CustRec.AcctNo, LEN(origkey2$)) <> origkey2$ OR UCASE$(LEFT$(CustRec.Company, LEN(origkey1$))) <> UCASE$(origkey1$) THEN
			IF LEFT$(lastkey2$, LEN(origkey2$)) <> origkey2$ OR UCASE$(LEFT$(lastkey1$, LEN(origkey1$))) <> UCASE$(origkey1$) THEN
				PrintError "No records found matching search criteria."
				ClearCustRecord
				CustRec.AcctNo = key2$
				CustRec.Company = key1$
				origkey1$ = ""
				origkey2$ = ""
				ButtonOpen 1, ButtonInquire(1), "Find", 2, 58, 0, 0, 1
				status = 0
				RecCurrent = FALSE
			ELSE
				key1$ = origkey1$
				key2$ = origkey2$
				status = NextRecord%(CustTabNum, key1$, origkey1$, lastkey1$, key2$, origkey2$, lastkey2$)
				RETRIEVE CustTabNum, CustRec
			END IF
		END IF
		IF status = 1 THEN
			ButtonOpen 1, ButtonInquire(1), "Next", 2, 58, 0, 0, 1
			RecCurrent = TRUE
			GOSUB InsertCustRec
			GOSUB CheckInvoice
		END IF
	ELSEIF status = 2 THEN
		PrintError "No records found matching search criteria."
		ClearCustRecord
		RecCurrent = FALSE
		CustRec.AcctNo = key2$
		CustRec.Company = key1$
		origkey1$ = ""
		origkey2$ = ""
		ButtonOpen 1, ButtonInquire(1), "Find", 2, 58, 0, 0, 1
	ELSEIF status = 3 THEN
		PrintError "Customer database is empty."
		RecCurrent = FALSE
	END IF
	GOSUB UpdateButtons
RETURN

RetrieveCustEditFields:
	CustRec.AcctNo = UCASE$(LTRIM$(EditFieldInquire$(1)))
	IF LEN(RTRIM$(CustRec.AcctNo)) = 0 THEN
		PrintError "Customer must have an account number."
		ErrFlag = TRUE
	END IF
	CustRec.Company = LTRIM$(EditFieldInquire$(2))
	IF LEN(RTRIM$(CustRec.Company)) = 0 THEN
		PrintError "Customer must have a company name."
		ErrFlag = TRUE
	END IF
	CustRec.Street = EditFieldInquire$(3)
	CustRec.City = EditFieldInquire$(4)
	CustRec.state = EditFieldInquire$(5)
	CustRec.Zip = EditFieldInquire$(6)
	CustRec.Phone1 = EditFieldInquire$(7)
	CustRec.Phone2 = EditFieldInquire$(8)
	CustRec.Contact = EditFieldInquire$(9)
RETURN

InsertCustRec:
	EditFieldOpen 1, RTRIM$(CustRec.AcctNo), 3, 13, 0, 7, 6, 5
	EditFieldOpen 2, RTRIM$(CustRec.Company), 6, 13, 0, 7, 39, 70
	lastkey2$ = EditFieldInquire$(1)
	lastkey1$ = EditFieldInquire$(2)
	EditFieldOpen 3, RTRIM$(CustRec.Street), 9, 13, 0, 7, 39, 70
	EditFieldOpen 4, RTRIM$(CustRec.City), 12, 13, 0, 7, 17, 70
	EditFieldOpen 5, RTRIM$(CustRec.state), 12, 36, 0, 7, 3, 2
	EditFieldOpen 6, RTRIM$(CustRec.Zip), 12, 46, 0, 7, 6, 5
	EditFieldOpen 7, RTRIM$(CustRec.Phone1), 15, 13, 0, 7, 15, 14
	EditFieldOpen 8, RTRIM$(CustRec.Phone2), 15, 37, 0, 7, 15, 14
	EditFieldOpen 9, RTRIM$(CustRec.Contact), 18, 13, 0, 7, 39, 70
	WindowLocate 3, 45
	WindowPrint -2, MID$(CustRec.Opened, 3, 2) + "-" + MID$(CustRec.Opened, 5, 2) + "-" + MID$(CustRec.Opened, 1, 2)
RETURN

ClearCustEditFields:
	EditFieldOpen 1, "", 3, 13, 0, 7, 6, 5
	EditFieldOpen 2, "", 6, 13, 0, 7, 39, 70
	EditFieldOpen 3, "", 9, 13, 0, 7, 39, 70
	EditFieldOpen 4, "", 12, 13, 0, 7, 17, 70
	EditFieldOpen 5, "", 12, 36, 0, 7, 3, 2
	EditFieldOpen 6, "", 12, 46, 0, 7, 6, 5
	EditFieldOpen 7, "", 15, 13, 0, 7, 15, 14
	EditFieldOpen 8, "", 15, 37, 0, 7, 15, 14
	EditFieldOpen 9, "", 18, 13, 0, 7, 39, 70
	WindowLocate 3, 45
	WindowPrint -2, "        "

	lastkey1$ = ""
	lastkey2$ = ""
	ClearCustRecord
	RecCurrent = FALSE
	IF level < 2 THEN ButtonOpen 1, 1, "Find", 2, 58, 0, 0, 1
	GOSUB UpdateButtons
RETURN

UpdateButtons:
	IF NOT RecCurrent THEN
		ButtonClose 3
		ButtonClose 5
		ButtonClose 6
		ButtonClose 7
		IF curBut = 5 THEN
			curBut = 1
			pushButton = 1
			ButtonSetState 1, 2
		END IF
		WindowColor 8, 7
		WindowLocate 6, 58
		WindowPrint -2, "< Update >"
		WindowLocate 8, 58
		WindowPrint -2, "< Delete >"
		WindowLocate 11, 58
		WindowPrint -2, "< P.O. >"
		WindowLocate 12, 58
		WindowPrint -2, "< Invoices >"
		WindowColor 0, 7
		HasInvoices = FALSE
	ELSE
		ButtonOpen 3, 1, "Update", 6, 58, 0, 0, 1
		ButtonOpen 5, 1, "Delete", 8, 58, 0, 0, 1
		IF level = 1 THEN
			WindowColor 8, 7
			WindowLocate 11, 58
			WindowPrint -2, "< P.O. >"
			WindowColor 0, 7
		ELSE
			ButtonOpen 6, 1, "P.O.", 11, 58, 0, 0, 1
		END IF
		IF HasInvoice THEN
			ButtonOpen 7, 1, "Invoices", 12, 58, 0, 0, 1
		ELSE
			ButtonClose 7
			WindowColor 8, 7
			WindowLocate 12, 58
			WindowPrint -2, "< Invoices >"
			WindowColor 0, 7
		END IF
	END IF
RETURN

CheckInvoice:
	HasInvoice = FALSE
	SETINDEX InvoTabNum, "InvAcctIndex"
	SEEKGE InvoTabNum, CustRec.AcctNo, "0", "0"
	IF NOT EOF(InvoTabNum) THEN
		RETRIEVE InvoTabNum, InvoiceRec
		IF InvoiceRec.AcctNo = CustRec.AcctNo THEN HasInvoice = TRUE
	END IF
	ClearInvoRecord
RETURN

ShowCustRec:
	DispCustWin handle

	IF LEN(RTRIM$(CustRec.AcctNo)) = 0 AND LEN(RTRIM$(CustRec.Company)) = 0 THEN
		GOSUB ClearCustEditFields
	ELSE
		GOSUB InsertCustRec
		IF RTRIM$(CustRec.Company) = "" THEN
			SETINDEX CustTabNum, "AcctIndex"
			SEEKEQ CustTabNum, CustRec.AcctNo
		ELSE
			SETINDEX CustTabNum, "CompanyIndex"
			SEEKEQ CustTabNum, CustRec.Company, CustRec.AcctNo
		END IF
		IF EOF(CustTabNum) THEN
			IF level < 2 THEN ButtonOpen 1, 1, "Find", 2, 58, 0, 0, 1
			lastkey1$ = ""
			lastkey2$ = ""
			RecCurrent = FALSE
		ELSE
			IF level < 2 THEN
				ButtonOpen 1, 1, "Next", 2, 58, 0, 0, 1
				RecCurrent = TRUE
				GOSUB CheckInvoice
			END IF
		END IF
	END IF

	IF level > 1 THEN
		ButtonOpen 1, 2, "Cancel", 16, 58, 0, 0, 1
		WindowColor 8, 7
		WindowLocate 2, 58
		WindowPrint -2, "< Next >"
		WindowLocate 5, 58
		WindowPrint -2, "< New >"
		WindowLocate 6, 58
		WindowPrint -2, "< Update >"
		WindowLocate 7, 58
		WindowPrint -2, "< Add >"
		WindowLocate 8, 58
		WindowPrint -2, "< Delete >"
		WindowLocate 11, 58
		WindowPrint -2, "< P.O. >"
		WindowLocate 12, 58
		WindowPrint -2, "< Invoices >"
		WindowLocate 15, 58
		WindowPrint -2, "< Commit >"
		WindowColor 0, 7
		curBut = 1
		currEditField = 0
		pushButton = 1
	ELSE
		GOSUB UpdateButtons
		ButtonOpen 2, 1, "New", 5, 58, 0, 0, 1
		ButtonOpen 4, 1, "Add", 7, 58, 0, 0, 1
		ButtonOpen 8, 1, "Commit", 15, 58, 0, 0, 1
		ButtonOpen 9, 1, "Cancel", 16, 58, 0, 0, 1
		curBut = 0
		currEditField = 1
		pushButton = 1
		ButtonSetState 1, 2
	END IF
RETURN

END SUB

' RunInventRec
' Controls inventory record form.  Provides viewing, updating, adding and
' deleting of inventory items.
'
' Handle - window handle
'
SUB RunInventRec (handle)
	ON LOCAL ERROR GOTO InventErr

	IF ISAMfile$ = "" THEN                          'database must be open
		PrintError "A database file must be opened before records can be viewed."
		EXIT SUB
	END IF

	savePosition = SAVEPOINT                        ' database savepoint
	ErrFlag = FALSE

	GOSUB ShowInventRec                             'display form

	' window control loop
	finished = FALSE
	WHILE finished = FALSE
		WindowDo curBut, currEditField                ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' button pressed
				currEditField = 0
				curBut = Dialog(1)
				ButtonSetState pushButton, 1
				ButtonSetState curBut, 2
				pushButton = curBut
				GOSUB InventRecButton
			CASE 2                                      ' edit field
				IF level <> 3 THEN
					curBut = 0
					currEditField = Dialog(2)
				END IF
			CASE 6                                      ' enter
				curBut = pushButton
				GOSUB InventRecButton
			CASE 7, 11                                  ' tab, down arrow
				IF level <> 3 THEN
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE IS < 6
								currEditField = currEditField + 1
							CASE 6
								currEditField = 0
								curBut = 1
						END SELECT
					CASE 2
						curBut = 4 + RecCurrent
					CASE 4
						curBut = 6 + RecCurrent
					CASE 7
						currEditField = 1
						curBut = 0
						ButtonSetState pushButton, 1
						pushButton = 1
						ButtonSetState 1, 2
					CASE ELSE
						curBut = curBut + 1
				END SELECT
				IF curBut <> 0 THEN
					ButtonSetState pushButton, 1
					ButtonSetState curBut, 2
					pushButton = curBut
				END IF
				END IF
			CASE 8, 10                                  ' back tab, up arrow
				IF level <> 3 THEN
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE 1
								currEditField = 0
								curBut = 7
							CASE ELSE
								currEditField = currEditField - 1
						END SELECT
					CASE 1
						currEditField = 6
						curBut = 0
					CASE 4
						curBut = 2 - RecCurrent
					CASE 6
						curBut = 4 - RecCurrent
					CASE ELSE
						curBut = curBut - 1
				END SELECT
				IF curBut <> 0 THEN
					ButtonSetState pushButton, 1
					ButtonSetState curBut, 2
					pushButton = curBut
				END IF
				END IF
			CASE 9                                      ' escape
				finished = CANCEL
			CASE 14                                     ' space bar
				IF curBut > 0 THEN GOSUB InventRecButton
		 END SELECT
	WEND

	IF finished = OK THEN
		CHECKPOINT
	ELSE
		ROLLBACK savePosition
	END IF

	WindowClose handle

	EXIT SUB

InventErr:
	SELECT CASE ERR                                 'must open databas file
		CASE 52
			PrintError "A database file must be opened before inventory records can be displayed."
			EXIT SUB
		CASE ELSE
			ShowError ERR
			IF ERR = 86 THEN                            'can't add duplicate records
				RESUME InventErrReturn
			ELSEIF ERR = 89 THEN                        'not enough ISAM buffers to continue
				WindowClose handle
				ROLLBACK savePosition
				EXIT SUB
			END IF
	END SELECT
RESUME NEXT
																									'process button push
InventRecButton:
	IF level = 3 THEN
		finished = CANCEL
	ELSE
	SELECT CASE curBut
		CASE 1                                        'get next record
			GOSUB RetrieveInventRecord
		CASE 2                                        'clear form for new record
			GOSUB ClearInventEditFields
		CASE 3                                        'update current record
			GOSUB RetrieveInventEditFields
			IF NOT ErrFlag THEN
				UPDATE InveTabNum, InventRec
			ELSE
				ErrFlag = FALSE
			END IF
		CASE 4                                        'add new record
			GOSUB RetrieveInventEditFields
			IF NOT ErrFlag THEN
				INSERT InveTabNum, InventRec
				GOSUB ClearInventEditFields
			ELSE
				ErrFlag = FALSE
			END IF
		CASE 5                                        'delete a record
			DELETE InveTabNum
			GOSUB ClearInventEditFields
		CASE 6                                        'done
			finished = OK
		CASE 7
			finished = CANCEL
	END SELECT
	END IF
InventErrReturn:
RETURN

RetrieveInventRecord:                             'get inventory record
	key$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
	status = NextRecord%(InveTabNum, key$, origkey$, lastkey$, "", "", "")

	IF status = 1 THEN
		RETRIEVE InveTabNum, InventRec
		IF LEFT$(InventRec.ItemNo, LEN(origkey$)) <> origkey$ THEN
			IF LEFT$(lastkey$, LEN(origkey$)) <> origkey$ THEN
				PrintError "No records found matching search criteria."
				ClearInveRecord
				InventRec.ItemNo = key$
				origkey$ = ""
				ButtonOpen 1, ButtonInquire(1), "Find", 2, 61, 0, 0, 1
				status = 0
				RecCurrent = FALSE
			ELSE
				key$ = origkey$
				status = NextRecord%(InveTabNum, key$, origkey$, lastkey$, "", "", "")
				RETRIEVE InveTabNum, InventRec
			END IF
		END IF
		IF status = 1 THEN
			ButtonOpen 1, ButtonInquire(1), "Next", 2, 61, 0, 0, 1
			RecCurrent = TRUE
			GOSUB InsertInventRec
		END IF
	ELSEIF status = 2 THEN
		PrintError "No records found matching search criteria."
		ClearInveRecord
		RecCurrent = FALSE
		InventRec.ItemNo = key$
		origkey$ = ""
		ButtonOpen 1, ButtonInquire(1), "Find", 2, 61, 0, 0, 1
	ELSEIF status = 3 THEN
		PrintError "Inventory database is empty."
		RecCurrent = FALSE
	END IF
	GOSUB UpdateRecButtons
RETURN

RetrieveInventEditFields:                         'get current edit field values
	InventRec.ItemNo = UCASE$(LTRIM$(EditFieldInquire$(1)))
	IF LEN(RTRIM$(InventRec.ItemNo)) = 0 THEN
		PrintError "Item must have an identification number."
		ErrFlag = TRUE
	END IF
	InventRec.Descrip = EditFieldInquire$(2)
	InventRec.Cost = VAL(EditFieldInquire$(3))
	IF InventRec.Cost = 0 AND LTRIM$(EditFieldInquire$(3)) <> "0.00" THEN
		PrintError "Unit cost must be a monetary amount."
		ErrFlag = TRUE
	END IF
	InventRec.Retail = VAL(EditFieldInquire$(4))
	IF InventRec.Retail = 0 AND LTRIM$(EditFieldInquire$(4)) <> "0.00" THEN
		PrintError "Retail price must be a monetary amount."
		ErrFlag = TRUE
	END IF
	InventRec.stock = VAL(EditFieldInquire$(5))
	IF InventRec.stock = 0 AND LTRIM$(EditFieldInquire$(5)) <> "0" THEN
		PrintError "Current stock must be a numeric value."
		ErrFlag = TRUE
	END IF
	InventRec.Vendor = EditFieldInquire$(6)
RETURN

InsertInventRec:                                  'insert record into database
	EditFieldOpen 1, RTRIM$(InventRec.ItemNo), 3, 13, 0, 7, 6, 5
	lastkey$ = LTRIM$(EditFieldInquire$(1))
	EditFieldOpen 2, RTRIM$(InventRec.Descrip), 6, 13, 0, 7, 42, 70
	EditFieldOpen 3, FormatS$(InventRec.Cost, "0.00"), 9, 13, 0, 7, 11, 10
	EditFieldOpen 4, FormatS$(InventRec.Retail, "0.00"), 9, 44, 0, 7, 11, 10
	EditFieldOpen 5, LTRIM$(STR$(InventRec.stock)), 12, 13, 0, 7, 8, 7
	EditFieldOpen 6, RTRIM$(InventRec.Vendor), 15, 13, 0, 7, 42, 70
RETURN

ClearInventEditFields:                            'clear edit fields
	EditFieldOpen 1, "", 3, 13, 0, 7, 6, 5
	EditFieldOpen 2, "", 6, 13, 0, 7, 42, 70
	EditFieldOpen 3, "", 9, 13, 0, 7, 11, 10
	EditFieldOpen 4, "", 9, 44, 0, 7, 11, 10
	EditFieldOpen 5, "", 12, 13, 0, 7, 8, 7
	EditFieldOpen 6, "", 15, 13, 0, 7, 42, 70

	lastkey$ = ""
	ClearInveRecord
	RecCurrent = FALSE
	IF level <> 3 THEN ButtonOpen 1, 1, "Find", 2, 61, 0, 0, 1
	GOSUB UpdateRecButtons
RETURN

UpdateRecButtons:                                 'set button states
	IF NOT RecCurrent THEN
		ButtonClose 3
		ButtonClose 5
		IF curBut = 5 THEN
			curBut = 1
			pushButton = 1
			ButtonSetState 1, 2
		END IF
		WindowColor 8, 7
		WindowLocate 6, 61
		WindowPrint -2, "< Update >"
		WindowLocate 8, 61
		WindowPrint -2, "< Delete >"
		WindowColor 0, 7
	ELSE
		ButtonOpen 3, 1, "Update", 6, 61, 0, 0, 1
		ButtonOpen 5, 1, "Delete", 8, 61, 0, 0, 1
	END IF
RETURN
																								 
ShowInventRec:                                    'display form
	DispInveWin handle

	IF LEN(RTRIM$(InventRec.ItemNo)) = 0 THEN
		GOSUB ClearInventEditFields
	ELSE
		GOSUB InsertInventRec
		SETINDEX InveTabNum, "ItemIndex"
		SEEKEQ InveTabNum, InventRec.ItemNo
		IF EOF(InveTabNum) THEN
			GOSUB ClearInventEditFields
			EditFieldOpen 1, RTRIM$(UCASE$(InventRec.ItemNo)), 3, 13, 0, 7, 6, 5
			IF level <> 3 THEN ButtonOpen 1, 1, "Find", 2, 61, 0, 0, 1
			lastkey$ = ""
			RecCurrent = FALSE
		ELSE
			IF level <> 3 THEN ButtonOpen 1, 1, "Next", 2, 61, 0, 0, 1
			RecCurrent = TRUE
		END IF
	END IF

	IF level = 3 THEN
		curBut = 1
		currEditField = 0
		pushButton = 1
		ButtonOpen 1, 1, "Cancel", 12, 61, 0, 0, 1
		WindowColor 8, 7
		WindowLocate 2, 61
		WindowPrint -2, "< Next >"
		WindowLocate 5, 61
		WindowPrint -2, "< New >"
		WindowLocate 6, 61
		WindowPrint -2, "< Update >"
		WindowLocate 7, 61
		WindowPrint -2, "< Add >"
		WindowLocate 8, 61
		WindowPrint -2, "< Delete >"
		WindowLocate 11, 61
		WindowPrint -2, "< Commit >"
		WindowColor 0, 7
	ELSE
		curBut = 0
		currEditField = 1
		pushButton = 1
		GOSUB UpdateRecButtons
		ButtonOpen 2, 1, "New", 5, 61, 0, 0, 1
		ButtonOpen 4, 1, "Add", 7, 61, 0, 0, 1
		ButtonOpen 6, 1, "Commit", 11, 61, 0, 0, 1
		ButtonOpen 7, 1, "Cancel", 12, 61, 0, 0, 1
	END IF
	ButtonSetState pushButton, 2
RETURN


END SUB

' RunInvoiceRec
' Controls purchase order form.  Allows purchase order to be made.
'
' Handle - window handle
'
SUB RunInvoice (handle)
DIM transBox AS ListBox, transList$(1 TO 4)

	ON LOCAL ERROR GOTO InvoiceErr

	IF ISAMfile$ = "" THEN                          'must have open ISAM file
		PrintError "A database file must be opened before purchase order can be made."
		EXIT SUB
	END IF

	savePosition = SAVEPOINT
	SETINDEX TranTabNum, "TransInvIndex"
	SETINDEX InveTabNum, "ItemIndex"

	GOSUB ShowInvoice

	' window control loop
	finished = FALSE
	WHILE finished = FALSE
		WindowDo curBut, currEditField                ' wait for event
		SELECT CASE Dialog(0)
			CASE 1                                      ' button pressed
				currEditField = 0
				curBut = Dialog(1)
				SELECT CASE curBut
					CASE 1, 2
						func = curBut
					CASE ELSE
						ButtonSetState pushButton, 1
						ButtonSetState curBut, 2
						pushButton = curBut
				END SELECT
				GOSUB InvoiceButton
			CASE 2                                      ' edit field
				IF level < 3 THEN
					curBut = 0
					currEditField = Dialog(2)
				END IF
			CASE 6                                      ' enter
				curBut = pushButton
				GOSUB InvoiceButton
			CASE 7                                      ' tab
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE 5
								currEditField = 0
								curBut = 2
							CASE 6
								currEditField = 0
								IF level = 3 THEN
									curBut = 5
								ELSE
									curBut = 3
								END IF
								ButtonSetState pushButton, 1
								pushButton = curBut
								ButtonSetState pushButton, 2
							CASE ELSE
								currEditField = currEditField + 1
						END SELECT
					CASE 1, 2
						IF level = 3 THEN
							curBut = 5
							ButtonSetState pushButton, 1
							pushButton = curBut
							ButtonSetState curBut, 2
						ELSE
							curBut = 0
							currEditField = 6
						END IF
					CASE 9
						ButtonSetState pushButton, 1
						IF level = 3 THEN
							curBut = 2
							pushButton = 5
						ELSE
							currEditField = 1
							curBut = 0
							pushButton = 1
						END IF
						ButtonSetState pushButton, 2
					CASE 6
						IF level = 3 OR numTrans = 0 THEN
							curBut = 9
						ELSE
							curBut = 7
						END IF
						ButtonSetState pushButton, 1
						pushButton = curBut
						ButtonSetState pushButton, 2
					CASE ELSE
						curBut = curBut + 1
						ButtonSetState pushButton, 1
						pushButton = curBut
						ButtonSetState pushButton, 2
				END SELECT
			CASE 8                                      ' back tab
				SELECT CASE curBut
					CASE 0
						SELECT CASE currEditField
							CASE 1
								currEditField = 0
								ButtonSetState pushButton, 1
								curBut = 9
								pushButton = curBut
								ButtonSetState pushButton, 2
							CASE 6
								currEditField = 0
								curBut = 2
							CASE ELSE
								currEditField = currEditField - 1
						END SELECT
					CASE 1, 2
						IF level = 3 THEN
							curBut = 9
							ButtonSetState pushButton, 1
							ButtonSetState 9, 2
							pushButton = curBut
						ELSE
							currEditField = 5
							curBut = 0
						END IF
					CASE 3
						currEditField = 6
						curBut = 0
					CASE 5
						IF level = 3 THEN
							curBut = 2
						ELSE
							ButtonSetState 5, 1
							curBut = 4
							pushButton = curBut
							ButtonSetState 4, 2
						END IF
					CASE 9
						IF level = 3 OR numTrans = 0 THEN
							curBut = 6
						ELSE
							curBut = 8
						END IF
						ButtonSetState pushButton, 1
						pushButton = curBut
						ButtonSetState pushButton, 2
					CASE ELSE
						ButtonSetState pushButton, 1
						curBut = curBut - 1
						pushButton = curBut
						ButtonSetState pushButton, 2
				END SELECT
			CASE 9                                      ' escape
				finished = CANCEL
			CASE 10, 12                                 ' up, left arrow
				temp = curBut
				curBut = 2
				func = 3
				GOSUB InvoiceButton
				curBut = temp
			CASE 11, 13                                 'down, right arrow
				temp = curBut
				curBut = 2
				func = 4
				GOSUB InvoiceButton
				curBut = temp
			CASE 14                                     ' space bar
				IF curBut > 2 THEN GOSUB InvoiceButton
		 END SELECT
	WEND

	IF finished = OK THEN
		CHECKPOINT
	ELSE
		ROLLBACK savePosition
	END IF
	GetNextInvoice

	WindowClose handle

	EXIT SUB

InvoiceErr:
	SELECT CASE ERR
		CASE 52                                     'no database open
			PrintError "A database file must be opened before transactions can be made."
			EXIT SUB
		CASE ELSE
			ShowError ERR                             'display general error message
			IF ERR = 89 THEN                          'not enough ISAM buffers to continue
				WindowClose handle
				ROLLBACK savePosition
				EXIT SUB
			END IF
	END SELECT
RESUME NEXT

InvoiceButton:                                    'process button selection
	SELECT CASE curBut
		CASE 1, 2                                     'scroll transaction list
			ScrollList transList$(), transBox, func, 1, topRow, lefCol

			currNo = transBox.listPos
			IF level <> 3 THEN
				state = ButtonInquire(3)
				IF currNo > numTrans THEN
					ButtonOpen 3, state, "Add", 2, 61, 0, 0, 1
				ELSE
					ButtonOpen 3, state, "Update", 2, 61, 0, 0, 1
				END IF
			END IF
			GOSUB InsertTransaction
			curBut = 2
		CASE 3                                        'add a transaction
			GOSUB AddTransaction
		CASE 4                                        'void a transaction
			GOSUB VoidTransaction
		CASE 5                                        'get a customer name
			ckey1$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
			ckey2$ = LTRIM$(EditFieldInquire$(2))
			IF (ckey1$ = clastKey1$ AND ckey2$ = clastKey2$) OR (ckey1$ = "" AND ckey2$ = "") THEN
				IF ckey1$ = "" THEN CustRec.AcctNo = ""
				IF ckey2$ = "" THEN CustRec.Company = ""
				RunCustRec WindowNext
				EditFieldOpen 1, RTRIM$(CustRec.AcctNo), 3, 13, 0, 7, 6, 5
				EditFieldOpen 2, RTRIM$(CustRec.Company), 6, 13, 0, 7, 41, 70
				clastKey1$ = EditFieldInquire$(1)
				clastKey2$ = EditFieldInquire$(2)
			ELSE
				status = NextRecord%(CustTabNum, ckey1$, corigKey1$, clastKey1$, ckey2$, corigKey2$, clastKey2$)
				IF status = 1 THEN
					RETRIEVE CustTabNum, CustRec
					IF LEFT$(CustRec.AcctNo, LEN(corigKey1$)) <> corigKey1$ OR UCASE$(LEFT$(CustRec.Company, LEN(corigKey2$))) <> UCASE$(corigKey2$) THEN
						PrintError "No records found matching search criteria."
						ClearCustRecord
						CustRec.AcctNo = ckey1$
						clastKey1$ = ckey1$
						CustRec.Company = ckey2$
						clastKey2$ = ckey2$
					ELSE
						EditFieldOpen 1, RTRIM$(CustRec.AcctNo), 3, 13, 0, 7, 6, 5
						EditFieldOpen 2, RTRIM$(CustRec.Company), 6, 13, 0, 7, 41, 70
						clastKey1$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
						clastKey2$ = EditFieldInquire$(2)
					END IF
				ELSEIF status = 2 THEN
					PrintError "No records found matching search criteria."
					ClearCustRecord
					CustRec.AcctNo = ckey1$
					clastKey1$ = ckey1$
					CustRec.Company = ckey2$
					clastKey2$ = ckey2$
				ELSEIF status = 3 THEN
					PrintError "Customer database is empty."
				END IF
			END IF
		CASE 6                                        'get an item number
			ikey$ = UCASE$(LTRIM$(EditFieldInquire$(3)))
			IF ikey$ <> ilastKey$ AND ikey$ <> "" THEN
				status = NextRecord%(InveTabNum, ikey$, iorigKey$, ilastKey$, "", "", "")
				IF status = 1 THEN
					RETRIEVE InveTabNum, InventRec
					IF LEFT$(InventRec.ItemNo, LEN(iorigKey$)) <> iorigKey$ THEN
						PrintError "No records found matching search criteria."
						ClearInveRecord
						InventRec.ItemNo = ikey$
						ilastKey$ = ikey$
					ELSE
						EditFieldOpen 3, RTRIM$(InventRec.ItemNo), 9, 13, 0, 7, 6, 5
						ilastKey$ = LTRIM$(EditFieldInquire$(3))
						EditFieldOpen 4, FormatS$(InventRec.Retail, "0.00"), 9, 29, 0, 7, 11, 10
					END IF
				ELSEIF status = 2 THEN
					PrintError "No records found matching search criteria."
					ClearInveRecord
					InventRec.ItemNo = ikey$
					ilastKey$ = ikey$
				ELSEIF status = 3 THEN
					PrintError "Inventory database is empty."
				END IF
			ELSE
				IF ikey$ = "" THEN InventRec.ItemNo = ""
				RunInventRec WindowNext
				EditFieldOpen 3, RTRIM$(InventRec.ItemNo), 9, 13, 0, 7, 6, 5
				ilastKey$ = UCASE$(LTRIM$(EditFieldInquire$(3)))
				IF ilastKey$ = "" THEN
					EditFieldOpen 4, "", 9, 29, 0, 7, 11, 10
				ELSE
					EditFieldOpen 4, FormatS$(InventRec.Retail, "0.00"), 9, 29, 0, 7, 11, 10
				END IF
			END IF
		CASE 7                                        'total the invoice
			junk = TotalInvoice
		CASE 8                                        'commit invoice
			InvoiceRec.AcctNo = UCASE$(EditFieldInquire$(1))
			IF LEN(RTRIM$(InvoiceRec.AcctNo)) = 0 THEN
				PrintError "Must specify account number before invoice can be committed."
			ELSE
				SETINDEX CustTabNum, "AcctIndex"
				SEEKEQ CustTabNum, InvoiceRec.AcctNo
				IF EOF(CustTabNum) THEN
					PrintError "Customer does not exist in customer database."
				ELSE
					InvoiceRec.Date = MID$(DATE$, 9, 2) + MID$(DATE$, 1, 2) + MID$(DATE$, 4, 2)
					finished = TotalInvoice
				END IF
			END IF
		CASE 9                                        'cancel operation
			finished = CANCEL
	END SELECT
RETURN

AddTransaction:
	IF currNo <= numTrans THEN
		SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, currNo
		RETRIEVE TranTabNum, TransRec
		origitem$ = TransRec.ItemNo
		origqty = TransRec.Quantity
		UpdateInventory origitem$, -(origqty)
	ELSE
		origitem$ = ""
	END IF

	TransRec.TransNo = currNo
	TransRec.InvoiceNo = InvoiceRec.InvoiceNo

	tItemNo$ = UCASE$(LTRIM$(EditFieldInquire$(3)))
	IF tItemNo$ = "" THEN
		PrintError "Must specify item number before transaction can be added."
		RETURN
	ELSE
		SEEKEQ InveTabNum, tItemNo$
		IF NOT EOF(InveTabNum) THEN
			TransRec.ItemNo = tItemNo$
		ELSE
			PrintError "Item not found in inventory database."
			RETURN
		END IF
	END IF

	tRetailPrice$ = LTRIM$(EditFieldInquire$(4))
	IF tRetailPrice$ = "" THEN
		RETRIEVE InveTabNum, InventRec
		TransRec.Price = InventRec.Retail
	ELSE
		TransRec.Price = VAL(tRetailPrice$)
		IF TransRec.Price = 0 AND tRetailPrice$ <> "0" THEN
			PrintError "Retail price must be a monetary amount."
			RETURN
		END IF
	END IF

	tQuantity$ = LTRIM$(EditFieldInquire$(5))
	IF tQuantity$ = "" THEN
		qty = 1
	ELSE
		qty = VAL(tQuantity$)
		IF qty = 0 AND tQuantity$ <> "0" THEN
			PrintError "Quantity must be a numeric value."
			RETURN
		ELSEIF qty = 0 THEN
			PrintError "Quantity cannot equal zero."
			RETURN
		END IF
	END IF

	UpdateInventory TransRec.ItemNo, qty
	IF qty <> 0 THEN
		TransRec.Quantity = qty
		IF currNo <= numTrans THEN
			UPDATE TranTabNum, TransRec
		ELSE
			INSERT TranTabNum, TransRec
			currNo = currNo + 1
			numTrans = numTrans + 1
			GOSUB UpdateInvButtons
			GOSUB ClearInvoiceEditFields
		END IF

		transBox.listLen = numTrans + 1
		transBox.listPos = currNo
		CreateListBox transList$(), transBox, 1
	ELSEIF currNo <= numTrans AND TransRec.ItemNo = origitem$ THEN
		UpdateInventory origitem$, origqty
	END IF
RETURN

InsertTransaction:
	SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, currNo
	IF EOF(TranTabNum) THEN
		GOSUB ClearInvoiceEditFields
		InventRec.ItemNo = ""
	ELSE
		RETRIEVE TranTabNum, TransRec
		EditFieldOpen 3, RTRIM$(TransRec.ItemNo), 9, 13, 0, 7, 6, 5
		EditFieldOpen 4, FormatS$(TransRec.Price, "0.00"), 9, 29, 0, 7, 11, 10
		EditFieldOpen 5, LTRIM$(STR$(TransRec.Quantity)), 9, 48, 0, 7, 7, 6
		SEEKEQ InveTabNum, TransRec.ItemNo
		RETRIEVE InveTabNum, InventRec
	END IF
	ilastKey$ = UCASE$(LTRIM$(EditFieldInquire$(3)))
RETURN

VoidTransaction:
	SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, currNo
	IF NOT EOF(TranTabNum) THEN
		RETRIEVE TranTabNum, TransRec
		UpdateInventory TransRec.ItemNo, -(TransRec.Quantity)
		DELETE TranTabNum

		FOR i = currNo + 1 TO numTrans
			RETRIEVE TranTabNum, TransRec
			TransRec.TransNo = TransRec.TransNo - 1
			UPDATE TranTabNum, TransRec
			MOVENEXT TranTabNum
		NEXT i
		numTrans = numTrans - 1
		state = ButtonInquire(3)
		IF currNo > numTrans THEN
			ButtonOpen 3, state, "Add", 2, 61, 0, 0, 1
		ELSE
			ButtonOpen 3, state, "Update", 2, 61, 0, 0, 1
		END IF
		GOSUB InsertTransaction

		transBox.listLen = numTrans + 1
		transBox.listPos = currNo
		CreateListBox transList$(), transBox, 1
	ELSE
		GOSUB ClearInvoiceEditFields
	END IF
	GOSUB UpdateInvButtons
RETURN

ClearInvoiceEditFields:
	EditFieldOpen 3, "", 9, 13, 0, 7, 6, 5
	EditFieldOpen 4, "", 9, 29, 0, 7, 11, 10
	EditFieldOpen 5, "1", 9, 48, 0, 7, 7, 6
RETURN

UpdateInvButtons:
	IF numTrans > 0 THEN
		ButtonOpen 7, 1, "Total", 10, 61, 0, 0, 1
		ButtonOpen 8, 1, "Commit", 12, 61, 0, 0, 1
	ELSE
		ButtonClose 7
		ButtonClose 8
		WindowColor 8, 7
		WindowLocate 10, 61
		WindowPrint -2, "< Total >"
		WindowLocate 12, 61
		WindowPrint -2, "< Commit >"
		WindowColor 0, 7
	END IF
RETURN

ShowInvoice:
	DispInvoWin handle
	topRow = 4
	lefCol = 3
	transBox.sBut = 1
	transBox.aBut = 2
	transBox.topRow = 12
	transBox.bLen = 4
	transBox.leftCol = 4
	transBox.bWid = 50
	transBox.listPos = 1
	 
	IF LEN(RTRIM$(InvoiceRec.AcctNo)) = 0 THEN
		SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, 1
		DO WHILE NOT EOF(TranTabNum)
			RETRIEVE TranTabNum, TransRec
			IF TransRec.InvoiceNo = InvoiceRec.InvoiceNo THEN
				DELETE TranTabNum
			ELSE
				EXIT DO
			END IF
		LOOP
		transBox.listLen = 1
		WindowLocate 1, 16
		WindowPrint -2, InvoiceRec.InvoiceNo
		WindowLocate 1, 48
		WindowPrint -2, MID$(DATE$, 1, 2) + "-" + MID$(DATE$, 4, 2) + "-" + MID$(DATE$, 9, 2)
		IF LEN(RTRIM$(CustRec.AcctNo)) = 0 OR LEN(RTRIM$(CustRec.Company)) = 0 THEN
			EditFieldOpen 1, "", 3, 13, 0, 7, 6, 5
			EditFieldOpen 2, "", 6, 13, 0, 7, 41, 70
			clastKey1$ = ""
			clastKey2$ = ""
			level = 1
		ELSE
			EditFieldOpen 1, RTRIM$(CustRec.AcctNo), 3, 13, 0, 7, 6, 5
			EditFieldOpen 2, RTRIM$(CustRec.Company), 6, 13, 0, 7, 41, 70
			clastKey1$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
			clastKey2$ = LTRIM$(EditFieldInquire$(2))
			level = 2
		END IF
		currNo = 1
		numTrans = 0
		GOSUB InsertTransaction
		EditFieldOpen 6, "", 19, 23, 0, 7, 7, 6
	ELSE
		level = 3
		count = 0
		SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, 1
		RETRIEVE TranTabNum, TransRec
		DO
			SEEKEQ TranTabNum, InvoiceRec.InvoiceNo, count + 1
			count = count + 1
		LOOP UNTIL EOF(TranTabNum)
		transBox.listLen = count - 1
		currNo = 1
		numTrans = count - 1
		GOSUB InsertTransaction

		WindowLocate 1, 16
		WindowPrint -2, InvoiceRec.InvoiceNo
		WindowLocate 1, 48
		WindowPrint -2, MID$(InvoiceRec.Date, 3, 2) + "-" + MID$(InvoiceRec.Date, 5, 2) + "-" + MID$(InvoiceRec.Date, 1, 2)

		SETINDEX CustTabNum, "AcctIndex"
		SEEKEQ CustTabNum, InvoiceRec.AcctNo
		IF EOF(CustTabNum) THEN
			PrintError "Customer no longer exists in customer database."
			Acct$ = InvoiceRec.AcctNo
			comp$ = ""
		ELSE
			RETRIEVE CustTabNum, CustRec
			Acct$ = CustRec.AcctNo
			comp$ = CustRec.Company
		END IF
		EditFieldOpen 1, RTRIM$(Acct$), 3, 13, 0, 7, 6, 5
		EditFieldOpen 2, RTRIM$(comp$), 6, 13, 0, 7, 41, 70
		clastKey1$ = UCASE$(LTRIM$(EditFieldInquire$(1)))
		clastKey2$ = LTRIM$(EditFieldInquire$(2))
		EditFieldOpen 6, LTRIM$(STR$(InvoiceRec.TaxRate)), 19, 23, 0, 7, 7, 6
		WindowLocate 18, 42
		WindowPrint -2, RIGHT$("          " + FormatS$(InvoiceRec.Total * 100 / (InvoiceRec.TaxRate + 100), "$#,##0.00"), 14)
		WindowLocate 19, 42
		WindowPrint -2, RIGHT$("          " + FormatS$(InvoiceRec.Total * InvoiceRec.TaxRate / (InvoiceRec.TaxRate + 100), "$#,##0.00"), 14)
		WindowLocate 20, 42
		WindowPrint -2, RIGHT$("          " + FormatS$(InvoiceRec.Total, "$#,##0.00"), 14)
	END IF
	CreateListBox transList$(), transBox, 1

	IF level = 3 THEN
		pushButton = 5
		curBut = 2
		currEditField = 0
		WindowColor 8, 7
		WindowLocate 2, 61
		WindowPrint -2, "< Update >"
		WindowLocate 3, 61
		WindowPrint -2, "< Void >"
		WindowLocate 10, 61
		WindowPrint -2, "< Total >"
		WindowLocate 11, 61
		WindowPrint -2, "< Commit >"
		WindowColor 0, 7
	ELSE
		currEditField = 1
		curBut = 0
		pushButton = 3
		GOSUB UpdateInvButtons
		ButtonOpen 3, 1, "Add", 2, 61, 0, 0, 1
		ButtonOpen 4, 1, "Void", 3, 61, 0, 0, 1
	END IF
	ButtonOpen 5, 1, "Customer", 6, 61, 0, 0, 1
	ButtonOpen 6, 1, "Item", 7, 61, 0, 0, 1
	ButtonOpen 9, 1, "Cancel", 13, 61, 0, 0, 1
	ButtonSetState pushButton, 2
RETURN
END SUB

