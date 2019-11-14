'       ISAMDEMO.BI  - isamdemo include file

'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'menu.bi'
'$INCLUDE: 'window.bi'
'$INCLUDE: 'format.bi'

'menu bar title locations
CONST FILETITLE = 1
CONST TRANSTITLE = 2
CONST CUSTTITLE = 3
CONST INVTITLE = 4

CONST OK = 1
CONST CANCEL = 2

'customer record type for ISAM table
TYPE CustRecord
	AcctNo          AS STRING * 5                   'account number
	Company         AS STRING * 70                  'company name
	Street          AS STRING * 70                  'street address
	City            AS STRING * 70                  'city name
	state           AS STRING * 2                   'state name
	Zip             AS STRING * 5                   'zip code
	Phone1          AS STRING * 14                  'phone number
	Phone2          AS STRING * 14                  'second phone number
	Contact         AS STRING * 70                  'contact name
	Opened          AS STRING * 6                   'date account opened
END TYPE

'inventory record type for ISAM table
TYPE InventRecord
	ItemNo          AS STRING * 5                   'item number
	Descrip         AS STRING * 70                  'description
	Cost            AS CURRENCY                     'item cost
	Retail          AS CURRENCY                     'retail price
	stock           AS INTEGER                      'current stock
	Vendor          AS STRING * 70                  'vendor
END TYPE

'invoice record type for ISAM table
TYPE InvoiceRecord
	InvoiceNo       AS STRING * 6                   'invoice number
	Date            AS STRING * 6                   'invoice date
	AcctNo          AS STRING * 5                   'account number
	TaxRate         AS DOUBLE                       'tax rate
	Total           AS CURRENCY                     'invoice total
END TYPE

'transaction record type for ISAM table
TYPE TransRecord
	TransNo         AS INTEGER                      'transaction number
	InvoiceNo       AS STRING * 6                   'invoice number
	ItemNo          AS STRING * 5                   'item number
	Quantity        AS INTEGER                      'quantity ordered
	Price           AS CURRENCY                     'price
END TYPE

'structure  for creating list box
TYPE ListBox
	bLen            AS INTEGER                      'box length
	bWid            AS INTEGER                      'box width
	maxLen          AS INTEGER                      'max display length
	listLen         AS INTEGER                      'list length
	curPos          AS INTEGER                      'current box position
	curTop          AS INTEGER                      'current top list item
	sBut            AS INTEGER                      'scroll button
	aBut            AS INTEGER                      'area button
	topRow          AS INTEGER                      'top row of box
	leftCol         AS INTEGER                      'left column of box
	listPos         AS INTEGER                      'actual list position
END TYPE

' functions
DECLARE FUNCTION GetBoundaries% (handle%, func%)
DECLARE FUNCTION GetFileCount% (fileSpec$)
DECLARE FUNCTION Max% (num1%, num2%)
DECLARE FUNCTION Min% (num1%, num2%)
DECLARE FUNCTION NextRecord% (TableNum%, key1$, origkey1$, lastkey1$, key2$, origkey2$, lastkey2$)
DECLARE FUNCTION TotalInvoice% ()

' subs
DECLARE SUB DispInveWin (handle%)
DECLARE SUB DispInvoWin (handle%)
DECLARE SUB DispCustWin (handle%)
DECLARE SUB Quit ()
DECLARE SUB CreateListBox (text$(), tbox AS ANY, func%)
DECLARE SUB ScrollList (text$(), tbox AS ANY, status%, func%, winRow%, winCol%)
DECLARE SUB GetNextInvoice ()
DECLARE SUB DrawList (text$(), tbox AS ANY, listtype%, oldbox AS ANY)
DECLARE SUB PrintError (text$)
DECLARE SUB ShowError (errorNum%)
DECLARE SUB RunCustRec (handle%)
DECLARE SUB RunInventRec (handle%)
DECLARE SUB RunInvoice (handle%)
DECLARE SUB RunList (handle%, func%)
DECLARE SUB ClearCustRecord ()
DECLARE SUB ClearInveRecord ()
DECLARE SUB ClearInvoRecord ()
DECLARE SUB ClearTranRecord ()
DECLARE SUB InitAll ()
DECLARE SUB UpdateInventory (ItemNo AS STRING, Quantity%)
DECLARE SUB CreateCInvList (text$(), cBox AS ANY, oBox AS ANY, barpos%)
DECLARE SUB CreateCustList (text$(), cBox AS ANY, oBox AS ANY, barpos%)
DECLARE SUB CreateIndexes ()
DECLARE SUB CreateInveList (text$(), cBox AS ANY, oBox AS ANY, barpos%)
DECLARE SUB CreateInvoList (text$(), cBox AS ANY, oBox AS ANY, barpos%)
DECLARE SUB CreateTranList (text$(), cBox AS ANY)
DECLARE SUB NewDB ()
DECLARE SUB OpenDB ()

'necessary declarations for using toolboxes
COMMON SHARED /uitools/ GloMenu           AS MenuMiscType
COMMON SHARED /uitools/ GloTitle()        AS MenuTitleType
COMMON SHARED /uitools/ GloItem()         AS MenuItemType
COMMON SHARED /uitools/ GloWindow()       AS windowType
COMMON SHARED /uitools/ GloButton()       AS buttonType
COMMON SHARED /uitools/ GloEdit()         AS EditFieldType
COMMON SHARED /uitools/ GloStorage        AS WindowStorageType
COMMON SHARED /uitools/ GloWindowStack()  AS INTEGER
COMMON SHARED /uitools/ GloBuffer$()

'isam table numbers
COMMON SHARED /isamdemo/ CustTabNum       AS INTEGER
COMMON SHARED /isamdemo/ InveTabNum       AS INTEGER
COMMON SHARED /isamdemo/ InvoTabNum       AS INTEGER
COMMON SHARED /isamdemo/ TranTabNum       AS INTEGER
'dialog level
COMMON SHARED /isamdemo/ level            AS INTEGER
'table lists boundaries
COMMON SHARED /isamdemo/ Bound1$
COMMON SHARED /isamdemo/ Bound2$
'table records
COMMON SHARED /isamdemo/ CustRec          AS CustRecord
COMMON SHARED /isamdemo/ InventRec        AS InventRecord
COMMON SHARED /isamdemo/ InvoiceRec       AS InvoiceRecord
COMMON SHARED /isamdemo/ TransRec         AS TransRecord
'isam file
COMMON SHARED /isamdemo/ ISAMfile$
COMMON /isamdemo/ finished                AS INTEGER
COMMON SHARED /isamdemo/ origdir$


