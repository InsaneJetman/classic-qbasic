' ===========================================================================
'
' WINDOW.BI
'
'  Copyright (C) 1989 Microsoft Corporation, All Rights Reserved
'
' ===========================================================================

' ===========================================================================
' windoType stores information about each window
' ===========================================================================

TYPE windowType
    handle      AS INTEGER
    row1        AS INTEGER
    col1        AS INTEGER
    row2        AS INTEGER
    col2        AS INTEGER
    cursorRow   AS INTEGER
    cursorCol   AS INTEGER
    highlight   AS INTEGER
    textFore    AS INTEGER
    textBack    AS INTEGER
    fore        AS INTEGER
    back        AS INTEGER
    windowType  AS INTEGER
    title       AS STRING * 15
END TYPE

' ===========================================================================
' buttonType stores info about buttons
' ===========================================================================

TYPE buttonType
    handle       AS INTEGER
    windowHandle AS INTEGER
    text         AS STRING * 30
    state        AS INTEGER
    buttonOn     AS INTEGER
    row1         AS INTEGER
    col1         AS INTEGER
    row2         AS INTEGER
    col2         AS INTEGER
    buttonType   AS INTEGER
END TYPE

' ===========================================================================
' EditField Type stores info about edit fields
' ===========================================================================

TYPE EditFieldType
    handle          AS INTEGER
    windowHandle    AS INTEGER
    text            AS STRING * 255
    fore            AS INTEGER
    back            AS INTEGER
    row             AS INTEGER
    col             AS INTEGER
    visLength       AS INTEGER
    maxLength       AS INTEGER
END TYPE

TYPE hotSpotType
    row1            AS INTEGER
    row2            AS INTEGER
    col1            AS INTEGER
    col2            AS INTEGER
    action          AS INTEGER
    misc            AS INTEGER
    misc2           AS INTEGER
END TYPE

TYPE WindowStorageType
    currWindow        AS INTEGER
    numWindowsOpen    AS INTEGER
    numButtonsOpen    AS INTEGER
    numEditFieldsOpen AS INTEGER
    DialogEdit        AS INTEGER
    DialogClose       AS INTEGER
    DialogButton      AS INTEGER
    DialogWindow      AS INTEGER
    DialogEvent       AS INTEGER
    DialogScroll      AS INTEGER
    DialogRow         AS INTEGER
    DialogCol         AS INTEGER
    oldDialogEdit     AS INTEGER
    oldDialogClose    AS INTEGER
    oldDialogButton   AS INTEGER
    oldDialogWindow   AS INTEGER
    oldDialogEvent    AS INTEGER
    oldDialogScroll   AS INTEGER
    oldDialogRow      AS INTEGER
    oldDialogCol      AS INTEGER
END TYPE

' ===========================================================================
' DECLARATIONS
' ===========================================================================

DECLARE SUB ButtonSetState (handle%, state%)
DECLARE SUB WindowDo (startButton%, startEdit%)
DECLARE SUB ButtonOpen (handle%, state%, title$, row%, col%, row2%, col2%, buttonType%)
DECLARE SUB WindowLine (row%)
DECLARE SUB WindowPrint (printMode%, text$)
DECLARE SUB WindowOpen (handle%, row1%, col1%, row2%, col2%, textFore%, textBack%, fore%, back%, highlight%, movewin%, closewin%, sizewin%, modalwin%, borderchar%, title$)
DECLARE SUB ButtonClose (handle%)
DECLARE SUB WindowClose (handle%)
DECLARE SUB WindowShadowSave ()
DECLARE SUB WindowShadowRefresh ()
DECLARE SUB WindowPrintTitle ()
DECLARE SUB ButtonShow (handle%)
DECLARE SUB WindowSave (handle%)
DECLARE SUB WindowRefresh (handle%)
DECLARE SUB BackgroundSave (handle%)
DECLARE SUB BackgroundRefresh (handle%)
DECLARE FUNCTION ListBox% (text$(), maxRec%)
DECLARE FUNCTION MaxScrollLength% (handle%)
DECLARE FUNCTION WindowCurrent% ()
DECLARE FUNCTION WindowBorder$ (winType%)
DECLARE FUNCTION WindowCols% (handle%)
DECLARE FUNCTION WindowRows% (handle%)
DECLARE FUNCTION Alert% (style%, text$, row1%, col1%, row2%, col2%, b1$, b2$, b3$)
DECLARE FUNCTION Dialog% (op%)
DECLARE FUNCTION ButtonInquire% (handle%)
DECLARE FUNCTION WindowNext% ()
DECLARE FUNCTION FindButton% (handle%)
DECLARE SUB WindowScroll (lines%)
DECLARE FUNCTION EditFieldInquire$ (handle%)
DECLARE FUNCTION FindEditField% (handle%)
DECLARE SUB EditFieldClose (handle%)
DECLARE SUB ButtonToggle (handle%)
DECLARE SUB EditFieldOpen (handle%, text$, row%, col%, fore%, back%, visLength%, maxLength%)
DECLARE SUB WindowBox (boxRow1%, boxCol1%, boxRow2%, boxCol2%)
DECLARE SUB WindowCls ()
DECLARE SUB WindowColor (fore%, back%)
DECLARE SUB WindowInit ()
DECLARE SUB WindowLocate (row%, col%)
DECLARE SUB WindowSetCurrent (handle%)
DECLARE FUNCTION WhichWindow% (row%, col%)

