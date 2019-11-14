'       CHRTDEMO.BAS - Main module of CHRTB demonstration program
'
'             Copyright (C) 1989-1990, Microsoft Corporation
'
'   This demo program uses the Presentation Graphics and User Interface
'   toolboxes to implement a general purpose charting package.
'   It consists of three modules (CHRTDEMO.BAS, CHRTDEM1.BAS and CHRTDEM2.BAS)
'   and one include file (CHRTDEMO.BI).  It requires access to both the
'   Presentation Graphics and User Interface toolboxes.
'
'   EMS is needed to load and run the demo under QBX.  If you do not
'   have EMS, refer to the command line compile instructions below which
'   will allow you to run the demo from the DOS prompt.  Running the
'   demo under QBX requires access to the Presentation Graphics and User
'   Interface toolboxes.  This can be done in one of two methods:
'       1) One large QuickLib covering both toolboxes can be created.  The
'          library "CHRTDEM.LIB" and QuickLib "CHRTDEM.QLB" are created
'          as follows:
'           BC /X/FS chrtb.bas;
'           BC /X/FS fontb.bas;
'           LIB chrtdem.lib + uitbefr.lib + fontasm + chrtasm + fontb + chrtb;
'           LINK /Q chrtdem.lib, chrtdem.qlb,,qbxqlb.lib;
'          Once created, just start QBX with this QuickLib and load the
'          demo's modules (chrtdemo.bas, chrtdem1.bas and chrtdem2.bas).
'
'       2) Either the Presentation Graphics or User Interface QuickLib
'          may be used alone provided the other's source code files
'          are loaded into the QBX environment.  If CHRTBEFR.QLB is
'          is used then WINDOW.BAS, GENERAL.BAS, MENU.BAS and MOUSE.BAS
'          must be loaded.  If UITBEFR.QLB is used then CHRTB.BAS and
'          FONTB.BAS must be loaded.  Once a QuickLib is specified and
'          all necessary source files are loaded, load the program
'          modules (chrtdemo.bas, chrtdem1.bas and chrtdem2.bas)
'
'   To create a compiled version of the chart demo program perform the
'   following steps:
'       BC /X/FS chrtb.bas;
'       BC /X/FS fontb.bas;
'       LIB chrtdem.lib + uitbefr.lib + fontasm + chrtasm + fontb + chrtb;
'       BC /X/FS chrtdemo.bas;
'       BC /FS chrtdem1.bas;
'       BC /FS chrtdem2.bas;
'       LINK /EX chrtdemo chrtdem1 chrtdem2, chrtdemo.exe,, chrtdem.lib;
'   "CHRTDEMO" can now be run from the command line.
'
'
DEFINT A-Z

'$INCLUDE: 'chrtdemo.bi'

' local functions
DECLARE FUNCTION GetLoadFile% (FileName$)
DECLARE FUNCTION GetSaveFile% (FileName$)
DECLARE FUNCTION GetFileCount% (fileSpec$)

' local subs
DECLARE SUB LoadChart (fileNum%)
DECLARE SUB ShowError (errorNum%)


' necessary variables for the toolboxes
DIM GloTitle(MAXMENU)           AS MenuTitleType
DIM GloItem(MAXMENU, MAXITEM)   AS MenuItemType
DIM GloWindow(MAXWINDOW)        AS windowType
DIM GloButton(MAXBUTTON)        AS buttonType
DIM GloEdit(MAXEDITFIELD)       AS EditFieldType
DIM GloWindowStack(MAXWINDOW)   AS INTEGER
DIM GloBuffer$(MAXWINDOW + 1, 2)

' variables shared across modules
DIM colors$(1 TO MAXCOLORS)                     'valid colors$
DIM styles$(1 TO MAXSTYLES)                     'border style list
DIM fonts$(1 TO MAXFONTS)                       'fonts list
DIM Cat$(1 TO cMaxValues)                       'category names
DIM setName$(1 TO cMaxSets)                     'set names
DIM setLen(1 TO cMaxSets)   AS INTEGER          '# values per set
DIM setVal!(1 TO cMaxValues, 1 TO cMaxSets)     ' actual values
DIM mode$(1 TO 13)                              'list of modes


    ' set up main error handler
    ON ERROR GOTO ErrorHandle

    ' initialize the program
    InitAll

    ' Main loop
    WHILE NOT finished
        kbd$ = MenuInkey$
        WHILE MenuCheck(2)
            HandleMenuEvent
        WEND
    WEND

    END

'catch all error handler
ErrorHandle:
    ShowError ERR
    WindowClose 1                               ' close any active windows
    WindowClose 2
RESUME NEXT

'
' Function Name: GetBestMode
'
' Description: Creates a list of valid screen modes for use by charting functions
'              and sets the initial screen mode to the highest resolution
'              possible.  If no graphic screen modes are available then
'              it causes the program to exit.
'
' Arguments: screenMode
'
SUB GetBestMode (screenMode)
SHARED mode$(), numModes AS INTEGER

ON LOCAL ERROR GOTO badmode                     ' trap screen mode errors

    ' test all possible screen modes creating a list of valid ones as we go
    numModes = 0
    FOR i = 13 TO 1 STEP -1
        valid = TRUE
        SCREEN i
        IF valid THEN
            numModes = numModes + 1
            mode$(numModes) = LTRIM$(STR$(i))
        END IF
    NEXT i

    ' exit if no modes available
    IF numModes = 0 THEN
        screenMode = 0
    ' set current screen mode to best possible
    ELSEIF mode$(1) = "13" THEN
        screenMode = VAL(mode$(2))
    ELSE
        screenMode = VAL(mode$(1))
    END IF

EXIT SUB

badmode:
    valid = FALSE
RESUME NEXT

END SUB

'
' Func Name: GetFileCount
'
' Description: Returns number of DOS files matching a given file spec
'
' Arguments: fileSpec$ - DOS file spec  (i.e. "*.*")
'
FUNCTION GetFileCount% (fileSpec$)

ON LOCAL ERROR GOTO GetCountError

    count = 0

    FileName$ = DIR$(fileSpec$)             ' Get first match if any

    DO WHILE FileName$ <> ""                ' continue until no more matches
        count = count + 1
        FileName$ = DIR$
    LOOP

    GetFileCount = count                    ' return count

    EXIT FUNCTION

GetCountError:

    ShowError ERR                               ' display error message

RESUME NEXT

END FUNCTION

'
' Func Name: GetLoadFile
'
' Description: Called by OpenChart, this prompts the user for a
'              DOS file to open.  It returns the file number of
'              the chart file with the actual file name being
'              passed back via the argument.
'
' Arguments: FileName$ - name of file to open
'
FUNCTION GetLoadFile% (FileName$)
DIM fileList$(1 TO 10)
DIM fileBox AS ListBox

ON LOCAL ERROR GOTO GetLoadError                ' handle file opening errors

    fileSpec$ = "*.CHT"                         ' default file spec
    origDir$ = CURDIR$
    origPos = 0                                 ' no file list element selected

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
    fileBox.scrollButton = 1
    fileBox.areaButton = 2
    fileBox.listLen = fileCount
    fileBox.topRow = 8
    fileBox.botRow = 14
    fileBox.leftCol = 7
    fileBox.rightCol = 22
    fileBox.listPos = origPos

    ' create window for display
    winRow = 6
    winCol = 25
    WindowOpen 1, winRow, winCol, 21, 52, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Open Chart"
    WindowLocate 2, 2
    WindowPrint 2, "File Name:"
    WindowBox 1, 13, 3, 27
    WindowLocate 5, 2
    WindowPrint -1, origDir$
    WindowLocate 7, 11
    WindowPrint 2, "Files"
    WindowLine 15

    ' create list box for file list
    CreateListBox fileList$(), fileBox, 5

    ' open edit field for file spec
    EditFieldOpen 1, fileSpec$, 2, 14, 0, 7, 13, 70

    ' open command buttons
    ButtonOpen 3, 2, "OK", 16, 5, 0, 0, 1
    ButtonOpen 4, 1, "Cancel", 16, 15, 0, 0, 1

    ' start with cursor in edit field
    currButton = 0
    currEditField = 1
    pushButton = 3

    ' control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField              ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2: currEditField = 0
                        ScrollList fileList$(), fileBox, currButton, 1, 0, winRow, winCol
                        currButton = 2
                    CASE 3, 4: pushButton = currButton
                        finished = TRUE
                END SELECT
            CASE 2                                      ' Edit Field
                currButton = 0
                currEditField = 1
            CASE 6                                      ' enter
                IF INSTR(EditFieldInquire$(1), "*") = 0 THEN finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 0: currButton = 2
                        currEditField = 0
                    CASE 1, 2:
                        currButton = 3
                        ButtonSetState 3, 2
                        ButtonSetState 4, 1
                        pushButton = 3
                    CASE 3:
                        currButton = 4
                        ButtonSetState 3, 1
                        ButtonSetState 4, 2
                        pushButton = 4
                    CASE 4:
                        currButton = 0
                        currEditField = 1
                        ButtonSetState 3, 2
                        ButtonSetState 4, 1
                        pushButton = 3
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 0: currButton = 4
                        currEditField = 0
                        ButtonSetState 3, 1
                        ButtonSetState 4, 2
                        pushButton = 4
                    CASE 1, 2:
                        currButton = 0
                        currEditField = 1
                    CASE 3:
                        currButton = 2
                    CASE 4:
                        currButton = 3
                        ButtonSetState 3, 2
                        ButtonSetState 4, 1
                        pushButton = 3
                END SELECT
            CASE 9                                      ' escape
                pushButton = 4
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                IF currButton = 1 OR currButton = 2 THEN ScrollList fileList$(), fileBox, currButton, 2, 0, winRow, winCol
            CASE 11, 13                                 'down, right arrow
                IF currButton = 1 OR currButton = 2 THEN ScrollList fileList$(), fileBox, currButton, 3, 0, winRow, winCol
            CASE 14                                     ' space bar
                IF currButton > 2 THEN
                    pushButton = currButton
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
            ELSEIF LEN(temp$) = 0 THEN
                PrintError "Must specify a name."
                finished = FALSE
            ELSE
                fileSpec$ = temp$
                fileNum% = FREEFILE
                OPEN fileSpec$ FOR INPUT AS fileNum%

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

                fileBox.listLen = fileCount
                fileBox.maxLen = Min(fileCount, fileBox.boxLen)
                origPos = 0
                fileBox.listPos = origPos
                fileBox.currTop = 1
                fileBox.currPos = 0
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

                DrawList fileList$(), fileBox, 0   ' redraw file list
            END IF
        END IF
    WEND

    ' if operation not canceled return file name and file number
    IF pushButton = 3 THEN
        FileName$ = fileSpec$
        GetLoadFile% = fileNum%
    ELSE
        GetLoadFile% = 0

        CHDRIVE MID$(origDir$, 1, 2)
        CHDIR MID$(origDir$, 3, LEN(origDir$))
    END IF

    WindowClose 1

    EXIT FUNCTION

' handle any file opening errors
GetLoadError:
    CLOSE fileNum%
    finished = FALSE                            ' don't allow exit until valid file chosen

    ShowError ERR                               ' display error message
RESUME NEXT

END FUNCTION

'
' Func Name: GetSaveFile
'
' Description: Prompts the user for a DOS file to save the current
'              chart data and settings in.  It returns the file number
'              with the actual file name being passed back via the
'              argument.
'
' Arguments: fileName$ - name of save file
'
FUNCTION GetSaveFile% (FileName$)

ON LOCAL ERROR GOTO GetSaveError                    ' handle file open errors

    ' Open window for display
    WindowOpen 1, 8, 20, 12, 58, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, "Save Chart As"
    WindowLocate 2, 2
    WindowPrint 2, "File Name:"
    WindowBox 1, 13, 3, 38
    WindowLine 4

    ' open edit field for file name
    EditFieldOpen 1, RTRIM$(FileName$), 2, 14, 0, 7, 24, 70

    ' open command buttons
    ButtonOpen 1, 2, "OK", 5, 6, 0, 0, 1
    ButtonOpen 2, 1, "Cancel", 5, 25, 0, 0, 1

    ' start with cursor in edit field
    currButton = 0
    currEditField = 1
    pushButton = 1

    ' control loop for window
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField              ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' Button pressed
                pushButton = Dialog(1)
                finished = TRUE
            CASE 2                                      ' Edit Field
                currButton = 0
                currEditField = 1
            CASE 6                                      ' enter
                finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 0, 1:
                        ButtonSetState currButton, 1
                        currButton = currButton + 1
                        pushButton = currButton
                        ButtonSetState pushButton, 2
                        currEditField = 0
                    CASE 2
                        currButton = 0
                        pushButton = 1
                        currEditField = 1
                        ButtonSetState 1, 2
                        ButtonSetState 2, 1
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 0:
                        currButton = 2
                        pushButton = 2
                        currEditField = 0
                        ButtonSetState 1, 1
                        ButtonSetState 2, 2
                    CASE 1
                        currButton = 0
                        currEditField = 1
                    CASE 2
                        currButton = 1
                        pushButton = 1
                        ButtonSetState 1, 2
                        ButtonSetState 2, 1
                END SELECT
            CASE 9                                      ' escape
                pushButton = 2
                finished = TRUE
            CASE 14                                     ' space bar
                IF currButton <> 0 THEN
                    finished = TRUE
                END IF
        END SELECT

        ' simple error checking before finishing
        IF finished = TRUE AND pushButton = 1 THEN
            temp$ = EditFieldInquire$(1)
            ' must specify a file
            IF temp$ = "" THEN
                PrintError "Must specify a name."
                finished = FALSE
            ' check if file is valid and can be opened
            ELSE
                ' open file
                fileNum% = FREEFILE
                OPEN temp$ FOR OUTPUT AS fileNum%

            END IF
        END IF
    WEND

    ' if operation not canceled return file name and file number
    IF pushButton = 1 THEN
        FileName$ = EditFieldInquire$(1)
        GetSaveFile% = fileNum%
    ELSE
        GetSaveFile% = 0
    END IF

    WindowClose 1

    EXIT FUNCTION

' local error handler
GetSaveError:
      finished = FALSE                              ' don't exit until valid file specified
      CLOSE fileNum%

      ShowError ERR                                 ' display errors
RESUME NEXT

END FUNCTION

'
' Sub Name: LoadChart
'
' Description: Loads chart data and settings from the given file.
'
' Arguments: fileNum%  - file number
'
SUB LoadChart (fileNum%)
SHARED Cat$(), catLen AS INTEGER
SHARED setLen() AS INTEGER, setName$(), setVal!()
SHARED screenMode AS INTEGER, numModes AS INTEGER, mode$()

ON LOCAL ERROR GOTO LoadError                       ' handle file loading errors

    ' Read file until EOF is reached:
    DO UNTIL EOF(fileNum%)
        ' get data type from file (C=category, V=value, T=title, S=setting):
        INPUT #fileNum%, type$

        ' category data
        IF UCASE$(type$) = "C" THEN
            INPUT #fileNum%, catLen
            FOR i% = 1 TO catLen
                INPUT #fileNum%, Cat$(i%)
            NEXT i%

        ' value data
        ELSEIF UCASE$(type$) = "V" THEN
            ' too many sets in file
            IF setNum >= cMaxSets THEN
                PrintError "Too many data sets in file. Extra sets lost."
                EXIT DO
            END IF

            setNum = setNum + 1
            INPUT #fileNum%, setName$(setNum)         ' get set name
            INPUT #fileNum%, setLen(setNum)           ' get set length
            FOR i% = 1 TO setLen(setNum)
                INPUT #fileNum%, setVal!(i%, setNum)  ' get set values
            NEXT i%

        ' title data
        ELSEIF UCASE$(type$) = "T" THEN
            INPUT #fileNum%, CEnv.MainTitle.title
            INPUT #fileNum%, CEnv.SubTitle.title
            INPUT #fileNum%, CEnv.XAxis.AxisTitle.title
            INPUT #fileNum%, CEnv.YAxis.AxisTitle.title

        ' chart settings
        ELSEIF UCASE$(type$) = "S" THEN
            INPUT #fileNum%, screenMode
            ' test for valid screen mode
            valid = FALSE
            FOR i = 1 TO numModes
                IF screenMode = VAL(mode$(i)) THEN valid = TRUE
            NEXT i
            IF NOT valid THEN
                IF mode$(1) = "13" THEN
                    screenMode = VAL(mode$(2))
                ELSE
                    screenMode = VAL(mode$(1))
                END IF
            END IF

            INPUT #fileNum%, CEnv.ChartType, CEnv.ChartStyle, CEnv.DataFont

            INPUT #fileNum%, CEnv.ChartWindow.X1, CEnv.ChartWindow.Y1, CEnv.ChartWindow.X2, CEnv.ChartWindow.Y2
            INPUT #fileNum%, CEnv.ChartWindow.Background, CEnv.ChartWindow.border, CEnv.ChartWindow.BorderStyle, CEnv.ChartWindow.BorderColor
            INPUT #fileNum%, CEnv.DataWindow.X1, CEnv.DataWindow.Y1, CEnv.DataWindow.X2, CEnv.DataWindow.Y2
            INPUT #fileNum%, CEnv.DataWindow.Background, CEnv.DataWindow.border, CEnv.DataWindow.BorderStyle, CEnv.DataWindow.BorderColor

            INPUT #fileNum%, CEnv.MainTitle.TitleFont, CEnv.MainTitle.TitleColor, CEnv.MainTitle.Justify
            INPUT #fileNum%, CEnv.SubTitle.TitleFont, CEnv.SubTitle.TitleColor, CEnv.SubTitle.Justify

            INPUT #fileNum%, CEnv.XAxis.Grid, CEnv.XAxis.GridStyle, CEnv.XAxis.AxisColor, CEnv.XAxis.Labeled
            INPUT #fileNum%, CEnv.XAxis.AxisTitle.TitleFont, CEnv.XAxis.AxisTitle.TitleColor, CEnv.XAxis.AxisTitle.Justify
            INPUT #fileNum%, CEnv.XAxis.RangeType, CEnv.XAxis.LogBase, CEnv.XAxis.AutoScale, CEnv.XAxis.ScaleMin
            INPUT #fileNum%, CEnv.XAxis.ScaleMax, CEnv.XAxis.ScaleFactor, CEnv.XAxis.TicFont, CEnv.XAxis.TicInterval, CEnv.XAxis.TicFormat, CEnv.XAxis.TicDecimals
            INPUT #fileNum%, CEnv.XAxis.ScaleTitle.title
            INPUT #fileNum%, CEnv.XAxis.ScaleTitle.TitleFont, CEnv.XAxis.ScaleTitle.TitleColor, CEnv.XAxis.ScaleTitle.Justify

            INPUT #fileNum%, CEnv.YAxis.Grid, CEnv.YAxis.GridStyle, CEnv.YAxis.AxisColor, CEnv.YAxis.Labeled
            INPUT #fileNum%, CEnv.YAxis.AxisTitle.TitleFont, CEnv.YAxis.AxisTitle.TitleColor, CEnv.YAxis.AxisTitle.Justify
            INPUT #fileNum%, CEnv.YAxis.RangeType, CEnv.YAxis.LogBase, CEnv.YAxis.AutoScale, CEnv.YAxis.ScaleMin
            INPUT #fileNum%, CEnv.YAxis.ScaleMax, CEnv.YAxis.ScaleFactor, CEnv.YAxis.TicFont, CEnv.YAxis.TicInterval, CEnv.YAxis.TicFormat, CEnv.YAxis.TicDecimals
            INPUT #fileNum%, CEnv.YAxis.ScaleTitle.title
            INPUT #fileNum%, CEnv.YAxis.ScaleTitle.TitleFont, CEnv.YAxis.ScaleTitle.TitleColor, CEnv.YAxis.ScaleTitle.Justify

            INPUT #fileNum%, CEnv.Legend.Legend, CEnv.Legend.Place, CEnv.Legend.TextColor, CEnv.Legend.TextFont, CEnv.Legend.AutoSize
            INPUT #fileNum%, CEnv.Legend.LegendWindow.X1, CEnv.Legend.LegendWindow.Y1, CEnv.Legend.LegendWindow.X2, CEnv.Legend.LegendWindow.Y2
            INPUT #fileNum%, CEnv.Legend.LegendWindow.Background, CEnv.Legend.LegendWindow.border, CEnv.Legend.LegendWindow.BorderStyle, CEnv.Legend.LegendWindow.BorderColor
        ELSE
            GOSUB LoadError
        END IF
    LOOP

    ' close the file
    CLOSE fileNum%

    ' clear any font pointers that don't map to current fonts
    ClearFonts

    ' initialize color list depending on newly loaded screen mode
    InitColors

    EXIT SUB

' handle any file format errors
LoadError:

    IF ERR THEN
        ShowError ERR
    ELSE
        PrintError "Invalid file format.  Can't continue loading."
    END IF

    CLOSE fileNum%                              ' close and exit
    EXIT SUB

RESUME NEXT

END SUB

'
' Sub Name: OpenChart
'
' Description: Handles both the "New" and "Open" operations from the
'              "File" menu title.
'
' Arguments: newFlag - flag for determining which operation (New or Open)
'                      to perform.
'
SUB OpenChart (newFlag)
SHARED saveFile$

    ' allow user to save current chart if necessary
    IF chartChanged THEN
        a$ = "|"
        a$ = a$ + "Current chart has not been saved.  Save now?"

        status = Alert(4, a$, 8, 15, 12, 65, "Yes", "No", "Cancel")

        ' save current chart
        IF status = OK THEN
            status = SaveChart(saveFile$, FALSE)
        END IF
    ELSE
        status = OK
    END IF

    IF status <> CANCEL THEN
        ' New option chosen so clear existing data, leave chart settings alone.
        IF newFlag = TRUE THEN
            MenuItemToggle GALLERYTITLE, CEnv.ChartType
            IF CEnv.ChartType = cPie THEN
                MenuSetState CHARTTITLE, 4, 1
                MenuSetState CHARTTITLE, 5, 1
                MenuSetState TITLETITLE, 3, 1
                MenuSetState TITLETITLE, 4, 1
            END IF
            InitChart
            saveFile$ = ""
        ' Open operation chosen so get file and load data
        ELSE
            fileNum% = GetLoadFile(saveFile$)
            ' if no errors opening file and operation not canceled then load data
            IF fileNum <> 0 THEN
                ' reset menu bar to nothing selected
                MenuItemToggle GALLERYTITLE, CEnv.ChartType
                IF CEnv.ChartType = cPie THEN
                    MenuSetState CHARTTITLE, 4, 1
                    MenuSetState CHARTTITLE, 5, 1
                    MenuSetState TITLETITLE, 3, 1
                    MenuSetState TITLETITLE, 4, 1
                END IF

                ClearData                       'clear current data

                setNum = 0
                LoadChart fileNum%             ' load the data
                
                ' set menu bar according to new chart settings
                MenuItemToggle GALLERYTITLE, CEnv.ChartType
                IF CEnv.ChartType = cPie THEN
                    MenuSetState CHARTTITLE, 4, 0
                    MenuSetState CHARTTITLE, 5, 0
                    MenuSetState TITLETITLE, 3, 0
                    MenuSetState TITLETITLE, 4, 0
                END IF

                ' new chart not changed
                chartChanged = FALSE

                ' chart data exists so allow user to view chart
                IF setNum > 0 THEN
                    MenuSetState VIEWTITLE, 2, 1
                END IF
            END IF
        END IF
    END IF

END SUB

'
' Sub Name: PrintError
'
' Description: Prints error messages on the screen in an Alert box.
'
' Arguments: text$ - error message
'
SUB PrintError (text$)

    textLen = LEN(text$) + 2
    lefCol = ((80 - textLen) / 2) - 1
    a$ = "| " + text$
    junk = Alert(4, a$, 8, lefCol, 12, textLen + lefCol, "", "", "")

END SUB

'
' Func Name: SaveChart
'
' Description: Performs both the "Save" and "Save AS" operations from
'              the "File" menu title.  If "Save As" was chosen or if
'              "Save" was chosen and no save file has been previously
'              specified, it prompts the user for a new file in
'              which to save the current chart.  Also returns the status of
'              save operation for use in other routines
'
' Arguments: fileName$ - name of previously specified save file (may be nil)
'            saveAsFlag - flag for invoking the "Save As" operation.
'
FUNCTION SaveChart% (FileName$, saveAsFlag)
SHARED Cat$(), catLen AS INTEGER
SHARED setLen() AS INTEGER, setName$(), setVal!()
SHARED screenMode AS INTEGER

ON LOCAL ERROR GOTO SaveError                   ' handle file errors

    ' get new file name if necessary
    IF FileName$ = "" OR saveAsFlag THEN
        fileNum% = GetSaveFile(FileName$)
    ' otherwise just open the file
    ELSE
        fileNum% = FREEFILE
        OPEN FileName$ FOR OUTPUT AS fileNum%
    END IF

    ' quit save if cancel chosen above or error occurred during open.
    IF fileNum% = 0 THEN
        SaveChart% = CANCEL                     ' return status
        EXIT FUNCTION
    END IF

    ' save category data
    IF catLen > 0 THEN
        PRINT #fileNum%, "C"
        PRINT #fileNum%, catLen

        FOR i% = 1 TO catLen
            PRINT #fileNum%, Cat$(i%)
        NEXT i%
    END IF

    ' save value data
    IF setNum > 0 THEN
        FOR j% = 1 TO setNum
            PRINT #fileNum%, "V"
            PRINT #fileNum%, setName$(j%)
            PRINT #fileNum%, setLen(j%)

            FOR i% = 1 TO setLen(j%)
                PRINT #fileNum%, setVal!(i%, j%)
            NEXT i%
        NEXT j%
    END IF

    ' save titles
    PRINT #fileNum%, "T"
    PRINT #fileNum%, CEnv.MainTitle.title
    PRINT #fileNum%, CEnv.SubTitle.title
    PRINT #fileNum%, CEnv.XAxis.AxisTitle.title
    PRINT #fileNum%, CEnv.YAxis.AxisTitle.title

    'save chart settings
    PRINT #fileNum%, "S"
    PRINT #fileNum%, screenMode

    PRINT #fileNum%, CEnv.ChartType, CEnv.ChartStyle, CEnv.DataFont

    PRINT #fileNum%, CEnv.ChartWindow.X1, CEnv.ChartWindow.Y1, CEnv.ChartWindow.X2, CEnv.ChartWindow.Y2
    PRINT #fileNum%, CEnv.ChartWindow.Background, CEnv.ChartWindow.border, CEnv.ChartWindow.BorderStyle, CEnv.ChartWindow.BorderColor
    PRINT #fileNum%, CEnv.DataWindow.X1, CEnv.DataWindow.Y1, CEnv.DataWindow.X2, CEnv.DataWindow.Y2
    PRINT #fileNum%, CEnv.DataWindow.Background, CEnv.DataWindow.border, CEnv.DataWindow.BorderStyle, CEnv.DataWindow.BorderColor

    PRINT #fileNum%, CEnv.MainTitle.TitleFont, CEnv.MainTitle.TitleColor, CEnv.MainTitle.Justify
    PRINT #fileNum%, CEnv.SubTitle.TitleFont, CEnv.SubTitle.TitleColor, CEnv.SubTitle.Justify

    PRINT #fileNum%, CEnv.XAxis.Grid, CEnv.XAxis.GridStyle, CEnv.XAxis.AxisColor, CEnv.XAxis.Labeled
    PRINT #fileNum%, CEnv.XAxis.AxisTitle.TitleFont, CEnv.XAxis.AxisTitle.TitleColor, CEnv.XAxis.AxisTitle.Justify
    PRINT #fileNum%, CEnv.XAxis.RangeType, CEnv.XAxis.LogBase, CEnv.XAxis.AutoScale, CEnv.XAxis.ScaleMin
    PRINT #fileNum%, CEnv.XAxis.ScaleMax, CEnv.XAxis.ScaleFactor, CEnv.XAxis.TicFont, CEnv.XAxis.TicInterval, CEnv.XAxis.TicFormat, CEnv.XAxis.TicDecimals
    PRINT #fileNum%, CEnv.XAxis.ScaleTitle.title
    PRINT #fileNum%, CEnv.XAxis.ScaleTitle.TitleFont, CEnv.XAxis.ScaleTitle.TitleColor, CEnv.XAxis.ScaleTitle.Justify

    PRINT #fileNum%, CEnv.YAxis.Grid, CEnv.YAxis.GridStyle, CEnv.YAxis.AxisColor, CEnv.YAxis.Labeled
    PRINT #fileNum%, CEnv.YAxis.AxisTitle.TitleFont, CEnv.YAxis.AxisTitle.TitleColor, CEnv.YAxis.AxisTitle.Justify
    PRINT #fileNum%, CEnv.YAxis.RangeType, CEnv.YAxis.LogBase, CEnv.YAxis.AutoScale, CEnv.YAxis.ScaleMin
    PRINT #fileNum%, CEnv.YAxis.ScaleMax, CEnv.YAxis.ScaleFactor, CEnv.YAxis.TicFont, CEnv.YAxis.TicInterval, CEnv.YAxis.TicFormat, CEnv.YAxis.TicDecimals
    PRINT #fileNum%, CEnv.YAxis.ScaleTitle.title
    PRINT #fileNum%, CEnv.YAxis.ScaleTitle.TitleFont, CEnv.YAxis.ScaleTitle.TitleColor, CEnv.YAxis.ScaleTitle.Justify
        
    PRINT #fileNum%, CEnv.Legend.Legend, CEnv.Legend.Place, CEnv.Legend.TextColor, CEnv.Legend.TextFont, CEnv.Legend.AutoSize
    PRINT #fileNum%, CEnv.Legend.LegendWindow.X1, CEnv.Legend.LegendWindow.Y1, CEnv.Legend.LegendWindow.X2, CEnv.Legend.LegendWindow.Y2
    PRINT #fileNum%, CEnv.Legend.LegendWindow.Background, CEnv.Legend.LegendWindow.border, CEnv.Legend.LegendWindow.BorderStyle, CEnv.Legend.LegendWindow.BorderColor

    CLOSE fileNum%

    SaveChart% = OK                             ' return status

    chartChanged = FALSE                        ' reset global change flag

    EXIT FUNCTION

' local error handler
SaveError:
      SaveChart% = CANCEL                       ' return cancel status
      CLOSE fileNum%

      ShowError ERR                             ' display error message

      EXIT FUNCTION                             ' exit on error
RESUME NEXT

END FUNCTION

'
' Sub Name: ShowError
'
' Description: Displays an appropriate error message for the given error
'
' Arguments: errorNum - error number
'
SUB ShowError (errorNum)
      SELECT CASE errorNum
        CASE 6:                                 ' overflow
            PrintError "Overflow occurred."
        CASE 14:                                ' out of space
            PrintError "Out of string space.  Please restart."
        CASE 53:                                ' file not found
            PrintError "File not found."
        CASE 62:                                ' input past end of file
            PrintError "Invalid file format. Can't continue loading."
        CASE 64:                                ' bad file name
            PrintError "Invalid file name."
        CASE 68:                                ' device unavailable
            PrintError "Selected device unavailable."
        CASE 71:                                ' disk not ready
            PrintError "Disk not ready."
        CASE 75:                                ' path access error
            PrintError "Invalid path."
        CASE 76:                                ' path not found
            PrintError "Path not found."
        CASE ELSE                               ' catch all
            PrintError "BASIC error #" + LTRIM$(STR$(ERR)) + " occurred."
     END SELECT


END SUB

'
' Sub Name: ViewData
'
' Description: Displays the current chart data and allows the user to
'              modify, delete or add to that data.
'
' Arguments: none
'
SUB ViewData
SHARED setVal!(), setLen()  AS INTEGER, setName$()
SHARED Cat$(), catLen AS INTEGER
SHARED GloEdit() AS EditFieldType

    ' temporary data storage that allows user to cancel all changes and
    ' restore original data
    DIM tsetVal$(1 TO 15, 1 TO 15), tCat$(1 TO 15), tsetName$(1 TO 15)
    DIM tsetNum AS INTEGER
    DIM tsetLen(1 TO 15) AS INTEGER
    DIM tcatLen  AS INTEGER

    ON LOCAL ERROR GOTO ViewDatError

    ' fill out temp data
    FOR i = 1 TO cMaxSets
        tsetName$(i) = setName$(i)
        tCat$(i) = Cat$(i)
        tsetLen(i) = setLen(i)
        FOR j = 1 TO tsetLen(i)
            tsetVal$(j, i) = LTRIM$(STR$(setVal!(j, i)))
        NEXT j
        FOR j = tsetLen(i) + 1 TO cMaxValues
            tsetVal$(j, i) = ""
        NEXT j
    NEXT i
    tsetNum = setNum
    tcatLen = catLen

    ' set up window
    winRow = 4
    winCol = 8
    WindowOpen 1, winRow, winCol, 23, 74, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Chart Data"
    WindowLocate 1, 2
    WindowPrint 2, "Series Name:"
    WindowBox 2, 2, 18, 24
    WindowLocate 1, 26
    WindowPrint 2, "Categories:"
    WindowBox 2, 26, 18, 48
    WindowLocate 1, 50
    WindowPrint 2, "Values:"
    WindowBox 2, 50, 18, 66
    WindowLine 19

    ' display chart data
    FOR i = 1 TO 15
        IF i < 10 THEN
            a$ = " "
        ELSE
            a$ = ""
        END IF
        a$ = a$ + LTRIM$(STR$(i)) + ". "
        WindowLocate i + 2, 3
        WindowPrint 2, a$ + tsetName$(i)
        WindowLocate i + 2, 27
        WindowPrint 2, a$ + tCat$(i)
        WindowLocate i + 2, 51
        WindowPrint 2, a$ + MID$(tsetVal$(i, 1), 1, 10)
    NEXT i
    ' highlight first set name
    EditFieldOpen 1, tsetName$(1), 3, 7, 7, 0, 17, 16

    IF tsetNum < cMaxSets THEN tsetNum = tsetNum + 1
    IF tcatLen < cMaxValues THEN tcatLen = tcatLen + 1
    IF tsetLen(1) < cMaxValues THEN tsetLen(1) = tsetLen(1) + 1

    ' area buttons
    ButtonOpen 1, 1, "", 3, 3, 17, 23, 4
    ButtonOpen 2, 1, "", 3, 27, 17, 47, 4
    ButtonOpen 3, 1, "", 3, 51, 17, 65, 4

    ' command buttons
    ButtonOpen 4, 1, "OK", 20, 15, 0, 0, 1
    ButtonOpen 5, 1, "Cancel", 20, 45, 0, 0, 1

    ' start with cursor in first set name edit field
    currButton = 1
    prevButton = 1
    currRow = 1
    currEditField = 1
    currCat = 1
    currVal = 1
    currSet = 1

    IF CEnv.ChartType = cPie THEN
        a$ = " Pie chart information||"
        a$ = a$ + " Only data values from the first series are plotted in pie charts. |"
        a$ = a$ + " Data values from the second series are used in determining whether|"
        a$ = a$ + " or not pie pieces are exploded.  Non-zero values in this series   |"
        a$ = a$ + " will cause corresponding pie pieces to be exploded.  All other    |"
        a$ = a$ + "  series will be ignored.                                           "

        junk = Alert(4, a$, 8, 7, 17, 75, "", "", "")
    END IF

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField

        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2, 3
                        currRow = Dialog(17)
                    CASE 4, 5
                        finished = TRUE
                END SELECT
                GOSUB UpdateEdit
            CASE 2                                      ' Edit Field
                currEditField = Dialog(2)
            CASE 6, 11                                  ' enter, down arrow
                IF currButton > 3 AND Dialog(0) = 6 THEN
                    finished = TRUE
                ELSE
                    currRow = currRow + 1
                    GOSUB UpdateEdit
                END IF
            CASE 7                                      'tab
                SELECT CASE currButton
                    CASE 1:
                        currButton = 2
                        currRow = currCat
                        GOSUB UpdateEdit
                    CASE 2:
                        currButton = 3
                        currRow = currVal
                        GOSUB UpdateEdit
                    CASE 3:
                        currButton = 4
                        ButtonToggle 4
                        GOSUB UpdateEdit
                    CASE 4:
                        currButton = 5
                        ButtonToggle 4
                        ButtonToggle 5
                    CASE 5:
                        currButton = 1
                        currRow = currSet
                        ButtonToggle 5
                        GOSUB UpdateEdit
                END SELECT
            CASE 8                                      'back tab
                SELECT CASE currButton
                    CASE 1:
                        currButton = 5
                        ButtonToggle 5
                        GOSUB UpdateEdit
                    CASE 2:
                        currButton = 1
                        currRow = currSet
                        GOSUB UpdateEdit
                    CASE 3:
                        currButton = 2
                        currRow = currCat
                        GOSUB UpdateEdit
                    CASE 4:
                        currButton = 3
                        currRow = currVal
                        ButtonToggle 4
                        GOSUB UpdateEdit
                    CASE 5:
                        currButton = 4
                        ButtonToggle 5
                        ButtonToggle 4
                END SELECT
            CASE 9                                      'escape
                currButton = 5
                finished = TRUE
            CASE 10:                                    'up arrow
                IF currButton < 4 THEN
                    currRow = currRow - 1
                    GOSUB UpdateEdit
                END IF
            CASE 14                                     'space
                IF currButton > 3 THEN finished = TRUE
        END SELECT

        ' give delete warning before exit
        IF finished = TRUE AND currButton = 4 THEN
            temp = FALSE
            FOR i = 1 TO tsetNum
                IF tsetName$(i) = "" AND tsetLen(i) > 0 AND NOT (tsetLen(i) = 1 AND tsetVal$(1, i) = "") THEN temp = TRUE
            NEXT i
            IF temp = TRUE THEN
                a$ = "|"
                a$ = a$ + "Series without names will be deleted upon exit."
                reply = Alert(4, a$, 8, 10, 12, 70, "OK", "Cancel", "")
                IF reply <> 1 THEN finished = FALSE
            END IF
        END IF
    WEND

    ' finished so save new data
    IF currButton = 4 THEN
        ClearData                                       ' clear existing data

        ' copy temporary values to permanent locations
        indx = 0
        FOR i = 1 TO tsetNum
            IF tsetName$(i) <> "" THEN
                indx = indx + 1
                setName$(indx) = tsetName$(i)              ' store set names
                indx2 = 0
                FOR j = 1 TO tsetLen(i)
                    IF tsetVal$(j, i) <> "" THEN
                        indx2 = indx2 + 1
                        setVal!(indx2, i) = VAL(tsetVal$(j, i))   ' store set values
                    END IF
                NEXT j
                setLen(indx) = indx2                     ' get set lengths
            END IF
        NEXT i
        setNum = indx

        ' clear leftover names and set lengths
        FOR i = setNum + 1 TO cMaxSets
            setName$(i) = ""
            setLen(i) = 0
        NEXT i

        ' store category names
        FOR i = 1 TO tcatLen
            Cat$(i) = tCat$(i)
        NEXT i
        catLen = tcatLen

        FOR i = tcatLen TO 1 STEP -1
            IF Cat$(i) = "" THEN
                catLen = catLen - 1
                IF catLen <= 0 THEN EXIT FOR
            ELSE
                EXIT FOR
            END IF
        NEXT i

        ' clear leftover category names
        FOR i = catLen + 1 TO cMaxValues
            Cat$(i) = ""
        NEXT i

        ' update active menu titles based on current data
        IF setNum > 0 THEN
            MenuSetState VIEWTITLE, 2, 1
            chartChanged = TRUE
        ELSE
            MenuSetState VIEWTITLE, 2, 0
        END IF
    END IF
    WindowClose 1


    EXIT SUB

ViewDatError:
    PrintError "BASIC error #" + LTRIM$(STR$(ERR)) + " occurred."
RESUME NEXT

' redraws the value edit column so it displays the current set's values
ResetVal:
    ' display new values
    FOR i = 1 TO cMaxValues
        WindowLocate i + 2, 55
        WindowPrint 2, tsetVal$(i, currSet) + STRING$(10 - LEN(tsetVal$(i, currSet)), " ")
    NEXT i

    IF tsetLen(currSet) = 0 THEN
        tsetLen(currSet) = tsetLen(currSet) + 1
    ELSEIF tsetLen(currSet) < cMaxValues AND tsetVal$(tsetLen(currSet), currSet) <> "" THEN
        tsetLen(currSet) = tsetLen(currSet) + 1
    END IF

    currVal = 31

RETURN

UpdateEdit:
    IF prevButton < 4 THEN GOSUB ClosePrevEdit

    SELECT CASE currButton
        CASE 1:
            IF currRow <= 0 THEN
                currRow = tsetNum
            ELSEIF currRow > 15 THEN
                currRow = 1
            ELSEIF currRow = tsetNum + 1 AND tsetName$(tsetNum) <> "" THEN
                tsetNum = tsetNum + 1
            ELSEIF currRow > tsetNum THEN
                currRow = 1
            END IF
            WindowColor 0, 7
            WindowLocate currSet + 2, 7
            WindowPrint 2, tsetName$(currSet) + STRING$(17 - LEN(tsetName$(currSet)), " ")

            FG = 7
            BG = 0
            vislen = 17
            totlen = 16
            currSet = currRow
            currCol = 7
            temp$ = tsetName$(currSet)
            IF prevButton = 1 THEN GOSUB ResetVal
        CASE 2:
            IF currRow <= 0 THEN
                currRow = tcatLen
            ELSEIF currRow > 15 THEN
                currRow = 1
            ELSEIF currRow > tcatLen THEN
                tcatLen = currRow
            END IF
            FG = 0
            BG = 7
            vislen = 17
            totlen = 16
            currCat = currRow
            currCol = 31
            temp$ = tCat$(currCat)
        CASE 3:
            IF currRow <= 0 THEN
                currRow = tsetLen(currSet)
            ELSEIF currRow > 15 THEN
                currRow = 1
            ELSEIF currRow = tsetLen(currSet) + 1 AND tsetVal$(tsetLen(currSet), currSet) <> "" AND currRow THEN
                tsetLen(currSet) = tsetLen(currSet) + 1
            ELSEIF currRow > tsetLen(currSet) THEN
                currRow = 1
            END IF
            FG = 0
            BG = 7
            vislen = 11
            totlen = 20
            currVal = currRow
            currCol = 55
            temp$ = tsetVal$(currVal, currSet)
        CASE ELSE
            prevButton = currButton
            RETURN
    END SELECT

    EditFieldOpen 1, temp$, currRow + 2, currCol, FG, BG, vislen, totlen
    currEditField = 1
    prevButton = currButton
RETURN

ClosePrevEdit:
    temp$ = RTRIM$(EditFieldInquire$(1))
    EditFieldClose 1
    currEditField = 0
    IF prevButton = 1 THEN
        WindowColor 7, 0
    ELSE
        WindowColor 0, 7
    END IF

    SELECT CASE prevButton
        CASE 1:
            tsetName$(currSet) = temp$
            temp$ = temp$ + STRING$(17 - LEN(temp$), " ")
            editRow = currSet + 2
            editCol = 7
        CASE 2:
            tCat$(currCat) = temp$
            editRow = currCat + 2
            editCol = 31
        CASE 3:
            tsetVal$(currVal, currSet) = temp$
            tval# = VAL(temp$)
            IF tval# = 0 AND temp$ <> "0" AND LEN(RTRIM$(temp$)) <> 0 THEN
                PrintError "Warning: Non-numeric values will default to zero for charting."
            END IF
            temp$ = MID$(temp$, 1, 10)
            editRow = currVal + 2
            editCol = 55
    END SELECT

    WindowLocate editRow, editCol
    WindowPrint 2, temp$
    WindowColor 0, 7
RETURN

END SUB

