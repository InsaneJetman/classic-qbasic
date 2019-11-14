'       CHRTDEM2.BAS - third module of the CHRTB demonstration program.
'
'               Copyright (C) 1989-1990, Microsoft Corporation
'
'   Main module - CHRTDEMO.BAS
'   Include files - CHRTDEMO.BI
'
'$INCLUDE: 'chrtdemo.bi'

' local functions
DECLARE FUNCTION TrueColr% (colr%)

' local subs
DECLARE SUB OpenChart (newFlag%)
DECLARE SUB Quit ()
DECLARE SUB InitFonts ()
DECLARE SUB InitStyles ()
DECLARE SUB SetDisplayColor ()
DECLARE SUB SetUpBackground ()
DECLARE SUB SetUpMenu ()
DECLARE SUB ViewChart ()
DECLARE SUB ViewFont ()
DECLARE SUB ViewScreenMode ()

DIM colorDisplay            AS INTEGER
DIM egacolor(0 TO 15)       AS INTEGER
DIM origPath$

DEFINT A-Z
'
' Sub Name: ClearData
'
' Description: Clears all chart data
' Arguments: None
'
SUB ClearData
SHARED Cat$(), catLen AS INTEGER
SHARED setVal!(), setLen() AS INTEGER, setName$()

    ' Can't view  chart when no data present
    MenuSetState VIEWTITLE, 2, 0

    ' Clear categories
    FOR i = 1 TO cMaxValues
        Cat$(i) = ""
    NEXT i
    catLen = 0

    ' Clear set names and values
    FOR i = 1 TO cMaxSets
        setName$(i) = ""
        setLen(i) = 0
        FOR j = 1 TO cMaxValues
            setVal!(j, i) = cMissingValue
        NEXT j
    NEXT i
    setNum = 0

    ' chart not changed
    chartChanged = FALSE

END SUB

'
' Sub Name: ClearFonts
'
' Description: Sets all chart font pointers to 1.  This is called
'              each time new fonts are loaded to ensure that
'              all chart fonts specify a meaningful font
'
' Arguments: None
'
SUB ClearFonts

    ' reset all font pointers if don't map to current fonts
    IF CEnv.DataFont > numFonts THEN CEnv.DataFont = 1
    IF CEnv.MainTitle.TitleFont > numFonts THEN CEnv.MainTitle.TitleFont = 1
    IF CEnv.SubTitle.TitleFont > numFonts THEN CEnv.SubTitle.TitleFont = 1
    IF CEnv.XAxis.AxisTitle.TitleFont > numFonts THEN CEnv.XAxis.AxisTitle.TitleFont = 1
    IF CEnv.XAxis.TicFont > numFonts THEN CEnv.XAxis.TicFont = 1
    IF CEnv.YAxis.AxisTitle.TitleFont > numFonts THEN CEnv.YAxis.AxisTitle.TitleFont = 1
    IF CEnv.YAxis.TicFont > numFonts THEN CEnv.YAxis.TicFont = 1
    IF CEnv.Legend.TextFont > numFonts THEN CEnv.Legend.TextFont = 1

END SUB

'
' Sub Name: CreateListBox
'
' Description: Creates a list box within the current window
' Arguments: text$() - the list
'            tbox    - the listBox
'            func    - function flag for DrawList
'
SUB CreateListBox (text$(), tbox AS ListBox, func)

    ' get box length
    tbox.boxLen = tbox.botRow - tbox.topRow - 1

    ' get displayable length
    IF tbox.listLen < tbox.boxLen THEN
        tbox.maxLen = tbox.listLen
    ELSE
        tbox.maxLen = tbox.boxLen
    END IF

    ' get box width
    tbox.boxWid = tbox.rightCol - tbox.leftCol - 1

    ' create box
    WindowBox tbox.topRow, tbox.leftCol, tbox.botRow, tbox.rightCol

    ' add scroll bar if necessary or if forced (func = 5)
    IF tbox.listLen <> tbox.maxLen OR func = 5 THEN
        ButtonOpen tbox.scrollButton, 1, "", tbox.topRow + 1, tbox.rightCol, tbox.botRow - 1, tbox.rightCol, 6
    ELSE
        tbox.scrollButton = 0
    END IF

    ' open area button
    ButtonOpen tbox.areaButton, 1, "", tbox.topRow + 1, tbox.leftCol + 1, tbox.botRow - 1, tbox.rightCol - 1, 4

    ' set current list element relative to list box top
    IF tbox.listPos <= tbox.maxLen THEN
        tbox.currTop = 1
        tbox.currPos = tbox.listPos
    ELSEIF tbox.listPos + tbox.maxLen > tbox.listLen + 1 THEN
        tbox.currTop = tbox.listLen - tbox.maxLen + 1
        tbox.currPos = tbox.listPos - tbox.currTop + 1
    ELSE
        tbox.currTop = tbox.listPos
        tbox.currPos = 1
    END IF

    ' Display list within the box
    DrawList text$(), tbox, func

END SUB

'
' Sub Name: DrawList
'
' Description: Displays a list within the boundaries of a list box
' Arguments: text$() - the list
'            tbox    - the listBox
'            func    - function flag for special operations
'
SUB DrawList (text$(), tbox AS ListBox, func)

    ' Draw each element of list that should currently appear in box
    FOR i% = 1 TO tbox.boxLen
        ' highlight current list element
        IF i% = tbox.currPos THEN
            WindowColor 7, 0
        ELSE
            WindowColor 0, 7
        END IF

        WindowLocate tbox.topRow + i%, tbox.leftCol + 1
        IF i <= tbox.maxLen THEN
            WindowPrint -1, LEFT$(text$(tbox.currTop + i% - 1) + STRING$(tbox.boxWid, " "), tbox.boxWid)
        ELSE
            WindowPrint -1, STRING$(tbox.boxWid, " ")
        END IF
    NEXT i%

    ' update scrollbar position indicator if scrollbar present
    IF tbox.scrollButton <> 0 THEN
        IF tbox.listLen <> 0 THEN
            position = (tbox.currTop + tbox.currPos - 1) * (tbox.maxLen - 2) / tbox.listLen
            IF position < 1 THEN
               position = 1
            ELSEIF position > tbox.maxLen - 2 THEN
              position = tbox.maxLen - 2
            END IF
        ELSE
            position = 1
        END IF
        ButtonSetState tbox.scrollButton, position
    END IF

    ' Reset color in case current element was last to be drawn
    WindowColor 0, 7

    ' update current position in case list has been scrolled
    tbox.listPos = tbox.currTop + tbox.currPos - 1

    ' handle special operation of immediately updating colors$ in title editfield
    SELECT CASE func
        CASE 2: SetAtt 12, tbox.listPos          ' update title editfield foreground color
    END SELECT

END SUB

'
' Func Name: HandleMenuEvent
'
' Description: Determines the action to be performed when user makes
'              a menu selection.
'
' Arguments: none
'
SUB HandleMenuEvent
SHARED saveFile$, colorDisplay AS INTEGER

    menu = MenuCheck(0)
    item = MenuCheck(1)

    SELECT CASE menu
        ' file menu title selection
        CASE FILETITLE
            SELECT CASE item
                ' new chart
                CASE 1: OpenChart TRUE
                ' open existing chart
                CASE 2: OpenChart FALSE
                ' save current chart
                CASE 3: junk = SaveChart(saveFile$, FALSE)
                ' save current chart under new name
                CASE 4: junk = SaveChart(saveFile$, TRUE)
                ' exit program
                CASE 6: Quit
            END SELECT

        ' view menu title selection
        CASE VIEWTITLE
            SELECT CASE item
                ' Display and edit existing chart data
                CASE 1: ViewData
                ' Display chart
                CASE 2: ViewChart
                ' Display and load fonts
                CASE 3: ViewFont
                ' Display and edit screen mode
                CASE 4: ViewScreenMode
            END SELECT

        ' Gallery menu title selection
        CASE GALLERYTITLE
            ' change chart type
            ChangeChartType item

        ' Chart menu title selection
        CASE CHARTTITLE
            SELECT CASE item
                ' Change chart window
                CASE 1: ChangeWindow 1, "Chart Window", CEnv.ChartWindow
                ' Change data window
                CASE 2: ChangeWindow 1, "Data Window", CEnv.DataWindow
                ' Change legend
                CASE 3: ChangeLegend
                ' Change X axis
                CASE 4: ChangeAxis "X Axis", CEnv.XAxis
                ' Change Y axis
                CASE 5: ChangeAxis "Y Axis", CEnv.YAxis
            END SELECT

        ' Title menu title selection
        CASE TITLETITLE
            SELECT CASE item
                ' Display and modify main title
                CASE 1: ChangeTitle 1, "Main Title", CEnv.MainTitle, 6, 16
                ' Display and modify sub title
                CASE 2: ChangeTitle 1, "Sub Title", CEnv.SubTitle, 6, 16
                ' Display and modify x axis title
                CASE 3:
                    ChangeTitle 1, "X-axis Title", CEnv.XAxis.AxisTitle, 6, 16
                    CEnv.XAxis.ScaleTitle.TitleColor = CEnv.XAxis.AxisTitle.TitleColor
                    CEnv.XAxis.ScaleTitle.Justify = CEnv.XAxis.AxisTitle.Justify
                ' Display and modify y axis title
                CASE 4:
                    ChangeTitle 1, "Y-axis Title", CEnv.YAxis.AxisTitle, 6, 16
                    CEnv.YAxis.ScaleTitle.TitleColor = CEnv.YAxis.AxisTitle.TitleColor
                    CEnv.YAxis.ScaleTitle.Justify = CEnv.YAxis.AxisTitle.Justify
            END SELECT

        ' Options menu title selection
        CASE OPTIONSTITLE
            colorDisplay = item - 2
            SetDisplayColor
    END SELECT

END SUB

'
' Func Name: InitAll
'
' Description: Performs all initialization for the program
'
' Arguments: none
'
SUB InitAll
SHARED finished AS INTEGER, screenMode AS INTEGER, saveFile$
SHARED origPath$, colorDisplay  AS INTEGER

    saveFile$ = ""                          ' No save file to begin with
    origPath$ = CURDIR$                     ' get working path
    colorDisplay = FALSE                    ' start with mono display
    GetBestMode screenMode                  ' get initial screen mode

    SCREEN 0                                ' init screen
    WIDTH 80, 25
    CLS

    MenuInit                                ' init menu routines
    WindowInit                              ' init window routines
    MouseInit                               ' init mouse routines

    ' exit if no graphic mode available
    IF screenMode = 0 THEN
        PrintError "No graphic screen modes available for charting. Exiting program."
        finished = TRUE
        EXIT SUB
    ELSE
        finished = FALSE
    END IF

    SetUpMenu                               ' Set up menu bar
    SetUpBackground                         ' Set up screen background
    InitChart                               ' Initialize chart
    InitColors                              ' Set up color list
    InitStyles                              ' Set up border style list
    InitFonts                               ' Set up font lists

    MenuShow                                ' display menu bar
    MouseShow                               ' display mouse

    '               display program introduction
    a$ = "Microsoft QuickChart|"
    a$ = a$ + "A Presentation Graphics Toolbox Demo|"
    a$ = a$ + "for|"
    a$ = a$ + "Microsoft BASIC 7.1 Professional Development System|"
    a$ = a$ + "Copyright (c) 1989-1990 Microsoft Corporation|"

    temp = Alert(4, a$, 9, 12, 15, 68, "Color", "Monochrome", "")

    ' set display to color or monochrome depending on colorDislay
    IF temp = 1 THEN colorDisplay = TRUE

    SetDisplayColor

END SUB

'
' Sub Name: InitChart
'
' Description: Initializes chart environment variables and other
'              related information.
'
' Arguments: None
'
SUB InitChart

    MenuItemToggle GALLERYTITLE, cBar       ' default chart type is BAR so
                                            ' set up menu that way

    DefaultChart CEnv, cBar, cPlain         ' Get defaults for chart variable

    ClearData                               ' Clear all chart data
    
END SUB

'
' Sub Name: Initcolors
'
' Description: Creates color list based on screen mode
'
' Arguments: None
'
SUB InitColors
SHARED screenMode AS INTEGER
SHARED egacolor() AS INTEGER

    ' init EGA colors$ for SetAtt
    egacolor(0) = 0
    egacolor(1) = 1
    egacolor(2) = 2
    egacolor(3) = 3
    egacolor(4) = 4
    egacolor(5) = 5
    egacolor(6) = 20
    egacolor(7) = 7
    egacolor(8) = 56
    egacolor(9) = 57
    egacolor(10) = 58
    egacolor(11) = 59
    egacolor(12) = 60
    egacolor(13) = 61
    egacolor(14) = 62
    egacolor(15) = 63

    ' create list of displayable colors$ based on screen mode
    SELECT CASE screenMode
        CASE 1
            numColors = 4
            REDIM color$(numColors)
            colors$(1) = "Black"
            colors$(2) = "White"
            colors$(3) = "Bright Cyan"
            colors$(4) = "Bright Magenta"
        CASE 2, 3, 4, 11
            numColors = 2
            REDIM color$(numColors)
            colors$(1) = "Black"
            colors$(2) = "White"
        CASE 7, 8, 9, 12, 13
            numColors = 16
            REDIM color$(numColors)
            colors$(1) = "Black"
            colors$(2) = "High White"
            colors$(3) = "Blue"
            colors$(4) = "Green"
            colors$(5) = "Cyan"
            colors$(6) = "Red"
            colors$(7) = "Magenta"
            colors$(8) = "Brown"
            colors$(9) = "White"
            colors$(10) = "Gray"
            colors$(11) = "Bright Blue"
            colors$(12) = "Bright Green"
            colors$(13) = "Bright Cyan"
            colors$(14) = "Bright Red"
            colors$(15) = "Bright Magenta"
            colors$(16) = "Yellow"
        CASE 10
            numColors = 4
            REDIM color$(numColors)
            colors$(1) = "Off"
            colors$(2) = "On High"
            colors$(3) = "On Normal"
            colors$(4) = "Blink"
    END SELECT

    ' reset chart color pointers to default values
    IF numColors < 16 THEN
        CEnv.ChartWindow.Background = 0
        CEnv.ChartWindow.BorderColor = 1
        CEnv.DataWindow.Background = 0
        CEnv.DataWindow.BorderColor = 1
        CEnv.MainTitle.TitleColor = 1
        CEnv.SubTitle.TitleColor = 1
        CEnv.XAxis.AxisColor = 1
        CEnv.XAxis.AxisTitle.TitleColor = 1
        CEnv.YAxis.AxisColor = 1
        CEnv.YAxis.AxisTitle.TitleColor = 1
        CEnv.Legend.TextColor = 1
        CEnv.Legend.LegendWindow.Background = 0
        CEnv.Legend.LegendWindow.BorderColor = 1
    END IF
END SUB

'
' Sub Name: InitFonts
'
' Description: sets up default font and initializes font list
'
' Arguments: None
'
SUB InitFonts
DIM FI AS FontInfo

    ' reset
    UnRegisterFonts
    SetMaxFonts 1, 1

    ' get default font
    DefaultFont Segment%, Offset%
    reg% = RegisterMemFont%(Segment%, Offset%)

    ' load default font
    numFonts = LoadFont("n1")
 
    IF numFonts = 0 THEN numFonts = 1

    fonts$(numFonts) = "IBM 8 Point"

    UnRegisterFonts
END SUB

'
' Sub Name: InitStyles
'
' Description: Initializes border styles list
'
' Arguments: None
'
SUB InitStyles

    ' create list of border styles
    styles$(1) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    styles$(2) = "ÄÄÄÄ    ÄÄÄÄ        "
    styles$(3) = "ÄÄÄÄ         ÄÄ      "
    styles$(4) = "ÄÄ  ÄÄ  ÄÄ      ÄÄ  "
    styles$(5) = "ÄÄ  Ä   ÄÄ  Ä       "
    styles$(6) = "ÄÄÄ ÄÄÄ ÄÄÄ ÄÄ    Ä "
    styles$(7) = "ÄÄÄ Ä Ä ÄÄÄ Ä     Ä "
    styles$(8) = "ÄÄÄÄ ÄÄ ÄÄ ÄÄÄÄ "
    styles$(9) = "ÄÄÄÄ ÄÄ ÄÄÄÄ ÄÄ "
    styles$(10) = "ÄÄÄÄ Ä Ä ÄÄ Ä     Ä "
    styles$(11) = "ÄÄ  ÄÄÄ Ä    Ä  ÄÄÄ "
    styles$(12) = "Ä Ä Ä   Ä Ä Ä       "
    styles$(13) = "Ä Ä Ä Ä Ä Ä Ä     Ä "
    styles$(14) = "ÄÄÄ  Ä  ÄÄÄ  Ä      "
    styles$(15) = "ÄÄ  Ä   Ä   Ä    Ä  "

END SUB

'
' Func Name: Min
'
' Description: Compares two numbers and returns the smallest
'
' Arguments: num1, num2 - numbers to compare
'
FUNCTION Min% (num1, num2)

    IF num1 <= num2 THEN
        Min% = num1
    ELSE
        Min% = num2
    END IF

END FUNCTION

'
' Sub Name: Quit
'
' Description: Exits the program after allowing the user a chance to
'              save the current chart
'
' Arguments: None
'
SUB Quit
SHARED finished AS INTEGER, saveFile$, origPath$

    ' Allow user to save chart if necessary
    IF chartChanged THEN
        a$ = "| " + "Current chart has not been saved.  Save now?"

        status = Alert(4, a$, 8, 15, 12, 65, "Yes", "No", "Cancel")

        ' save chart
        IF status = OK THEN
            status = SaveChart(saveFile$, FALSE)
        END IF
    ELSE
        status = OK
    END IF

    ' quit if operation has not been canceled.
    IF status <> CANCEL THEN
        CHDRIVE MID$(origPath$, 1, 2)
        CHDIR MID$(origPath$, 3, LEN(origPath$))
        finished = TRUE
        MouseHide
        COLOR 15, 0
        CLS
    END IF

END SUB

'
' Sub Name: ScrollList
'
' Description: Handles scrolling for a list box.
'
' Arguments: text$() - list
'            tbox - list box
'            currButton - current button
'            status - to determine if button was pressed, or up or down arrow
'                     keys were used
'            func - for special operations (passed to DrawList)
'            winRow - top row of current window
'            winCol - left column of current window
'
SUB ScrollList (text$(), tbox AS ListBox, currButton, status, func, winRow, winCol)

    ' scroll using scroll buttons
    IF currButton = tbox.scrollButton AND status = 1 THEN
        SELECT CASE Dialog(19)
            ' scroll up
            CASE -1:
                IF tbox.currTop > 1 THEN
                    tbox.currTop = tbox.currTop - 1
                    tbox.currPos = tbox.currPos + 1
                    IF tbox.currPos > tbox.maxLen THEN tbox.currPos = tbox.maxLen
                END IF
            ' scroll down
            CASE -2:
                IF tbox.currTop + tbox.maxLen <= tbox.listLen THEN
                    tbox.currTop = tbox.currTop + 1
                    tbox.currPos = tbox.currPos - 1
                    IF tbox.currPos < 1 THEN tbox.currPos = 1
                END IF
            ' scroll to position
            CASE ELSE:
                position = Dialog(19)
                IF position > 1 THEN
                    position = position * (tbox.listLen) / (tbox.boxLen - 2)
                    IF position < 1 THEN
                        positon = 1
                    ELSEIF position > tbox.listLen THEN
                        position = tbox.listLen
                    END IF
                END IF

                IF tbox.currTop <= position AND tbox.currTop + tbox.maxLen > position THEN
                    tbox.currPos = position - tbox.currTop + 1
                ELSEIF position <= tbox.maxLen THEN
                    tbox.currTop = 1
                    tbox.currPos = position
                ELSE
                    tbox.currTop = position - tbox.maxLen + 1
                    tbox.currPos = position - tbox.currTop + 1
                END IF
        END SELECT

    ' area button chosen
    ELSEIF status = 1 THEN
        ' make selected position the current position
        IF Dialog(17) <= tbox.maxLen THEN
            tbox.currPos = Dialog(17)
            DrawList text$(), tbox, func
        END IF

        ' poll for repeated scrolling while mouse button is down
        DO
            X! = TIMER
            MousePoll r, c, lb, rb              ' poll mouse
            IF lb = TRUE THEN
                ' if below list box then scroll down
                IF r > tbox.botRow + winRow - 2 THEN
                    GOSUB Down1
                ' if above list box then scroll up
                ELSEIF r < tbox.topRow + winRow THEN
                    GOSUB Up1
                ' if to right of list box then scroll down
                ELSEIF c > tbox.rightCol + winCol - 2 THEN
                    GOSUB Down1
                ' if to left of list box then scroll up
                ELSEIF c < tbox.leftCol + winCol THEN
                    GOSUB Up1
                ' inside box
                ELSEIF r - winRow - tbox.topRow + 1 <= tbox.maxLen THEN
                    tbox.currPos = r - winRow - tbox.topRow + 1
                END IF
                
                ' draw list
                DrawList text$(), tbox, func
            ELSE
                EXIT DO
            END IF
            WHILE TIMER < X! + .05: WEND
        LOOP

    ' up arrow key hit
    ELSEIF status = 2 THEN
        GOSUB Up1

    ' down arrow key hit
    ELSEIF status = 3 THEN
        GOSUB Down1
    END IF

    DrawList text$(), tbox, func                    ' redraw list

    EXIT SUB

' scroll list up one
Up1:
    IF tbox.currPos > 1 THEN
        tbox.currPos = tbox.currPos - 1
    ELSEIF tbox.currTop > 1 THEN
        tbox.currTop = tbox.currTop - 1
    END IF
RETURN

' scroll list down one
Down1:
    IF tbox.currPos < tbox.maxLen THEN
        tbox.currPos = tbox.currPos + 1
    ELSEIF tbox.currTop + tbox.maxLen <= tbox.listLen THEN
        tbox.currTop = tbox.currTop + 1
    END IF
RETURN

END SUB

'
' Sub Name: Setatt
'
' Description: Changes a color's attribute to that of another color's.
'              This is used in the ChangeTitle routine to allow user
'              color selections to immediately change the foreground
'              color of the title edit field.  This allows the user
'              to view the colors as they would look on a chart
'
' Arguments: change - color to change
'            source - color to change to
'
SUB SetAtt (change, source)
SHARED screenMode AS INTEGER
SHARED egacolor() AS INTEGER

    ' map colors$ based on screen mode
    SELECT CASE screenMode
        CASE 10:
            IF source > 2 THEN
                temp = 9                            ' set "normal" and "blink" to white
            ELSE
                temp = source                       ' off = black; high white = bright white
            END IF
        CASE 1:
            IF source = 3 THEN                      ' map to cyan
                temp = 13
            ELSEIF source = 4 THEN                  ' map to magenta
                temp = 15
            ELSE                                    ' others okay
                temp = source
            END IF
        CASE ELSE
            temp = source                           ' colors$ okay
    END SELECT

    ' change attribute
    DIM regs AS RegType
    regs.ax = &H1000
    regs.bx = 256 * egacolor(TrueColr(temp)) + change
    CALL INTERRUPT(&H10, regs, regs)

END SUB

'
' Sub Name: SetDisplayColor
'
' Description: Changes the program's display to monochrome (no colors) or
'              to color (include colors in menu bar) based on the value of
'              colorDisplay.
'
' Arguments: none
'
SUB SetDisplayColor
SHARED colorDisplay AS INTEGER

    MouseHide

    ' redraw background based on display color
    SetUpBackground

    ' set menu bar to include colors
    IF colorDisplay THEN
        MenuSetState OPTIONSTITLE, 1, 2
        MenuSetState OPTIONSTITLE, 2, 1
        MenuColor 0, 7, 4, 8, 0, 4, 7
    ' set monochrome menu bar
    ELSE
        MenuSetState OPTIONSTITLE, 1, 1
        MenuSetState OPTIONSTITLE, 2, 2
        MenuColor 0, 7, 15, 8, 7, 0, 15
    END IF

    MenuShow
    MouseShow

END SUB

'
' Sub Name: SetUpBackground
'
' Description: Creates and displays background screen pattern
'
' Arguments: none
'
SUB SetUpBackground
SHARED colorDisplay AS INTEGER

    MouseHide

    WIDTH , 25
    IF colorDisplay THEN
        COLOR 15, 1                             ' set color for background
    ELSE
        COLOR 15, 0
    END IF
    CLS

    FOR a = 2 TO 80 STEP 4                      ' create and display pattern
        FOR b = 2 TO 25 STEP 2
            LOCATE b, a
            PRINT CHR$(250);
        NEXT b
    NEXT a

    MouseShow

END SUB

'
' Sub Name: SetUpMenu
'
' Description: Creates menu bar for the program
'
' Arguments: none
'
SUB SetUpMenu

    ' file menu title
    MenuSet FILETITLE, 0, 1, "File", 1
    MenuSet FILETITLE, 1, 1, "New", 1
    MenuSet FILETITLE, 2, 1, "Open ...", 1
    MenuSet FILETITLE, 3, 1, "Save", 1
    MenuSet FILETITLE, 4, 1, "Save As ...", 6
    MenuSet FILETITLE, 5, 1, "-", 1
    MenuSet FILETITLE, 6, 1, "Exit", 2

    ' view menu title
    MenuSet VIEWTITLE, 0, 1, "View", 1
    MenuSet VIEWTITLE, 1, 1, "Data ...", 1
    MenuSet VIEWTITLE, 2, 1, "Chart        F5", 1
    MenuSet VIEWTITLE, 3, 1, "Fonts ...", 1
    MenuSet VIEWTITLE, 4, 1, "Screen Mode ...", 1

    ' gallery menu title
    MenuSet GALLERYTITLE, 0, 1, "Gallery", 1
    MenuSet GALLERYTITLE, 1, 1, "Bar ...", 1
    MenuSet GALLERYTITLE, 2, 1, "Column ...", 1
    MenuSet GALLERYTITLE, 3, 1, "Line ...", 1
    MenuSet GALLERYTITLE, 4, 1, "Scatter ...", 1
    MenuSet GALLERYTITLE, 5, 1, "Pie ...", 1

    ' chart menu title
    MenuSet CHARTTITLE, 0, 1, "Chart", 1
    MenuSet CHARTTITLE, 1, 1, "Chart Window ...", 1
    MenuSet CHARTTITLE, 2, 1, "Data Window ...", 1
    MenuSet CHARTTITLE, 3, 1, "Legend ...", 1
    MenuSet CHARTTITLE, 4, 1, "X Axis ...", 1
    MenuSet CHARTTITLE, 5, 1, "Y Axis ...", 1

    ' title menu title
    MenuSet TITLETITLE, 0, 1, "Title", 1
    MenuSet TITLETITLE, 1, 1, "Main ...", 1
    MenuSet TITLETITLE, 2, 1, "Sub ...", 1
    MenuSet TITLETITLE, 3, 1, "X Axis ...", 1
    MenuSet TITLETITLE, 4, 1, "Y Axis ...", 1

    ' options menu title
    MenuSet OPTIONSTITLE, 0, 1, "Options", 1
    MenuSet OPTIONSTITLE, 1, 1, "Color", 1
    MenuSet OPTIONSTITLE, 2, 1, "Monochrome", 1

    ' setup short cuts for some menu choices
    ShortCutKeySet VIEWTITLE, 2, CHR$(0) + CHR$(63)     ' F5 = View Chart

    ' set original menu colors for monochrome screen
    MenuColor 0, 7, 15, 8, 7, 0, 15
    MenuPreProcess

END SUB

'
' Function Name: TrueColr
'
' Description: Maps a given chart color to its actual color
'              and returns this color.  This is needed because the chart
'              colors start with BLACK = 1 and HIGH WHITE = 2
'
' Arguments: colr - chart color number
'
FUNCTION TrueColr% (colr)

    IF colr = 1 THEN                                ' black
        TrueColr% = 0                               ' bright white
    ELSEIF colr = 2 THEN
        TrueColr% = 15
    ELSE
        TrueColr% = colr - 2                        ' all others
    END IF

END FUNCTION

'
' Sub Name: ViewChart
'
' Description: Displays the chart
'
' Arguments: none
'
SUB ViewChart
SHARED setVal!(), Cat$(), setLen() AS INTEGER, setName$()
SHARED screenMode AS INTEGER

    ' When a chart is drawn, data is moved from the 2-dimensional array
    ' into arrays suitable for the charting library routines.  The
    ' following arrays are used directly in calls to the charting routines:
    DIM ValX1!(1 TO cMaxValues)                    ' pass to chart routine
    DIM ValY1!(1 TO cMaxValues)
    DIM ValX2!(1 TO cMaxValues, 1 TO cMaxSeries)   ' pass to chartMS routine
    DIM ValY2!(1 TO cMaxValues, 1 TO cMaxSeries)

    DIM explode(1 TO cMaxValues)  AS INTEGER       ' explode pie chart pieces


   ' Make sure some data exists
   IF setNum <= 0 THEN
       a$ = "|"
       a$ = a$ + "No data available for chart."
       junk = Alert(4, a$, 8, 15, 12, 65, "", "", "")
       EXIT SUB
   END IF

   ' find the longest series
   maxLen% = 0
   FOR i% = 1 TO setNum
      IF setLen(i%) > maxLen% THEN maxLen% = setLen(i%)
   NEXT i%

   ' Set up the proper screen mode (exit if not valid)
   ChartScreen screenMode
   IF ChartErr = cBadScreen THEN
        PrintError "Invalid screen mode. Can't display chart."
        EXIT SUB
   END IF

   ' Process depending on chart type
   SELECT CASE CEnv.ChartType
      CASE cBar, cColumn, cLine, cPie:
         ' If the chart is a single series one or a pie chart:
         IF setNum = 1 OR CEnv.ChartType = cPie THEN

            ' Transfer data into a single dimension array:
            FOR i% = 1 TO maxLen%
               ValX1!(i%) = setVal!(i%, 1)
            NEXT i%

            IF CEnv.ChartType = cPie THEN
                ' determine which pieces to explode
                FOR i% = 1 TO maxLen%
                    IF setVal!(i%, 2) <> 0 THEN
                        explode(i%) = 1
                    ELSE
                        explode(i%) = 0
                    END IF
                NEXT i%

                ' display pie chart
                ChartPie CEnv, Cat$(), ValX1!(), explode(), maxLen%
            ELSE
                Chart CEnv, Cat$(), ValX1!(), maxLen%
            END IF

         ' If multiple series, then data is OK so just call routine:
         ELSE
            ChartMS CEnv, Cat$(), setVal!(), maxLen%, 1, setNum, setName$()
         END IF

      CASE cScatter:
         ' Make sure there's enough data sets:
         IF setNum = 1 THEN
            SCREEN 0
            WIDTH 80
            SetUpBackground
            MenuShow
            MouseShow
            a$ = "|"
            a$ = a$ + "Too few data sets for Scatter chart"
            junk = Alert(4, a$, 8, 15, 12, 65, "", "", "")
            EXIT SUB

         ' If it's a single series scatter, transfer data to one-
         ' dimensional arrays and make chart call:
         ELSEIF setNum = 2 THEN
            FOR i% = 1 TO maxLen%
               ValX1!(i%) = setVal!(i%, 1)
               ValY1!(i%) = setVal!(i%, 2)
            NEXT i%
            ChartScatter CEnv, ValX1!(), ValY1!(), maxLen%

         ' If it's a multiple series scatter, transfer odd columns to
         ' X-axis data array and even columns to Y-axis array and make
         ' chart call:
         ELSE
            FOR j% = 2 TO setNum STEP 2
               FOR i% = 1 TO maxLen%
                  ValX2!(i%, j% \ 2) = setVal!(i%, j% - 1)
                  ValY2!(i%, j% \ 2) = setVal!(i%, j%)
               NEXT i%
            NEXT j%

            ChartScatterMS CEnv, ValX2!(), ValY2!(), maxLen%, 1, setNum \ 2, setName$()
         END IF

   END SELECT

   ' If there's been a "fatal" error, indicate what it was:
   IF ChartErr <> 0 THEN
       GOSUB ViewError

   ' Otherwise, just wait for a keypress:
   ELSE
      ' Wait for keypress
      DO
            c$ = INKEY$
            MousePoll r, c, lb, rb
      LOOP UNTIL c$ <> "" OR lb OR rb
      SCREEN 0
      WIDTH 80
      SetUpBackground
      MenuShow
      MouseShow
   END IF

EXIT SUB

' handle charting errors
ViewError:

    ' re-init the display
    SCREEN 0
    WIDTH 80
    SetUpBackground
    MenuShow
    MouseShow

    ' display appropriate error message
    SELECT CASE ChartErr
        CASE cBadDataWindow:
            PrintError "Data window cannot be displayed in available space."
        CASE cBadLegendWindow:
            PrintError "Invalid legend coordinates."
        CASE cTooFewSeries:
            PrintError "Too few series to plot."
        CASE cTooSmallN:
            PrintError "No data in series."
        CASE IS > 200:                              ' basic error
            PrintError "BASIC error #" + LTRIM$(STR$(ChartErr - 200)) + " occurred."
        CASE ELSE:                                  ' extraneous error
            PrintError "Charting error #" + LTRIM$(STR$(ChartErr)) + " occurred."
    END SELECT

RETURN

END SUB

'
' Sub Name: ViewFont
'
' Description: Displays list of registered fonts and allows user to
'              select one or more of these fonts to load
'
' Arguments: none
'
SUB ViewFont
SHARED screenMode AS INTEGER
SHARED origPath$
DIM FI AS FontInfo
DIM rfonts$(1 TO MAXFONTS)

    SetMaxFonts MAXFONTS, MAXFONTS

    ' get default font
    DefaultFont Segment%, Offset%
    numReg = RegisterMemFont%(Segment%, Offset%)

    ' use font files that are best suited for current screen mode
    IF MID$(origPath$, LEN(origPath$), 1) = "\" THEN
        t$ = ""
    ELSE
        t$ = "\"
    END IF
    SELECT CASE screenMode
        CASE 2, 8
            cour$ = origPath$ + t$ + "COURA.FON"
            helv$ = origPath$ + t$ + "HELVA.FON"
            tims$ = origPath$ + t$ + "TMSRA.FON"
        CASE 11, 12
            cour$ = origPath$ + t$ + "COURE.FON"
            helv$ = origPath$ + t$ + "HELVE.FON"
            tims$ = origPath$ + t$ + "TMSRE.FON"
        CASE ELSE
            cour$ = origPath$ + t$ + "COURB.FON"
            helv$ = origPath$ + t$ + "HELVB.FON"
            tims$ = origPath$ + t$ + "TMSRB.FON"
    END SELECT
    ' register courier fonts
    numReg = numReg + RegisterFonts%(cour$)
    fontname$ = cour$
    IF FontErr > 0 THEN GOSUB FontError

    ' register helvetica fonts
    numReg = numReg + RegisterFonts%(helv$)
    fontname$ = helv$
    IF FontErr > 0 THEN GOSUB FontError

    ' register times roman fonts
    numReg = numReg + RegisterFonts%(tims$)
    fontname$ = tims$
    IF FontErr > 0 THEN GOSUB FontError

    ' create a list of registered fonts
    FOR i = 1 TO numReg
        GetRFontInfo i, FI
        rfonts$(i) = RTRIM$(MID$(FI.FaceName, 1, 8)) + STR$(FI.Points) + " Point"
    NEXT i

    ' set up window display
    winRow = 5
    winCol = 25
    WindowOpen 1, winRow, winCol, winRow + numReg + 1, 51, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Chart Fonts"

    ' open buttons for each font in list
    FOR i% = 1 TO numReg
        ButtonOpen i, 1, rfonts$(i), i, 4, 0, 0, 2
        FOR j% = 1 TO numFonts
            IF fonts$(j%) = rfonts$(i%) THEN ButtonSetState i, 2
        NEXT j%
    NEXT i%

    WindowLine numReg + 1
    ButtonOpen numReg + 1, 2, "Load", numReg + 2, 4, 0, 0, 1
    ButtonOpen numReg + 2, 1, "Cancel ", numReg + 2, 15, 0, 0, 1

    ' start with cursor on first button
    currButton = 1
    pushButton = numReg + 1

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, 0
        SELECT CASE Dialog(0)
            CASE 1                                     ' button pressed
                currButton = Dialog(1)
                IF currButton > numReg THEN
                    pushButton = currButton
                    finished = TRUE
                ELSE
                    ButtonToggle currButton
                END IF
            CASE 6                                      ' enter
                finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE numReg, numReg + 1:
                        currButton = currButton + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE numReg + 2:
                        currButton = 1
                        ButtonSetState pushButton, 1
                        pushButton = numReg + 1
                        ButtonSetState pushButton, 2
                    CASE ELSE:
                        currButton = currButton + 1
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 1:
                        currButton = numReg + 2
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE numReg + 2:
                        currButton = numReg + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE ELSE:
                        currButton = currButton - 1
                    END SELECT
            CASE 9                                      ' escape
                pushButton = numReg + 2
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                IF currButton <= numReg THEN ButtonSetState currButton, 2
            CASE 11, 13                                 ' down, right arrow
                IF currButton <= numReg THEN ButtonSetState currButton, 1
            CASE 14                                     ' space bar
                IF currButton <= numReg THEN
                    ButtonToggle currButton
                ELSE
                    finished = TRUE
                END IF
        END SELECT

    ' finished and not cancelled
    IF finished AND pushButton = numReg + 1 THEN
        ' create font spec for load operation
        FontSpec$ = ""
        FOR i% = 1 TO numReg
            IF ButtonInquire(i) = 2 THEN
                FontSpec$ = FontSpec$ + "/n" + LTRIM$(STR$(i))
            END IF
        NEXT i%

        ' default if none chosen
        IF FontSpec$ = "" THEN
            PrintError "No fonts selected - using default."
            numFonts = LoadFont%("N1")
            REDIM fonts$(1)
            fonts$(1) = rfonts$(1)
        ELSE
            ' load selected fonts
            numLoaded = LoadFont%(RIGHT$(FontSpec$, LEN(FontSpec$) - 1))

            ' notify user of error and let them try again.
            IF FontErr <> 0 THEN
                GOSUB FontError
                finished = FALSE
                currButton = 1
            ELSE
                REDIM fonts$(numLoaded)
                ' create a list of loaded fonts
                FOR i = 1 TO numLoaded
                    SelectFont i
                    GetFontInfo FI
                    fonts$(i) = RTRIM$(MID$(FI.FaceName, 1, 8)) + STR$(FI.Points) + " Point"
                NEXT i
                numFonts = numLoaded
                ClearFonts
            END IF
        END IF
    ' reload existing fonts if operation cancelled
    ELSEIF finished = TRUE AND pushButton = numReg + 2 THEN
        FontSpec$ = ""
        FOR i = 1 TO numReg
            FOR j% = 1 TO numFonts
                IF fonts$(j%) = rfonts$(i%) THEN FontSpec$ = FontSpec$ + "/n" + LTRIM$(STR$(i))
            NEXT j%
        NEXT i
        numFonts = LoadFont%(RIGHT$(FontSpec$, LEN(FontSpec$) - 1))
    END IF

    WEND

    UnRegisterFonts

    WindowClose 1

    EXIT SUB

' handle font loading errors
FontError:
    SELECT CASE FontErr
        CASE cNoFontMem:
            PrintError "Not enough memory to load selected fonts."
        CASE cFileNotFound:
            PrintError fontname$ + " font file not found."
        CASE cTooManyFonts:
            numReg = MAXFONTS
        CASE cBadFontFile:
            PrintError "Invalid font file format for " + fontname$ + "."
        CASE cNoFonts:
            PrintError "No fonts are loaded."
        CASE cBadFontType:
            PrintError "Font not a bitmap font."
        CASE IS > 200:                                  ' basic error
            PrintError "BASIC error #" + LTRIM$(STR$(FontErr - 200)) + " occurred."
        CASE ELSE                                       ' unplanned font error
            PrintError "Font error #" + LTRIM$(STR$(FontErr)) + " occurred."
    END SELECT

RETURN

END SUB

'
' Sub Name: ViewScreenMode
'
' Description: Displays list of valid screen modes and allows the
'              user to select one for viewing the chart
'
' Arguments: none
'
SUB ViewScreenMode
SHARED screenMode AS INTEGER, numModes AS INTEGER, mode$()

DIM modeBox AS ListBox

    ' set up list box containing valid screen modes
    modeBox.scrollButton = 1
    modeBox.areaButton = 2
    modeBox.listLen = numModes
    modeBox.topRow = 1
    modeBox.botRow = numModes + 2
    modeBox.leftCol = 7
    modeBox.rightCol = 21

    ' determine current screen mode
    FOR i = 1 TO numModes
        IF screenMode = VAL(mode$(i)) THEN modeBox.listPos = i
    NEXT i

    ' set up display window
    winRow = 6
    winCol = 25
    WindowOpen 1, winRow, winCol, winRow + numModes + 3, 51, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Screen Mode"
    WindowLine numModes + 3

    ' create the list box
    CreateListBox mode$(), modeBox, 0

    ' open command buttons
    ButtonOpen 3, 2, "OK ", numModes + 4, 4, 0, 0, 1
    ButtonOpen 4, 1, "Cancel ", numModes + 4, 16, 0, 0, 1

    
        a$ = "Screen Mode Warning ||"
        a$ = a$ + "Selecting screen modes that support less than |"
        a$ = a$ + "than 16 colors will reset all chart colors to |"
        a$ = a$ + "their black and white defaults.               |"
        a$ = a$ + "|" + " Fonts should be reloaded after screen mode is   |"
        a$ = a$ + " changed to ensure best font match for screen   |"
        a$ = a$ + " resolution.                                     "
        junk = Alert(4, a$, 6, 15, 16, 65, "", "", "")
    

    ' start with cursor in area button
    currButton = 2
    pushButton = 3

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, 0                         ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                     ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2:
                        ScrollList mode$(), modeBox, currButton, 1, 0, winRow, winCol
                        currButton = 2
                    CASE 3, 4:
                        pushButton = currButton
                        finished = TRUE
                END SELECT
            CASE 6                                      ' enter
                finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 1, 2:
                        currButton = 3
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 3
                    CASE 3:
                        currButton = 4
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 4
                    CASE 4:
                        ButtonSetState currButton, 1
                        currButton = 2
                        pushButton = 3
                        ButtonSetState pushButton, 2
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 1, 2:
                        currButton = 4
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 4
                    CASE 3: currButton = 2
                    CASE 4:
                        currButton = 3
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 3
                    END SELECT
            CASE 9                                      ' escape
                pushButton = 4
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList mode$(), modeBox, currButton, 2, 0, winRow, winCol
                END SELECT
            CASE 11, 13                                 ' down, right arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList mode$(), modeBox, currButton, 3, 0, winRow, winCol
                END SELECT
            CASE 14                                     ' space bar
                IF currButton > 2 THEN finished = TRUE
        END SELECT
    WEND

    ' if not canceled
    IF pushButton = 3 THEN
        ' change screen mode
        IF screenMode <> VAL(mode$(modeBox.listPos)) THEN
            IF setNum > 0 THEN chartChanged = TRUE

            screenMode = VAL(mode$(modeBox.listPos))

            ' reset window coords
            CEnv.ChartWindow.X1 = 0
            CEnv.ChartWindow.Y1 = 0
            CEnv.ChartWindow.X2 = 0
            CEnv.ChartWindow.Y2 = 0

            ' change color list based on new screen mode
            InitColors
        END IF
    END IF

    WindowClose 1

END SUB

