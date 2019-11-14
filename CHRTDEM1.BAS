'       CHRTDEM1.BAS - second module of the CHRTB demonstration program.
'
'               Copyright (C) 1989-1990, Microsoft Corporation
'
'   Main module - CHRTDEMO.BAS
'   Include files - CHRTDEMO.BI
'
'$INCLUDE: 'chrtdemo.bi'

'local subs
DECLARE SUB ChangeStyle ()

DEFINT A-Z
'
' Sub Name: ChangeAxis
'
' Description: Allows user to view and change attributes of either
'              chart axis.
'
' Arguments: title$ - window title
'            axis - X or Y axis variable
'
SUB ChangeAxis (title$, axis AS AxisType)

    DIM colorBox AS ListBox
    DIM styleBox AS ListBox
    DIM fontBox AS ListBox

    ' set up color list box
    colorBox.scrollButton = 2
    colorBox.areaButton = 3
    colorBox.listLen = numColors
    colorBox.topRow = 3
    colorBox.botRow = 16
    colorBox.leftCol = 4
    colorBox.rightCol = 18
    colorBox.listPos = axis.AxisColor + 1

    ' set up border style list box
    styleBox.scrollButton = 5
    styleBox.areaButton = 6
    styleBox.listLen = MAXSTYLES
    styleBox.topRow = 5
    styleBox.botRow = 16
    styleBox.leftCol = 24
    styleBox.rightCol = 40
    styleBox.listPos = axis.GridStyle

    ' set up font list box
    fontBox.scrollButton = 8
    fontBox.areaButton = 9
    fontBox.listLen = numFonts
    fontBox.topRow = 5
    fontBox.botRow = 9
    fontBox.leftCol = 46
    fontBox.rightCol = 65
    fontBox.listPos = axis.TicFont

    ' open window for display
    winRow = 4
    winCol = 6
    WindowOpen 1, winRow, winCol, 22, 73, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, title$
    WindowBox 1, 2, 17, 20
    WindowLocate 2, 4
    WindowPrint 2, "Axis Color:"
    WindowBox 1, 22, 17, 42
    WindowLocate 4, 24
    WindowPrint 2, "Grid Style:"
    WindowBox 1, 44, 17, 67
    WindowLocate 4, 46
    WindowPrint 2, "Label Font:"
    WindowLocate 10, 46
    WindowPrint 2, "Range Type:"
    WindowBox 11, 46, 16, 65
    WindowLocate 14, 48
    WindowPrint 2, "Log Base:"
    WindowBox 13, 57, 15, 63
    WindowLine 18

    ' create list boxes
    CreateListBox colors$(), colorBox, 0
    CreateListBox styles$(), styleBox, 0
    CreateListBox fonts$(), fontBox, 0

    ' open control buttons
    ButtonOpen 4, 1, "Display Grid", 2, 24, 0, 0, 2
    ButtonOpen 7, 1, "Display Labels", 2, 46, 0, 0, 2
    ButtonOpen 10, 1, "Lin", 12, 48, 0, 0, 3
    ButtonOpen 11, 1, "Log", 12, 57, 0, 0, 3
    ButtonOpen 12, 2, "OK ", 19, 10, 0, 0, 1
    ButtonOpen 13, 1, "Cancel ", 19, 26, 0, 0, 1
    ButtonOpen 14, 1, "Axis Title ", 19, 46, 0, 0, 1

    ' edit field for log base
    EditFieldOpen 1, LTRIM$(STR$(axis.LogBase)), 14, 58, 0, 7, 5, 20

    
    currButton = 3                                      ' start with cursor on first button (Autoscale)
    currEditField = 0

    optionButton = axis.RangeType + 9                   ' set proper state for buttons
    ButtonToggle optionButton
    IF axis.Labeled THEN ButtonToggle 7
    IF axis.Grid THEN ButtonToggle 4

    pushButton = 12                                     ' active command button

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField              ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 4, 7
                        ButtonToggle currButton
                        currEditField = 0
                    CASE 10, 11
                        ButtonToggle optionButton
                        optionButton = currButton
                        ButtonToggle optionButton
                        currEditField = 0
                    CASE 2, 3
                        currEditField = 0
                        ScrollList colors$(), colorBox, currButton, 1, 0, winRow, winCol
                        currButton = 3
                    CASE 5, 6
                        currEditField = 0
                        ScrollList styles$(), styleBox, currButton, 1, 0, winRow, winCol
                        currButton = 6
                    CASE 8, 9
                        currEditField = 0
                        ScrollList fonts$(), fontBox, currButton, 1, 0, winRow, winCol
                        currButton = 9
                    CASE 12, 13
                        pushButton = currButton
                        finished = TRUE
                    CASE 14
                        currEditField = 0
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                        ChangeTitle 2, title$ + " Title", axis.AxisTitle, 6, 14
                END SELECT
            CASE 2                                      ' edit field
                currEditField = 1
                currButton = 0
            CASE 6                                      ' enter
                SELECT CASE pushButton
                    CASE 12, 13: finished = TRUE
                    CASE 14: ChangeTitle 2, title$ + " Title", axis.AxisTitle, 6, 14
                END SELECT
                currButton = pushButton
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 0:
                        currEditField = 0
                        currButton = 12
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 2, 3: currButton = 4
                    CASE 4: currButton = 6
                    CASE 5, 6: currButton = 7
                    CASE 7: currButton = 9
                    CASE 8, 9: currButton = optionButton
                    CASE 10, 11:
                        currButton = 0
                        currEditField = 1
                    CASE 12, 13:
                        currButton = currButton + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 14:
                        ButtonSetState currButton, 1
                        pushButton = 12
                        ButtonSetState pushButton, 2
                        currButton = 3
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 0:
                        currEditField = 0
                        currButton = optionButton
                    CASE 2, 3:
                        currButton = 14
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 4: currButton = 3
                    CASE 5, 6: currButton = 4
                    CASE 7: currButton = 6
                    CASE 8, 9: currButton = 7
                    CASE 10, 11: currButton = 9
                    CASE 12:
                        currButton = 0
                        currEditField = 1
                    CASE 13, 14:
                        currButton = currButton - 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                END SELECT
            CASE 9                                      ' escape
                pushButton = 13
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                SELECT CASE currButton
                    CASE 4, 7: ButtonSetState currButton, 2
                    CASE 2, 3: ScrollList colors$(), colorBox, currButton, 2, 0, winRow, winCol
                    CASE 5, 6: ScrollList styles$(), styleBox, currButton, 2, 0, winRow, winCol
                    CASE 8, 9: ScrollList fonts$(), fontBox, currButton, 2, 0, winRow, winCol
                    CASE 10, 11:
                        ButtonToggle currButton
                        currButton = 21 - currButton
                        optionButton = currButton
                        ButtonToggle optionButton
                END SELECT
            CASE 11, 13                                 ' down, right arrow
                SELECT CASE currButton
                    CASE 1, 4, 7: ButtonSetState currButton, 1
                    CASE 2, 3: ScrollList colors$(), colorBox, currButton, 3, 0, winRow, winCol
                    CASE 5, 6: ScrollList styles$(), styleBox, currButton, 3, 0, winRow, winCol
                    CASE 8, 9: ScrollList fonts$(), fontBox, currButton, 3, 0, winRow, winCol
                    CASE 10, 11:
                        ButtonToggle currButton
                        currButton = 21 - currButton
                        optionButton = currButton
                        ButtonToggle optionButton
                END SELECT
            CASE 14                                     ' space bar
                SELECT CASE currButton
                    CASE 1, 4, 7: ButtonToggle currButton
                    CASE 12, 13: finished = TRUE
                    CASE 14: ChangeTitle 2, title$ + " Title", axis.AxisTitle, 6, 14
                END SELECT
        END SELECT

        ' error checking on log base before exiting
        IF finished AND pushButton = 12 THEN
            IF VAL(EditFieldInquire(1)) <= 0 THEN
                PrintError " Log base must be greater than zero."
                currEditField = 1
                currButton = 0
                finished = FALSE
            ELSEIF VAL(EditFieldInquire(1)) = 1 THEN
                PrintError " Log base cannot equal one. Overflow results."
                currEditField = 1
                currButton = 0
                finished = FALSE
            END IF
        END IF
    WEND

    ' if not canceled then assign and return new values
    IF pushButton = 12 THEN
        IF setNum > 0 THEN chartChanged = TRUE

        axis.LogBase = VAL(EditFieldInquire(1))
        axis.Grid = (ButtonInquire(4) = 2)
        axis.Labeled = (ButtonInquire(7) = 2)
        axis.RangeType = optionButton - 9
        axis.AxisColor = colorBox.listPos - 1
        axis.ScaleTitle.TitleColor = axis.AxisTitle.TitleColor
        axis.ScaleTitle.Justify = axis.AxisTitle.Justify
        axis.GridStyle = styleBox.listPos
        axis.TicFont = fontBox.listPos
    END IF

    WindowClose 1

END SUB

'
' Sub Name: ChangeChartType
'
' Description: Changes chart type based on menu selection and
'              allows the user access to changing the chart style.
'
' Arguments: ctype - new chart type
'
SUB ChangeChartType (ctype)

    'change type if user selected a different type
    IF CEnv.ChartType <> ctype THEN
        IF setNum > 0 THEN chartChanged = TRUE

        ' reset chosen type
        MenuItemToggle GALLERYTITLE, CEnv.ChartType
        ' reset other affected menu items
        IF CEnv.ChartType = cPie THEN
            MenuSetState CHARTTITLE, 4, 1
            MenuSetState CHARTTITLE, 5, 1
            MenuSetState TITLETITLE, 3, 1
            MenuSetState TITLETITLE, 4, 1
        END IF

        CEnv.ChartType = ctype

        'if new type is pie then turn off some items
        IF CEnv.ChartType = cPie THEN
            MenuSetState CHARTTITLE, 4, 0
            MenuSetState CHARTTITLE, 5, 0
            MenuSetState TITLETITLE, 3, 0
            MenuSetState TITLETITLE, 4, 0
        END IF

        ' set type in menu bar
        MenuItemToggle GALLERYTITLE, CEnv.ChartType
    END IF

    ' allow user to change chart style
    ChangeStyle

END SUB

'
' Sub Name: ChangeLegend
'
' Description: Allows user to view and modify all attributes of the chart
'              legend
'
' Arguments: none
'
SUB ChangeLegend

    DIM fgColorBox AS ListBox
    DIM fontBox AS ListBox

    ' set up foreground color box
    fgColorBox.scrollButton = 6
    fgColorBox.areaButton = 7
    fgColorBox.listLen = numColors
    fgColorBox.topRow = 3
    fgColorBox.botRow = 10
    fgColorBox.leftCol = 27
    fgColorBox.rightCol = 41
    fgColorBox.listPos = CEnv.Legend.TextColor + 1

    ' set up font box
    fontBox.scrollButton = 8
    fontBox.areaButton = 9
    fontBox.listLen = numFonts
    fontBox.topRow = 3
    fontBox.botRow = 10
    fontBox.leftCol = 43
    fontBox.rightCol = 57
    fontBox.listPos = CEnv.Legend.TextFont

    ' set up display window
    winRow = 6
    winCol = 10
    WindowOpen 1, winRow, winCol, 18, 69, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Chart Legend"
    WindowBox 1, 2, 11, 23
    WindowLocate 5, 4
    WindowPrint 2, "Location:"
    WindowBox 6, 4, 10, 21
    WindowBox 1, 25, 11, 59
    WindowLocate 2, 27
    WindowPrint 2, "Text Color:"
    WindowLocate 2, 43
    WindowPrint 2, "Text Font:"
    WindowLine 12

    ' create list boxes
    CreateListBox colors$(), fgColorBox, 0
    CreateListBox fonts$(), fontBox, 0

    ' open command buttons
    ButtonOpen 1, 1, "Display Legend", 2, 4, 0, 0, 2
    ButtonOpen 2, 1, "Autosize", 3, 4, 0, 0, 2
    ButtonOpen 3, 1, "Overlay", 7, 6, 0, 0, 3
    ButtonOpen 4, 1, "Bottom", 8, 6, 0, 0, 3
    ButtonOpen 5, 1, "Right", 9, 6, 0, 0, 3
    ButtonOpen 10, 2, "OK ", 13, 8, 0, 0, 1
    ButtonOpen 11, 1, "Cancel ", 13, 21, 0, 0, 1
    ButtonOpen 12, 1, "Legend Window ", 13, 38, 0, 0, 1

    currButton = 1                                      ' start with cursor on first button

    ' set button states based on current values
    optionButton = CEnv.Legend.Place + 2
    ButtonToggle optionButton
    IF CEnv.Legend.Legend THEN ButtonToggle 1
    IF CEnv.Legend.AutoSize THEN ButtonToggle 2
    pushButton = 10

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, 0                          ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2: ButtonToggle currButton
                    CASE 3, 4, 5
                        ButtonToggle optionButton
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 6, 7:
                        ScrollList colors$(), fgColorBox, currButton, 1, 0, winRow, winCol
                        currButton = 7
                    CASE 8, 9:
                        ScrollList fonts$(), fontBox, currButton, 1, 0, winRow, winCol
                        currButton = 9
                    CASE 10, 11
                        pushButton = currButton
                        finished = TRUE
                    CASE 12
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 12
                        ChangeWindow 2, "Legend Window", CEnv.Legend.LegendWindow
                END SELECT
            CASE 6                                      ' enter
                IF pushButton <> 12 THEN
                    finished = TRUE
                ELSE
                    ChangeWindow 2, "Legend Window", CEnv.Legend.LegendWindow
                END IF
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 1: currButton = 2
                    CASE 2: currButton = optionButton
                    CASE 3, 4, 5: currButton = 7
                    CASE 6, 7: currButton = 9
                    CASE 8, 9:
                        currButton = 10
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 10, 11:
                        currButton = currButton + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 12:
                        ButtonSetState currButton, 1
                        pushButton = 10
                        ButtonSetState pushButton, 2
                        currButton = 1
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 1:
                        currButton = 12
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 2: currButton = 1
                    CASE 3, 4, 5: currButton = 2
                    CASE 6, 7: currButton = optionButton
                    CASE 8, 9: currButton = 7
                    CASE 10: currButton = 9
                    CASE 11, 12:
                        currButton = currButton - 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                END SELECT
            CASE 9                                      ' escape
                pushButton = 11
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                SELECT CASE currButton
                    CASE 1, 2: ButtonSetState currButton, 2
                    CASE 3:
                        ButtonToggle currButton
                        currButton = 5
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 4, 5:
                        ButtonToggle currButton
                        currButton = currButton - 1
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 6, 7: ScrollList colors$(), fgColorBox, currButton, 2, 0, winRow, winCol
                    CASE 8, 9: ScrollList fonts$(), fontBox, currButton, 2, 0, winRow, winCol
                END SELECT
            CASE 11, 13                                 ' down, right arrow
                SELECT CASE currButton
                    CASE 1, 2: ButtonSetState currButton, 1
                    CASE 3, 4:
                        ButtonToggle currButton
                        currButton = currButton + 1
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 5:
                        ButtonToggle currButton
                        currButton = 3
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 6, 7: ScrollList colors$(), fgColorBox, currButton, 3, 0, winRow, winCol
                    CASE 8, 9: ScrollList fonts$(), fontBox, currButton, 3, 0, winRow, winCol
                END SELECT
            CASE 14                                     ' space bar
                SELECT CASE currButton
                    CASE 1, 2: ButtonToggle currButton
                    CASE 10, 11: finished = TRUE
                    CASE 12: ChangeWindow 2, "Legend Window", CEnv.Legend.LegendWindow
               END SELECT
        END SELECT
    WEND

    ' if not canceled then return the new values
    IF pushButton = 10 THEN
        IF setNum > 0 THEN chartChanged = TRUE

        CEnv.Legend.TextColor = fgColorBox.listPos - 1
        CEnv.Legend.TextFont = fontBox.listPos
        CEnv.Legend.AutoSize = (ButtonInquire(2) = 2)
        CEnv.Legend.Legend = (ButtonInquire(1) = 2)
        CEnv.Legend.Place = optionButton - 2
    END IF

    WindowClose 1

END SUB

'
' Sub Name: ChangeStyle
'
' Description: Allows user to view and modify the chart style
'
' Arguments: none
'
SUB ChangeStyle
DIM fontBox AS ListBox

    ' determine button labels based on chart type
    SELECT CASE CEnv.ChartType
        CASE cBar, cColumn
            style1$ = "Adjacent"
            style2$ = "Stacked"
        CASE cLine, cScatter
            style1$ = "Lines"
            style2$ = "No Lines"
        CASE cPie
            style1$ = "Percentages"
            style2$ = "No Percentages"
    END SELECT

    topRow = 8
    leftCol = 26
    ' if pie, line or scatter chart then add data font
    IF CEnv.ChartType > 2 THEN
        WindowOpen 1, topRow, leftCol, 19, 47, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Chart Style"
        okLine = 12

        WindowLocate 5, 3
        WindowPrint -2, "Data Font:"
        ' set up list box containing valid fonts
        fontBox.scrollButton = 3
        fontBox.areaButton = 4
        fontBox.listLen = numFonts
        fontBox.topRow = 6
        fontBox.botRow = 10
        fontBox.leftCol = 3
        fontBox.rightCol = 20
        fontBox.listPos = CEnv.DataFont
        CreateListBox fonts$(), fontBox, 0
    ELSE
        WindowOpen 1, topRow, leftCol, 13, 47, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, "Chart Style"
        okLine = 6
    END IF

    ' open buttons
    ButtonOpen 1, 1, style1$, 2, 3, 1, 0, 3
    ButtonOpen 2, 1, style2$, 3, 3, 1, 0, 3
    WindowLine okLine - 1
    ButtonOpen 5, 2, "OK", okLine, 3, 1, 0, 1
    ButtonOpen 6, 1, "Cancel", okLine, 11, 1, 0, 1

    pushButton = 5
    optionButton = CEnv.ChartStyle                     ' set current style
    currButton = optionButton
    ButtonSetState optionButton, 2

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, 0                          ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      'button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2:
                        ButtonSetState optionButton, 1
                        optionButton = currButton
                        ButtonSetState optionButton, 2
                    CASE 3, 4:
                        ScrollList fonts$(), fontBox, currButton, 1, 0, topRow, leftCol
                        currButton = 4
                    CASE 5, 6:
                        finished = TRUE
                END SELECT
            CASE 6                                      'enter
                finished = TRUE
            CASE 7                                      'tab
                SELECT CASE currButton
                    CASE 1, 2:
                        IF CEnv.ChartType > 2 THEN
                            currButton = 4
                        ELSE
                            currButton = 5
                            ButtonSetState pushButton, 1
                            pushButton = currButton
                            ButtonSetState pushButton, 2
                        END IF
                    CASE 3, 4:
                        currButton = 5
                        ButtonSetState pushButton, 1
                        pushButton = currButton
                        ButtonSetState currButton, 2
                    CASE 5:
                        currButton = 6
                        ButtonSetState pushButton, 1
                        pushButton = currButton
                        ButtonSetState currButton, 2
                    CASE 6:
                        currButton = optionButton
                        ButtonSetState pushButton, 1
                        pushButton = 5
                        ButtonSetState pushButton, 2
                END SELECT
            CASE 8                                      'back tab
                SELECT CASE currButton
                    CASE 1, 2:
                        currButton = 6
                        ButtonSetState pushButton, 1
                        pushButton = currButton
                        ButtonSetState pushButton, 2
                    CASE 3, 4:
                        currButton = optionButton
                    CASE 5:
                        IF CEnv.ChartType > 2 THEN
                            currButton = 4
                        ELSE
                            currButton = optionButton
                        END IF
                    CASE 6:
                        currButton = 5
                        ButtonSetState pushButton, 1
                        pushButton = currButton
                        ButtonSetState currButton, 2
                END SELECT
            CASE 9                                      'escape
                finished = TRUE
                pushButton = 5
            CASE 10, 12                                 'up, left arrow
                SELECT CASE currButton
                    CASE 1, 2:
                        ButtonSetState currButton, 1
                        currButton = 3 - currButton
                        optionButton = currButton
                        ButtonSetState currButton, 2
                    CASE 3, 4:
                        ScrollList fonts$(), fontBox, currButton, 2, 0, topRow, leftCol
                END SELECT
            CASE 11, 13                                 'down, right arrow
                SELECT CASE currButton
                    CASE 1, 2:
                        ButtonSetState currButton, 1
                        currButton = 3 - currButton
                        optionButton = currButton
                        ButtonSetState currButton, 2
                    CASE 3, 4:
                        ScrollList fonts$(), fontBox, currButton, 3, 0, topRow, leftCol
                END SELECT
            CASE 14                                     'space bar
                IF currButton > 4 THEN finished = TRUE
        END SELECT
    WEND

    ' if not canceled then set new chart style
    IF pushButton = 5 THEN
        IF setNum > 0 THEN chartChanged = TRUE
        CEnv.ChartStyle = optionButton
        IF CEnv.ChartType > 2 THEN CEnv.DataFont = fontBox.listPos
    END IF

    WindowClose 1

END SUB

'
' Sub Name: ChangeTitle
'
' Description: Allows user to view and modify the chart titles
'
' Arguments: handle - window number
'            wTitle$ - window title
'            title -  chart title
'            topRow - top row of window
'            leftCol - left column of window
'
SUB ChangeTitle (handle, wTitle$, title AS TitleType, topRow, leftCol)
SHARED mode$(), numModes AS INTEGER

    DIM colorBox AS ListBox
    DIM fontBox AS ListBox

    ' set up foreground color box
    colorBox.scrollButton = 1
    colorBox.areaButton = 2
    colorBox.listLen = numColors
    colorBox.topRow = 6
    colorBox.botRow = 10
    colorBox.leftCol = 2
    colorBox.rightCol = 16
    colorBox.listPos = title.TitleColor + 1

    ' set up font box
    fontBox.scrollButton = 3
    fontBox.areaButton = 4
    fontBox.listLen = numFonts
    fontBox.topRow = 6
    fontBox.botRow = 10
    fontBox.leftCol = 18
    fontBox.rightCol = 36
    fontBox.listPos = title.TitleFont

    ' set up display window
    WindowOpen handle, topRow, leftCol, topRow + 11, leftCol + 50, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, wTitle$
    WindowLocate 2, 2
    WindowPrint 2, "Title:"
    WindowBox 1, 8, 3, 50
    WindowBox 6, 38, 10, 50
    WindowLine 4
    WindowLine 11
    WindowLocate 5, 1
    WindowPrint -1, " Color:          Font:               Justify:"

    ' set color attribute for title editfield background to that of the chart background
    IF mode$(1) = "10" OR (mode$(1) = "2" AND mode$(2) <> "1") OR mode$(1) = "3" THEN
        func = 0
        EditFieldOpen 1, RTRIM$(title.title), 2, 9, 0, 7, 41, 70
    ELSE
        SetAtt 5, CEnv.ChartWindow.Background + 1
        EditFieldOpen 1, RTRIM$(title.title), 2, 9, 12, 5, 41, 70
        func = 2
    END IF

    ' create list boxes
    CreateListBox colors$(), colorBox, func
    CreateListBox fonts$(), fontBox, 0

    ' open buttons
    ButtonOpen 5, 1, "Left", 7, 39, 0, 0, 3
    ButtonOpen 6, 1, "Center", 8, 39, 0, 0, 3
    ButtonOpen 7, 1, "Right", 9, 39, 0, 0, 3
    ButtonOpen 8, 2, "OK ", 12, 10, 0, 0, 1
    ButtonOpen 9, 1, "Cancel ", 12, 33, 0, 0, 1

    currButton = 0                                      ' start in edit field
    currEditField = 1
    optionButton = 4 + title.Justify                    ' set button state
    ButtonToggle optionButton
    pushButton = 8

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField              ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2
                        currEditField = 0
                        ScrollList colors$(), colorBox, currButton, 1, func, topRow, leftCol
                        currButton = 2
                    CASE 3, 4
                        currEditField = 0
                        ScrollList fonts$(), fontBox, currButton, 1, 0, topRow, leftCol
                        currButton = 4
                    CASE 5, 6, 7
                        ButtonToggle optionButton
                        optionButton = currButton
                        ButtonToggle optionButton
                        currEditField = 0
                    CASE 8, 9
                        pushButton = currButton
                        finished = TRUE
                END SELECT
            CASE 2                                      ' edit field
                currButton = 0
                currEditField = 1
            CASE 6                                      ' enter
                finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 0:
                        currButton = 2
                        currEditField = 0
                    CASE 1, 2: currButton = 4
                    CASE 3, 4: currButton = optionButton
                    CASE 5, 6, 7:
                        currButton = 8
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 8
                    CASE 8:
                        currButton = currButton + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 9:
                        ButtonSetState currButton, 1
                        pushButton = 8
                        ButtonSetState pushButton, 2
                        currButton = 0
                        currEditField = 1
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 0:
                        currButton = 9
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = 9
                        currEditField = 0
                    CASE 1, 2:
                        currButton = 0
                        currEditField = 1
                    CASE 3, 4: currButton = 2
                    CASE 5, 6, 7: currButton = 4
                    CASE 8: currButton = optionButton
                    CASE 9:
                        currButton = currButton - 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                END SELECT
            CASE 9                                      ' escape
                pushButton = 9
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList colors$(), colorBox, currButton, 2, func, topRow, leftCol
                    CASE 3, 4: ScrollList fonts$(), fontBox, currButton, 2, 0, topRow, leftCol
                    CASE 5:
                        ButtonToggle currButton
                        currButton = 7
                        optionButton = 7
                        ButtonToggle optionButton
                    CASE 6, 7:
                        ButtonToggle currButton
                        currButton = currButton - 1
                        optionButton = currButton
                        ButtonToggle optionButton
                END SELECT
            CASE 11, 13                                 ' down, right arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList colors$(), colorBox, currButton, 3, func, topRow, leftCol
                    CASE 3, 4: ScrollList fonts$(), fontBox, currButton, 3, 0, topRow, leftCol
                    CASE 5, 6:
                        ButtonToggle currButton
                        currButton = currButton + 1
                        optionButton = currButton
                        ButtonToggle optionButton
                    CASE 7:
                        ButtonToggle currButton
                        currButton = 5
                        optionButton = 5
                        ButtonToggle optionButton
                END SELECT
            CASE 14                                     ' space bar
                IF currButton > 7 THEN
                    pushButton = currButton
                    finished = TRUE
                END IF
        END SELECT
    WEND

    ' done and not canceled so return new title information
    IF pushButton = 8 THEN
        IF setNum > 0 THEN chartChanged = TRUE

        title.title = EditFieldInquire(1)
        title.TitleFont = fontBox.listPos
        title.TitleColor = colorBox.listPos - 1
        title.Justify = optionButton - 4
    END IF

    WindowClose handle

END SUB

'
' Sub Name: ChangeWindow
'
' Description: Allows user to view and modify any of the chart windows
'
' Arguments: handle - window number
'            wTitle$ - window title
'            win - chart window
'
SUB ChangeWindow (handle, title$, win AS RegionType)

    DIM bgColorBox AS ListBox
    DIM bdColorBox AS ListBox
    DIM bdStyleBox AS ListBox

    ' set up background color box
    bgColorBox.scrollButton = 1
    bgColorBox.areaButton = 2
    bgColorBox.listLen = numColors
    bgColorBox.topRow = 4
    bgColorBox.botRow = 14
    bgColorBox.leftCol = 4
    bgColorBox.rightCol = 18
    bgColorBox.listPos = win.Background + 1

    ' set up border color box
    bdColorBox.scrollButton = 3
    bdColorBox.areaButton = 4
    bdColorBox.listLen = numColors
    bdColorBox.topRow = 5
    bdColorBox.botRow = 14
    bdColorBox.leftCol = 24
    bdColorBox.rightCol = 38
    bdColorBox.listPos = win.BorderColor + 1

    ' set up border style box
    bdStyleBox.scrollButton = 5
    bdStyleBox.areaButton = 6
    bdStyleBox.listLen = MAXSTYLES
    bdStyleBox.topRow = 5
    bdStyleBox.botRow = 14
    bdStyleBox.leftCol = 40
    bdStyleBox.rightCol = 54
    bdStyleBox.listPos = win.BorderStyle

    ' set up display window
    winRow = 5
    winCol = 3
    WindowOpen handle, winRow, winCol, 21, 76, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 2, title$
    WindowBox 1, 2, 15, 20
    WindowLocate 2, 5
    WindowPrint 2, "Background"
    WindowLocate 3, 5
    WindowPrint 2, "Color:"
    WindowBox 1, 22, 15, 56
    WindowLocate 4, 24
    WindowPrint 2, "Border Color:"
    WindowLocate 4, 40
    WindowPrint 2, "Border Style:"
    WindowBox 1, 58, 15, 73
    WindowLocate 2, 60
    WindowPrint 2, "Coordinates:"
    WindowBox 3, 63, 5, 71
    WindowLocate 4, 60
    WindowPrint 2, "X1:"
    WindowBox 6, 63, 8, 71
    WindowLocate 7, 60
    WindowPrint 2, "Y1:"
    WindowBox 9, 63, 11, 71
    WindowLocate 10, 60
    WindowPrint 2, "X2:"
    WindowBox 12, 63, 14, 71
    WindowLocate 13, 60
    WindowPrint 2, "Y2:"
    WindowLine 16

    CreateListBox colors$(), bgColorBox, 0
    CreateListBox colors$(), bdColorBox, 0
    CreateListBox styles$(), bdStyleBox, 0

    ButtonOpen 7, 1, "Display Border", 2, 24, 0, 0, 2
    ButtonOpen 8, 2, "OK ", 17, 14, 0, 0, 1
    ButtonOpen 9, 1, "Cancel ", 17, 51, 0, 0, 1

    EditFieldOpen 1, LTRIM$(STR$(win.X1)), 4, 64, 0, 7, 7, 10
    EditFieldOpen 2, LTRIM$(STR$(win.Y1)), 7, 64, 0, 7, 7, 10
    EditFieldOpen 3, LTRIM$(STR$(win.X2)), 10, 64, 0, 7, 7, 10
    EditFieldOpen 4, LTRIM$(STR$(win.Y2)), 13, 64, 0, 7, 7, 10

    currButton = 2                                      ' start in first list box
    currEditField = 0
    IF win.border = TRUE THEN ButtonSetState 7, 2
    pushButton = 8

    ' window control loop
    finished = FALSE
    WHILE NOT finished
        WindowDo currButton, currEditField              ' wait for event
        SELECT CASE Dialog(0)
            CASE 1                                      ' button pressed
                currButton = Dialog(1)
                SELECT CASE currButton
                    CASE 1, 2
                        currEditField = 0
                        ScrollList colors$(), bgColorBox, currButton, 1, 0, winRow, winCol
                        currButton = 2
                    CASE 3, 4
                        currEditField = 0
                        ScrollList colors$(), bdColorBox, currButton, 1, 0, winRow, winCol
                        currButton = 4
                    CASE 5, 6
                        currEditField = 0
                        ScrollList styles$(), bdStyleBox, currButton, 1, 0, winRow, winCol
                        currButton = 6
                    CASE 7
                        ButtonToggle currButton
                        currEditField = 0
                    CASE 8, 9
                        pushButton = currButton
                        finished = TRUE
                END SELECT
            CASE 2                                      ' edit field
                currEditField = Dialog(2)
                currButton = 0
            CASE 6                                      ' enter
                finished = TRUE
            CASE 7                                      ' tab
                SELECT CASE currButton
                    CASE 0:
                        SELECT CASE currEditField
                            CASE 1, 2, 3: currEditField = currEditField + 1
                            CASE 4:
                                currEditField = 0
                                currButton = 8
                                ButtonSetState pushButton, 1
                                ButtonSetState currButton, 2
                                pushButton = currButton
                        END SELECT
                    CASE 1, 2: currButton = 7
                    CASE 3, 4: currButton = 6
                    CASE 5, 6:
                        currButton = 0
                        currEditField = 1
                    CASE 7: currButton = 4
                    CASE 8:
                        currButton = currButton + 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 9:
                        ButtonSetState currButton, 1
                        pushButton = 8
                        ButtonSetState pushButton, 2
                        currButton = 2
                        currEditField = 0
                END SELECT
            CASE 8                                      ' back tab
                SELECT CASE currButton
                    CASE 0:
                        SELECT CASE currEditField
                            CASE 1:
                                currEditField = 0
                                currButton = 6
                            CASE 2, 3, 4: currEditField = currEditField - 1
                        END SELECT
                    CASE 1, 2:
                        currButton = 9
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                    CASE 3, 4: currButton = 7
                    CASE 5, 6: currButton = 4
                    CASE 7: currButton = 2
                    CASE 8:
                        currButton = 0
                        currEditField = 4
                    CASE 9:
                        currButton = currButton - 1
                        ButtonSetState pushButton, 1
                        ButtonSetState currButton, 2
                        pushButton = currButton
                END SELECT
            CASE 9                                      ' escape
                pushButton = 9
                finished = TRUE
            CASE 10, 12                                 ' up, left arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList colors$(), bgColorBox, currButton, 2, 0, winRow, winCol
                    CASE 3, 4: ScrollList colors$(), bdColorBox, currButton, 2, 0, winRow, winCol
                    CASE 5, 6: ScrollList styles$(), bdStyleBox, currButton, 2, 0, winRow, winCol
                    CASE 7: ButtonSetState currButton, 2
                END SELECT
            CASE 11, 13                                 ' down, right arrow
                SELECT CASE currButton
                    CASE 1, 2: ScrollList colors$(), bgColorBox, currButton, 3, 0, winRow, winCol
                    CASE 3, 4: ScrollList colors$(), bdColorBox, currButton, 3, 0, winRow, winCol
                    CASE 5, 6: ScrollList styles$(), bdStyleBox, currButton, 3, 0, winRow, winCol
                    CASE 7: ButtonSetState currButton, 1
                END SELECT
            CASE 14                                     ' space bar
                SELECT CASE currButton
                    CASE 7: ButtonToggle currButton
                    CASE 8, 9: finished = TRUE
                END SELECT
        END SELECT
    WEND

    ' return new window information
    IF pushButton = 8 THEN
        IF setNum > 0 THEN chartChanged = TRUE

        win.X1 = VAL(EditFieldInquire(1))
        win.Y1 = VAL(EditFieldInquire(2))
        win.X2 = VAL(EditFieldInquire(3))
        win.Y2 = VAL(EditFieldInquire(4))
        win.Background = bgColorBox.listPos - 1
        win.border = (ButtonInquire(7) = 2)
        win.BorderColor = bdColorBox.listPos - 1
        win.BorderStyle = bdStyleBox.listPos
    END IF

    WindowClose handle

END SUB

