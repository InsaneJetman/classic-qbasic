'============================================================================
'
'    WINDOW.BAS - Window Routines for the User Interface Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  NOTE:
'           This sample source code toolbox is intended to demonstrate some
'           of the extended capabilities of Microsoft BASIC 7.1 Professional
'           Development system that can help to leverage the professional
'           developer's time more effectively.  While you are free to use,
'           modify, or distribute the routines in this module in any way you
'           find useful, it should be noted that these are examples only and
'           should not be relied upon as a fully-tested "add-on" library.
'
'  PURPOSE: These routines provide dialog box and window support to the
'           user interface toolbox.
'
'  For information on creating a library and QuickLib from the routines
'  contained in this file, read the comment header of GENERAL.BAS.
'
'==========================================================================

DEFINT A-Z

'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'menu.bi'
'$INCLUDE: 'window.bi'


COMMON SHARED /uitools/ GloMenu           AS MenuMiscType
COMMON SHARED /uitools/ GloTitle()        AS MenuTitleType
COMMON SHARED /uitools/ GloItem()         AS MenuItemType
COMMON SHARED /uitools/ GloWindow()       AS windowType
COMMON SHARED /uitools/ GloButton()       AS buttonType
COMMON SHARED /uitools/ GloEdit()         AS EditFieldType
COMMON SHARED /uitools/ GloStorage        AS WindowStorageType
COMMON SHARED /uitools/ GloWindowStack()  AS INTEGER
COMMON SHARED /uitools/ GloBuffer$()

FUNCTION Alert (style, text$, row1, col1, row2, col2, b1$, b2$, b3$)

    ' =======================================================================
    ' Open an alert window, then return the button that was pushed
    ' =======================================================================

    Alert = 0

    ' =======================================================================
    ' Make sure coordinates and butttons are valid
    ' =======================================================================

    IF row1 >= MINROW AND row2 <= MAXROW AND col1 >= MINCOL AND col2 <= MAXCOL THEN

        IF b1$ = "" THEN
            b1$ = "OK"
            b2$ = ""
            b3$ = ""
        END IF

        IF b2$ = "" THEN
            b3$ = ""
        END IF

        ' ===================================================================
        ' If a window is available, compute button locations
        ' ===================================================================

        alertWindow = WindowNext

        IF alertWindow <> 0 THEN

            minWidth = 3
            buttonTotal = 0
          
            IF b1$ <> "" THEN
                minWidth = minWidth + 7 + LEN(b1$):
                buttonTotal = buttonTotal + 1
            END IF

            IF b2$ <> "" THEN
                minWidth = minWidth + 7 + LEN(b2$)
                buttonTotal = buttonTotal + 1
            END IF

            IF b3$ <> "" THEN
                minWidth = minWidth + 7 + LEN(b3$)
                buttonTotal = buttonTotal + 1
            END IF

            actualWidth = col2 - col1 + 1
            actualHeight = row2 - row1 + 1

            ' ===============================================================
            ' If size is valid, open window, print text, open buttons
            ' ===============================================================

            IF actualWidth >= minWidth AND actualHeight >= 3 THEN

                WindowOpen alertWindow, row1, col1, row2, col2, 0, 7, 0, 7, 15, 0, 0, 0, 1, 1, ""
                WindowLine actualHeight - 1

                text$ = text$ + "|"
                WHILE text$ <> ""
                    x$ = LEFT$(text$, INSTR(text$, "|") - 1)
                    text$ = RIGHT$(text$, LEN(text$) - LEN(x$) - 1)
                    WindowPrint style, x$
                WEND

                charTotal = LEN(b1$) + LEN(b2$) + LEN(b3$) + 4 * buttonTotal
                avgSpace = INT((actualWidth - charTotal) / (buttonTotal + 1))

                IF LEN(b1$) > 0 THEN
                    ButtonOpen 1, 2, b1$, actualHeight, avgSpace + 1, 0, 0, 1
                END IF

                IF LEN(b2$) > 0 THEN
                    ButtonOpen 2, 1, b2$, actualHeight, avgSpace * 2 + LEN(b1$) + 5, 0, 0, 1
                END IF

                IF LEN(b3$) > 0 THEN
                    ButtonOpen 3, 1, b3$, actualHeight, avgSpace * 3 + LEN(b1$) + LEN(b2$) + 9, 0, 0, 1
                END IF

                ' ===========================================================
                ' Main window processing loop
                ' ===========================================================

                currButton = 1

                ExitFlag = FALSE
                WHILE NOT ExitFlag
                    WindowDo currButton, 0
                    SELECT CASE Dialog(0)
                        CASE 1                      'Button Pressed
                            Alert = Dialog(1)
                            ExitFlag = TRUE
                        CASE 6, 14                  'Enter or Space
                            Alert = currButton
                            ExitFlag = TRUE
                        CASE 7                      'Tab
                            ButtonSetState currButton, 1
                            currButton = (currButton) MOD buttonTotal + 1
                            ButtonSetState currButton, 2
                        CASE 8                      'BackTab
                            ButtonSetState currButton, 1
                            currButton = (currButton + buttonTotal - 2) MOD buttonTotal + 1
                            ButtonSetState currButton, 2
                        CASE 9
                            IF UCASE$(b1$) = "CANCEL" THEN
                                Alert = 1
                            END IF
                            IF UCASE$(b2$) = "CANCEL" THEN
                                Alert = 2
                            END IF
                            IF UCASE$(b3$) = "CANCEL" THEN
                                Alert = 3
                            END IF
                            ExitFlag = TRUE
                        CASE ELSE
                    END SELECT
                WEND

                WindowClose alertWindow
               
            END IF
        END IF
    END IF

END FUNCTION

SUB BackgroundRefresh (handle)

    ' =======================================================================
    ' Refresh the background behind a window
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        MouseHide
        PutBackground GloWindow(handle).row1 - 1, GloWindow(handle).col1 - 1, GloBuffer$(handle, 1)
        MouseShow
    END IF
END SUB

SUB BackgroundSave (handle)

    ' =======================================================================
    ' Save the background before a window opens, or is moved... etc
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        MouseHide
        GetBackground GloWindow(handle).row1 - 1, GloWindow(handle).col1 - 1, GloWindow(handle).row2 + 1, GloWindow(handle).col2 + 1, GloBuffer$(handle, 1)
        MouseShow
    END IF
END SUB

SUB ButtonClose (handle)

    ' =======================================================================
    ' Make sure a window is actually opened
    ' =======================================================================

    windo = WindowCurrent

    IF windo > 0 THEN

        ' ===================================================================
        ' If handle=0, recursively close all buttons in the CURRENT WINDOW only
        ' ===================================================================

        IF handle = 0 THEN
            IF GloStorage.numButtonsOpen > 0 THEN
                FOR A = GloStorage.numButtonsOpen TO 1 STEP -1
                    IF GloButton(A).windowHandle = windo THEN
                        ButtonClose GloButton(A).handle
                    END IF
                NEXT A
            END IF
        ELSE
            ' ===============================================================
            ' Get the index into the global array based on handle, and
            ' currWindow
            ' ===============================================================

            button = FindButton(handle)

            ' ===============================================================
            ' If valid, hide button, then squeeze array, decrement totals
            ' ===============================================================

            IF button > 0 THEN
                            
                COLOR GloWindow(windo).fore, GloWindow(windo).back
                SELECT CASE GloButton(button).buttonType
                    CASE 1, 2, 3
                        LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1
                        MouseHide
                        PRINT SPACE$(4 + LEN(RTRIM$(GloButton(button).text$)));
                        MouseShow
                    CASE 6
                        MouseHide
                        FOR A = 1 TO GloButton(button).row2 - GloButton(button).row1 + 1
                            LOCATE GloWindow(windo).row1 + GloButton(button).row1 + A - 2, GloWindow(windo).col1 + GloButton(button).col1 - 1
                            PRINT " ";
                        NEXT A
                        MouseShow
                    CASE 7
                        LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1
                        MouseHide
                        PRINT SPACE$(GloButton(button).col2 - GloButton(button).col1 + 1);
                        MouseShow
                    CASE ELSE
                END SELECT


                GloStorage.numButtonsOpen = GloStorage.numButtonsOpen - 1
                WHILE button <= GloStorage.numButtonsOpen
                    GloButton(button).row1 = GloButton(button + 1).row1
                    GloButton(button).col1 = GloButton(button + 1).col1
                    GloButton(button).row2 = GloButton(button + 1).row2
                    GloButton(button).col2 = GloButton(button + 1).col2
                    GloButton(button).text = GloButton(button + 1).text
                    GloButton(button).handle = GloButton(button + 1).handle
                    GloButton(button).state = GloButton(button + 1).state
                    GloButton(button).buttonType = GloButton(button + 1).buttonType
                    GloButton(button).windowHandle = GloButton(button + 1).windowHandle
                    button = button + 1
                WEND
            END IF
        END IF
    END IF

END SUB

FUNCTION ButtonInquire (handle)

    ' =======================================================================
    ' If valid, return then state of the button
    ' =======================================================================

    button = FindButton(handle)

    IF button > 0 THEN
        ButtonInquire = GloButton(button).state
    ELSE
        ButtonInquire = 0
    END IF

END FUNCTION

SUB ButtonOpen (handle, state, title$, row1, col1, row2, col2, buttonType)

    ' =======================================================================
    ' Open a button - first check if window can be resized - If so, do not
    ' open!
    ' =======================================================================

    IF MID$(WindowBorder$(GloWindow(WindowCurrent).windowType), 9, 1) = "+" THEN
        resize = TRUE
    END IF

    IF (resize AND buttonType >= 6) OR NOT resize THEN

        ' ===================================================================
        ' If scroll bar, then make sure "state" is valid, given bar length
        ' ===================================================================

        IF buttonType = 6 THEN
            length = (row2 - row1) - 1
            IF state < 1 THEN state = 1
            IF state > length THEN state = length
        END IF

        IF buttonType = 7 THEN
            length = (col2 - col1) - 1
            IF state < 1 THEN state = 1
            IF state > length THEN state = length
        END IF


        ' ===================================================================
        ' If valid state and type, increment totals, and store button info
        ' ===================================================================

        IF (buttonType = 1 AND state >= 1 AND state <= 3) OR (buttonType >= 2 AND buttonType <= 3 AND state >= 1 AND state <= 2) OR (buttonType >= 4 AND buttonType <= 7) THEN
            ButtonClose handle

            GloStorage.numButtonsOpen = GloStorage.numButtonsOpen + 1
            GloButton(GloStorage.numButtonsOpen).row1 = row1
            GloButton(GloStorage.numButtonsOpen).col1 = col1
            GloButton(GloStorage.numButtonsOpen).row2 = row2
            GloButton(GloStorage.numButtonsOpen).col2 = col2
            GloButton(GloStorage.numButtonsOpen).text = title$
            GloButton(GloStorage.numButtonsOpen).state = state
            GloButton(GloStorage.numButtonsOpen).handle = handle
            GloButton(GloStorage.numButtonsOpen).buttonType = buttonType
            GloButton(GloStorage.numButtonsOpen).windowHandle = WindowCurrent
            ButtonShow handle
        ELSE
            PRINT "Cannot open button on window that can be re-sized!"
            END
        END IF
    END IF
END SUB

SUB ButtonSetState (handle, state)

    button = FindButton(handle)
    windo = WindowCurrent

    ' =======================================================================
    ' If valid state for the type of button, assign the new state, and re-show
    ' =======================================================================

    IF button > 0 AND windo > 0 THEN
        SELECT CASE GloButton(button).buttonType
            CASE 1
                IF state >= 1 AND state <= 3 THEN
                    GloButton(button).state = state
                END IF
            CASE 2, 3
                IF state = 1 OR state = 2 THEN
                    GloButton(button).state = state
                END IF
            CASE 4, 5
            CASE 6
                IF state <> GloButton(button).state THEN
                    MouseHide
                    COLOR 0, 7
                    LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1 + GloButton(button).state, GloWindow(windo).col1 + GloButton(button).col1 - 1
                    PRINT CHR$(176);
                    GloButton(button).state = state
                    LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1 + GloButton(button).state, GloWindow(windo).col1 + GloButton(button).col1 - 1
                    PRINT CHR$(219);
                    MouseShow
                END IF
            CASE 7
                IF state <> GloButton(button).state THEN
                    MouseHide
                    COLOR 0, 7
                    LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1 + GloButton(button).state
                    PRINT CHR$(176);
                    GloButton(button).state = state
                    LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1 + GloButton(button).state
                    PRINT CHR$(219);
                    MouseShow
                END IF
            CASE ELSE
        END SELECT
    END IF

    ButtonShow handle
END SUB

SUB ButtonShow (handle)

    button = FindButton(handle)
    windo = WindowCurrent

    ' =======================================================================
    ' If valid, show the button based on button type and button state
    ' =======================================================================

    IF button > 0 THEN
        LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1

        MouseHide
        SELECT CASE GloButton(button).buttonType
            CASE 1
                SELECT CASE GloButton(button).state
                    CASE 1
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT "< " + RTRIM$(GloButton(button).text$) + " >";
                    CASE 2
                        COLOR GloWindow(windo).highlight, GloWindow(windo).textBack
                        PRINT "<";
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT " "; RTRIM$(GloButton(button).text$); " ";
                        COLOR GloWindow(windo).highlight, GloWindow(windo).textBack
                        PRINT ">";
                    CASE 3
                        COLOR GloWindow(windo).textBack, GloWindow(windo).textFore
                        PRINT "< " + RTRIM$(GloButton(button).text$) + " >";
                END SELECT
            CASE 2
                SELECT CASE GloButton(button).state
                    CASE 1
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT "[ ] " + RTRIM$(GloButton(button).text$);
                    CASE 2
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT "[X] " + RTRIM$(GloButton(button).text$);
                END SELECT
            CASE 3
                SELECT CASE GloButton(button).state
                    CASE 1
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT "( ) " + RTRIM$(GloButton(button).text$);
                    CASE 2
                        COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                        PRINT "() " + RTRIM$(GloButton(button).text$);
                END SELECT
            CASE 4, 5
            CASE 6
                COLOR 0, 7
                PRINT CHR$(24);
                FOR A = 1 TO GloButton(button).row2 - GloButton(button).row1 - 1
                    LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1 + A, GloWindow(windo).col1 + GloButton(button).col1 - 1
                    IF A = GloButton(button).state THEN
                        PRINT CHR$(219);
                    ELSE
                        PRINT CHR$(176);
                    END IF
                NEXT A
                LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1 + A, GloWindow(windo).col1 + GloButton(button).col1 - 1
                PRINT CHR$(25);
            CASE 7
                COLOR 0, 7
                PRINT CHR$(27); STRING$(GloButton(button).col2 - GloButton(button).col1 - 1, 176); CHR$(26);
                LOCATE GloWindow(windo).row1 + GloButton(button).row1 - 1, GloWindow(windo).col1 + GloButton(button).col1 - 1 + GloButton(button).state
                PRINT CHR$(219);
            CASE ELSE
                PRINT "Error in Button Parameter";
        END SELECT
        MouseShow
    END IF
END SUB

SUB ButtonToggle (handle)

    button = FindButton(handle)
    windo = WindowCurrent

    ' =======================================================================
    ' If valid button, and state is 1 or 2, toggle button
    ' =======================================================================

    IF button > 0 THEN
        IF GloButton(button).state = 1 OR GloButton(button).state = 2 THEN
           GloButton(button).state = 3 - GloButton(button).state
        END IF
    END IF

    ButtonShow handle

END SUB

FUNCTION Dialog (op)

    ' =======================================================================
    ' Based on global variables set in WindowDo, return proper event ID/Info
    ' =======================================================================

    SELECT CASE op

        ' ===================================================================
        ' Return event ID, and reset all variables.
        ' ===================================================================

        CASE 0
            GloStorage.DialogButton = GloStorage.oldDialogButton
            GloStorage.DialogEdit = GloStorage.oldDialogEdit
            GloStorage.DialogWindow = GloStorage.oldDialogWindow
            GloStorage.DialogClose = GloStorage.oldDialogClose
            GloStorage.DialogScroll = GloStorage.oldDialogScroll
            GloStorage.DialogRow = GloStorage.oldDialogRow
            GloStorage.DialogCol = GloStorage.oldDialogCol
            Dialog = GloStorage.oldDialogEvent

            GloStorage.oldDialogButton = 0
            GloStorage.oldDialogEdit = 0
            GloStorage.oldDialogWindow = 0
            GloStorage.oldDialogClose = 0
            GloStorage.oldDialogScroll = 0
            GloStorage.oldDialogRow = 0
            GloStorage.oldDialogCol = 0

        ' ===================================================================
        ' If button is pressed, dialog(0) is 1, and dialog(1) is the button
        ' number
        ' ===================================================================

        CASE 1
            Dialog = GloStorage.DialogButton


        ' ===================================================================
        ' If edit field is clicked, dialog(0) is 2, and dialog(2) is the edit
        ' field number
        ' ===================================================================

        CASE 2
            Dialog = GloStorage.DialogEdit

        ' ===================================================================
        ' If another window is clicked, dialog(0)=3, and dialog(3)=window
        ' number
        ' ===================================================================

        CASE 3
            Dialog = GloStorage.DialogWindow

        ' ===================================================================
        ' If a field button was pressed This returns the row (relative to
        ' window position) of the click
        ' ===================================================================

        CASE 17
            Dialog = GloStorage.DialogRow

        ' ===================================================================
        ' If a field button was pressed This returns the column (relative to
        ' window position) of the click
        ' ===================================================================

        CASE 18
            Dialog = GloStorage.DialogCol

        ' ===================================================================
        ' If a scroll bar was clicked, return new position of marker
        ' ===================================================================

        CASE 19
            Dialog = GloStorage.DialogScroll

        ' ===================================================================
        ' Bad call, so return 0
        ' ===================================================================

        CASE ELSE
            Dialog = 0
    END SELECT


END FUNCTION

SUB EditFieldClose (handle)

    ' =======================================================================
    ' Close an edit field
    ' =======================================================================

    windo = WindowCurrent

    IF windo > 0 THEN
        IF handle = 0 THEN

            ' ===============================================================
            ' If handle = 0, then recursivily close all edit fields
            ' ===============================================================

            IF GloStorage.numEditFieldsOpen > 0 THEN
                FOR A = GloStorage.numEditFieldsOpen TO 1 STEP -1
                    IF GloEdit(A).windowHandle = windo THEN
                        EditFieldClose GloEdit(A).handle
                    END IF
                NEXT A
            END IF
        ELSE

            ' ===============================================================
            ' else, erase edit field, then squeeze array, decrement total
            ' variables
            ' ===============================================================

            editField = FindEditField(handle)

            IF editField > 0 THEN
                LOCATE GloWindow(windo).row1 + GloEdit(editField).row - 1, GloWindow(windo).col1 + GloEdit(editField).col - 1
                COLOR GloWindow(windo).fore, GloWindow(windo).back
                MouseHide
                PRINT SPACE$(GloEdit(editField).visLength);
                MouseShow

                GloStorage.numEditFieldsOpen = GloStorage.numEditFieldsOpen - 1
                WHILE editField <= GloStorage.numEditFieldsOpen
                    GloEdit(editField).row = GloEdit(editField + 1).row
                    GloEdit(editField).col = GloEdit(editField + 1).col
                    GloEdit(editField).text = GloEdit(editField + 1).text
                    GloEdit(editField).handle = GloEdit(editField + 1).handle
                    GloEdit(editField).visLength = GloEdit(editField + 1).visLength
                    GloEdit(editField).maxLength = GloEdit(editField + 1).maxLength
                    GloEdit(editField).windowHandle = GloEdit(editField + 1).windowHandle
                    editField = editField + 1
                WEND
            END IF
        END IF
    END IF
END SUB

FUNCTION EditFieldInquire$ (handle)

    ' =======================================================================
    ' If valid edit field, return the value.  Note edit$ is terminated
    ' by a CHR$(0), or maxLength, or 255 chars.
    ' =======================================================================

    editField = FindEditField(handle)
    windo = WindowCurrent
    EditFieldInquire$ = ""

    IF editField > 0 THEN
        x$ = GloEdit(editField).text$
        x = INSTR(x$, CHR$(0)) - 1
        IF x >= 0 THEN
            EditFieldInquire$ = LEFT$(x$, x)
        ELSE
            EditFieldInquire$ = x$
        END IF
    END IF

END FUNCTION

SUB EditFieldOpen (handle, text$, row, col, fore, back, visLength, maxLength)

    ' =======================================================================
    ' If window can be re-sized, do not open edit field
    ' =======================================================================

    IF MID$(WindowBorder$(GloWindow(WindowCurrent).windowType), 9, 1) <> "+" THEN

        ' ===================================================================
        ' Close edit field by the same handle if it exists
        ' ===================================================================

        EditFieldClose handle

        windo = WindowCurrent

        ' ===================================================================
        ' If no colors given, use default window colors
        ' ===================================================================

        IF fore = 0 AND back = 0 THEN
            fore = GloWindow(windo).fore
            back = GloWindow(windo).back
        END IF
      
        ' ===================================================================
        ' Increment totals, and store edit field info
        ' ===================================================================

        GloStorage.numEditFieldsOpen = GloStorage.numEditFieldsOpen + 1
        GloEdit(GloStorage.numEditFieldsOpen).row = row
        GloEdit(GloStorage.numEditFieldsOpen).col = col
        GloEdit(GloStorage.numEditFieldsOpen).fore = fore
        GloEdit(GloStorage.numEditFieldsOpen).back = back
        GloEdit(GloStorage.numEditFieldsOpen).text = text$ + CHR$(0)
        GloEdit(GloStorage.numEditFieldsOpen).visLength = visLength
        GloEdit(GloStorage.numEditFieldsOpen).maxLength = maxLength
        GloEdit(GloStorage.numEditFieldsOpen).windowHandle = windo
        GloEdit(GloStorage.numEditFieldsOpen).handle = handle

        LOCATE GloWindow(windo).row1 + row - 1, GloWindow(windo).col1 + col - 1
        COLOR fore, back

        'Create temp$ so that padding with spaces doesn't alter the original text$
        IF LEN(text$) < visLength THEN
            temp$ = text$ + SPACE$(visLength - LEN(text$))
        ELSE
            temp$ = LEFT$(text$, visLength)
        END IF
        PRINT temp$;
       
    ELSE
        PRINT "EditField cannot be opened on a window that can be re-sized!"
    END IF
END SUB

FUNCTION FindButton (handle)

    ' =======================================================================
    ' Given a handle, return the index into the global array that stores
    ' buttons.  Each button is uniquely described by a handle, and a window#
    ' This SUB program assumes that you want the current window.
    ' =======================================================================

    FindButton = 0

    IF GloStorage.numButtonsOpen > 0 THEN
        A = 0
        curr = WindowCurrent
        DO
            A = A + 1
        LOOP UNTIL (GloButton(A).handle = handle AND GloButton(A).windowHandle = curr) OR A = GloStorage.numButtonsOpen

        IF GloButton(A).handle = handle AND GloButton(A).windowHandle = curr THEN
            FindButton = A
        END IF
    END IF

END FUNCTION

FUNCTION FindEditField (handle)

    ' =======================================================================
    ' Given a handle, return the index into the global array that stores
    ' edit fields.  Each button is uniquely described by a handle, and a
    ' window number. This SUB program assumes the you want the current window.
    ' =======================================================================

    FindEditField = 0

    IF GloStorage.numEditFieldsOpen > 0 THEN
        A = 0
        curr = WindowCurrent
        DO
            A = A + 1
        LOOP UNTIL (GloEdit(A).handle = handle AND GloEdit(A).windowHandle = curr) OR A = GloStorage.numEditFieldsOpen

        IF GloEdit(A).handle = handle AND GloEdit(A).windowHandle = curr THEN
            FindEditField = A
        END IF
    END IF

END FUNCTION

' ==========================================================================
' The ListBox FUNCTION can be modified to accept a box width parameter. This
' will enable you to specify the width of a listbox when you call the ListBox
' FUNCTION. Below you will find two FUNCTION statements. The first is the
' default ListBox FUNCTION that takes only two arguments.  The second allows
' you to specify a box width parameter. As configured, the listbox width is
' assumed to be 14. This default is idal for listboxes that contain file
' names. To use the second form of the ListBox FUNCTION, that
' lets you specify the listbox width, comment out the first FUNCTION
' statement and remove the ' from the beginning of the second FUNCTION
' statement. Change the WINDOW.BI file so that the DECLARE statement matches
' the actual FUNCTION as follows:
'
' DECLARE FUNCTION ListBox (Text$(), MaxRec%, BoxWidth%)
'
' You also need to comment out the "BoxWidth = 14" statement that occurs just the
' after second FUNCTION statement.
'
' When you use the ListBox FUNCTION be sure to include a box width parameter
' as the third argument.  All calculations will be automatically performed
' to properly display the listbox.
'
' ===========================================================================
'
FUNCTION ListBox (text$(), MaxRec)
'FUNCTION ListBox (text$(), MaxRec, BoxWidth)

    ' Comment out the following line if you modify this function to allow
    ' specification of a ListBox width parameter in the function call.

    BoxWidth = 14

    GOSUB ListBoxWidthCalc

    ' =======================================================================
    ' Open up a modal window and put the right buttons in it
    ' =======================================================================
    
    WindowOpen 1, 4, StartRowPos, 20, StopRowPos, 0, 7, 0, 7, 15, 0, 0, 0, 1, 1, ""
                                                          
    WindowBox 1, 6, 14, BoxEndPos
    ButtonOpen 1, 1, "", 2, BoxEndPos, 13, BoxEndPos, 6     'Scroll Bar
    ButtonOpen 2, 2, "OK", 16, 6, 0, 0, 1                   'OK button
    ButtonOpen 3, 1, "Cancel", 16, BoxEndPos - 9, 0, 0, 1   'Cancel button
    ButtonOpen 4, 1, "", 1, 8, 1, AreaEndPos, 4             'Area above box
    ButtonOpen 5, 1, "", 2, 7, 13, AreaEndPos + 1, 4        'Area of box
    ButtonOpen 6, 1, "", 14, 8, 14, AreaEndPos, 4           'Area below box

    currTop = 1
    currPos = 1
    currButton = 2
   
    GOSUB ListBoxDrawText

    ExitFlag = FALSE

    ' =======================================================================
    ' Process window events...
    '  IMPORTANT:  Window moving, and re-sizing is handled automatically
    '  The window type dictates when this is allowed to happen.
    ' =======================================================================

    WHILE NOT ExitFlag
        WindowDo currButton, 0
        x = Dialog(0)

        SELECT CASE x
            CASE 1
                button = Dialog(1)
                SELECT CASE button
                    CASE 1
                        scrollCode = Dialog(19)
                        SELECT CASE scrollCode
                            CASE -1:   GOSUB ListBoxUp
                            CASE -2:   GOSUB ListBoxDown
                            CASE ELSE: GOSUB ListBoxMove
                        END SELECT
                    CASE 2
                        ListBox = currTop + currPos - 1
                        ExitFlag = TRUE
                    CASE 3
                        ListBox = 0
                        ExitFlag = TRUE
                    CASE 4
                        GOSUB ListBoxUp
                    CASE 5
                        GOSUB ListBoxAssign
                    CASE 6
                        GOSUB ListBoxDown
                END SELECT
            CASE 6, 14
                SELECT CASE currButton
                    CASE 0, 2
                        ListBox = currTop + currPos - 1
                        ExitFlag = TRUE
                    CASE 3
                        ListBox = 0
                        ExitFlag = TRUE
                    CASE ELSE
                END SELECT
            CASE 7
                SELECT CASE currButton
                    CASE 0
                        currButton = 2
                    CASE 2
                        ButtonToggle 2
                        ButtonToggle 3
                        currButton = 3
                    CASE 3
                        ButtonToggle 2
                        ButtonToggle 3
                        currButton = 0
                END SELECT
            CASE 8
                SELECT CASE currButton
                    CASE 0
                        ButtonToggle 2
                        ButtonToggle 3
                        currButton = 3
                    CASE 2
                        currButton = 0
                    CASE 3
                        ButtonToggle 2
                        ButtonToggle 3
                        currButton = 2
                END SELECT
            CASE 9
                ListBox = 0
                ExitFlag = TRUE
            CASE 10, 12
                IF currButton = 0 THEN
                    GOSUB ListBoxUp
                END IF
            CASE 11, 13
                IF currButton = 0 THEN
                    GOSUB ListBoxDown
                END IF
            CASE 16
                scrollCode = 1
                GOSUB ListBoxMove
            CASE 17
                scrollCode = 10
                GOSUB ListBoxMove
            CASE 18
                GOSUB ListBoxPgUp
            CASE 19
                GOSUB ListBoxPgDn
            CASE ELSE
        END SELECT
    WEND

    WindowClose 0
    EXIT FUNCTION

ListBoxUp:
    oldRec = currTop + currPos - 1
    currPos = currPos - 1
    IF currPos < 1 THEN
        currPos = 1
        currTop = currTop - 1
        IF currTop < 1 THEN
            currTop = 1
        END IF
    END IF
    newRec = currTop + currPos - 1
    IF oldRec <> newRec THEN
        GOSUB ListBoxDrawText
        GOSUB ListBoxNewBarPos
    END IF
RETURN

ListBoxDown:
    oldRec = currTop + currPos - 1
    IF MaxRec > 12 THEN
        currPos = currPos + 1
        IF currPos > 12 THEN
            currPos = 12
            currTop = currTop + 1
            IF currTop + currPos - 1 > MaxRec THEN
                currTop = currTop - 1
            END IF
        END IF
    ELSE
        IF currPos + 1 <= MaxRec THEN
            currPos = currPos + 1
        END IF
    END IF

    newRec = currTop + currPos - 1
    IF oldRec <> newRec THEN
        GOSUB ListBoxDrawText
        GOSUB ListBoxNewBarPos
    END IF
RETURN

ListBoxPgUp:
    oldRec = currTop + currPos - 1
    currTop = currTop - 12
    IF currTop < 1 THEN
        currTop = 1
        currPos = 1
    END IF
    newRec = currTop + currPos - 1
    IF oldRec <> newRec THEN
        GOSUB ListBoxDrawText
        GOSUB ListBoxNewBarPos
    END IF
RETURN

ListBoxPgDn:
    oldRec = currTop + currPos - 1
    IF MaxRec > 12 THEN
        currTop = currTop + 12
            IF currTop + 12 > MaxRec THEN
                currTop = MaxRec - 11
                currPos = 12
            END IF
    ELSE
        currPos = MaxRec
    END IF

    newRec = currTop + currPos - 1
    IF oldRec <> newRec THEN
        GOSUB ListBoxDrawText
        GOSUB ListBoxNewBarPos
    END IF
RETURN

ListBoxAssign:
    currPos = Dialog(17)
    IF currPos > MaxRec THEN currPos = MaxRec
    GOSUB ListBoxDrawText
    GOSUB ListBoxNewBarPos

RETURN

ListBoxMove:
    SELECT CASE scrollCode
        CASE 1:      newPos = 1
        CASE 2 TO 9: newPos = scrollCode * MaxRec / 10
        CASE 10:     newPos = MaxRec
    END SELECT
    
    IF newPos < 1 THEN newPos = 1
    IF newPos > MaxRec THEN newPos = MaxRec

    currPos = newPos - currTop + 1
    IF currPos <= 0 THEN
        currTop = newPos
        currPos = 1
    ELSEIF currPos > 12 THEN
        currPos = 12
        currTop = newPos - 11
    END IF
    GOSUB ListBoxDrawText
    GOSUB ListBoxNewBarPos
RETURN

ListBoxDrawText:
    FOR A = currTop TO currTop + 11
        IF A <= MaxRec THEN
            IF currTop + currPos - 1 = A THEN
                WindowColor 7, 0
            ELSE
                WindowColor 0, 7
            END IF

            WindowLocate A - currTop + 2, 8
            WindowPrint -1, LEFT$(text$(A) + STRING$(BoxWidth + 1, " "), BoxWidth + 1)
        END IF
    NEXT A
    WindowColor 0, 7
    RETURN

ListBoxNewBarPos:
    IF currPos = 1 AND currTop = 1 THEN
        newState = 1
    ELSE
        newState = (currTop + currPos - 1) * 10 / MaxRec
        IF newState < 1 THEN newState = 1
        IF newState > 10 THEN newState = 10
    END IF
    ButtonSetState 1, newState
RETURN

ListBoxWidthCalc:
    IF BoxWidth < 14 THEN BoxWidth = 14
    IF BoxWidth > 55 THEN BoxWidth = 55
    StartRowPos = 40 - ((BoxWidth + 14) / 2)
    StopRowPos = StartRowPos + BoxWidth + 14
    BoxEndPos = BoxWidth + 10
    AreaEndPos = BoxWidth + 8
RETURN

END FUNCTION

FUNCTION MaxScrollLength (handle)

    ' =======================================================================
    ' If valid, return then maximum length of scroll bar
    ' =======================================================================

    button = FindButton(handle)

    IF button > 0 THEN
        SELECT CASE GloButton(button).buttonType
            CASE 6
                MaxScrollLength = GloButton(button).row2 - GloButton(button).row1 - 1
            CASE 7
                MaxScrollLength = GloButton(button).col2 - GloButton(button).col1 - 1
            CASE ELSE
                MaxScrollLength = 0
        END SELECT
    ELSE
        MaxScrollLength = 0
    END IF

END FUNCTION

FUNCTION WhichWindow (row, col)

    ' =======================================================================
    ' Returns the window number where the row,col points to.  Takes into
    ' account which windows overlap which other windows by going down
    ' the GloWindowStack from the top.
    ' =======================================================================

    x = GloStorage.numWindowsOpen
    Found = FALSE
    WhichWindow = 0

    WHILE x > 0 AND NOT Found
        handle = GloWindowStack(x)
        row1 = GloWindow(handle).row1 - 1
        col1 = GloWindow(handle).col1 - 1
        row2 = GloWindow(handle).row2 + 1
        col2 = GloWindow(handle).col2 + 1

        IF row >= row1 AND row <= row2 AND col >= col1 AND col <= col2 THEN
            WhichWindow = handle
            Found = TRUE
        ELSE
            x = x - 1
        END IF
    WEND

END FUNCTION

FUNCTION WindowBorder$ (windowType)

    ' =======================================================================
    ' Returns a window border for the given window type.
    ' You may customize this to create custom windows.  See external
    ' documentation for a discussion of window borders
    ' =======================================================================

    SELECT CASE ABS(windowType)
        CASE 1
            WindowBorder$ = " °          ST"
        CASE 2
            WindowBorder$ = "=           ST"
        CASE 3
            WindowBorder$ = "=°          ST"
        CASE 4
            WindowBorder$ = "        +   ST"
        CASE 5
            WindowBorder$ = " °      +   ST"
        CASE 6
            WindowBorder$ = "=       +   ST"
        CASE 7
            WindowBorder$ = "=°      +   ST"
        CASE 8
            WindowBorder$ = "ÚÄ¿³ ³ÀÄÙÃÄ´ST"
        CASE 9
            WindowBorder$ = "Ú°¿³ ³ÀÄÙÃÄ´ST"
        CASE 10
            WindowBorder$ = "=Ä¿³ ³ÀÄÙÃÄ´ST"
        CASE 11
            WindowBorder$ = "=°¿³ ³ÀÄÙÃÄ´ST"
        CASE 12
            WindowBorder$ = "ÚÄ¿³ ³ÀÄ+ÃÄ´ST"
        CASE 13
            WindowBorder$ = "Ú°¿³ ³ÀÄ+ÃÄ´ST"
        CASE 14
            WindowBorder$ = "=Ä¿³ ³ÀÄ+ÃÄ´ST"
        CASE 15
            WindowBorder$ = "=°¿³ ³ÀÄ+ÃÄ´ST"
        CASE 16
            WindowBorder$ = "ÉÍ»º ºÈÍ¼ÌÍ¹ST"
        CASE 17
            WindowBorder$ = "É°»º ºÈÍ¼ÌÍ¹ST"
        CASE 18
            WindowBorder$ = "=Í»º ºÈÍ¼ÌÍ¹ST"
        CASE 19
            WindowBorder$ = "=°»º ºÈÍ¼ÌÍ¹ST"
        CASE 20
            WindowBorder$ = "ÉÍ»º ºÈÍ+ÌÍ¹ST"
        CASE 21
            WindowBorder$ = "É°»º ºÈÍ+ÌÍ¹ST"
        CASE 22
            WindowBorder$ = "=Í»º ºÈÍ+ÌÍ¹ST"
        CASE 23
            WindowBorder$ = "=°»º ºÈÍ+ÌÍ¹ST"

        ' ===================================================================
        ' Put any custom-designed border styles after this point and before
        ' the CASE ELSE statement.
        ' ===================================================================

        CASE ELSE
            WindowBorder$ = "            ST"
   
    END SELECT

END FUNCTION

SUB WindowBox (boxRow1, boxCol1, boxRow2, boxCol2)

    ' =======================================================================
    ' Draw a box, given coordinates based on the current window
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN
        row1 = GloWindow(windo).row1 + boxRow1 - 1
        row2 = GloWindow(windo).row1 + boxRow2 - 1
        col1 = GloWindow(windo).col1 + boxCol1 - 1
        col2 = GloWindow(windo).col1 + boxCol2 - 1
        fore = GloWindow(windo).fore
        back = GloWindow(windo).back
        border$ = "ÚÄ¿³ ³ÀÄÙ"

        Box row1, col1, row2, col2, fore, back, border$, 0
    END IF

END SUB

SUB WindowClose (handle)

    ' =======================================================================
    ' Close window # handle.  If handle is 0, recursively close all windows
    ' =======================================================================

    IF handle = 0 THEN
        IF GloStorage.numWindowsOpen > 0 THEN
            FOR x = GloStorage.numWindowsOpen TO 1 STEP -1
                WindowClose GloWindowStack(x)
            NEXT x
        END IF
    ELSE

        ' ===================================================================
        ' If valid window,
        ' ===================================================================

        IF GloWindow(handle).handle <> -1 THEN

            ' ===============================================================
            ' Make the window you want to close the top window
            ' ===============================================================

            WindowSetCurrent handle

            ' ===============================================================
            ' If top window has shadow, hide shadow
            ' ===============================================================

            IF INSTR(WindowBorder$(GloWindow(GloStorage.currWindow).windowType), "S") THEN
                WindowShadowRefresh
            END IF

            ' ===============================================================
            ' Close all edit fields, and button on top window
            ' ===============================================================

            EditFieldClose 0
            ButtonClose 0
            MouseHide

            ' ===============================================================
            ' Restore the background of the window + clear data
            ' ===============================================================

            BackgroundRefresh handle

            GloBuffer$(handle, 1) = ""
            GloBuffer$(handle, 2) = ""

            GloWindow(handle).handle = -1

            ' ===============================================================
            ' Decrement total number of windows
            ' ===============================================================

            GloStorage.numWindowsOpen = GloStorage.numWindowsOpen - 1

            ' ===============================================================
            ' If some windows still open, assign curr Window to top window,
            ' show shadow is the currWindow has a shadow
            ' ===============================================================

            IF GloStorage.numWindowsOpen > 0 THEN
                GloStorage.currWindow = GloWindowStack(GloStorage.numWindowsOpen)

                IF INSTR(WindowBorder$(GloWindow(GloStorage.currWindow).windowType), "S") THEN
                    WindowShadowSave
                END IF
            ELSE

                ' ===========================================================
                ' If no more windows open, assign 0 to the currWindow variable
                ' ===========================================================

                GloStorage.currWindow = 0
            END IF
            MouseShow
        END IF
    END IF

END SUB

SUB WindowCls

    ' =======================================================================
    ' If curr window is valid, clear the window
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN
        WindowScroll 0
    END IF

END SUB

SUB WindowColor (fore, back)

    ' =======================================================================
    ' If curr window is valid, assign the colors to the variables
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN
        GloWindow(windo).textFore = fore
        GloWindow(windo).textBack = back
    END IF

END SUB

FUNCTION WindowCols (handle)

    ' =======================================================================
    ' If window Handle is valid, return number of columns in that window
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        WindowCols = GloWindow(handle).col2 - GloWindow(handle).col1 + 1
    END IF

END FUNCTION

FUNCTION WindowCurrent

    ' =======================================================================
    ' Simply return the current window, as stored in the global array
    ' =======================================================================

    WindowCurrent = GloStorage.currWindow

END FUNCTION

SUB WindowDo (startButton, startEdit)

  DIM RB$(1 TO 4)

    ' =======================================================================
    ' Main Processing loop.  Init, go to proper mode, exit
    ' =======================================================================

    GOSUB WindowDoInit
    GOSUB WindowDoComputeHotSpots

    IF WindowDoMode = 1 THEN
        GOSUB WindowDoEditMode
    ELSE
        GOSUB WindowDoButtonMode
    END IF
    LOCATE , , 0
    EXIT SUB

' ===========================================================================
' If startEdit is=0 then do button mode.  In button mode, we wait
' for any keyboard event or mouse event that flips the ExitFlag.
' Then we exit.  It's very simple really, don't try to make it complicated.
' ===========================================================================

WindowDoButtonMode:
    GOSUB WindowDoShowTextCursor
    WHILE NOT ExitFlag
        GOSUB WindowDoMouse
        GOSUB WindowDoButtonKbd
    WEND
    GOSUB WindowDoHideTextCursor
RETURN

' ===========================================================================
' If startEdit>0 then go to edit mode.  Here we also wait for a mouse event
' or kbd event to flip the ExitFlag, but in the mean time, we trap the
' alphanumberic keys, and arrow keys, and use them to edit the current
' edit field.  (StartEdit is the current edit field.)  Again, there's no magic.
' (well maybe just a little...)
' ===========================================================================

WindowDoEditMode:
    GOSUB WindowDoEditInit
    WHILE NOT ExitFlag
        GOSUB WindowDoMouse
        GOSUB WindowDoEditKbd
    WEND
    GOSUB WindowDoEditExit
RETURN

' ===========================================================================
' Set initial flags, determine where cursor should be located, and figure
' out which mode we should be in (edit mode or button mode)
' ===========================================================================

WindowDoInit:

    ' =======================================================================
    ' Simply abort if there is no window open.
    ' =======================================================================

    windo = WindowCurrent
    IF windo = 0 THEN EXIT SUB

    REDIM HSClick(MAXHOTSPOT) AS hotSpotType
    REDIM HSRel(MAXHOTSPOT) AS hotSpotType

    ExitFlag = FALSE
    ButtonHighLight = FALSE

    border$ = WindowBorder$(GloWindow(windo).windowType)
    WindowDoMode = 2

    ' =======================================================================
    ' If startEdit>0, assign the index value to currEditField, and set
    ' WindowDoMode to 1
    ' =======================================================================

    IF startEdit > 0 THEN
        index = FindEditField(startEdit)
        IF index > 0 THEN
            currEditField = index
            WindowDoMode = 1
            origCursorRow = GloWindow(windo).row1 + GloEdit(index).row - 1
            origCursorCol = GloWindow(windo).col1 + GloEdit(index).col - 1
        END IF
    END IF

    ' =======================================================================
    ' If start button>0, then set current cursor location properly
    ' =======================================================================

    IF startButton > 0 THEN
        index = FindButton(startButton)
        IF index > 0 THEN
            currButton = index
            origCursorRow = GloWindow(windo).row1 + GloButton(index).row1 - 1
            origCursorCol = GloWindow(windo).col1 + GloButton(index).col1

            ' ===============================================================
            ' For area buttons decrement the cursor position
            ' ===============================================================

            SELECT CASE GloButton(index).buttonType
                CASE 4
                    origCursorCol = origCursorCol - 1
                CASE ELSE
            END SELECT

        END IF
    END IF

    currCursorRow = origCursorRow
    currCursorCol = origCursorCol
RETURN

' ===========================================================================
' Checks buttons, editfields, etc. and stores where the hot spots are
' ===========================================================================

WindowDoComputeHotSpots:
    numHSClick = 0
    numHSRel = 0

    ' =======================================================================
    ' If upper left corder of border is "=", then that's a close box
    ' Furthermore, a close box is a release type event so store in HSRel
    ' =======================================================================

    IF MID$(border$, 1, 1) = "=" THEN
        numHSRel = numHSRel + 1
        HSRel(numHSRel).row1 = GloWindow(windo).row1 - 1
        HSRel(numHSRel).row2 = GloWindow(windo).row1 - 1
        HSRel(numHSRel).col1 = GloWindow(windo).col1 - 1
        HSRel(numHSRel).col2 = GloWindow(windo).col1 - 1
        HSRel(numHSRel).action = 4
        HSRel(numHSRel).misc = windo
    END IF

    ' =======================================================================
    ' If lower right corner is "+", then that's a re-size box
    ' Further more, a re-size box is a click event, so store in HSClick
    ' =======================================================================

    IF MID$(border$, 9, 1) = "+" THEN
        numHSClick = numHSClick + 1
        HSClick(numHSClick).row1 = GloWindow(windo).row2 + 1
        HSClick(numHSClick).row2 = GloWindow(windo).row2 + 1
        HSClick(numHSClick).col1 = GloWindow(windo).col2 + 1
        HSClick(numHSClick).col2 = GloWindow(windo).col2 + 1
        HSClick(numHSClick).action = 5
        HSClick(numHSClick).misc = 0
    END IF

    ' =======================================================================
    ' Likewise, a "°" chr$(176) is a move bar.  That's also a click event
    ' =======================================================================

    IF MID$(border$, 2, 1) = "°" THEN
        numHSClick = numHSClick + 1
        HSClick(numHSClick).row1 = GloWindow(windo).row1 - 1
        HSClick(numHSClick).row2 = GloWindow(windo).row1 - 1
        HSClick(numHSClick).col1 = GloWindow(windo).col1
        HSClick(numHSClick).col2 = GloWindow(windo).col2
        HSClick(numHSClick).action = 15
        HSClick(numHSClick).misc = 0
    END IF

    ' =======================================================================
    ' Buttons are click, and release events.
    ' Click, and the cursor goes there, and the button is highlighted.
    ' Release, and the selection is made
    ' =======================================================================

    IF GloStorage.numButtonsOpen > 0 THEN
        button = 0
        WHILE button < GloStorage.numButtonsOpen
            button = button + 1
            IF GloButton(button).windowHandle = windo THEN
                numHSClick = numHSClick + 1
                HSClick(numHSClick).row1 = GloWindow(windo).row1 + GloButton(button).row1 - 1
                HSClick(numHSClick).row2 = GloWindow(windo).row1 + GloButton(button).row1 - 1
                HSClick(numHSClick).col1 = GloWindow(windo).col1 + GloButton(button).col1 - 1
                HSClick(numHSClick).col2 = GloWindow(windo).col1 + GloButton(button).col1 + 2 + LEN(RTRIM$(GloButton(button).text$))
                HSClick(numHSClick).action = 1
                HSClick(numHSClick).misc = GloButton(button).handle
                HSClick(numHSClick).misc2 = GloButton(button).buttonType

                numHSRel = numHSRel + 1
                HSRel(numHSRel).row1 = GloWindow(windo).row1 + GloButton(button).row1 - 1
                HSRel(numHSRel).row2 = GloWindow(windo).row1 + GloButton(button).row1 - 1
                HSRel(numHSRel).col1 = GloWindow(windo).col1 + GloButton(button).col1 - 1
                HSRel(numHSRel).col2 = GloWindow(windo).col1 + GloButton(button).col1 + 2 + LEN(RTRIM$(GloButton(button).text$))
                HSRel(numHSRel).action = 1
                HSRel(numHSRel).misc = GloButton(button).handle
                HSRel(numHSRel).misc2 = GloButton(button).buttonType

                ' ===========================================================
                ' Adjust previous info to handle special cases for
                ' "field" buttons, and "scroll bar" buttons.
                ' ===========================================================

                SELECT CASE GloButton(button).buttonType
                    CASE 4
                        numHSRel = numHSRel - 1
                        HSClick(numHSClick).row2 = GloWindow(windo).row1 + GloButton(button).row2 - 1
                        HSClick(numHSClick).col2 = GloWindow(windo).col1 + GloButton(button).col2 - 1
                    CASE 5
                        numHSClick = numHSClick - 1
                        HSRel(numHSRel).row2 = GloWindow(windo).row1 + GloButton(button).row2 - 1
                        HSRel(numHSRel).col2 = GloWindow(windo).col1 + GloButton(button).col2 - 1
                    CASE 6
                        numHSRel = numHSRel - 1
                        HSClick(numHSClick).row2 = GloWindow(windo).row1 + GloButton(button).row2 - 1
                        HSClick(numHSClick).col2 = GloWindow(windo).col1 + GloButton(button).col1 - 1
                    CASE 7
                        numHSRel = numHSRel - 1
                        HSClick(numHSClick).row2 = GloWindow(windo).row1 + GloButton(button).row1 - 1
                        HSClick(numHSClick).col2 = GloWindow(windo).col1 + GloButton(button).col2 - 1
                    CASE ELSE
                END SELECT
            END IF
        WEND
    END IF

    ' =======================================================================
    ' EditFields are Click events
    ' =======================================================================

    IF GloStorage.numEditFieldsOpen > 0 THEN
        editField = 0
        WHILE editField < GloStorage.numEditFieldsOpen
            editField = editField + 1
            IF GloEdit(editField).windowHandle = windo THEN
                numHSClick = numHSClick + 1
                HSClick(numHSClick).row1 = GloWindow(windo).row1 + GloEdit(editField).row - 1
                HSClick(numHSClick).row2 = GloWindow(windo).row1 + GloEdit(editField).row - 1
                HSClick(numHSClick).col1 = GloWindow(windo).col1 + GloEdit(editField).col - 1
                HSClick(numHSClick).col2 = GloWindow(windo).col1 + GloEdit(editField).col + GloEdit(editField).visLength - 1
                HSClick(numHSClick).action = 2
                HSClick(numHSClick).misc = GloEdit(editField).handle
            END IF
        WEND
    END IF

    ' =======================================================================
    ' Feel free to add your own hot spots!  One good idea is if the
    ' right hand side of the border is ², make that a scroll bar!  Adding
    ' that would be good practice -- and fun too!
    ' =======================================================================

RETURN

' ===========================================================================
' Polls the mouse
' ===========================================================================

WindowDoMouse:

    MousePoll MouseRow, MouseCol, lButton, rButton
   
    ' =======================================================================
    ' If lButton is down, then keep checking for click events until it's released
    ' =======================================================================

    IF lButton THEN
        WHILE lButton AND MouseRow <> 1 AND NOT ExitFlag
            GOSUB WindowDoCheckClickEvent
            IF Found THEN
                GOSUB WindowDoClickEvent
            END IF

            MousePoll MouseRow, MouseCol, lButton, rButton
        WEND

        ' ===================================================================
        ' If the button was released (and no click event occured) then check
        ' for a release event!
        ' ===================================================================

        IF NOT lButton AND MouseRow <> 1 AND NOT ExitFlag THEN
            GOSUB WindowDoCheckReleaseEvent
            IF Found THEN
                GOSUB WindowDoReleaseEvent
            ELSE

                ' ===========================================================
                ' If no release event, then see if mouse was released in another
                ' window.  This is a special case release event
                ' ===========================================================

                GOSUB WindowDoCheckOtherWindow
            END IF

            ' ===============================================================
            ' Un highlight the button if the mouse was released for any reason
            ' ===============================================================

            GOSUB WindowDoUnHighlightButton

        END IF
     

    END IF

    ' =======================================================================
    ' If in button mode, return cursor to original spot.
    ' =======================================================================

    IF WindowDoMode = 2 THEN
        currCursorRow = origCursorRow
        currCursorCol = origCursorCol
        GOSUB WindowDoShowTextCursor
    END IF
    
RETURN

' ===========================================================================
' Used only in Button mode.  Checks for menu event with MenuInkey$,
' then checks for all the misc events.  See below
' If an event is found, the proper event ID is stored, and ExifFlag is set
' ===========================================================================

WindowDoButtonKbd:

    ' =======================================================================
    ' Only check menu if window type > 0.
    ' =======================================================================

    IF GloWindow(windo).windowType < 0 THEN
        kbd$ = INKEY$
    ELSE
        kbd$ = MenuInkey$
    END IF

    ' =======================================================================
    ' The following is a list of key strokes that can be detected. You can
    ' add more as needed, but you will need to change any programs that use
    ' the existing configuration.  The numbers associated with each key are
    ' the numbers that are returned by Dialog(0).
    ' =======================================================================

    SELECT CASE kbd$
        CASE CHR$(13)
            GloStorage.oldDialogEvent = 6          'Return
            ExitFlag = TRUE
        CASE CHR$(9)
            GloStorage.oldDialogEvent = 7          'Tab
            ExitFlag = TRUE
        CASE CHR$(0) + CHR$(15)
            GloStorage.oldDialogEvent = 8          'Back Tab
            ExitFlag = TRUE
        CASE CHR$(27)
            GloStorage.oldDialogEvent = 9          'Escape
            ExitFlag = TRUE
        CASE CHR$(0) + "H"
            GloStorage.oldDialogEvent = 10         'Up
            ExitFlag = TRUE
        CASE CHR$(0) + "P"
            GloStorage.oldDialogEvent = 11         'Down
            ExitFlag = TRUE
        CASE CHR$(0) + "K"
            GloStorage.oldDialogEvent = 12         'Left
            ExitFlag = TRUE
        CASE CHR$(0) + "M"
            GloStorage.oldDialogEvent = 13         'Right
            ExitFlag = TRUE
        CASE " "
            GloStorage.oldDialogEvent = 14         'Space
            ExitFlag = TRUE
        CASE CHR$(0) + "G"
            GloStorage.oldDialogEvent = 16         'Home
            ExitFlag = TRUE
        CASE CHR$(0) + "O"
            GloStorage.oldDialogEvent = 17         'End
            ExitFlag = TRUE
        CASE CHR$(0) + "I"
            GloStorage.oldDialogEvent = 18         'PgUp
            ExitFlag = TRUE
        CASE CHR$(0) + "Q"
            GloStorage.oldDialogEvent = 19         'PgDn
            ExitFlag = TRUE
        CASE "menu"
            GloStorage.oldDialogEvent = 20         'Menu
            ExitFlag = TRUE
        CASE ELSE
    END SELECT
RETURN

' ===========================================================================
' Checks mouseRow, mouseCol against all the click events stored in HSClick
' ===========================================================================

WindowDoCheckClickEvent:
    currEvent = 1
    Found = FALSE

    WHILE NOT Found AND currEvent <= numHSClick
        IF MouseRow >= HSClick(currEvent).row1 AND MouseRow <= HSClick(currEvent).row2 AND MouseCol >= HSClick(currEvent).col1 AND MouseCol <= HSClick(currEvent).col2 THEN
            Found = TRUE
        ELSE
            currEvent = currEvent + 1
        END IF
    WEND

    IF NOT Found THEN
        GOSUB WindowDoUnHighlightButton
    END IF

RETURN

' ===========================================================================
' Checks mouseRow,mouseCol against all the release events in HSRel
' ===========================================================================

WindowDoCheckReleaseEvent:
    currEvent = 1
    Found = FALSE

    WHILE NOT Found AND currEvent <= numHSRel
        IF MouseRow >= HSRel(currEvent).row1 AND MouseRow <= HSRel(currEvent).row2 AND MouseCol >= HSRel(currEvent).col1 AND MouseCol <= HSRel(currEvent).col2 THEN
            Found = TRUE
        ELSE
            currEvent = currEvent + 1
        END IF
    WEND
RETURN

' ===========================================================================
' Calls WhichWindow to see if mouseRow, mouseCol is in another window
' If it is, that's event ID #3, so set it, and set ExitFlag to TRUE
' ===========================================================================

WindowDoCheckOtherWindow:
    IF GloWindow(windo).windowType > 0 THEN
        otherWindow = WhichWindow(MouseRow, MouseCol)
        IF otherWindow AND (otherWindow <> windo) THEN
            GloStorage.oldDialogEvent = 3
            GloStorage.oldDialogWindow = otherWindow
            ExitFlag = TRUE
        END IF
    END IF
RETURN
     
' ===========================================================================
' If there was a release event, this routine handles it
' ===========================================================================

WindowDoReleaseEvent:

    SELECT CASE HSRel(currEvent).action
        CASE 1                                      'Released on Button
            GloStorage.oldDialogEvent = 1
            GloStorage.oldDialogButton = HSRel(currEvent).misc
            ExitFlag = TRUE
        CASE 4                                      'Released on Close Box
            GloStorage.oldDialogEvent = 4
            GloStorage.oldDialogClose = HSRel(currEvent).misc
            ExitFlag = TRUE
        CASE ELSE
    END SELECT
RETURN

' ===========================================================================
' If there was a click event, this routine handles it
' ===========================================================================

WindowDoClickEvent:

    SELECT CASE HSClick(currEvent).action
        CASE 1                                          'ButtonClick
            SELECT CASE HSClick(currEvent).misc2
                CASE 1
                    IF ButtonHighLight THEN
                        IF currButton <> HSClick(currEvent).misc THEN
                            ButtonSetState currButton, origState
                            currButton = HSClick(currEvent).misc
                            ButtonSetState currButton, 3
                        END IF
                    ELSE
                        currButton = HSClick(currEvent).misc
                        origState = ButtonInquire(currButton)
                        ButtonHighLight = TRUE
                        ButtonSetState currButton, 3
                    END IF
              
                    currCursorRow = HSClick(currEvent).row1
                    currCursorCol = HSClick(currEvent).col1 + 1
                    GOSUB WindowDoShowTextCursor
                CASE 2, 3
                    currCursorRow = HSClick(currEvent).row1
                    currCursorCol = HSClick(currEvent).col1 + 1
                    GOSUB WindowDoShowTextCursor
                CASE 4
                    IF ButtonHighLight THEN
                        ButtonSetState currButton, origState
                    END IF

                    GloStorage.oldDialogEvent = 1
                    GloStorage.oldDialogButton = HSClick(currEvent).misc
                    GloStorage.oldDialogRow = MouseRow - HSClick(currEvent).row1 + 1
                    GloStorage.oldDialogCol = MouseCol - HSClick(currEvent).col1 + 1
                    ExitFlag = TRUE
                CASE 6
                    GloStorage.oldDialogEvent = 1
                    GloStorage.oldDialogButton = HSClick(currEvent).misc

                    IF MouseRow = HSClick(currEvent).row1 THEN
                        GloStorage.oldDialogScroll = -1
                    ELSEIF MouseRow = HSClick(currEvent).row2 THEN
                        GloStorage.oldDialogScroll = -2
                    ELSE
                        GloStorage.oldDialogScroll = MouseRow - HSClick(currEvent).row1
                    END IF

                    ExitFlag = TRUE
                CASE 7
                    GloStorage.oldDialogEvent = 1
                    GloStorage.oldDialogButton = HSClick(currEvent).misc

                    IF MouseCol = HSClick(currEvent).col1 THEN
                        GloStorage.oldDialogScroll = -1
                    ELSEIF MouseCol = HSClick(currEvent).col2 THEN
                        GloStorage.oldDialogScroll = -2
                    ELSE
                        GloStorage.oldDialogScroll = MouseCol - HSClick(currEvent).col1
                    END IF

                    ExitFlag = TRUE
                CASE ELSE
            END SELECT
        CASE 2                                      'Edit Field Click
            GloStorage.oldDialogEvent = 2           'Event ID #2
            GloStorage.oldDialogEdit = HSClick(currEvent).misc
            ExitFlag = TRUE
        CASE 5
            GOSUB WindowDoSize                      'Internally handle Re-Size
            ExitFlag = TRUE
            GloStorage.oldDialogEvent = 5
        CASE 15
            GOSUB WindowDoHideTextCursor
            GOSUB WindowDoMove                      'Internally handle Move
            ExitFlag = TRUE
            GloStorage.oldDialogEvent = 15
        CASE ELSE
            
    END SELECT

    IF HSClick(currEvent).action <> 1 THEN
        GOSUB WindowDoUnHighlightButton
    END IF

RETURN

' ===========================================================================
' Un-highlight a button
' ===========================================================================

WindowDoUnHighlightButton:
    IF ButtonHighLight THEN
        ButtonSetState currButton, origState
        ButtonHighLight = FALSE
        GOSUB WindowDoShowTextCursor
    END IF
RETURN
   
' ===========================================================================
' Handle the move window click -- drag the window around until button released
' ===========================================================================

WindowDoMove:
    MouseHide
    WindowSave windo
    BackgroundRefresh windo
    IF INSTR(WindowBorder$(GloWindow(windo).windowType), "S") THEN
      WindowShadowRefresh
    END IF

    oldWinrow1 = GloWindow(windo).row1
    oldWincol1 = GloWindow(windo).col1
    oldWinrow2 = GloWindow(windo).row2
    oldWincol2 = GloWindow(windo).col2

    GOSUB DrawRubberBand

    WindowPrintTitle
    MouseShow

    MouseBorder MINROW, (MouseCol - GloWindow(windo).col1 + 1 + MINCOL), (MAXROW - WindowRows(windo) - 1), (MAXCOL - (GloWindow(windo).col2 - MouseCol) - 1)
    
    oldMouseRow = MouseRow
    oldMouseCol = MouseCol

    DO
        MousePoll MouseRow, MouseCol, lButton, rButton
        IF MouseRow <> oldMouseRow OR MouseCol <> oldMouseCol THEN
            MouseHide

            GOSUB EraseRubberBand

            oldWinrow1 = oldWinrow1 - oldMouseRow + MouseRow
            oldWinrow2 = oldWinrow2 - oldMouseRow + MouseRow
            oldWincol1 = oldWincol1 - oldMouseCol + MouseCol
            oldWincol2 = oldWincol2 - oldMouseCol + MouseCol
          
            oldMouseRow = MouseRow
            oldMouseCol = MouseCol
           
            GOSUB DrawRubberBand
            MouseShow
        END IF

    LOOP UNTIL NOT lButton

    MouseHide
    GOSUB EraseRubberBand
    GloWindow(windo).row1 = oldWinrow1
    GloWindow(windo).row2 = oldWinrow2
    GloWindow(windo).col1 = oldWincol1
    GloWindow(windo).col2 = oldWincol2
    BackgroundSave windo
    WindowRefresh windo
    IF INSTR(WindowBorder$(GloWindow(windo).windowType), "S") THEN
        WindowShadowSave
    END IF
    GloBuffer$(windo, 2) = ""
    MouseShow
    MouseBorder 1, 1, MAXROW, MAXCOL
    GOSUB WindowDoComputeHotSpots
RETURN

' ===========================================================================
' Re-Size window -- Drag box around until button released, then exit
' with eventID #5  -- Window need refreshing
' ===========================================================================

WindowDoSize:
    ButtonClose 0
    MouseHide
    WindowSave windo

    ' ======================================================================
    ' Comment out the next line if you want to retain the window contents
    ' while resizing the window.
    ' ======================================================================

    BackgroundRefresh windo

    IF INSTR(WindowBorder$(GloWindow(windo).windowType), "S") THEN
      WindowShadowRefresh
    END IF
    
    oldWinrow1 = GloWindow(windo).row1
    oldWincol1 = GloWindow(windo).col1
    oldWinrow2 = GloWindow(windo).row2
    oldWincol2 = GloWindow(windo).col2

    GOSUB DrawRubberBand

    MouseShow
    MouseBorder GloWindow(windo).row1 + 3, GloWindow(windo).col1 + 6, MAXROW, MAXCOL

    oldMouseRow = MouseRow
    oldMouseCol = MouseCol

    DO
        MousePoll MouseRow, MouseCol, lButton, rButton
        IF MouseRow <> oldMouseRow OR MouseCol <> oldMouseCol THEN
            MouseHide

            GOSUB EraseRubberBand

            oldWinrow2 = oldWinrow2 - oldMouseRow + MouseRow
            oldWincol2 = oldWincol2 - oldMouseCol + MouseCol
          
            oldMouseRow = MouseRow
            oldMouseCol = MouseCol
           
            GOSUB DrawRubberBand
            MouseShow
        END IF
    LOOP UNTIL NOT lButton

    MouseHide
    GOSUB EraseRubberBand
    WindowShadowRefresh
    BackgroundRefresh windo
    GloWindow(windo).row2 = oldWinrow2
    GloWindow(windo).col2 = oldWincol2
    BackgroundSave windo
    Box GloWindow(windo).row1 - 1, GloWindow(windo).col1 - 1, GloWindow(windo).row2 + 1, GloWindow(windo).col2 + 1, GloWindow(windo).fore, GloWindow(windo).back, WindowBorder$(GloWindow(windo).windowType), 0
    GloBuffer$(windo, 2) = ""
    WindowPrintTitle

    IF INSTR(WindowBorder$(GloWindow(windo).windowType), "S") THEN
        WindowShadowSave
    END IF
    MouseShow

    MouseBorder 1, 1, MAXROW, MAXCOL
RETURN

' ===========================================================================
' Draw rubber band of current window
' ===========================================================================

DrawRubberBand:
  GetBackground oldWinrow1 - 1, oldWincol1 - 1, oldWinrow1 - 1, oldWincol2 + 1, RB$(1)
  GetBackground oldWinrow2 + 1, oldWincol1 - 1, oldWinrow2 + 1, oldWincol2 + 1, RB$(2)
  GetBackground oldWinrow1 - 1, oldWincol1 - 1, oldWinrow2 + 1, oldWincol1 - 1, RB$(3)
  GetBackground oldWinrow1 - 1, oldWincol2 + 1, oldWinrow2 + 1, oldWincol2 + 1, RB$(4)
  Box oldWinrow1 - 1, oldWincol1 - 1, oldWinrow2 + 1, oldWincol2 + 1, GloWindow(windo).highlight, GloWindow(windo).back, WindowBorder$(GloWindow(windo).windowType), 0
RETURN

' ===========================================================================
' Erase rubber band of current window
' ===========================================================================

EraseRubberBand:
  PutBackground oldWinrow1 - 1, oldWincol1 - 1, RB$(1)
  PutBackground oldWinrow2 + 1, oldWincol1 - 1, RB$(2)
  PutBackground oldWinrow1 - 1, oldWincol1 - 1, RB$(3)
  PutBackground oldWinrow1 - 1, oldWincol2 + 1, RB$(4)
  RETURN

WindowDoHideTextCursor:
    LOCATE , , 0
RETURN


WindowDoShowTextCursor:
    IF currCursorRow <> 0 AND currCursorCol <> 0 THEN
        LOCATE currCursorRow, currCursorCol, 1
    ELSE
        LOCATE , , 0
    END IF
RETURN

' ===========================================================================
' If in edit mode, this routine gets info from the global arrays
' ===========================================================================

WindowDoEditInit:
    row = GloWindow(windo).row1 + GloEdit(currEditField).row - 1
    col = GloWindow(windo).col1 + GloEdit(currEditField).col - 1
    fore = GloEdit(currEditField).fore
    back = GloEdit(currEditField).back
    visLength = GloEdit(currEditField).visLength
    maxLength = GloEdit(currEditField).maxLength
    editField$ = LEFT$(GloEdit(currEditField).text$, maxLength)
    insertMode = TRUE
    InsertKey = GetShiftState(7)
    
    ' =======================================================================
    ' Make sure everything's the right length
    ' =======================================================================

    x = INSTR(editField$, CHR$(0)) - 1
    IF x >= 0 THEN
        editField$ = LEFT$(editField$, x)
    END IF

    IF LEN(editField$) >= visLength THEN
        firstchar = LEN(editField$) - visLength + 2
        cursor = visLength - 1
    ELSE
        firstchar = 1
        cursor = LEN(editField$)
    END IF

    GOSUB WindowDoEditPrint

RETURN

' ===========================================================================
' Handles the edit kbd event trapping.  Some keys trigger events
' (e.g. TAB is event ID #7)  Others affect the current edit field string (DEL)
' ===========================================================================

WindowDoEditKbd:
    IF GetShiftState(7) = InsertKey THEN
        insertMode = TRUE
        LOCATE , , , 6, 7
    ELSE
        insertMode = FALSE
        LOCATE , , , 0, 7
    END IF

    LOCATE row, col + cursor, 1

    GOSUB WindowDoMouse

    ' =======================================================================
    ' Only call MenuInkey$ if menuType > 0
    ' =======================================================================

    IF GloWindow(windo).windowType < 0 THEN
        kbd$ = INKEY$
    ELSE
        kbd$ = MenuInkey$
    END IF

    ' =======================================================================
    ' Either key is an event, and the exitFlag is set, or something happens
    ' to the current edit string.
    ' =======================================================================

    SELECT CASE kbd$
        CASE CHR$(13)
            GloStorage.oldDialogEvent = 6          'Return
            ExitFlag = TRUE
        CASE CHR$(9)
            GloStorage.oldDialogEvent = 7          'Tab
            ExitFlag = TRUE
        CASE CHR$(0) + CHR$(15)
            GloStorage.oldDialogEvent = 8          'Back Tab
            ExitFlag = TRUE
        CASE CHR$(27)
            GloStorage.oldDialogEvent = 9          'Escape
            ExitFlag = TRUE
        CASE CHR$(0) + "H"
            GloStorage.oldDialogEvent = 10         'Up
            ExitFlag = TRUE
        CASE CHR$(0) + "P"
            GloStorage.oldDialogEvent = 11         'Down
            ExitFlag = TRUE
        CASE CHR$(0) + "M"                         'Right
            GOSUB WindowDoEditRight
        CASE CHR$(0) + "K"
            cursor = cursor - 1
            IF cursor < 0 THEN
                cursor = cursor + 1
                IF firstchar > 1 THEN
                    firstchar = firstchar - 1
                    GOSUB WindowDoEditPrint
                END IF
            END IF

        CASE CHR$(0) + "S"
            IF cursor + firstchar <= LEN(editField$) THEN
                editField$ = LEFT$(editField$, cursor + firstchar - 1) + RIGHT$(editField$, LEN(editField$) - (cursor + firstchar))
                GOSUB WindowDoEditPrint
            END IF
        CASE CHR$(8)
            IF firstchar + cursor > 1 THEN
                editField$ = LEFT$(editField$, cursor + firstchar - 2) + RIGHT$(editField$, LEN(editField$) - (cursor + firstchar) + 1)
                GOSUB WindowDoEditPrint
                SELECT CASE cursor
                    CASE 0
                        firstchar = firstchar - 1
                        GOSUB WindowDoEditPrint
                    CASE 1
                        IF firstchar > 1 THEN
                            firstchar = firstchar - 1
                            GOSUB WindowDoEditPrint
                        ELSE
                            cursor = cursor - 1
                        END IF
                    CASE ELSE
                        cursor = cursor - 1
                END SELECT
            END IF
        CASE CHR$(0) + "G"                         'Home
            firstchar = 1
            cursor = 0
            GOSUB WindowDoEditPrint
        CASE CHR$(0) + "O"                         'End
            IF LEN(editField$) >= visLength THEN
                cursor = visLength - 1
                firstchar = LEN(editField$) - visLength + 2
                GOSUB WindowDoEditPrint
            ELSE
                firstchar = 1
                cursor = LEN(editField$)
            END IF
        CASE CHR$(0) + "u"                         'Ctrl+end
            editField$ = LEFT$(editField$, firstchar + cursor - 1)
            GOSUB WindowDoEditPrint
        CASE "menu"
            GloStorage.oldDialogEvent = 20         'Menu
            ExitFlag = TRUE

        CASE CHR$(32) TO CHR$(255)                 'Alphanumeric
            IF insertMode THEN
                IF LEN(editField$) < maxLength THEN
                    editField$ = LEFT$(editField$, cursor + firstchar - 1) + kbd$ + RIGHT$(editField$, LEN(editField$) - (cursor + firstchar) + 1)
                    GOSUB WindowDoEditPrint
                    GOSUB WindowDoEditRight
                ELSE
                    BEEP
                END IF
            ELSE
                IF cursor + firstchar > LEN(editField$) THEN
                    IF LEN(editField$) < maxLength THEN
                        editField$ = editField$ + kbd$
                        MouseHide
                        PRINT kbd$;
                        MouseShow
                    END IF
                ELSE
                    MID$(editField$, cursor + firstchar, 1) = kbd$
                    MouseHide
                    PRINT kbd$;
                    MouseShow
                END IF
                       
                GOSUB WindowDoEditRight
            END IF

    END SELECT
RETURN

' ===========================================================================
' Moves the cursor right 1 space.  This is used twice, so it is its own
' routine
' ===========================================================================

WindowDoEditRight:
    cursor = cursor + 1
    IF cursor + firstchar - 1 > LEN(editField$) THEN
        cursor = cursor - 1
    ELSEIF cursor + firstchar - 1 > maxLength THEN
        cursor = cursor - 1
    ELSEIF cursor = visLength THEN
        firstchar = firstchar + 1
        cursor = cursor - 1
        GOSUB WindowDoEditPrint
    END IF
RETURN

' ===========================================================================
' Upon exit, store the current edit field string back into the global array
' ===========================================================================

WindowDoEditExit:
    GloEdit(currEditField).text$ = editField$ + CHR$(0)
    LOCATE , , 0, 6, 7
RETURN

' ===========================================================================
' Prints the edit field in the proper color, at the proper location
' ===========================================================================

WindowDoEditPrint:
    MouseHide
    COLOR fore, back
    LOCATE row, col
    PRINT MID$(editField$ + SPACE$(visLength), firstchar, visLength);
    MouseShow
RETURN

END SUB

SUB WindowInit

    ' =======================================================================
    ' Initialize totals
    ' =======================================================================

    GloStorage.currWindow = -1
    GloStorage.numWindowsOpen = 0
    GloStorage.numButtonsOpen = 0
    GloStorage.numEditFieldsOpen = 0

    ' =======================================================================
    ' Clear all windows
    ' =======================================================================

    FOR A = 1 TO MAXWINDOW
        GloWindow(A).handle = -1
        GloWindow(A).row1 = 0
        GloWindow(A).col1 = 0
        GloWindow(A).row2 = 0
        GloWindow(A).col2 = 0
        GloWindow(A).fore = 0
        GloWindow(A).back = 0
        GloWindow(A).windowType = 0
        GloWindow(A).title = ""
        GloWindowStack(A) = -1
    NEXT A

    ' =======================================================================
    ' Clear all buttons
    ' =======================================================================

    FOR A = 1 TO MAXBUTTON
        GloButton(A).handle = -1
        GloButton(A).windowHandle = -1
        GloButton(A).text = ""
        GloButton(A).state = 0
        GloButton(A).buttonOn = FALSE
        GloButton(A).row1 = 0
        GloButton(A).col1 = 0
        GloButton(A).row2 = 0
        GloButton(A).col2 = 0
        GloButton(A).buttonType = 0
    NEXT A

    ' =======================================================================
    ' Clear all edit fields
    ' =======================================================================

    FOR A = 1 TO MAXEDITFIELD
        GloEdit(A).handle = 0
        GloEdit(A).windowHandle = 0
        GloEdit(A).text = ""
        GloEdit(A).row = 0
        GloEdit(A).col = 0
        GloEdit(A).visLength = 0
        GloEdit(A).maxLength = 0
        GloEdit(A).fore = 0
        GloEdit(A).back = 0
    NEXT A

END SUB

SUB WindowLine (row)

    ' =======================================================================
    ' If window is valid, draw a horizontal line at the row which is passed
    ' =======================================================================

    windo = WindowCurrent
    
    IF windo > 0 THEN
        IF row >= 1 OR row <= WindowRows(windo) THEN

            topRow = GloWindow(windo).row1
            leftCol = GloWindow(windo).col1 - 1
            rightCol = GloWindow(windo).col2 + 1
            border$ = WindowBorder$(GloWindow(windo).windowType)

            LOCATE topRow + row - 1, leftCol
            MouseHide
            COLOR GloWindow(windo).fore, GloWindow(windo).back

            IF MID$(border$, 11, 1) = " " THEN
                PRINT STRING$(rightCol - leftCol + 1, CHR$(196))
            ELSE
                PRINT MID$(border$, 10, 1); STRING$(rightCol - leftCol - 1, MID$(border$, 11, 1)); MID$(border$, 12, 1)
            END IF
           
            MouseShow
        END IF
    END IF

END SUB

SUB WindowLocate (row, col)

    ' =======================================================================
    ' If window is valid, assign the passed row and col to the global variables
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN
        GloWindow(windo).cursorRow = row
        GloWindow(windo).cursorCol = col
    END IF

END SUB

FUNCTION WindowNext
        
    ' =======================================================================
    ' Loop through window array, and find first unused window, return handle
    ' If no window found, return 0
    ' =======================================================================

    Found = FALSE
    A = 1
    WHILE A <= MAXWINDOW AND NOT Found
        IF GloWindow(A).handle = -1 THEN
            Found = TRUE
        ELSE
            A = A + 1
        END IF
    WEND

    IF Found THEN
        WindowNext = A
    ELSE
        WindowNext = 0
    END IF

END FUNCTION

SUB WindowOpen (handle, row1, col1, row2, col2, textFore, textBack, fore, back, highlight, movewin, closewin, sizewin, modalwin, borderchar, title$)

    ' =======================================================================
    ' Open Window!   First make sure coordinates are valid
    ' =======================================================================
    IF row1 > row2 THEN SWAP row1, row2
    IF col1 > col2 THEN SWAP col1, col2

    IF col1 >= MINCOL + 1 AND row1 >= MINROW + 1 AND col2 <= MAXCOL - 1 AND row2 <= MAXROW - 1 THEN

        ' ===================================================================
        ' Close window by save number if it already exists
        ' ===================================================================

        WindowClose handle

        ' ===================================================================
        ' Evaluate argument list to determine windowType
        ' ===================================================================

        IF movewin THEN windowType = 1
        IF closewin THEN windowType = windowType + 2
        IF sizewin THEN windowType = windowType + 4
        IF borderchar = 1 THEN windowType = windowType + 8
        IF borderchar = 2 THEN windowType = windowType + 16
        IF windowType = 0 THEN windowType = 99
        IF modalwin THEN windowType = -windowType

        border$ = WindowBorder(windowType)

        ' ===================================================================
        ' hide current window's shadow if it has one
        ' ===================================================================

        MouseHide
        IF GloStorage.numWindowsOpen > 0 THEN
            IF INSTR(WindowBorder$(GloWindow(GloWindowStack(GloStorage.numWindowsOpen)).windowType), "S") THEN
                WindowShadowRefresh
            END IF
        END IF

        ' ===================================================================
        ' Assign new values to window array
        ' ===================================================================

        GloWindow(handle).handle = handle
        GloWindow(handle).row1 = row1
        GloWindow(handle).col1 = col1
        GloWindow(handle).row2 = row2
        GloWindow(handle).col2 = col2
        GloWindow(handle).cursorRow = 1
        GloWindow(handle).cursorCol = 1
        GloWindow(handle).fore = fore
        GloWindow(handle).back = back
        GloWindow(handle).textFore = textFore
        GloWindow(handle).textBack = textBack
        GloWindow(handle).highlight = highlight
        GloWindow(handle).windowType = windowType
        GloWindow(handle).title = title$

        ' ===================================================================
        ' Save background, then draw window
        ' ===================================================================

        BackgroundSave handle
        Box row1 - 1, col1 - 1, row2 + 1, col2 + 1, fore, back, border$, 1
        MouseShow

        ' ===================================================================
        ' Assign handle to currWindow, incr total windows, push handle on stack
        ' ===================================================================

        GloStorage.currWindow = handle
        GloStorage.numWindowsOpen = GloStorage.numWindowsOpen + 1
        GloWindowStack(GloStorage.numWindowsOpen) = handle

        ' ===================================================================
        ' Print window title, and shadow
        ' ===================================================================

        WindowPrintTitle
        IF INSTR(border$, "S") THEN
            WindowShadowSave
        END IF
    END IF

END SUB

SUB WindowPrint (printMode, text$)

    ' =======================================================================
    ' If window is valid, print text$ using mode printMode%.  See
    ' External documentation for details on printMode%
    ' =======================================================================

    windo = WindowCurrent

    IF windo > 0 THEN
        SELECT CASE printMode

            ' ===============================================================
            ' Truncate printing
            ' ===============================================================

            CASE 1, -1
                length = WindowCols(windo) - GloWindow(windo).cursorCol + 1
                LOCATE GloWindow(windo).row1 + GloWindow(windo).cursorRow - 1, GloWindow(windo).col1 + GloWindow(windo).cursorCol - 1
                COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                MouseHide
                PRINT LEFT$(text$, length);
                MouseShow
                IF printMode < 0 THEN
                    GloWindow(windo).cursorCol = GloWindow(windo).cursorCol + LEN(text$)
                    IF GloWindow(windo).cursorCol > WindowCols(windo) THEN
                        GloWindow(windo).cursorCol = WindowCols(windo) + 1
                    END IF
                ELSE
                    GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                    GloWindow(windo).cursorCol = 1
                    IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                        WindowScroll 1
                        GloWindow(windo).cursorRow = WindowRows(windo)
                    END IF
                END IF
            ' ===============================================================
            ' Character wrapping
            ' ===============================================================

            CASE 2, -2
                COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                A$ = text$
                WHILE LEN(A$) > 0
                    length = WindowCols(windo) - GloWindow(windo).cursorCol + 1
                    LOCATE GloWindow(windo).row1 + GloWindow(windo).cursorRow - 1, GloWindow(windo).col1 + GloWindow(windo).cursorCol - 1

                    MouseHide
                    PRINT LEFT$(A$, length);
                    MouseShow

                    IF length < LEN(A$) THEN
                        A$ = RIGHT$(A$, LEN(A$) - length)
                        GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                        GloWindow(windo).cursorCol = 1
                        IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                            WindowScroll 1
                            GloWindow(windo).cursorRow = WindowRows(windo)
                        END IF
                    ELSE
                        IF printMode < 0 THEN
                            GloWindow(windo).cursorCol = GloWindow(windo).cursorCol + LEN(A$)
                            IF GloWindow(windo).cursorCol > WindowCols(windo) THEN
                                GloWindow(windo).cursorCol = WindowCols(windo) + 1
                            END IF
                        ELSE
                            GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                            GloWindow(windo).cursorCol = GloWindow(windo).cursorCol
                            IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                                WindowScroll 1
                                GloWindow(windo).cursorRow = WindowRows(windo)
                            END IF
                        END IF
                        A$ = ""
                    END IF
                WEND

            ' ===============================================================
            ' Word wrapping
            ' ===============================================================

            CASE 3, -3
                COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                A$ = text$
                WHILE LEN(A$) > 0
                    length = WindowCols(windo) - GloWindow(windo).cursorCol + 1
                    LOCATE GloWindow(windo).row1 + GloWindow(windo).cursorRow - 1, GloWindow(windo).col1 + GloWindow(windo).cursorCol - 1

                    IF length < LEN(A$) THEN
                        x = length + 1
                        b$ = " " + A$
                        WHILE MID$(b$, x, 1) <> " "
                            x = x - 1
                        WEND
                        x = x - 1

                        MouseHide
                        IF x = 0 THEN
                            PRINT LEFT$(A$, length);
                            A$ = RIGHT$(A$, LEN(A$) - length)
                        ELSE
                            PRINT LEFT$(A$, x);
                            A$ = RIGHT$(A$, LEN(A$) - x)
                        END IF
                        MouseShow

                        x = 1
                        b$ = A$ + " "
                        WHILE MID$(b$, x, 1) = " "
                            x = x + 1
                        WEND

                        IF x = LEN(b$) THEN
                            A$ = ""
                        ELSEIF x > 1 THEN
                            A$ = RIGHT$(A$, LEN(A$) - x + 1)
                        END IF
                           
                        GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                        GloWindow(windo).cursorCol = 1
                        IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                            WindowScroll 1
                            GloWindow(windo).cursorRow = WindowRows(windo)
                        END IF
                    ELSE

                    MouseHide
                    PRINT LEFT$(A$, length);
                    MouseShow
                        IF printMode < 0 THEN
                            GloWindow(windo).cursorCol = GloWindow(windo).cursorCol + LEN(A$)
                            IF GloWindow(windo).cursorCol > WindowCols(windo) THEN
                                GloWindow(windo).cursorCol = WindowCols(windo) + 1
                            END IF
                        ELSE
                            GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                            GloWindow(windo).cursorCol = GloWindow(windo).cursorCol
                            IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                                WindowScroll 1
                                GloWindow(windo).cursorRow = WindowRows(windo)
                            END IF
                        END IF
                        A$ = ""
                    END IF
                WEND

            ' ===============================================================
            ' Centered text printing
            ' ===============================================================

            CASE 4
                COLOR GloWindow(windo).textFore, GloWindow(windo).textBack
                IF LEN(text$) >= WindowCols(windo) THEN
                    LOCATE GloWindow(windo).row1 + GloWindow(windo).cursorRow - 1, GloWindow(windo).col1
                    MouseHide
                    PRINT LEFT$(text$, length);
                    MouseShow
                ELSE
                    LOCATE GloWindow(windo).row1 + GloWindow(windo).cursorRow - 1, GloWindow(windo).col1 - 1 + INT((WindowCols(windo) / 2) + .9) - LEN(text$) / 2
                    MouseHide
                    PRINT text$
                    MouseShow
                END IF

                GloWindow(windo).cursorRow = GloWindow(windo).cursorRow + 1
                GloWindow(windo).cursorCol = 1
                IF GloWindow(windo).cursorRow > WindowRows(windo) THEN
                    WindowScroll 1
                    GloWindow(windo).cursorRow = WindowRows(windo)
                END IF
        END SELECT
    END IF

END SUB

SUB WindowPrintTitle

    ' =======================================================================
    ' Print title of current window if the border$ says it's valid
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN

        title$ = GloWindow(windo).title
        border$ = WindowBorder$(GloWindow(windo).windowType)

        
        IF INSTR(border$, "T") THEN
            tx$ = RTRIM$(title$)
            IF LEN(tx$) > 0 THEN
                COLOR GloWindow(windo).highlight, GloWindow(windo).back
                MouseHide
                length = WindowCols(windo)
                IF (LEN(tx$) + 2) < length THEN
                    LOCATE GloWindow(windo).row1 - 1, GloWindow(windo).col1 + INT(length / 2 - LEN(tx$) / 2) - 1
                    PRINT " "; tx$; " ";
                ELSE
                    LOCATE GloWindow(windo).row1 - 1, GloWindow(windo).col1
                    PRINT LEFT$(" " + tx$ + " ", (GloWindow(windo).col2 - GloWindow(windo).col1 + 1))
                END IF
                MouseShow
            END IF
        END IF
    END IF
    
END SUB

SUB WindowRefresh (handle)

    ' =======================================================================
    ' Refresh the window -- used for window move, window resize, and
    ' WindowSetCurrent
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        MouseHide
        PutBackground GloWindow(handle).row1 - 1, GloWindow(handle).col1 - 1, GloBuffer$(handle, 2)
        MouseShow
    END IF

END SUB

FUNCTION WindowRows (handle)

    ' =======================================================================
    ' Returns number of rows if handle is a valid window
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        WindowRows = GloWindow(handle).row2 - GloWindow(handle).row1 + 1
    END IF

END FUNCTION

SUB WindowSave (handle)

    ' =======================================================================
    ' Saves the window handle%
    ' =======================================================================

    IF GloWindow(handle).handle > 0 THEN
        MouseHide
        GetBackground GloWindow(handle).row1 - 1, GloWindow(handle).col1 - 1, GloWindow(handle).row2 + 1, GloWindow(handle).col2 + 1, GloBuffer$(handle, 2)
        MouseShow
    END IF

END SUB

SUB WindowScroll (lines)

    ' =======================================================================
    ' Scroll just the window area.
    ' =======================================================================
    
    windo = WindowCurrent
    IF windo > 0 THEN
        MouseHide
        CALL Scroll(GloWindow(windo).row1, GloWindow(windo).col1, GloWindow(windo).row2, GloWindow(windo).col2, lines, GloWindow(windo).back)
        MouseShow
    END IF

END SUB

SUB WindowSetCurrent (handle)

    ' =======================================================================
    ' If window is valid, and not already the current window
    ' =======================================================================

    IF GloWindow(handle).handle <> -1 AND handle <> WindowCurrent THEN

        ' ===================================================================
        ' If current window has a shadow, hide the shadow
        ' ===================================================================

        MouseHide
        IF INSTR(WindowBorder$(GloWindow(GloStorage.currWindow).windowType), "S") THEN
            WindowShadowRefresh
        END IF

        ' ===================================================================
        ' Save all windows on top of the one to be current, and refresh the
        ' background of each
        ' ===================================================================

        x = GloStorage.numWindowsOpen
        WHILE GloWindowStack(x) <> handle
            WindowSave GloWindowStack(x)
            BackgroundRefresh GloWindowStack(x)
            x = x - 1
        WEND

        ' ===================================================================
        ' Save the window to be made the current window
        ' ===================================================================

        WindowSave handle
        BackgroundRefresh handle

        ' ===================================================================
        ' Replace each window that was on top of handle, and squeeze stack
        ' ===================================================================

        IF handle <> GloWindowStack(GloStorage.numWindowsOpen) THEN
            FOR A = x + 1 TO GloStorage.numWindowsOpen
                BackgroundSave GloWindowStack(A)
                WindowRefresh GloWindowStack(A)
                GloBuffer$(GloWindowStack(A), 2) = ""
                GloWindowStack(A - 1) = GloWindowStack(A)
            NEXT A
        END IF

        ' ===================================================================
        ' Save new background of new current window.
        ' ===================================================================

        BackgroundSave handle
        WindowRefresh handle
        GloBuffer$(handle, 2) = ""
        MouseShow

        GloStorage.currWindow = handle
        GloWindowStack(GloStorage.numWindowsOpen) = handle

        ' ===================================================================
        ' Show shadow if current window has one
        ' ===================================================================

        IF INSTR(WindowBorder$(GloWindow(handle).windowType), "S") THEN
            WindowShadowSave
        END IF

    END IF

END SUB

SUB WindowShadowRefresh

    ' =======================================================================
    ' If window is current, replace what was under the shadow
    ' =======================================================================

    windo = WindowCurrent
    IF windo > 0 THEN
        row1 = GloWindow(windo).row1
        row2 = GloWindow(windo).row2
        col1 = GloWindow(windo).col1
        col2 = GloWindow(windo).col2

        ' ===================================================================
        ' If shadow partially (or fully) off screen, adjust coordinates
        ' ===================================================================

        MouseHide
        IF col1 <= MAXCOL - 2 THEN
            PutBackground row1, col2 + 2, GloBuffer$(MAXWINDOW + 1, 1)
        END IF
        IF row2 <= MAXROW - 2 THEN
            PutBackground row2 + 2, col1 + 1, GloBuffer$(MAXWINDOW + 1, 2)
        END IF
        MouseShow
    END IF

END SUB

SUB WindowShadowSave

    ' =======================================================================
    ' If current window valid, draw the shadow, storing what is underneath
    ' it first.
    ' =======================================================================

    windo = WindowCurrent

    IF windo > 0 THEN
        row1 = GloWindow(windo).row1
        row2 = GloWindow(windo).row2
        col1 = GloWindow(windo).col1
        col2 = GloWindow(windo).col2

        ' ===================================================================
        ' If shadow is partially, or fully off screen, adjust coordinates
        ' ===================================================================

        IF col2 > MAXCOL - 2 THEN
            shadowWidth = -1
        ELSEIF col2 = MAXCOL - 2 THEN
            shadowWidth = 0
        ELSE
            shadowWidth = 1
        END IF

        MouseHide

        ' ===================================================================
        ' Save background, the draw shadow
        ' ===================================================================

        IF col2 <= MAXCOL - 2 THEN
            GetBackground row1, col2 + 2, row2 + 1, col2 + 2 + shadowWidth, GloBuffer$(MAXWINDOW + 1, 1)
            AttrBox row1, col2 + 2, row2 + 1, col2 + 2 + shadowWidth, 8
        END IF

        IF row2 <= MAXROW - 2 THEN
            GetBackground row2 + 2, col1 + 1, row2 + 2, col2 + 2 + shadowWidth, GloBuffer$(MAXWINDOW + 1, 2)
            AttrBox row2 + 2, col1 + 1, row2 + 2, col2 + 2 + shadowWidth, 8
        END IF
        MouseShow
    END IF

END SUB

