'============================================================================
'
'     MENU.BAS - Pull-down Menu Routines for the User Interface Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  NOTE:    This sample source code toolbox is intended to demonstrate some
'           of the extended capabilities of Microsoft BASIC 7.1 Professional
'           Development system that can help to leverage the professional
'           developer's time more effectively.  While you are free to use,
'           modify, or distribute the routines in this module in any way you
'           find useful, it should be noted that these are examples only and
'           should not be relied upon as a fully-tested "add-on" library.
'
'  PURPOSE: These are the routines which provide support for the pull-down
'           menus in the user interface toolbox.
'
'  For information on creating a library and QuickLib from the routines
'  contained in this file, read the comment header of GENERAL.BAS.
'
'============================================================================

DEFINT A-Z

'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'menu.bi'

COMMON SHARED /uitools/ GloMenu    AS MenuMiscType
COMMON SHARED /uitools/ GloTitle() AS MenuTitleType
COMMON SHARED /uitools/ GloItem()  AS MenuItemType

FUNCTION MenuCheck (action%) STATIC

    SELECT CASE action

    '=======================================================================
    ' This simulates "polling" for a menu event.  If a menu event occured,
    ' GloMenu.currMenu and .currItem are set.  When MenuCheck(0) is
    ' called, these values are transfered to .lastMenu and .lastItem.
    ' MenuCheck(0) then returns the menu number, or 0 (FALSE) if none
    ' selected as of last call
    '=======================================================================

        CASE 0
            GloMenu.lastMenu = GloMenu.currMenu
            GloMenu.lastItem = GloMenu.currItem
            GloMenu.currMenu = 0
            GloMenu.currItem = 0
            MenuCheck = GloMenu.lastMenu

        '===================================================================
        ' Returns the menu item last selected.  Functions only after a call
        ' to MenuCheck(0)
        '===================================================================

        CASE 1
            MenuCheck = GloMenu.lastItem

        '===================================================================
        ' Checks GloMenu.currMenu and .currItem.  If both are not 0, this
        ' returns TRUE meaning a menu has been selected since MenuCheck(0)
        ' was last called.  This does not change any values, it simply
        ' reports on the current state.
        '===================================================================

        CASE 2
            IF GloMenu.currMenu = 0 OR GloMenu.currItem = 0 THEN
                MenuCheck = FALSE
            ELSE
                MenuCheck = TRUE
            END IF
        CASE ELSE
            MenuCheck = 0
    END SELECT

END FUNCTION

SUB MenuColor (fore, back, highlight, disabled, cursorFore, cursorBack, cursorHi)

    GloMenu.fore = fore
    GloMenu.back = back
    GloMenu.highlight = highlight
    GloMenu.disabled = disabled
    GloMenu.cursorFore = cursorFore
    GloMenu.cursorBack = cursorBack
    GloMenu.cursorHi = cursorHi

END SUB

SUB MenuDo STATIC

    '=======================================================================
    ' If menu event trapping turned off, return immediately
    '=======================================================================

    IF NOT GloMenu.MenuOn THEN
        EXIT SUB
    END IF

    '=======================================================================
    ' Initialize MenuDo's variables, and then enter the main loop
    '=======================================================================

    GOSUB MenuDoInit

    WHILE NOT MenuDoDone

        '===================================================================
        ' If in MouseMode then
        '   if button is pressed, check where mouse is and react acccordingly.
        '   if button not pressed, switch to keyboard mode.
        '===================================================================
        IF mouseMode THEN
            MousePoll mouseRow, mouseCol, lButton, rButton
            IF lButton THEN
                IF mouseRow = 1 THEN
                    GOSUB MenuDoGetMouseMenu
                ELSE
                    GOSUB MenuDoGetMouseItem
                END IF
            ELSE
                mouseMode = FALSE
                GOSUB MenuDoMouseRelease
                IF NOT pulldown THEN
                    GOSUB MenuDoShowTitleAccessKeys
                END IF
            END IF
        ELSE

            '===============================================================
            ' If in keyboard mode, show the cursor, wait for key, hide cursor
            ' Perform the desired action based on what key was pressed.
            '===============================================================

            GOSUB MenuDoShowCursor
            GOSUB MenuDoGetKey
            GOSUB MenuDoHideCursor

            SELECT CASE kbd$
                CASE "enter":       GOSUB MenuDoEnter
                CASE "up":          GOSUB MenuDoUp
                CASE "down":        GOSUB menuDoDown
                CASE "left":        GOSUB MenuDoLeft
                CASE "right":       GOSUB MenuDoRight
                CASE "escape":      GOSUB MenuDoEscape
                CASE "altReleased": GOSUB MenuDoAltReleased
                CASE "mouse":       GOSUB MenuDoMousePress
                CASE ELSE:          GOSUB MenuDoAccessKey
            END SELECT
        END IF
    WEND
    GOSUB MenuDoHideTitleAccessKeys
    EXIT SUB

'===========================================================================
' Initialize variables for proper MenuDo execution.
'===========================================================================

MenuDoInit:
    REDIM buffer$(MAXMENU), copyFlag(MAXMENU)             'Stores screen backround

    FOR a = 1 TO MAXMENU
        buffer$(a) = ""                         '1 buffer per menu
        copyFlag(a) = FALSE                     'FALSE means not copied yet
    NEXT a

    pulldown = FALSE                            'FALSE means no menu is shown
    MenuDoDone = FALSE                          'FALSE means keep going in loop

    altWasReleased = FALSE                      'Set to TRUE if ALT is pressed
                                                'and then released

    altWasPressedAgain = FALSE                  'Set to TRUE is ALT is pressed
                                                'and then released, and then
                                                'pressed again.

    '=======================================================================
    ' If mouse installed and button is pressed, then set MouseMode to TRUE
    ' Else, set MouseMode to FALSE
    '=======================================================================
  
    MousePoll mouseRow, mouseCol, lButton, rButton
    IF lButton THEN
        mouseMode = TRUE
        currMenu = 0
        currItem = 0
    ELSE
        mouseMode = FALSE
        currMenu = 1
        currItem = 0
        GOSUB MenuDoShowTitleAccessKeys
    END IF

RETURN

'===========================================================================
' This shows the cursor at the location CurrMenu,CurrItem.
'===========================================================================

MenuDoShowCursor:

    MouseHide
    IF currMenu <> 0 AND RTRIM$(GloItem(currMenu, currItem).text) <> "-" THEN
        IF currItem = 0 THEN
            COLOR GloMenu.cursorFore, GloMenu.cursorBack
            LOCATE 1, GloTitle(currMenu).lColTitle
            PRINT " "; RTRIM$(GloTitle(currMenu).text); " ";
            IF NOT mouseMode THEN
               COLOR GloMenu.cursorHi, GloMenu.cursorBack
               LOCATE 1, GloTitle(currMenu).lColTitle + GloTitle(currMenu).accessKey
               PRINT MID$(GloTitle(currMenu).text, GloTitle(currMenu).accessKey, 1);
            END IF
        ELSE
            IF GloItem(currMenu, currItem).state = 2 THEN
                chk$ = CHR$(175)
            ELSE
                chk$ = " "
            END IF

            COLOR GloMenu.cursorFore, GloMenu.cursorBack
            LOCATE GloItem(currMenu, currItem).row, GloTitle(currMenu).lColItem + 1
            PRINT chk$; LEFT$(GloItem(currMenu, currItem).text, GloTitle(currMenu).itemLength); " ";

            IF GloItem(currMenu, currItem).state > 0 THEN
                COLOR GloMenu.cursorHi, GloMenu.cursorBack
                LOCATE GloItem(currMenu, currItem).row, col + GloItem(currMenu, currItem).accessKey + 1
                PRINT MID$(GloItem(currMenu, currItem).text, GloItem(currMenu, currItem).accessKey, 1);
            END IF

        END IF
    END IF
    MouseShow

RETURN

'===========================================================================
' This hides the cursor at the location CurrMenu,CurrItem.
'===========================================================================

MenuDoHideCursor:

    MouseHide
    IF currMenu <> 0 AND RTRIM$(GloItem(currMenu, currItem).text) <> "-" THEN
        IF currItem = 0 THEN
            SELECT CASE GloTitle(currMenu).state
                CASE 0: COLOR GloMenu.disabled, GloMenu.back
                CASE 1, 2: COLOR GloMenu.fore, GloMenu.back
                CASE ELSE
            END SELECT
            LOCATE 1, GloTitle(currMenu).lColTitle
            PRINT " "; RTRIM$(GloTitle(currMenu).text); " ";

            IF GloTitle(currMenu).state > 0 THEN
                COLOR GloMenu.highlight, GloMenu.back
                LOCATE 1, GloTitle(currMenu).lColTitle + GloTitle(currMenu).accessKey
                PRINT MID$(GloTitle(currMenu).text, GloTitle(currMenu).accessKey, 1);
            END IF
        ELSE
            IF GloItem(currMenu, currItem).state = 2 THEN
                chk$ = CHR$(175)
            ELSE
                chk$ = " "
            END IF
            SELECT CASE GloItem(currMenu, currItem).state
                CASE 0: COLOR GloMenu.disabled, GloMenu.back
                CASE 1, 2: COLOR GloMenu.fore, GloMenu.back
                CASE ELSE
            END SELECT
            LOCATE GloItem(currMenu, currItem).row, GloTitle(currMenu).lColItem + 1
            PRINT chk$; LEFT$(GloItem(currMenu, currItem).text, GloTitle(currMenu).itemLength); " ";

            IF GloItem(currMenu, currItem).state > 0 THEN
                COLOR GloMenu.highlight, GloMenu.back
                LOCATE GloItem(currMenu, currItem).row, col + GloItem(currMenu, currItem).accessKey + 1
                PRINT MID$(GloItem(currMenu, currItem).text, GloItem(currMenu, currItem).accessKey, 1);
            END IF

        END IF
    END IF
    MouseShow
RETURN

'===========================================================================
' Handles state where mouse is at row #1.
'===========================================================================

MenuDoGetMouseMenu:

    '=======================================================================
    ' Computes the menu number based on mouse column location.  Uses info
    ' calculated in MenuShow()
    '=======================================================================

    newMenu = CVI(MID$(GloMenu.menuIndex, mouseCol * 2 - 1, 2))

    IF GloTitle(newMenu).state <> 1 THEN
        newMenu = 0
    END IF

    '=======================================================================
    ' If new menu<>current menu, hide current menu, show new menu, assign new
    ' menu to current menu
    '=======================================================================

    IF newMenu <> currMenu THEN
        GOSUB MenuDoHidePullDown
        currMenu = newMenu
        currItem = 0
        GOSUB menuDoShowPullDown
    END IF

RETURN

'===========================================================================
' Handles state where mouse is not in row #1.  If a menu is down, it picks
' the proper menu item based on which row the mouse is located
'===========================================================================

MenuDoGetMouseItem:

    '=======================================================================
    ' If pulldown, and mouse column is within the menu area, then compute new
    ' item  based on computations done in MenuShow.  If not in box, then new
    ' item = 0
    '=======================================================================

    IF pulldown THEN
        IF mouseCol >= GloTitle(currMenu).lColItem AND mouseCol <= GloTitle(currMenu).rColItem AND mouseRow <= GloTitle(currMenu).lowestRow AND mouseRow - 2 <= MAXITEM THEN
            newItem = GloItem(currMenu, mouseRow - 2).index
        ELSE
            newItem = 0
        END IF

        ' ===================================================================
        ' If current item <> new item, hide old cursor, show new cursor,
        ' assign new item to current item.
        ' ===================================================================

        IF currItem <> newItem THEN
            IF currItem <> 0 THEN
                GOSUB MenuDoHideCursor
            END IF
            currItem = newItem
            GOSUB MenuDoShowCursor
        END IF
    END IF
RETURN

' ===========================================================================
' Handles state when MenuDo is in mouse mode, and mouse button is released.
' ===========================================================================

MenuDoMouseRelease:
    menuMode = FALSE

    ' =======================================================================
    ' If no menu selected, then exit MenuDo returning 0s for menu and item
    ' =======================================================================

    IF currMenu = 0 THEN
        GloMenu.currMenu = 0
        GloMenu.currItem = 0
        MenuDoDone = TRUE
    ELSE

        ' ===================================================================
        ' If menu is down, but no item is selected then
        '    if mouse is on the top row, simply gosub the MenuDoDown routine
        '    else hide menu then exit MenuDo returning 0's for menu and item
        ' ===================================================================

        IF currItem = 0 THEN
            IF mouseRow = 1 THEN
                GOSUB menuDoDown
            ELSE
                GOSUB MenuDoHidePullDown
                GloMenu.currMenu = 0
                GloMenu.currItem = 0
                MenuDoDone = TRUE
            END IF
        ELSE

            ' ===============================================================
            ' If current (menu,item)'s state is disabled, then just beep
            ' ===============================================================

            IF GloItem(currMenu, currItem).state = 0 THEN
                BEEP

            ' ===============================================================
            ' If current (menu,item)'s state is a line
            ' then exit MenuDo returning 0s for menu and item
            ' ===============================================================

            ELSEIF RTRIM$(GloItem(currMenu, currItem).text) = "-" THEN
                GOSUB MenuDoHidePullDown
                GloMenu.currMenu = 0
                GloMenu.currItem = 0
                MenuDoDone = TRUE
            ELSE

                ' ===========================================================
                ' Otherwise, selection must be valid, exit MenuDo, returning
                ' proper menu,item pair in the proper global variables
                ' ===========================================================
                GOSUB MenuDoHidePullDown
                GloMenu.currMenu = currMenu
                GloMenu.currItem = currItem
                MenuDoDone = TRUE
            END IF
        END IF
    END IF
RETURN

' ==========================================================================
' This routine shows the menu bar's access keys
' ==========================================================================

MenuDoShowTitleAccessKeys:
    MouseHide
    COLOR GloMenu.highlight, GloMenu.back
    FOR menu = 1 TO MAXMENU
        IF GloTitle(menu).state = 1 THEN
            LOCATE 1, GloTitle(menu).lColTitle + GloTitle(menu).accessKey
            PRINT MID$(GloTitle(menu).text, GloTitle(menu).accessKey, 1);
        END IF
    NEXT menu
    MouseShow
RETURN


' ===========================================================================
' This routine hides the menu bar's access keys
' ===========================================================================

MenuDoHideTitleAccessKeys:
    MouseHide
    COLOR GloMenu.fore, GloMenu.back
    FOR menu = 1 TO MAXMENU
        IF GloTitle(menu).state = 1 THEN
            LOCATE 1, GloTitle(menu).lColTitle + GloTitle(menu).accessKey
            PRINT MID$(GloTitle(menu).text, GloTitle(menu).accessKey, 1);
        END IF
    NEXT menu
    MouseShow
RETURN

' ===========================================================================
' Waits for key press, then returns the key press.  It also returns several
' tokens such as "menu", or "altReleased" in special cases.  Read on...
' ===========================================================================

MenuDoGetKey:
    DO
        kbd$ = INKEY$

        ' ===================================================================
        ' If ALT key pressed, then if it was a access key (Alt+A..) reduce
        '  the Alt+A to A.
        '  Also set the altPressed flags to reflect the current state of the
        '  ALT key.
        ' ===================================================================

        IF GetShiftState(3) THEN
            IF kbd$ = "" THEN
                IF altWasReleased THEN
                    altWasPressedAgain = TRUE
                END IF
            ELSE
                altWasPressedAgain = FALSE
                kbd$ = AltToASCII(kbd$)
            END IF
            altWasReleased = FALSE
        ELSE

            ' ===============================================================
            ' If ALT key is released (initially), then pressed, then released
            ' again with no other action in between, then return the
            ' token "altReleased"
            ' ===============================================================

            IF altWasPressedAgain THEN
                kbd$ = "altReleased"
                altWasPressedAgain = FALSE
            ELSE

                ' ===========================================================
                ' Based on the key that was pressed, return the proper token
                ' ===========================================================

                altWasReleased = TRUE
                                           
                SELECT CASE kbd$
                    CASE CHR$(27) + "": kbd$ = "escape"
                    CASE CHR$(32) + "": kbd$ = ""
                    CASE CHR$(13) + "": kbd$ = "enter"
                    CASE CHR$(0) + "H": kbd$ = "up"
                    CASE CHR$(0) + "P": kbd$ = "down"
                    CASE CHR$(0) + "K": kbd$ = "left"
                    CASE CHR$(0) + "M": kbd$ = "right"
                    CASE ELSE
                        IF LEN(kbd$) = 1 THEN
                            kbd$ = UCASE$(kbd$)
                        END IF
                END SELECT
            END IF
        END IF

        ' ===================================================================
        ' If mouse button is pressed, it overrides all key actions, and
        ' the token "mouse" is returned
        ' ===================================================================

        MousePoll mouseRow, mouseCol, lButton, rButton
        IF lButton THEN
            kbd$ = "mouse"
        END IF

    LOOP UNTIL kbd$ <> ""
    
RETURN


' ===========================================================================
' Handles the state where the up arrow is pressed.  It searches for the
' first non empty, non "-" (dashed) item.
' ===========================================================================

MenuDoUp:
    IF currItem <> 0 THEN
        DO
            currItem = (currItem + MAXITEM - 2) MOD MAXITEM + 1
        LOOP UNTIL GloItem(currMenu, currItem).state >= 0 AND RTRIM$(GloItem(currMenu, currItem).text) <> "-"
    END IF
RETURN


' ===========================================================================
' Handles 2 different states:
'
'  State 1: Menu is open, and the down arrow is pressed.
'
'  State 2: Any time a new menu is opened, and the top item
'      is to be the current item.  Specifically:
'          - When no menu is opened, and the down arrow is pressed
'          - When the mouse is released over the menu title
'          - When a menu is opened, and the user hits right/left arrow
'          - When enter is pressed while cursor is on title bar
'          - When a access key is used on the title bar.
' ===========================================================================

menuDoDown:
    DO
        IF currItem = 0 THEN
            GOSUB MenuDoHideTitleAccessKeys
            GOSUB menuDoShowPullDown
            currItem = (currItem) MOD MAXITEM + 1
        ELSEIF currItem > 0 THEN
            currItem = (currItem) MOD MAXITEM + 1
        END IF
       
    LOOP UNTIL GloItem(currMenu, currItem).state >= 0 AND RTRIM$(GloItem(currMenu, currItem).text) <> "-"
RETURN


' ===========================================================================
' Handles state when the left arrow is pressed.  If a menu is down, it
' hides it.  It then finds the first valid menu to the left.  If the menu
' was initially down, then the new menu is pulled down as well
' ===========================================================================

MenuDoLeft:
    IF pulldown THEN
        GOSUB MenuDoHidePullDown
        pulldown = TRUE
    END IF

    DO
        currMenu = (currMenu + MAXMENU - 2) MOD MAXMENU + 1
    LOOP UNTIL GloTitle(currMenu).state = 1
    
    IF pulldown THEN
        currItem = 0
        GOSUB menuDoDown
    END IF
RETURN


' ===========================================================================
' Handles state when the right arrow is pressed.  If a menu is down, it
' hides it.  It then finds the first valid menu to the right.  If the menu
' was initially down, then the new menu is pulled down as well
' ===========================================================================

MenuDoRight:
    IF pulldown THEN
        GOSUB MenuDoHidePullDown
        pulldown = TRUE
    END IF

    DO
        currMenu = (currMenu) MOD MAXMENU + 1
    LOOP UNTIL GloTitle(currMenu).state = 1

    IF pulldown THEN
        currItem = 0
        GOSUB menuDoDown
    END IF
RETURN


' ===========================================================================
' Handles state when the ESC key is pressed.  First hides the menu, and
' then exits menuDo, returning 0's in the proper global variables
' ===========================================================================

MenuDoEscape:
    GOSUB MenuDoHidePullDown
    GloMenu.currMenu = 0
    GloMenu.currItem = 0
    MenuDoDone = TRUE
RETURN

' ===========================================================================
' Handles state when Enter is pressed.  If on a valid item, return the
' proper (menu,item) pair and exit.  Else beep.  If on a valid menu
' this will open the menu by calling MenuDoDown
' ===========================================================================

MenuDoEnter:
    IF currItem = 0 THEN
        IF GloTitle(currMenu).state = 0 THEN
            BEEP
        ELSE
            GOSUB menuDoDown
        END IF
    ELSE
        IF GloItem(currMenu, currItem).state <= 0 THEN
            BEEP
        ELSE
            GOSUB MenuDoHidePullDown
            GloMenu.currMenu = currMenu
            GloMenu.currItem = currItem
            MenuDoDone = TRUE
        END IF
    END IF
RETURN


' ===========================================================================
' If ALT pressed and released with nothing else happening in between, it
' will exit if no menu is open, or close the menu if one is open.
' ===========================================================================

MenuDoAltReleased:
    IF pulldown THEN
        GOSUB MenuDoHidePullDown
        currItem = 0
        GOSUB MenuDoShowTitleAccessKeys
    ELSE
        GloMenu.currMenu = 0
        GloMenu.currItem = 0
        MenuDoDone = TRUE
    END IF
RETURN


' ===========================================================================
' If mouse is pressed while in keyboard mode, this routine assigns
' TRUE to MouseMode, resets the item, and hides the access keys
' ===========================================================================

MenuDoMousePress:
    mouseMode = TRUE
    currItem = 0
    IF NOT pulldown THEN
        GOSUB MenuDoHideTitleAccessKeys
    END IF
RETURN


' ===========================================================================
' If a access key is pressed
' ===========================================================================

MenuDoAccessKey:

    ' =======================================================================
    ' If an access key is pressed
    '   If no menu selected, search titles for matching access key, and open
    '      than menu.
    ' =======================================================================

    IF currItem = 0 THEN
        newMenu = (currMenu + MAXMENU - 2) MOD MAXMENU + 1
        loopEnd = (currMenu + MAXMENU - 2) MOD MAXMENU + 1
        DO
            newMenu = (newMenu) MOD MAXMENU + 1
        LOOP UNTIL (UCASE$(MID$(GloTitle(newMenu).text, GloTitle(newMenu).accessKey, 1)) = kbd$ AND GloTitle(newMenu).state = 1) OR newMenu = loopEnd

        IF kbd$ = UCASE$(MID$(GloTitle(newMenu).text, GloTitle(newMenu).accessKey, 1)) THEN
            currMenu = newMenu
            GOSUB menuDoDown
        END IF
    ELSE

        ' ===================================================================
        ' If menu is selected, search items for matching access key, and
        ' select that (menu,item) and exit MenuDo if item is enabled
        ' ===================================================================

        newItem = (currItem + MAXITEM - 2) MOD MAXITEM + 1
        loopEnd = (currItem + MAXITEM - 2) MOD MAXITEM + 1
        DO
            newItem = (newItem) MOD MAXITEM + 1
        LOOP UNTIL (UCASE$(MID$(GloItem(currMenu, newItem).text, GloItem(currMenu, newItem).accessKey, 1)) = kbd$ AND GloItem(currMenu, newItem).state > 0 AND RTRIM$(GloItem(currMenu, newItem).text) <> "-") OR newItem = loopEnd

                                        
        IF kbd$ = UCASE$(MID$(GloItem(currMenu, newItem).text, GloItem(currMenu, newItem).accessKey, 1)) THEN
            currItem = newItem
           
            IF GloItem(currMenu, currItem).state <= 0 THEN
                BEEP
            ELSE
                GOSUB MenuDoHidePullDown
                GloMenu.currMenu = currMenu
                GloMenu.currItem = currItem
                MenuDoDone = TRUE
            END IF
        END IF
    END IF
RETURN

' ===========================================================================
' Draws the menu -- only if menu is enabled.
' ===========================================================================

menuDoShowPullDown:
    IF currMenu <> 0 AND GloTitle(currMenu).state = 1 THEN

        ' ===================================================================
        ' Copies the background if this is the first time this particular
        ' menu is being drawn
        ' ===================================================================

        MouseHide
        IF NOT copyFlag(currMenu) THEN
            IF GloTitle(currMenu).rColItem - GloTitle(currMenu).lColItem < LEN(GloTitle(currMenu).text) THEN
                GloTitle(currMenu).rColItem = GloTitle(currMenu).lColItem + LEN(GloTitle(currMenu).text)
            END IF

            GetBackground 1, GloTitle(currMenu).lColItem, GloTitle(currMenu).lowestRow, GloTitle(currMenu).rColItem + 2, buffer$(currMenu)
            copyFlag(currMenu) = TRUE
        END IF

        ' ===================================================================
        ' Draw the menu, this is pretty straight forward
        ' ===================================================================
        pulldown = TRUE
        length = GloTitle(currMenu).itemLength
        IF length = 0 THEN length = 6
        lowestRow = 3
        col = GloTitle(currMenu).lColItem

        COLOR GloMenu.cursorFore, GloMenu.cursorBack
        LOCATE 1, GloTitle(currMenu).lColTitle
        PRINT " "; RTRIM$(GloTitle(currMenu).text); " ";

        COLOR GloMenu.fore, GloMenu.back
        LOCATE 2, col
        PRINT "Ú"; STRING$(length + 2, "Ä"); "¿"

        FOR item = 1 TO MAXITEM
            IF GloItem(currMenu, item).state >= 0 THEN
                IF GloItem(currMenu, item).state = 2 THEN
                    chk$ = CHR$(175)
                ELSE
                    chk$ = " "
                END IF

                LOCATE GloItem(currMenu, item).row, col
                COLOR GloMenu.fore, GloMenu.back

                IF RTRIM$(GloItem(currMenu, item).text) = "-" THEN
                    PRINT "Ã"; STRING$(length + 2, "Ä"); "´"
                ELSE
                    PRINT "³"; chk$;
                    IF GloItem(currMenu, item).state > 0 THEN
                        COLOR GloMenu.fore, GloMenu.back
                    ELSE
                        COLOR GloMenu.disabled, GloMenu.back
                    END IF
                    PRINT LEFT$(GloItem(currMenu, item).text + SPACE$(20), length);
                    COLOR GloMenu.fore, GloMenu.back
                    PRINT " ³";

                    IF GloItem(currMenu, item).state > 0 THEN
                        COLOR GloMenu.highlight, GloMenu.back
                        LOCATE GloItem(currMenu, item).row, col + GloItem(currMenu, item).accessKey + 1
                        PRINT MID$(GloItem(currMenu, item).text, GloItem(currMenu, item).accessKey, 1);
                    END IF
                END IF
                lowestRow = GloItem(currMenu, item).row + 1
            END IF
        NEXT item

        COLOR GloMenu.fore, GloMenu.back
        LOCATE lowestRow, col
        PRINT "À"; STRING$(length + 2, "Ä"); "Ù";

        rCol = col + length + 5

        AttrBox 3, rCol - 1, lowestRow, rCol, 8
        AttrBox lowestRow + 1, col + 2, lowestRow + 1, rCol, 8
    END IF

    MouseShow

RETURN

' ===========================================================================
' Replace the background over the menu
' ===========================================================================

MenuDoHidePullDown:
    IF pulldown THEN
        MouseHide

        PutBackground 1, GloTitle(currMenu).lColItem, buffer$(currMenu)
        
        MouseShow
        pulldown = FALSE
    END IF
RETURN
   
END SUB

SUB MenuEvent

    ' =======================================================================
    ' If ALT key is pressed, let MenuDo take over.  NOTE:  This will
    ' not call MenuDo if the ALT key has not been released at least
    ' once since the last time MenuDo was called.  This prevents the menu
    ' from flashing if the user simply holds down the ALT key.
    ' =======================================================================

    IF GetShiftState(3) THEN
        IF GloMenu.altKeyReset THEN
            MenuDo
            GloMenu.altKeyReset = FALSE
        END IF
    ELSE
        GloMenu.altKeyReset = TRUE
    END IF

    ' =======================================================================
    ' Call MenuDo if the mouse button is down, and the cursor is on the top row
    ' =======================================================================

    MousePoll mouseRow, mouseCol, lButton, rButton
    IF mouseRow = 1 AND lButton THEN
        MenuDo
    END IF

END SUB

SUB MenuInit

    ' =======================================================================
    '  Initialize global menu arrays
    ' =======================================================================

    FOR menu = 1 TO MAXMENU
        GloTitle(menu).text = ""
        GloTitle(menu).state = -1            'state of -1 means "empty"
        GloTitle(menu).rColItem = 0           'These get set in MenuShow
        GloTitle(menu).lColItem = 0           ' |
        GloTitle(menu).rColTitle = 0          ' |
        GloTitle(menu).lColTitle = 0          ' |
        GloTitle(menu).itemLength = 0         ' |
        GloTitle(menu).accessKey = 1            'Initial AccessKey of 1

        FOR item = 1 TO MAXITEM
            GloItem(menu, item).text = ""
            GloItem(menu, item).state = -1      'state of -1 means "empty"
            GloItem(menu, item).index = 0       'These get set in MenuShow
            GloItem(menu, item).row = 0         '  |
            GloItem(menu, item).accessKey = 1   'Initial AccessKey of 1
        NEXT item
    NEXT menu

    ' =======================================================================
    ' Initialize mouse
    ' =======================================================================

    MouseInit

    ' =======================================================================
    ' Set initial state of ALT key to "reset"
    ' Clear out shortcut key index
    ' Set initial state of menu to ON
    ' =======================================================================

    GloMenu.altKeyReset = TRUE
    GloMenu.shortcutKeyIndex = STRING$(100, 0)
    GloMenu.MenuOn = TRUE

    GloMenu.fore = 0
    GloMenu.back = 7
    GloMenu.highlight = 15
    GloMenu.disabled = 8
    GloMenu.cursorFore = 7
    GloMenu.cursorBack = 0
    GloMenu.cursorHi = 15

END SUB

FUNCTION MenuInkey$ STATIC

    ' =======================================================================
    ' Scan keyboard, return KBD$ by default -- unless it is over written below
    ' =======================================================================

    kbd$ = INKEY$
    MenuInkey$ = kbd$

    ' =======================================================================
    ' Check if KBD$ matches a shortcut key.  If it does, return "menu" instead
    ' of the key that was pressed
    ' =======================================================================

    ShortCutKeyEvent kbd$
    IF MenuCheck(2) THEN
        MenuInkey$ = "menu"
    ELSE

        ' ===================================================================
        ' Call menu event, which looks at mouse, and state of ALT key
        ' If a menu item is selected, return "menu" instead of KBD$
        ' ===================================================================

        MenuEvent
        IF MenuCheck(2) THEN
            MenuInkey$ = "menu"
        END IF
    END IF

END FUNCTION

SUB MenuItemToggle (menu, item)

    IF item >= 0 AND menu >= 1 AND item <= MAXITEM AND menu <= MAXMENU THEN

        IF item = 0 OR GloItem(menu, item).state < 1 OR GloItem(menu, item).state > 2 THEN
            SOUND 2000, 40
        ELSE
            GloItem(menu, item).state = 3 - GloItem(menu, item).state
        END IF

    END IF
END SUB

DEFSNG A-Z
SUB MenuOff

    ' =======================================================================
    ' Simply assigns FALSE to the proper global variable
    ' =======================================================================

    GloMenu.MenuOn = FALSE

END SUB

DEFINT A-Z
SUB MenuOn

    ' =======================================================================
    ' Simply assigns TRUE to the proper global variable
    ' =======================================================================

    GloMenu.MenuOn = TRUE

END SUB

SUB MenuPreProcess STATIC

    currCol = 2     'Represents the col where first menu title is located
   
    ' =======================================================================
    ' Menu index is a fast way of decoding which menu the mouse cursor
    ' is pointing to based on the col of the cursor.  See MENU.BI for details.
    ' =======================================================================

    GloMenu.menuIndex = STRING$(160, 0)

    ' =======================================================================
    ' Process each menu, one at a time
    ' =======================================================================

    FOR menu = 1 TO MAXMENU

        ' ===================================================================
        ' If state is empty, or text is "" then clear out data for that menu
        ' ===================================================================

        IF GloTitle(menu).state < 0 OR LEN(RTRIM$(GloTitle(menu).text)) = 0 THEN
            GloTitle(menu).rColItem = 0
            GloTitle(menu).lColItem = 0
            GloTitle(menu).rColTitle = 0
            GloTitle(menu).lColTitle = 0
            GloTitle(menu).itemLength = 0
            GloTitle(menu).state = -1
       ELSE
            ' ===============================================================
            ' else, assign data about the column location to the global storage
            ' ===============================================================

            GloTitle(menu).lColTitle = currCol
            GloTitle(menu).rColTitle = currCol + LEN(RTRIM$(GloTitle(menu).text)) + 1
            GloTitle(menu).lColItem = currCol - 1

            IF GloTitle(menu).rColTitle > MAXCOL THEN
                BEEP: CLS : PRINT "Menu bar longer than screen!  Cannot function!"
                END
            END IF

            ' ===============================================================
            ' Update the index about where the menu is located, increment
            ' currCol
            ' ===============================================================
 
            FOR index = currCol TO currCol + LEN(RTRIM$(GloTitle(menu).text)) + 1
                MID$(GloMenu.menuIndex, index * 2 - 1, 2) = MKI$(menu)
            NEXT index
            
            currCol = currCol + LEN(RTRIM$(GloTitle(menu).text)) + 2

            ' ===============================================================
            ' Process the items in the menu, computing the
            ' longest item, and preparing the row index
            ' ===============================================================

            GloTitle(menu).itemLength = 0
            currRow = 3
            iFlag = FALSE

            FOR item = 1 TO MAXITEM
                GloItem(menu, currRow - 2).index = 0
                IF GloItem(menu, item).state >= 0 THEN
                    GloItem(menu, currRow - 2).index = item
                    GloItem(menu, item).row = currRow
                    currRow = currRow + 1
                    IF LEN(RTRIM$(GloItem(menu, item).text)) > GloTitle(menu).itemLength THEN
                        GloTitle(menu).itemLength = LEN(RTRIM$(GloItem(menu, item).text))
                    END IF
                    iFlag = TRUE
                END IF
            NEXT item

            ' ===============================================================
            ' If all items were empty, disable the menu itself
            ' else, assign the longest length to the proper variable
            ' ===============================================================

            IF NOT iFlag THEN
                GloTitle(menu).state = 0
            ELSE
                GloTitle(menu).rColItem = GloTitle(menu).lColItem + GloTitle(menu).itemLength + 3
                IF GloTitle(menu).rColItem > MAXCOL - 2 THEN
                   diff = GloTitle(menu).rColItem - (MAXCOL - 2)
                   GloTitle(menu).rColItem = GloTitle(menu).rColItem - diff
                   GloTitle(menu).lColItem = GloTitle(menu).lColItem - diff
                END IF
            END IF

        END IF

        GloTitle(menu).lowestRow = currRow + 1
    NEXT menu

END SUB

SUB MenuSet (menu, item, state, text$, accessKey) STATIC

    IF accessKey > LEN(text$) THEN accessKey = LEN(text$)

    IF item >= 0 AND menu >= 1 AND item <= MAXITEM AND menu <= MAXMENU THEN

        ' ===================================================================
        ' Assign parameters to proper global menu variables
        ' ===================================================================

        IF item = 0 THEN
            IF state < -1 OR state > 1 THEN
                SOUND 3000, 40
            ELSE
                GloTitle(menu).text = text$
                GloTitle(menu).state = state
                GloTitle(menu).accessKey = accessKey
            END IF
        ELSE
            IF state < -1 OR state > 2 THEN
                SOUND 4000, 40
            ELSE
                GloItem(menu, item).text = text$
                GloItem(menu, item).state = state
                GloItem(menu, item).accessKey = accessKey
            END IF
        END IF
    END IF

END SUB

SUB MenuSetState (menu, item, state) STATIC

    ' =======================================================================
    ' Assign parameters to proper global menu variables
    ' =======================================================================

    IF item = 0 THEN
        IF state < 0 OR state > 1 OR GloTitle(menu).state < 0 THEN
            SOUND 5000, 40
        ELSE
            GloTitle(menu).state = state
        END IF
    ELSE
        IF state < 0 OR state > 2 OR GloItem(menu, item).state < 0 THEN
            SOUND 6000, 40
        ELSE
            GloItem(menu, item).state = state
        END IF
    END IF

END SUB

DEFSNG A-Z
SUB MenuShow

    ' =======================================================================
    ' This section actually prints the menu on the screen
    ' =======================================================================

    COLOR GloMenu.fore, GloMenu.back
    LOCATE 1, 1
    PRINT SPACE$(MAXCOL);

    FOR menu = 1 TO MAXMENU
        SELECT CASE GloTitle(menu).state
            CASE 0:
                COLOR GloMenu.disabled, GloMenu.back
                LOCATE 1, GloTitle(menu).lColTitle + 1
                PRINT RTRIM$(GloTitle(menu).text$);
            CASE 1:
                COLOR GloMenu.fore, GloMenu.back
                LOCATE 1, GloTitle(menu).lColTitle + 1
                PRINT RTRIM$(GloTitle(menu).text$);
            CASE ELSE
        END SELECT

    NEXT menu

END SUB

DEFINT A-Z
SUB ShortCutKeyDelete (menu, item) STATIC

    '=======================================================================
    ' Search through shortcut key index until the menu,item pair is found
    ' or the end of the list is reached.
    '=======================================================================

    ptr = -1
    DO
        ptr = ptr + 1
        temp = CVI(MID$(GloMenu.shortcutKeyIndex, ptr * 4 + 1, 2))
        testMenu = INT(temp / 256)
        testItem = INT(temp MOD 256)
    LOOP UNTIL (menu = testMenu AND item = testItem) OR testMenu = 0 AND testItem = 0 OR ptr = 25

    '=======================================================================
    ' If a match is found, delete the shortcut key by squeezing out the four
    ' bytes that represents the shortcut key, and adding four chr$(0) at the
    ' end.
    '=======================================================================

    IF menu = testMenu AND item = testItem THEN
        GloMenu.shortcutKeyIndex = LEFT$(GloMenu.shortcutKeyIndex, ptr * 4) + RIGHT$(GloMenu.shortcutKeyIndex, 96 - ptr * 4) + STRING$(4, 0)
    END IF

END SUB

SUB ShortCutKeyEvent (theKey$)

    '=======================================================================
    ' If menu event trapping turned off, return immediately
    '=======================================================================

    IF NOT GloMenu.MenuOn THEN
        EXIT SUB
    END IF

    '=======================================================================
    ' Make sure the length of theKey$ is two bytes by adding a chr$(0) if
    ' necessary.  If the length is > 2, make it null.
    '=======================================================================

    SELECT CASE LEN(theKey$)
        CASE 1
            theKey$ = theKey$ + CHR$(0)
        CASE 2
        CASE ELSE
            theKey$ = ""
    END SELECT

    '=======================================================================
    ' Search the shortcut key list for a match -- only if theKey$ is valid.
    '=======================================================================

    IF theKey$ <> "" THEN

        ptr = -1
        DO
            ptr = ptr + 1
            testKey$ = MID$(GloMenu.shortcutKeyIndex, ptr * 4 + 3, 2)

        LOOP UNTIL theKey$ = testKey$ OR testKey$ = STRING$(2, 0) OR ptr = 25

        '===================================================================
        ' If match is found, make sure menu choice is valid (state > 0)
        ' If so, assign the proper global variables.
        '===================================================================

    IF theKey$ = testKey$ THEN
            temp = CVI(MID$(GloMenu.shortcutKeyIndex, ptr * 4 + 1, 2))
            tempMenu = INT(temp / 256)
            tempItem = INT(temp MOD 256)

            IF GloItem(tempMenu, tempItem).state > 0 THEN
                GloMenu.currMenu = tempMenu
                GloMenu.currItem = tempItem
            END IF
        END IF
    END IF

END SUB

SUB ShortCutKeySet (menu, item, shortcutKey$)

    '=======================================================================
    ' Make sure the length of theKey$ is two bytes by adding a chr$(0) if
    ' necessary.  If the length is >2, make it null.
    '=======================================================================

    SELECT CASE LEN(shortcutKey$)
        CASE 1
            shortcutKey$ = shortcutKey$ + CHR$(0)
        CASE 2
        CASE ELSE
            shortcutKey$ = ""
    END SELECT

    '=======================================================================
    ' First delete the shortcut key, just in case it already exists, and then
    ' and the shortcut key to the front of the shortcut key index string.
    '=======================================================================

    ShortCutKeyDelete menu, item
    IF shortcutKey$ <> "" THEN
        newKey$ = MKI$(menu * 256 + item) + shortcutKey$
        GloMenu.shortcutKeyIndex = newKey$ + LEFT$(GloMenu.shortcutKeyIndex, 396)
    END IF

END SUB

