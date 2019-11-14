' ===========================================================================
'
' UIDEMO.BAS Copyright (c) 1989-1990 Microsoft Corporation
'
' ===========================================================================
' ===========================================================================
' Decls, includes, and dimensions
' ===========================================================================
DEFINT A-Z
DECLARE SUB AboutDemo ()
DECLARE SUB AboutUIP ()
DECLARE SUB AboutMouse ()
DECLARE SUB AboutAccess ()
DECLARE SUB AboutQuick ()
DECLARE SUB AboutWindows ()
DECLARE SUB ColorDisplay ()
DECLARE SUB DemoAlert ()
DECLARE SUB DemoDialog ()
DECLARE SUB DemoDialogEZ ()
DECLARE SUB DemoFileNameListBox ()
DECLARE SUB DemoListBox ()
DECLARE SUB DemoWindow ()
DECLARE SUB DemoScrollBar ()
DECLARE SUB DemoResize ()
DECLARE SUB MonoDisplay ()
DECLARE SUB SetupDesktop ()
DECLARE SUB SetupMenu ()
DECLARE FUNCTION GetFileCount% (FileSpec$)

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

DIM GloTitle(MAXMENU)           AS MenuTitleType
DIM GloItem(MAXMENU, MAXITEM)   AS MenuItemType
DIM GloWindow(MAXWINDOW)        AS windowType
DIM GloButton(MAXBUTTON)        AS buttonType
DIM GloEdit(MAXEDITFIELD)       AS EditFieldType
DIM GloWindowStack(MAXWINDOW)   AS INTEGER
DIM GloBuffer$(MAXWINDOW + 1, 2)

DIM SHARED DisplayType          AS INTEGER

    ' =======================================================================
    ' Initialize Demo
    ' =======================================================================

    MenuInit
    WindowInit
    MouseShow
    MonoDisplay

    ' =======================================================================
    ' Show Opening alert window
    ' =======================================================================

         
         a$ = "User Interface Toolbox Demo|"
    a$ = a$ + "for|"
    a$ = a$ + "Microsoft BASIC 7.1 Professional Development System|"
    a$ = a$ + "Copyright (c) 1989-1990 Microsoft Corporation|"

    x = Alert(4, a$, 9, 10, 14, 70, "Color", "Monochrome", "")

    IF x = 1 THEN
        DisplayType = TRUE
        ColorDisplay
    END IF

    ' =======================================================================
    ' Main Loop : Stay in loop until DemoFinished set to TRUE
    ' =======================================================================

    DemoFinished = FALSE

    WHILE NOT DemoFinished
        kbd$ = MenuInkey$
        WHILE MenuCheck(2)
            GOSUB MenuTrap
        WEND
    WEND

    ' =======================================================================
    ' End Program
    ' =======================================================================

    MouseHide
    COLOR 15, 0
    CLS
    END



' ===========================================================================
' If a menu event occured, call the proper demo, or if Exit, set demoFinished
' ===========================================================================

MenuTrap:
    menu = MenuCheck(0)
    item = MenuCheck(1)
   
    SELECT CASE menu
        CASE 1
            SELECT CASE item
                CASE 1:  DemoAlert
                CASE 2:  DemoDialogEZ
                CASE 3:  DemoDialog
                CASE 4:  DemoListBox
                CASE 5:  DemoFileNameListBox
                CASE 6:  DemoScrollBar
                CASE 7:  DemoWindow
                CASE 8:  DemoResize
                CASE 10: DemoFinished = TRUE
            END SELECT
        CASE 2
            SELECT CASE item
                CASE 1: ColorDisplay
                CASE 2: MonoDisplay
                
            END SELECT
        CASE 3
            SELECT CASE item
                CASE 1: AboutDemo
                CASE 2: AboutUIP
                CASE 3: AboutWindows
                CASE 4: AboutMouse
                CASE 5: AboutAccess
                CASE 6: AboutQuick
            END SELECT
        CASE ELSE
    END SELECT
RETURN

SUB AboutAccess
         a$ = "                      Access Keys||"
    a$ = a$ + "Access keys are the keys on the menu bar that are highlighted|"
    a$ = a$ + "when you press the Alt key. If you press a letter that is|"
    a$ = a$ + "highlighted in a menu title, that menu will be selected.||"
    a$ = a$ + "Once a pull-down menu is displayed, each menu item also has a|"
    a$ = a$ + "highlighted letter associated with each choice. Press the|"
    a$ = a$ + "letter that corresponds to the menu item you want to select.||"
    a$ = a$ + "If, after you press Alt, you change your mind, press the Alt|"
    a$ = a$ + "key again to cancel."
   
    junk = Alert(1, a$, 7, 9, 20, 69, "", "", "")

END SUB

SUB AboutDemo
         a$ = "                      About This Demo||"
    a$ = a$ + "Running this program provides a visual demonstration of most|"
    a$ = a$ + "of the features implemented in the new User Interface Toolbox|"
    a$ = a$ + "for the BASIC Compiler 7.1.||"
    a$ = a$ + "In addition to serving as a demo of toolbox features, the|"
    a$ = a$ + "source code that makes up this program can also serve as a|"
    a$ = a$ + "programming example of how to implement these features in|"
    a$ = a$ + "your programs. While the demo is relatively simple, it does|"
    a$ = a$ + "illustrate almost all the features available."
    
    junk = Alert(1, a$, 7, 9, 19, 69, "", "", "")
END SUB

SUB AboutMouse
         a$ = "                      Using the Mouse||"
    a$ = a$ + "Virtually all operations in the User Interface Toolbox can|"
    a$ = a$ + "be accomplished using the mouse. Move the mouse cursor to|"
    a$ = a$ + "whatever you want to select and press the left button.||"
    a$ = a$ + "In addition to being able to make a choice with a mouse,|"
    a$ = a$ + "you can also perform a number of operations on windows.|"
    a$ = a$ + "Using the mouse you can close, move, or resize windows|"
    a$ = a$ + "depending on the particular features of the window that is|"
    a$ = a$ + "active."
   
    junk = Alert(1, a$, 7, 9, 19, 69, "", "", "")

END SUB

SUB AboutQuick
         a$ = "                      Quick Keys||"
    a$ = a$ + "Quick keys are optional keys that you can define in addition|"
    a$ = a$ + "to the normal access keys that must be specified when you|"
    a$ = a$ + "set up the individual menu items.||"
    a$ = a$ + "Quick keys normally reduce selection of a menu item to one|"
    a$ = a$ + "keystroke. For example, this demo uses function keys F1 thru|"
    a$ = a$ + "F8 to select menu choices that demonstrate different features|"
    a$ = a$ + "of the User Interface Toolbox.  Additionally, Ctrl-X is the|"
    a$ = a$ + "Quick key that exits this demonstration program."
   
    junk = Alert(1, a$, 7, 9, 19, 69, "", "", "")

END SUB

SUB AboutUIP
         a$ = "                 About the User Interface||"
    a$ = a$ + "The user interface provided with this toolbox is designed to|"
    a$ = a$ + "provide much the same functionality as that found in the QBX|"
    a$ = a$ + "programming environment. The menus, check boxes, option|"
    a$ = a$ + "buttons, and other interface features operate similarly to|"
    a$ = a$ + "their QBX counterparts. ||"
    a$ = a$ + "If you know how to navigate QBX, you know how to navigate|"
    a$ = a$ + "the interface provided by the User Interface Toolbox."
   
    junk = Alert(1, a$, 7, 9, 18, 69, "", "", "")
END SUB

SUB AboutWindows
         a$ = "                     About the Windows||"
    a$ = a$ + "Several border characters used by the windows in the User|"
    a$ = a$ + "Interface Toolbox have special significance.  Any window that|"
    a$ = a$ + "has a '=' in the upper-left corner can be closed by selecting|"
    a$ = a$ + "that character with the mouse. Windows with the '°' character|"
    a$ = a$ + "across the window's top row can be moved around the screen by|"
    a$ = a$ + "selecting that area with the mouse.  The '+' character in the|"
    a$ = a$ + "lower-right corner means that the window can be resized by|"
    a$ = a$ + "selecting the '+' character with the mouse.||"
    a$ = a$ + "Note that none of these features can be accessed without a|"
    a$ = a$ + "mouse. "

    junk = Alert(1, a$, 7, 9, 21, 69, "", "", "")

END SUB

SUB ColorDisplay
    DisplayType = TRUE
    MouseHide
    SetupMenu
    MenuSetState 2, 1, 2
    MenuSetState 2, 2, 1
    SetupDesktop
    MenuShow
    MouseShow
END SUB

SUB DemoAlert

    ' =======================================================================
    ' Simple little demo of how easy alerts are to use.
    ' =======================================================================

         a$ = "|"
    a$ = a$ + "This is an Alert Box.| |"
    a$ = a$ + "It was created using a simple one|"
    a$ = a$ + "line command.  Notice the buttons|"
    a$ = a$ + "below.  They are user definable|"
    a$ = a$ + "yet their spacing is automatic."

    B$ = "You Selected OK"

    C$ = "You Selected Cancel"

    SELECT CASE Alert(4, a$, 6, 20, 15, 60, "OK", "Cancel", "")
        CASE 1
            x = Alert(4, B$, 10, 25, 12, 55, "OK", "", "")
        CASE 2
            x = Alert(4, C$, 10, 25, 12, 55, "OK", "", "")
    END SELECT

END SUB

SUB DemoDialog

    ' =======================================================================
    ' This is about as complex as they get.  As you can see it is still very
    ' simple - just a lot bigger.  This sub exactly duplicates the
    ' functionality of the QuickBASIC Search-Change dialog box.
    ' =======================================================================

    ' =======================================================================
    ' Open Window, place a horizontal line on row 13
    ' =======================================================================

    WindowOpen 1, 6, 11, 19, 67, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, ""

    WindowLine 13

    ' =======================================================================
    ' Print the text, and boxes for the edit fields
    ' =======================================================================

    WindowLocate 2, 2
    WindowPrint 2, "Find What:"
    WindowBox 1, 14, 3, 56

    WindowLocate 5, 2
    WindowPrint 2, "Change To:"
    WindowBox 4, 14, 6, 56


    ' =======================================================================
    ' Print the title of the window -- This overides the string in WindowOpen
    ' =======================================================================

    WindowLocate 0, 26
    WindowPrint 1, " Change "

    WindowBox 8, 32, 12, 56

    ' =======================================================================
    ' Open Edit fields
    ' =======================================================================

    search$ = ""
    replace$ = ""
    EditFieldOpen 1, search$, 2, 15, 0, 0, 40, 39

    EditFieldOpen 2, replace$, 5, 15, 0, 0, 40, 39

    ' =======================================================================
    ' Open all buttons
    ' =======================================================================

    ButtonOpen 1, 1, "Match Upper/Lowercase", 9, 2, 0, 0, 2
    ButtonOpen 2, 1, "Whole Word", 10, 2, 0, 0, 2
    ButtonOpen 3, 1, "1. Active Window", 9, 34, 0, 0, 3
    ButtonOpen 4, 2, "2. Current Module", 10, 34, 0, 0, 3
    ButtonOpen 5, 1, "3. All Modules", 11, 34, 0, 0, 3
    ButtonOpen 6, 2, "Find and Verify", 14, 2, 0, 0, 1
    ButtonOpen 7, 1, "Change All", 14, 22, 0, 0, 1
    ButtonOpen 8, 1, "Cancel", 14, 38, 0, 0, 1
    ButtonOpen 9, 1, "Help", 14, 49, 0, 0, 1

    ' =======================================================================
    ' Set initial states to match initial button settings
    ' =======================================================================

    MatchState = FALSE
    WordState = FALSE
    searchState = 2
    pushButton = 1
    currButton = 0
    currEditField = 1

    ' =======================================================================
    ' Do until exitFlag is set
    ' =======================================================================

    ExitFlag = FALSE
    WHILE NOT ExitFlag
        WindowDo currButton, currEditField
        SELECT CASE Dialog(0)
            CASE 0, 3, 4, 5, 20

            ' ==============================================================
            ' If edit field clicked, assign currEditField to Dialog(2)
            ' ==============================================================

            CASE 2
                currButton = 0
                currEditField = Dialog(2)

            ' ==============================================================
            ' If escape is hit,  set pushbutton = 0 and exit flag
            ' ==============================================================

            CASE 9  '(Escape)
                pushButton = 3
                ExitFlag = TRUE
               
            ' ==============================================================
            ' If return is hit, perform action based on the current button
            ' Button 9 is the help button.  In that case, show help, else just
            ' exit
            ' ==============================================================

            CASE 6
                SELECT CASE currButton
                    CASE 9
                        a$ = "Sample Help Window"
                        ButtonSetState pushButton + 5, 1
                        pushButton = 4
                        ButtonSetState pushButton + 5, 2
                        junk = Alert(4, a$, 7, 9, 19, 69, "", "", "")
                    CASE ELSE
                        ExitFlag = TRUE
                END SELECT


            ' ==============================================================
            ' A Button was pushed with mouse. Perform the desired action
            ' based on Button
            ' ==============================================================

            CASE 1
                currButton = Dialog(1)
                currEditField = 0
                SELECT CASE currButton
                    CASE 1
                        MatchState = NOT MatchState
                        ButtonToggle 1
                    CASE 2
                        WordState = NOT WordState
                        ButtonToggle 2
                    CASE 3, 4, 5
                        ButtonSetState searchState + 2, 1
                        searchState = Dialog(1) - 2
                        ButtonSetState searchState + 2, 2
                    CASE 6, 7, 8
                        pushButton = Dialog(1) - 5
                        ExitFlag = TRUE
                    CASE 9
                        a$ = "Sample Help Window"
                        ButtonSetState pushButton + 5, 1
                        pushButton = Dialog(1) - 5
                        ButtonSetState pushButton + 5, 2
                        junk = Alert(4, a$, 7, 9, 19, 69, "", "", "")
                    CASE ELSE
                END SELECT


            ' ==============================================================
            ' Tab was hit.  Depending upon the current button, or current edit field,
            ' assign the new values to currButton, and currEditField
            ' ==============================================================

            CASE 7  'tab
                SELECT CASE currButton
                    CASE 0
                        SELECT CASE currEditField
                            CASE 1
                                currEditField = 2
                          
                            CASE ELSE
                                currButton = 1
                                currEditField = 0
                        END SELECT
                    CASE 1
                        currButton = 2
                    CASE 6, 7, 8
                        currButton = currButton + 1
                        ButtonSetState pushButton + 5, 1
                        pushButton = currButton - 5
                        ButtonSetState pushButton + 5, 2
                    CASE 3, 4, 5
                        currButton = 6
                    CASE 2
                        currButton = 2 + searchState
                    CASE 9
                        currButton = 0
                        ButtonSetState pushButton + 5, 1
                        pushButton = 1
                        ButtonSetState pushButton + 5, 2
                        currEditField = 1
                END SELECT


            ' ==============================================================
            ' Same for Back Tab, only reverse.
            ' ==============================================================

            CASE 8 'back tab
                SELECT CASE currButton
                    CASE 0
                        SELECT CASE currEditField
                            CASE 1
                                currButton = 9
                                ButtonSetState pushButton + 5, 1
                                pushButton = currButton - 5
                                ButtonSetState pushButton + 5, 2
                                currEditField = 0
                            CASE 2
                                currEditField = 1
                        END SELECT
                    CASE 1
                        currButton = 0
                        currEditField = 2
                    CASE 7, 8, 9
                        currButton = currButton - 1
                        ButtonSetState pushButton + 5, 1
                        pushButton = currButton - 5
                        ButtonSetState pushButton + 5, 2
                    CASE 3, 4, 5
                        currButton = 2
                    CASE 6
                        currButton = 2 + searchState
                    CASE 2
                        currButton = 1
                END SELECT


            ' ==============================================================
            ' Up arrow only affects buttons 1,2,3,4,5  (the radial and check
            ' buttons)
            ' ==============================================================

            CASE 10 'up arrow
                SELECT CASE currButton
                    CASE 1
                        IF NOT MatchState THEN
                            MatchState = TRUE
                            ButtonToggle 1
                        END IF
                    CASE 2
                        IF NOT WordState THEN
                            WordState = TRUE
                            ButtonToggle 2
                        END IF
                    CASE 3
                        ButtonSetState searchState + 2, 1
                        searchState = 3
                        currButton = 5
                        ButtonSetState searchState + 2, 2
                    CASE 4, 5
                        ButtonSetState searchState + 2, 1
                        searchState = searchState - 1
                        currButton = currButton - 1
                        ButtonSetState searchState + 2, 2
                END SELECT
              

            ' ==============================================================
            ' Same with down arrow, only reverse
            ' ==============================================================

            CASE 11 'down
                SELECT CASE currButton
                    CASE 1
                        IF MatchState THEN
                            MatchState = NOT MatchState
                            ButtonToggle 1
                        END IF
                    CASE 2
                        IF WordState THEN
                            WordState = NOT WordState
                            ButtonToggle 2
                        END IF
                    CASE 3, 4
                        ButtonSetState searchState + 2, 1
                        searchState = searchState + 1
                        currButton = currButton + 1
                        ButtonSetState searchState + 2, 2
                    CASE 5
                        ButtonSetState searchState + 2, 1
                        searchState = 1
                        currButton = 3
                        ButtonSetState searchState + 2, 2
                END SELECT
           
            ' ==============================================================
            ' Left arrow only affects button 1 and 2  (the check buttons)
            ' ==============================================================

            CASE 12 'Left Arrow
               SELECT CASE currButton
                    CASE 1
                        IF NOT MatchState THEN
                            MatchState = TRUE
                            ButtonToggle 1
                        END IF
                    CASE 2
                        IF NOT WordState THEN
                            WordState = TRUE
                            ButtonToggle 2
                        END IF
                    CASE 3
                        ButtonSetState searchState + 2, 1
                        searchState = 3
                        currButton = 5
                        ButtonSetState searchState + 2, 2

                    CASE 4, 5
                        ButtonSetState searchState + 2, 1
                        searchState = searchState - 1
                        currButton = currButton - 1
                        ButtonSetState searchState + 2, 2

               END SELECT


            ' ==============================================================
            ' Right arrow only affects button 1 and 2  (the check buttons)
            ' ==============================================================
           
            CASE 13 'Right Arrow
                SELECT CASE currButton
                    CASE 1
                        IF MatchState THEN
                            MatchState = NOT MatchState
                            ButtonToggle 1
                        END IF
                    CASE 2
                        IF WordState THEN
                            WordState = NOT WordState
                            ButtonToggle 2
                        END IF
                    CASE 3, 4
                        ButtonSetState searchState + 2, 1
                        searchState = searchState + 1
                        currButton = currButton + 1
                        ButtonSetState searchState + 2, 2
                    CASE 5
                        ButtonSetState searchState + 2, 1
                        searchState = 1
                        currButton = 3
                        ButtonSetState searchState + 2, 2

                END SELECT

            ' ==============================================================
            ' Space will toggle a check button, or select a push button (including help)
            ' ==============================================================

            CASE 14 'space
                SELECT CASE currButton
                    CASE 1
                        MatchState = NOT MatchState
                        ButtonToggle 1
                    CASE 2
                        WordState = NOT WordState
                        ButtonToggle 2
                    CASE 6, 7, 8
                        pushButton = currButton - 5
                        ExitFlag = TRUE
                    CASE 9
                        a$ = "Sample Help Window"
                        ButtonSetState pushButton + 5, 1
                        pushButton = 4
                        ButtonSetState pushButton + 5, 2
                        junk = Alert(4, a$, 7, 9, 19, 69, "", "", "")
                    CASE ELSE
                END SELECT
            CASE ELSE
        END SELECT
    WEND


    ' =======================================================================
    ' Prepare data for final alert box that says what the final state was.
    ' =======================================================================
  
    search$ = EditFieldInquire(1)
    replace$ = EditFieldInquire(2)

   
    WindowClose 1
    IF pushButton = 3 THEN
        a$ = "You Selected CANCEL"
        x = Alert(4, a$, 10, 25, 12, 55, "OK", "", "")
    ELSE
        IF pushButton = 1 THEN
            a$ = "You selected VERIFY.  Here are your other selections:| |"
        ELSE
            a$ = "You selected CHANGE ALL.  Here are your other selections:| |"
        END IF

        IF MatchState THEN
            a$ = a$ + "   Match Upper/Lowercase = Yes|"
        ELSE
            a$ = a$ + "   Match Upper/Lowercase = No|"
        END IF

        IF WordState THEN
            a$ = a$ + "   Whole Word            = Yes|"
        ELSE
            a$ = a$ + "   Whole Word            = No|"
        END IF

        SELECT CASE searchState
            CASE 1: a$ = a$ + "   Search space          = Active Window|"
            CASE 2: a$ = a$ + "   Search space          = Current Module|"
            CASE 3: a$ = a$ + "   Search space          = All Modules|"
        END SELECT

        a$ = a$ + "   Search string : " + search$ + "|"
        a$ = a$ + "   Replace string: " + replace$ + "|"
       
        x = Alert(2, a$, 7, 11, 15, 69, "OK", "", "")
    END IF
END SUB

SUB DemoDialogEZ


    ' =======================================================================
    ' Open Window, write text, and open button and edit field
    ' =======================================================================

    WindowOpen 1, 8, 20, 13, 60, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, ""
   
    WindowLocate 2, 2
    WindowPrint 2, "Your Name:"
    WindowBox 1, 14, 3, 38

    EditFieldOpen 1, "", 2, 15, 0, 0, 23, 22
    WindowLine 5
    ButtonOpen 1, 2, "OK", 6, 17, 0, 0, 1


    ' =======================================================================
    ' Set initial state + go into main loop
    ' =======================================================================

    currButton = 0
    currEditField = 1

    ExitFlag = FALSE
   
    WHILE NOT ExitFlag
        WindowDo currButton, currEditField
        SELECT CASE Dialog(0)
            CASE 1, 6                       'Button, or Enter, exit loop
                ExitFlag = TRUE
            CASE 2                          'EditField, switch to edit field
                currButton = 0
                currEditField = 1
            CASE 7, 8                       'tab and backTab, flip/flop state
                IF currButton = 1 THEN
                    currButton = 0
                    currEditField = 1
                ELSE
                    currButton = 1
                    currEditField = 0
                END IF
            CASE 14                         'space - if on button then exit
                IF currButton = 1 THEN
                    ExitFlag = TRUE
                END IF
            CASE 9                          'escape
                WindowClose 1
                EXIT SUB
            CASE ELSE
        END SELECT
    WEND

    ' =======================================================================
    ' Assign the variable before closing the window, and close the window
    ' =======================================================================

    yourName$ = EditFieldInquire$(1)

    WindowClose 1
   
    IF LEN(yourName$) <> 0 THEN
        junk = Alert(4, "Hello " + yourName$ + ".", 10, 20, 12, 60, "OK", "", "")
    ELSE
        junk = Alert(4, "I understand. You wish to remain anonymous!", 10, 15, 12, 65, "OK", "", "")
    END IF
    
END SUB

SUB DemoFileNameListBox
    
    WindowOpen 1, 8, 20, 15, 60, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, TRUE, 1, ""

    WindowLocate 2, 4
    WindowPrint 4, "Enter a file specification:"
    WindowBox 3, 4, 5, 38

    EditFieldOpen 1, "*.*", 4, 5, 0, 0, 23, 22
    WindowLine 7
    ButtonOpen 1, 2, "OK", 8, 17, 0, 0, 1

    ' =======================================================================
    ' Set initial state + go into main loop
    ' =======================================================================

    currButton = 0
    currEditField = 1
   
    ExitFlag = FALSE
    WHILE NOT ExitFlag
        WindowDo currButton, currEditField
        SELECT CASE Dialog(0)
            CASE 1, 6                       'Button, or Enter, exit loop
                ExitFlag = TRUE
            CASE 2                          'EditField, switch to edit field
                currButton = 0
                currEditField = 1
            CASE 7, 8                       'tab and backTab, flip/flop state
                IF currButton = 1 THEN
                    currButton = 0
                    currEditField = 1
                ELSE
                    currButton = 1
                    currEditField = 0
                END IF
            CASE 9                          'escape
                WindowClose 1
                EXIT SUB
            CASE 14                         'space - if on button then exit
                IF currButton = 1 THEN
                    ExitFlag = TRUE
                END IF
            CASE ELSE
        END SELECT
    WEND

    ' =======================================================================
    ' Assign the variable before closing the window, and close the window
    ' =======================================================================

    FileSpec$ = EditFieldInquire$(1)

    ' =======================================================================
    ' Make sure its a valid file name
    ' =======================================================================

    delimit = INSTR(FileSpec$, ".")

    IF delimit THEN
        fileName$ = LEFT$(FileSpec$, delimit - 1)
        fileExt$ = RIGHT$(FileSpec$, LEN(FileSpec$) - (delimit))
    ELSE
        fileName$ = FileSpec$
        fileExt$ = ""
    END IF

    IF LEN(FileSpec$) = 0 OR LEN(fileName$) > 8 OR LEN(fileExt$) > 3 THEN
        WindowClose 1
        junk = Alert(4, "You didn't enter a valid file specification.", 10, 15, 12, 62, "OK", "", "")
        EXIT SUB
    END IF

    FileCount = GetFileCount(FileSpec$)

    IF FileCount THEN

        REDIM FileList$(FileCount)

    ELSE

        WindowClose 1
        junk = Alert(4, "No match to your file specification could be found.", 10, 10, 12, 70, "OK", "", "")
        EXIT SUB
    END IF
   
    FileList$(1) = DIR$(FileSpec$)
   
    FOR Indx = 2 TO FileCount
        FileList$(Indx) = DIR$
    NEXT Indx
   
    x = ListBox(FileList$(), UBOUND(FileList$))

    SELECT CASE x
        CASE 0
            junk = Alert(4, "You selected CANCEL", 10, 25, 12, 55, "OK", "", "")
        CASE ELSE
            junk = Alert(4, "You selected " + FileList$(x), 10, 25, 12, 55, "OK", "", "")
    END SELECT

    WindowClose 1
END SUB

SUB DemoListBox

    REDIM x$(30), y$(30)

    x$(1) = "Orange":                 y$(1) = "Orange you glad I didn't say Banana?"
    x$(2) = "Butter":                 y$(2) = "Try margarine! less cholesterol"
    x$(3) = "Corn":                   y$(3) = "Some people call it maize."
    x$(4) = "Potato":                 y$(4) = "Wouldn't you prefer stuffing?"
    x$(5) = "Grape":                  y$(5) = "Grape balls of fire!"
    x$(6) = "Cherry":                 y$(6) = "Don't chop down the tree!"
    x$(7) = "Lettuce":                y$(7) = "Two heads are better than one."
    x$(8) = "Lima bean":              y$(8) = "Who's Lima? and why do I have her beans?"
    x$(9) = "Carrot":                 y$(9) = "What's up Doc?"
    x$(10) = "Rice":                  y$(10) = "Yes, but can you use chopsticks?"
    x$(11) = "Steak":                 y$(11) = "Ooo.. Big spender."
    x$(12) = "Meatloaf":              y$(12) = "It must be Thursday."
    x$(13) = "Stuffing":              y$(13) = "Wouldn't you prefer potatoes?"
    x$(14) = "Wine":                  y$(14) = "Remember: 'Party Responsibly.'"
    x$(15) = "Pea":                   y$(15) = "Comes with the princess."
    x$(16) = "Gravy":                 y$(16) = "like home made! (Only no lumps)"
    x$(17) = "Pancake":               y$(17) = "Three for a dollar!"
    x$(18) = "Waffle":                y$(18) = "Syrup on your waffle sir?"
    x$(19) = "Broccoli":              y$(19) = "Little trees..."
    x$(20) = "Oatmeal":               y$(20) = "Yuck.."
                            
    x = ListBox(x$(), 20)

    SELECT CASE x
        CASE 0
            y = Alert(4, "You Selected Cancel", 10, 25, 12, 55, "OK", "", "")
        CASE ELSE
            y = Alert(4, y$(x), 10, 38 - LEN(y$(x)) \ 2, 12, 43 + LEN(y$(x)) \ 2, "OK", "", "")
    END SELECT

END SUB

SUB DemoResize

    ' =======================================================================
    ' Define Window's text string
    ' =======================================================================

    REDIM x$(19)
    x$(1) = "Resize Me!  Hello there!  Welcome to the wonderful world"
    x$(2) = "of Windows.  This demo shows how BASIC programmers can"
    x$(3) = "use a re-sizable window in their own applications."
    x$(4) = ""
    x$(5) = "This demo consists of a single window (this window) which"
    x$(6) = "can be moved, closed, or re-sized.  When the user resizes"
    x$(7) = "a window, an event code of 5 is returned.  Upon receiving"
    x$(8) = "the event code, the programmer can then do whatever is"
    x$(9) = "needed to refresh the window. "
    x$(10) = ""
    x$(11) = "The text in this window simply truncates when the window"
    x$(12) = "is made smaller, but text can be made to wrap either by"
    x$(13) = "character, or at the spaces between words. The choice is"
    x$(14) = "the programmer's."
    x$(15) = ""
    x$(16) = "The programmer has many tools available to make the"
    x$(17) = "job very easy such as functions that return the window"
    x$(18) = "size, and simple one-line calls to perform actions like"
    x$(19) = "opening or closing a window. "


    ' =======================================================================
    ' Open up a resizeable window
    ' =======================================================================
    
    WindowOpen 1, 4, 5, 4, 16, 0, 7, 0, 7, 8, TRUE, TRUE, TRUE, FALSE, 1, "-Window 1-"

    GOSUB DemoResizeDrawText

    ExitFlag = FALSE

    ' =======================================================================
    ' Process window events...
    '  IMPORTANT:  Window moving, and re-sizing is handled automatically
    '  The window type dictates when this is allowed to happen.
    ' =======================================================================

    WHILE NOT ExitFlag
        WindowDo 0, 0
        SELECT CASE Dialog(0)
            CASE 4, 9
                WindowClose WindowCurrent         'Close current window
                ExitFlag = TRUE
            CASE 5
                GOSUB DemoResizeDrawText
            CASE 20
                ExitFlag = TRUE                   'Exit if menu action
            CASE ELSE
        END SELECT
    WEND

    WindowClose 0

EXIT SUB
                                                                     
DemoResizeDrawText:
    WindowCls
         
    FOR a = 1 TO 19
        IF a <= WindowRows(1) THEN
            WindowLocate a, 1
            WindowPrint -1, x$(a)
       END IF
    NEXT a
RETURN

END SUB

SUB DemoScrollBar

    ' =======================================================================
    ' Open up a closeable window
    ' =======================================================================
    
    IF NOT DisplayType THEN
        WindowOpen 1, 4, 10, 20, 70, 0, 7, 0, 7, 15, FALSE, TRUE, FALSE, FALSE, 1, "Scroll Bar Demo"
    ELSE
        WindowOpen 1, 4, 10, 20, 70, 15, 5, 15, 5, 14, FALSE, TRUE, FALSE, FALSE, 1, "Scroll Bar Demo"
    END IF
                               
    ButtonOpen 1, 3, "", 4, 4, 14, 4, 6
    ButtonOpen 2, 4, "", 4, 6, 14, 6, 6
    ButtonOpen 3, 5, "", 4, 8, 14, 8, 6
    ButtonOpen 4, 4, "", 4, 10, 14, 10, 6
    ButtonOpen 5, 4, "", 4, 12, 14, 12, 6
    ButtonOpen 6, 9, "", 4, 16, 4, 50, 7
    ButtonOpen 7, 9, "", 6, 16, 6, 50, 7
    ButtonOpen 8, 8, "", 8, 16, 8, 50, 7
    ButtonOpen 9, 10, "", 10, 16, 10, 50, 7
    ButtonOpen 10, 12, "", 12, 16, 12, 50, 7
    ButtonOpen 11, 11, "", 14, 16, 14, 50, 7

    ExitFlag = FALSE

    ' =======================================================================
    ' Process window events...
    '   IMPORTANT:  Window moving, and re-sizing is handled automatically
    '   The window type dictates when this is allowed to happen.
    ' =======================================================================

    WHILE NOT ExitFlag
        WindowDo 0, 0
        x = Dialog(0)

        SELECT CASE x
            CASE 1
                button = Dialog(1)
               
                scrollCode = Dialog(19)
                currState = ButtonInquire(button)

                SELECT CASE scrollCode
                    CASE -1
                        IF currState > 1 THEN
                            newState = currState - 1
                        END IF
                    CASE -2
                        IF currState < MaxScrollLength(button) THEN
                            newState = currState + 1
                        END IF
                    CASE ELSE
                        newState = scrollCode
                END SELECT

                ButtonSetState button, newState
               
            CASE 4, 9
                WindowClose WindowCurrent         'Close current window
                ExitFlag = TRUE
            CASE 20
                ExitFlag = TRUE                   'Exit if menu action
            CASE ELSE
        END SELECT
    WEND

    WindowClose 0

END SUB

SUB DemoWindow
   
    REDIM z$(4 TO 6, 6)

    ' =======================================================================
    ' Open up 6 windows, showcase the features, and make each a different color
    ' =======================================================================
    IF NOT DisplayType THEN
        WindowOpen 1, 6, 5, 12, 25, 0, 7, 0, 7, 15, FALSE, FALSE, FALSE, FALSE, 0, ""
    ELSE
        WindowOpen 1, 6, 5, 12, 25, 0, 4, 0, 4, 15, FALSE, FALSE, FALSE, FALSE, 0, ""
    END IF
    WindowPrint 1, "Features:"
    WindowPrint 1, "No Title bar"
    WindowPrint 1, "No border"

    IF NOT DisplayType THEN
        WindowOpen 2, 8, 15, 14, 35, 0, 7, 0, 7, 15, TRUE, FALSE, FALSE, FALSE, 1, "-Window 2-"
    ELSE
        WindowOpen 2, 8, 15, 14, 35, 0, 2, 0, 2, 15, TRUE, FALSE, FALSE, FALSE, 1, "-Window 2-"
    END IF
    WindowPrint 1, "Features:"
    WindowPrint 1, "Title bar"
    WindowPrint 1, "Moveable window"
    WindowPrint 1, "Single-line border"

    IF NOT DisplayType THEN
        WindowOpen 3, 10, 25, 16, 45, 0, 7, 0, 7, 15, FALSE, TRUE, FALSE, FALSE, 1, "-Window 3-"
    ELSE
        WindowOpen 3, 10, 25, 16, 45, 0, 3, 0, 3, 15, FALSE, TRUE, FALSE, FALSE, 1, "-Window 3-"
    END IF
    WindowPrint 1, "Features:"
    WindowPrint 1, "Title bar"
    WindowPrint 1, "Closeable window"
    WindowPrint 1, "Single-line border"

    WindowOpen 4, 12, 35, 18, 55, 0, 7, 0, 7, 15, FALSE, FALSE, TRUE, FALSE, 1, "-Window 4-"
    z$(4, 1) = "Features:"
    z$(4, 2) = "Title bar"
    z$(4, 3) = "Resizeable window"
    z$(4, 4) = "Single-line border"
    ValidLines = 4
    GOSUB DemoReDrawText

    IF NOT DisplayType THEN
        WindowOpen 5, 14, 45, 20, 65, 0, 7, 0, 7, 15, TRUE, TRUE, TRUE, FALSE, 1, "-Window 5-"
    ELSE
        WindowOpen 5, 14, 45, 20, 65, 0, 5, 0, 5, 15, TRUE, TRUE, TRUE, FALSE, 1, "-Window 5-"
    END IF
    z$(5, 1) = "Features:"
    z$(5, 2) = "Title bar"
    z$(5, 3) = "Moveable window"
    z$(5, 4) = "Closeable window"
    z$(5, 5) = "Resizeable window"
    z$(5, 6) = "Single-line border"
    ValidLines = 6
    GOSUB DemoReDrawText

    IF NOT DisplayType THEN
        WindowOpen 6, 16, 55, 22, 75, 0, 7, 0, 7, 15, TRUE, TRUE, TRUE, FALSE, 2, "-Window 6-"
    ELSE
        WindowOpen 6, 16, 55, 22, 75, 0, 6, 0, 6, 15, TRUE, TRUE, TRUE, FALSE, 2, "-Window 6-"
    END IF
    z$(6, 1) = "Features:"
    z$(6, 2) = "Title bar"
    z$(6, 3) = "Moveable window"
    z$(6, 4) = "Closeable window"
    z$(6, 5) = "Resizeable window"
    z$(6, 6) = "Double-line border"
    ValidLines = 6
    GOSUB DemoReDrawText
   
    ' =======================================================================
    ' Show alert box describing what is going on
    ' =======================================================================

         a$ = "WINDOWS:  This demo displays six windows, each representing one "
    a$ = a$ + "or more of the features that are available.  You may use the "
    a$ = a$ + "mouse to select windows, move windows, resize windows, or close "
    a$ = a$ + "windows. You can also select border characters and define your "
    a$ = a$ + "window title.| |You should know that this demo "
    a$ = a$ + "consists of only six window open commands, and a 12 line "
    a$ = a$ + "Select Case statement to handle the actual processing."
                                           
    choice = Alert(3, a$, 7, 15, 18, 65, "OK", "Cancel", "")

    IF choice = 1 THEN
        ExitFlag = FALSE
    ELSE
        ExitFlag = TRUE
    END IF

    ' =======================================================================
    ' Process window events...
    '  IMPORTANT:  Window moving, and re-sizing is handled automatically
    '  The windowtype dictates when this is allowed to happen.
    ' =======================================================================

    WHILE NOT ExitFlag
        WindowDo 0, 0
        SELECT CASE Dialog(0)
            CASE 3
                WindowSetCurrent Dialog(3)        'Change current window
            CASE 4
                WindowClose WindowCurrent         'Close current window
            CASE 5
                GOSUB DemoReDrawText              'Redraw text when resizing
            CASE 9
                ExitFlag = TRUE                   'Exit if escape key pressed
            CASE 20
                ExitFlag = TRUE                   'Exit if menu action
            CASE ELSE
        END SELECT
    WEND

    WindowClose 0

EXIT SUB

DemoReDrawText:
    WindowCls
   
    FOR a = 1 TO ValidLines
        IF a <= WindowRows(WindowCurrent) THEN
            WindowLocate a, 1
            WindowPrint -1, z$(WindowCurrent, a)
        END IF
    NEXT a
RETURN

END SUB

FUNCTION GetFileCount (FileSpec$)
    count = 0
    fileName$ = DIR$(FileSpec$)
    DO WHILE fileName$ <> ""
        count = count + 1
        fileName$ = DIR$
    LOOP
    GetFileCount = count
END FUNCTION

SUB MonoDisplay
    DisplayType = FALSE
    MouseHide
    SetupMenu
    MenuSetState 2, 1, 1
    MenuSetState 2, 2, 2
    SetupDesktop
    MenuShow
    MouseShow
END SUB

DEFSNG A-Z
SUB SetupDesktop STATIC

    MouseHide

    WIDTH , 25

    IF DisplayType THEN
        COLOR 15, 1      'Color
    ELSE
        COLOR 15, 0      'Monochrome
    END IF
    CLS

    FOR a = 2 TO 80 STEP 4
        FOR B = 2 TO 25 STEP 2
            LOCATE B, a
            PRINT CHR$(250);
        NEXT B
    NEXT a

    MouseShow
END SUB

DEFINT A-Z
SUB SetupMenu

    MenuSet 1, 0, 1, "Demos", 1
    MenuSet 1, 1, 1, "Alert Window         F1", 1
    MenuSet 1, 2, 1, "Dialog Box (Simple)  F2", 13
    MenuSet 1, 3, 1, "Dialog Box (Complex) F3", 13
    MenuSet 1, 4, 1, "List Boxes           F4", 1
    MenuSet 1, 5, 1, "List Box w/File List F5", 12
    MenuSet 1, 6, 1, "Scroll Bars          F6", 8
    MenuSet 1, 7, 1, "Windows - Multiple   F7", 11
    MenuSet 1, 8, 1, "Window - Resizable   F8", 10
    MenuSet 1, 9, 1, "-", 1
    MenuSet 1, 10, 1, "Exit             Ctrl-X", 2
                          
    MenuSet 2, 0, 1, "Options", 1
    MenuSet 2, 1, 1, "Color", 1
    MenuSet 2, 2, 1, "Monochrome", 1
    

    MenuSet 3, 0, 1, "Help", 1
    MenuSet 3, 1, 1, "About This Demo", 12
    MenuSet 3, 2, 1, "About The User Interface", 11
    MenuSet 3, 3, 1, "About the Windows", 11
    MenuSet 3, 4, 1, "Using the Mouse", 11
    MenuSet 3, 5, 1, "Using Access Keys", 7
    MenuSet 3, 6, 1, "Using Quick Keys", 7

    ShortCutKeySet 1, 1, CHR$(0) + CHR$(59) ' F1
    ShortCutKeySet 1, 2, CHR$(0) + CHR$(60) ' F2
    ShortCutKeySet 1, 3, CHR$(0) + CHR$(61) ' F3
    ShortCutKeySet 1, 4, CHR$(0) + CHR$(62) ' F4
    ShortCutKeySet 1, 5, CHR$(0) + CHR$(63) ' F5
    ShortCutKeySet 1, 6, CHR$(0) + CHR$(64) ' F6
    ShortCutKeySet 1, 7, CHR$(0) + CHR$(65) ' F7
    ShortCutKeySet 1, 8, CHR$(0) + CHR$(66) ' F8

    ShortCutKeySet 1, 10, CHR$(24)          ' Ctrl-X

    IF NOT DisplayType THEN
        MenuColor 0, 7, 15, 8, 7, 0, 15   'Best for monochrome and colors
    ELSE
        MenuColor 0, 7, 4, 8, 15, 0, 12   'Best for color
    END IF

    MenuPreProcess

END SUB

