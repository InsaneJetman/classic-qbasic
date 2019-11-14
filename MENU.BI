' ===========================================================================
'
' MENU.BI
'
'  Copyright (C) 1989 Microsoft Corporation, All Rights Reserved
'
' ===========================================================================

' ===========================================================================
' MenuTitleType stores information about each menu's title, and the left and
' right margins of the actual pull down menus.
' ===========================================================================

TYPE MenuTitleType
    text        AS STRING * 15      'Menu title's text
    state       AS INTEGER          'Menu's state  -1 empty, 0 disabled, 1 enabled
    lowestRow   AS INTEGER          'lowest row of the menu
    rColItem    AS INTEGER          'Right hand side of the menu
    lColItem    AS INTEGER
    rColTitle   AS INTEGER
    lColTitle   AS INTEGER
    itemLength  AS INTEGER
    accessKey   AS INTEGER
END TYPE

' ===========================================================================
' MenuItemType stores information about menu items
' ===========================================================================

TYPE MenuItemType               '(GloItem)
    text        AS STRING * 30
    state       AS INTEGER
    index       AS INTEGER
    row         AS INTEGER
    accessKey   AS INTEGER
END TYPE

' ===========================================================================
' MenuMiscType stores information about menu color attributes, current menu,
' previous menu, and an assortment of other miscellaneous information
' ===========================================================================

TYPE MenuMiscType               '(GloStorage)
    lastMenu        AS INTEGER
    lastItem        AS INTEGER
    currMenu        AS INTEGER
    currItem        AS INTEGER

    MenuOn          AS INTEGER
    altKeyReset     AS INTEGER
    menuIndex       AS STRING * 160
    shortcutKeyIndex AS STRING * 100

    fore            AS INTEGER
    back            AS INTEGER
    highlight       AS INTEGER
    disabled        AS INTEGER
    cursorFore      AS INTEGER
    cursorBack      AS INTEGER
    cursorHi        AS INTEGER
END TYPE

' ===========================================================================
' DECLARATIONS
' ===========================================================================

DECLARE SUB MenuItemToggle (menu%, item%)
DECLARE SUB MenuInit ()
DECLARE SUB MenuColor (fore%, back%, highlight%, disabled%, cursorFore%, cursorBack%, cursorHi%)
DECLARE SUB MenuSet (menu%, item%, state%, text$, accessKey%)
DECLARE SUB MenuShow ()
DECLARE SUB MenuPreProcess ()
DECLARE SUB MenuEvent ()
DECLARE FUNCTION MenuInkey$ ()
DECLARE SUB MenuDo ()
DECLARE FUNCTION MenuCheck% (action%)
DECLARE SUB MenuOn ()
DECLARE SUB MenuOff ()
DECLARE SUB ShortCutKeySet (menu%, item%, shortcutKey$)
DECLARE SUB ShortCutKeyDelete (menu%, item%)
DECLARE SUB ShortCutKeyEvent (theKey$)
DECLARE SUB MenuSetState (menu%, item%, state%)

