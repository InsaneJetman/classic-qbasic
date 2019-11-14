'       CHRTDEMO.BI - Include file for chart demo program (CHRTDEMO.BAS)
'
'               Copyright (C) 1989-1990, Microsoft Corporation
'


'$INCLUDE: 'chrtb.bi'
'$INCLUDE: 'fontb.bi'
'$INCLUDE: 'general.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'menu.bi'
'$INCLUDE: 'window.bi'

CONST cMaxValues = 15                       'Maximum values
CONST cMaxSets = 15                         'Maximum sets
CONST cMaxSeries = 15                       'Maximum data series

CONST CANCEL = 3                            'status flags
CONST OK = 1

CONST FILETITLE = 1                         'menu bar locations
CONST VIEWTITLE = 2
CONST GALLERYTITLE = 3
CONST CHARTTITLE = 4
CONST TITLETITLE = 5
CONST OPTIONSTITLE = 6

CONST MAXCOLORS = 16                        ' max colors in color lists
CONST MAXSTYLES = 15                        ' max border styles
CONST MAXFONTS = 16                         ' max fonts

' Data structure for creating a scrollable list box within any window.
TYPE ListBox
    boxLen                      AS INTEGER
    boxWid                      AS INTEGER
    maxLen                      AS INTEGER
    listLen                     AS INTEGER
    currPos                     AS INTEGER
    currTop                     AS INTEGER
    scrollButton                AS INTEGER
    areaButton                  AS INTEGER
    topRow                      AS INTEGER
    botRow                      AS INTEGER
    leftCol                     AS INTEGER
    rightCol                    AS INTEGER
    listPos                     AS INTEGER
END TYPE

'subs and funcs that are used across modules
DECLARE FUNCTION Min% (num1%, num2%)
DECLARE FUNCTION SaveChart% (FileName$, saveAsFlag%)

DECLARE SUB InitChart ()
DECLARE SUB ViewData ()
DECLARE SUB GetBestMode (screenMode%)
DECLARE SUB ClearData ()
DECLARE SUB DrawList (text$(), tbox AS ANY, func%)
DECLARE SUB HandleMenuEvent ()
DECLARE SUB InitAll ()
DECLARE SUB ChangeAxis (title$, axis AS ANY)
DECLARE SUB ChangeLegend ()
DECLARE SUB ChangeWindow (handle%, title$, win AS ANY)
DECLARE SUB ChangeTitle (handle%, wTitle$, title AS ANY, topRow%, leftCol%)
DECLARE SUB ChangeChartType (ctype%)
DECLARE SUB ScrollList (text$(), tbox AS ANY, currButton%, status%, func%, winRow%, winCol%)
DECLARE SUB CreateListBox (text$(), tbox AS ANY, func%)
DECLARE SUB SetAtt (change%, source%)
DECLARE SUB ClearFonts ()
DECLARE SUB InitColors ()
DECLARE SUB PrintError (text$)

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

' chart environment variables
COMMON SHARED /chrtdemo/ CEnv AS ChartEnvironment   'Main

COMMON SHARED /chrtdemo/ chartChanged AS INTEGER    'change flag
COMMON SHARED /chrtdemo/ setNum AS INTEGER          'number of data sets

COMMON SHARED /chrtdemo/ colors$()                  'color list
COMMON SHARED /chrtdemo/ numColors AS INTEGER       'number of colors
COMMON SHARED /chrtdemo/ styles$()                  'border sytles
COMMON SHARED /chrtdemo/ fonts$()                   'loaded fonts
COMMON SHARED /chrtdemo/ numFonts AS INTEGER        'number of fonts

' chart data variables
COMMON /chrtdemo/ catLen AS INTEGER                 '# of categories
COMMON /chrtdemo/ Cat$()                            'category names
COMMON /chrtdemo/ setName$()                        'set names
COMMON /chrtdemo/ setLen()   AS INTEGER             '# values per set
COMMON /chrtdemo/ setVal!()                         ' actual values
COMMON /chrtdemo/ screenMode AS INTEGER             'screen mode
COMMON /chrtdemo/ mode$()                           'list of modes
COMMON /chrtdemo/ numModes AS INTEGER
COMMON /chrtdemo/ finished  AS INTEGER
COMMON /chrtdemo/ saveFile$                         'chart file

