'*** CHRTB.BI - Chart Routine Include File for the Presentation Graphics Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  Purpose:
'
'     This is the INCLUDE file that interfaces a BASIC application with the
'     Presentation Graphics Charting Toolbox.  It should be included in any
'     BASIC application using the toolbox.  The file contains DECLAREs
'     for all of the user-accessible routines as well as TYPE definitions
'     for the ChartEnvironment variable used by most routines in the
'     toolbox and its elements.  Also included are CONST definitions
'     that define symbolically some of the parameters that must be
'     specified in using the charting routines.
'
'**************************************************************************


TYPE TitleType
   Title        AS STRING * 70      ' Title text
   TitleFont    AS INTEGER          ' Font for title text
   TitleColor   AS INTEGER          ' Color of title
   Justify      AS INTEGER          ' 1=Left, 2=Center, 3=Right
END TYPE

TYPE AxisType
   Grid         AS INTEGER          ' 0=No, 1=Yes
   GridStyle    AS INTEGER          ' Style number for grid lines
   AxisTitle    AS TitleType        ' Axis title
   AxisColor    AS INTEGER          ' Color of axis
   Labeled      AS INTEGER          ' 0=No tic marks, etc, 1=Tics, etc
   RangeType    AS INTEGER          ' 1=Linear, 2=Log
   LogBase      AS SINGLE           ' Base for log axis
   AutoScale    AS INTEGER          ' 0=Manual, 1=Auto scale
   ScaleMin     AS SINGLE           ' Minimum scale value
   ScaleMax     AS SINGLE           ' Maximum scale value
   ScaleFactor  AS SINGLE           ' Factor for scaling values on axis
   ScaleTitle   AS TitleType        ' Title for scale factor
   TicFont      AS INTEGER          ' Font for tic labels
   TicInterval  AS SINGLE           ' Distance between tics in world coordinates
   TicFormat    AS INTEGER          ' 1=Normal, 2=Exponential format labels
   TicDecimals  AS INTEGER          ' Number of decimals in value label
END TYPE

TYPE RegionType
   X1           AS INTEGER          ' Left side of window (in pixels)
   Y1           AS INTEGER          ' Top of window (in pixels)
   X2           AS INTEGER          ' Right side of window (in pixels)
   Y2           AS INTEGER          ' Bottom of window (in pixels)
   Background   AS INTEGER          ' Background color
   Border       AS INTEGER          ' 0=No border, 1=Border
   BorderStyle  AS INTEGER          ' Style number for border LINE
   BorderColor  AS INTEGER          ' Color to draw border in
END TYPE

TYPE LegendType
   Legend       AS INTEGER          ' 0=No legend, 1=Legend
   Place        AS INTEGER          ' 1=Overlay, 2=Bottom, 3=Right
   TextColor    AS INTEGER          ' Color of text in legend
   TextFont     AS INTEGER          ' Font for legend text
   AutoSize     AS INTEGER          ' 0=Manual, 1=Auto
   LegendWindow AS RegionType       ' Legend window specification
END TYPE

TYPE ChartEnvironment
   ChartType    AS INTEGER          ' 1=Bar, 2=Column, 3=Line, 4=Scatter, 5=Pie
   ChartStyle   AS INTEGER          ' Depends on type
   DataFont     AS INTEGER          ' Font to use for plot characters
   ChartWindow  AS RegionType       ' Overall chart window
   DataWindow   AS RegionType       ' Data portion of chart
   MainTitle    AS TitleType        ' Main title options
   SubTitle     AS TitleType        ' Second line title options
   XAxis        AS AxisType         ' X-axis options
   YAxis        AS AxisType         ' Y-axis options
   Legend       AS LegendType       ' Legend options
END TYPE


DECLARE SUB DefaultChart (Env AS ChartEnvironment, ChartType AS INTEGER, ChartStyle AS INTEGER)

DECLARE SUB AnalyzeChart (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, N AS INTEGER)
DECLARE SUB AnalyzeChartMS (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())
DECLARE SUB AnalyzePie (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, Expl() AS INTEGER, N AS INTEGER)
DECLARE SUB AnalyzeScatter (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER)
DECLARE SUB AnalyzeScatterMS (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())

DECLARE SUB Chart (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, N AS INTEGER)
DECLARE SUB ChartMS (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())
DECLARE SUB ChartScatter (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER)
DECLARE SUB ChartScatterMS (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())
DECLARE SUB ChartPie (Env AS ChartEnvironment, Cat$(), Value() AS SINGLE, Expl() AS INTEGER, N AS INTEGER)


'-- Text routines:
DECLARE SUB DefaultFont (SEG Segment%, SEG Offset%)
DECLARE SUB LabelChartH (Env AS ChartEnvironment, X AS INTEGER, Y AS INTEGER, Font AS INTEGER, TxtColor AS INTEGER, TxtString$)
DECLARE SUB LabelChartV (Env AS ChartEnvironment, X AS INTEGER, Y AS INTEGER, Font AS INTEGER, TxtColor AS INTEGER, TxtString$)
                                     
'-- Screen-mode routines:
DECLARE SUB ChartScreen (N AS INTEGER)


'-- Palette routines:
DECLARE SUB GetPaletteDef (PaletteC() AS INTEGER, PaletteS() AS INTEGER, PaletteP$(), PaletteCh() AS INTEGER, PaletteB() AS INTEGER)
DECLARE SUB SetPaletteDef (PaletteC() AS INTEGER, PaletteS() AS INTEGER, PaletteP$(), PaletteCh() AS INTEGER, PaletteB() AS INTEGER)
DECLARE SUB ResetPaletteDef ()
DECLARE FUNCTION GetPattern$ (Bits%, PatternNum%)
DECLARE FUNCTION MakeChartPattern$ (RefPattern$, FG AS INTEGER, BG AS INTEGER)

'-- COMMON declaration for the error variable:
COMMON SHARED /ChartLib/ ChartErr AS INTEGER


'-- Constant definitions for setting charting parameters:
CONST cPalLen = 15                 ' Length of charting palette

CONST cMissingValue = -3.4E+38     ' Denotes "missing" value in data

CONST cNo = 0
CONST cYes = NOT cNo

CONST cLeft = 1                    ' title placement
CONST cCenter = 2
CONST cRight = 3

CONST cCategory = 1                ' category axis
CONST cValue = 2                   ' value axis

CONST cNormFormat = 1              ' tic labels in decimal format
CONST cExpFormat = 2               ' tic labels in exp format

CONST cLinear = 1                  ' linear axis
CONST cLog = 2                     ' log axis

CONST cOverlay = 1                 ' Legend.Place
CONST cBottom = 2

CONST cBar = 1                     ' bar chart
CONST cPlain = 1                   ' unstacked data
CONST cStacked = 2                 ' stacked data

CONST cColumn = 2                  ' column chart

CONST cLine = 3                    ' line chart
CONST cLines = 1                   ' Lines connecting points
CONST cNoLines = 2                 ' No lines connecting points

CONST cScatter = 4                 ' scatter chart

CONST cPie = 5                     ' pie chart
CONST cPercent = 1                 ' Displays slice percent
CONST cNoPercent = 2               ' Does not display slice percent

' Constant definitions for error messages (number greater than 100
' are "fatal" errors and will cause charting routines to exit):
CONST cBadDataWindow = 105         ' DataWindow calculated too small
CONST cBadLegendWindow = 110       ' LegendWindow coordinates invalid
CONST cBadLogBase = 15             ' LogBase <= 0
CONST cBadScaleFactor = 20         ' ScaleFactor = 0
CONST cBadScreen = 25              ' Invalid screen mode
CONST cBadStyle = 30               ' Invalid chart style
CONST cBadType = 135               ' Invalid chart type
CONST cTooFewSeries = 155          ' Too few series (First > Last)
CONST cTooSmallN = 160             ' No data in series (N=0)
CONST cBadPalette = 165            ' Palette not dimensioned correctly
CONST cPalettesNotSet = 170        ' Palette not init'd (GetPaletteDef)
CONST cNoFontSpace = 175           ' No room to load default font
CONST cCLUnexpectedOff = 200       ' Added to ERR for unexpected error

