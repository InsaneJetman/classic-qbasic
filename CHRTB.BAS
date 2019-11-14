'*** CHRTB.BAS - Chart Routines for the Presentation Graphics Toolbox in
'           Microsoft BASIC 7.1, Professional Development System
'              Copyright (C) 1987-1990, Microsoft Corporation
'
'  NOTE:  This sample source code toolbox is intended to demonstrate some
'  of the extended capabilities of Microsoft BASIC 7.1 Professional Development
'  system that can help to leverage the professional developer's time more
'  effectively.  While you are free to use, modify, or distribute the routines
'  in this module in any way you find useful, it should be noted that these are
'  examples only and should not be relied upon as a fully-tested "add-on"
'  library.
'
'  PURPOSE: This file contains the BASIC source code for the Presentation
'           Graphics Toolbox Chart Routines.
'
'  To create a library and QuickLib containing the charting routines found
'  in this file, follow these steps:
'       BC /X/FS chrtb.bas
'       LIB chrtb.lib + chrtb + chrtasm + qbx.lib;
'       LINK /Q chrtb.lib, chrtb.qlb,,qbxqlb.lib;
'  If you are going to use this CHRTB.QLB QuickLib in conjunction with
'  the font source code (FONTB.BAS) or the UI toobox source code
'  (GENERAL.BAS, WINDOW.BAS, MENU.BAS and MOUSE.BAS), you need to
'  include the assembly code routines referenced in these files.  For the
'  font routines, create CHRTB.LIB as follows before you create the
'  QuickLib:
'       LIB chrtb.lib + chrtb + chrtasm + fontasm + qbx.lib;
'  For the UI toolbox routines, create the library as follows:
'       LIB chrtb.lib + chrtb + chrtasm + uiasm + qbx.lib;
'**************************************************************************

' Constants:

CONST cTicSize = .02            ' Percent of axis length to use for tic length
CONST cMaxChars = 255           ' Maximum ASCII value allowed for character
CONST cBarWid = .8              ' Percent of category width to use for bar
CONST cPiVal = 3.141592         ' A value for PI
CONST cFalse = 0                ' Logical false
CONST cTrue = NOT cFalse        ' Logical true

' CHRTB.BI contains all of the TYPE definitions and SUB declarations
' that are accessible to the library user as well as CONST definitions for
' some routine parameters and error messages:

'$INCLUDE: 'CHRTB.BI'

' FONTB.BI contains all of the TYPE definitions and SUB declarations
' required for graphics text:

'$INCLUDE: 'FONTB.BI'

' Below are TYPE definitions local to this module:

' TYPE for recording information on title spacing:
TYPE TitleLayout
	Top         AS INTEGER        ' Space above first title
	TitleOne    AS INTEGER        ' Height of first title
	Middle      AS INTEGER        ' Space between first and second titles
	TitleTwo    AS INTEGER        ' Height of second title
	Bottom      AS INTEGER        ' Space below second title
	TotalSize   AS INTEGER        ' Sum of all the above
END TYPE

' TYPE for recording information on the legend layout:
TYPE LegendLayout
	NumCol      AS INTEGER        ' Number of columns in legend
	NumRow      AS INTEGER        ' Number of rows in legend
	SymbolSize  AS INTEGER        ' Height of symbol
	LabelOffset AS INTEGER        ' Space between start of symbol and label
	RowSpacing  AS INTEGER        ' Space between tops of rows
	ColSpacing  AS INTEGER        ' Spacing between beginnings of columns
	HorizBorder AS INTEGER        ' Top and bottom border
	VertBorder  AS INTEGER        ' Left and right border
END TYPE

' TYPE for a group of global parameters:
TYPE GlobalParams
	SysFlag     AS INTEGER        ' cYes means Analyze call is from system
	Initialized AS INTEGER        ' cYes means clInitChart has been called
  
	PaletteScrn AS INTEGER        ' Screen mode for which palette is set
	PaletteBits AS INTEGER        ' Bits per pixel for current screen mode
	PaletteSet  AS INTEGER        ' cYes means palette has been initialized
	White       AS INTEGER        ' White attribute in current screen mode

	Aspect      AS SINGLE         ' Current screen aspect
	MaxXPix     AS INTEGER        ' Screen size along X axis
	MaxYPix     AS INTEGER        ' Screen size along Y axis
	MaxColor    AS INTEGER        ' Maximum color number for current screen

	ChartWid    AS INTEGER        ' Width of chart window
	ChartHgt    AS INTEGER        ' Height of chart window
	CwX1        AS INTEGER        ' Left side of chart window
	CwY1        AS INTEGER        ' Top edge of chart window
	CwX2        AS INTEGER        ' Right side of chart window
	CwY2        AS INTEGER        ' Bottom edge of chart window

	XStagger    AS INTEGER        ' Boolean, true if category labels overflow
	ValLenX     AS INTEGER        ' Maximum length of value labels on X-axis
	ValLenY     AS INTEGER        ' Maximum length of value labels on Y-axis

	NVals       AS INTEGER        ' Number of data values in data series
	NSeries     AS INTEGER        ' Number of series of data
	MSeries     AS INTEGER        ' If multiple-series chart then cYes, else
											' cNo
	XMode       AS INTEGER        ' Axis mode of x axis
	YMode       AS INTEGER        ' Axis mode of y axis
END TYPE

' FUNCTION and SUB declarations for procedures local to this module:

DECLARE FUNCTION clBuildBitP$ (Bits%, C%, InP$)
DECLARE FUNCTION clBuildPlaneP$ (Bits%, C%, InP$)
DECLARE FUNCTION clColorMaskL% (Bits%, Colr%)
DECLARE FUNCTION clGetStyle% (StyleNum%)
DECLARE FUNCTION clMaxVal (A, B)
DECLARE FUNCTION clMap2Pal% (N%)
DECLARE FUNCTION clMap2Attrib% (N%)
DECLARE FUNCTION clMaxStrLen% (Txt$(), First%, Last%)
DECLARE FUNCTION clVal2Str$ (X, Places%, Format%)

DECLARE SUB clAdjustScale (Axis AS AxisType)
DECLARE SUB clAnalyzeC (Cat$(), N%, SLabels$(), First%, Last%)
DECLARE SUB clAnalyzeS (N%, SLabels$(), First%, Last%)
DECLARE SUB clBuildPalette (ScrnMode%, Bits%)
DECLARE SUB clChkInit ()
DECLARE SUB clChkFonts ()
DECLARE SUB clChkForErrors (Env AS ChartEnvironment, TypeMin%, TypeMax%, N%, First%, Last%)
DECLARE SUB clChkChartWindow (Env AS ChartEnvironment)
DECLARE SUB clChkPalettes (C%(), s%(), P$(), Char%(), B%())
DECLARE SUB clClearError ()
DECLARE SUB clColorMaskH (Bits%, Colr%, CMask%())
DECLARE SUB clDrawAxes (Cat$())
DECLARE SUB clDrawDataWindow ()
DECLARE SUB clDrawChartWindow ()
DECLARE SUB clDrawTitles ()
DECLARE SUB clDrawLegend (SeriesLabel$(), First AS INTEGER, Last AS INTEGER)
DECLARE SUB clDrawBarData ()
DECLARE SUB clDrawColumnData ()
DECLARE SUB clDrawLineData ()
DECLARE SUB clDrawPieData (value(), Expl%(), N%)
DECLARE SUB clDrawScatterData ()
DECLARE SUB clFilter (A AS AxisType, AxisMode%, D1(), D2(), N%)
DECLARE SUB clFilterMS (A AS AxisType, AxisMode%, D1(), D2(), N%, First%, Last%)
DECLARE SUB clFlagSystem ()
DECLARE SUB clFormatTics (A AS AxisType)
DECLARE SUB clHPrint (X%, Y%, Txt$)
DECLARE SUB clInitChart ()
DECLARE SUB clInitStdStruc ()
DECLARE SUB clLabelXTics (Axis AS AxisType, Cat$(), TicX, TicTotX%, TicY, YBoundry%)
DECLARE SUB clLabelYTics (Axis AS AxisType, Cat$(), TicX, TicY, TicTotY%)
DECLARE SUB clLayoutTitle (TL AS ANY, T1 AS ANY, T2 AS ANY)
DECLARE SUB clPrintTitle (TitleVar AS TitleType, Y%)
DECLARE SUB clRenderBar (X1, Y1, X2, Y2, C%)
DECLARE SUB clRenderWindow (W AS RegionType)
DECLARE SUB clScaleAxis (A AS AxisType, AxisMode%, D1())
DECLARE SUB clSelectChartWindow ()
DECLARE SUB clSelectRelWindow (W AS RegionType)
DECLARE SUB clSetAxisModes ()
DECLARE SUB clSetChartFont (N AS INTEGER)
DECLARE SUB clSetError (ErrNo AS INTEGER)
DECLARE SUB clSetCharColor (N%)
DECLARE SUB clSetGlobalParams ()
DECLARE SUB clSizeDataWindow (Cat$())
DECLARE SUB clLayoutLegend (SeriesLabel$(), First%, Last%)
DECLARE SUB clSpaceTics ()
DECLARE SUB clSpaceTicsA (A AS AxisType, AxisMode%, AxisLen%, TicWid%)
DECLARE SUB clTitleXAxis (A AS AxisType, X1%, X2%, YBoundry%)
DECLARE SUB clTitleYAxis (A AS AxisType, Y1%, Y2%)
DECLARE SUB clUnFlagSystem ()
DECLARE SUB clVPrint (X%, Y%, Txt$)


' Variable definitions local to this module:

DIM PaletteC%(0 TO cPalLen)            ' List of colors     for drawing data
DIM PaletteS%(0 TO cPalLen)            ' List of styles     for drawing data
DIM PaletteP$(0 TO cPalLen)            ' List of patterns   for drawing data
DIM PaletteCh%(0 TO cPalLen)           ' List of plot chars for drawing data
DIM PaletteB%(0 TO cPalLen)            ' List of patterns   for borders

DIM StdChars%(0 TO cPalLen)            ' Holds default plot characters

DIM DAxis         AS AxisType          ' Default axis settings
DIM DWindow       AS RegionType        ' Default window settings
DIM DLegend       AS LegendType        ' Default legend settings
DIM DTitle        AS TitleType         ' Default title settings

DIM XTitleLayout  AS TitleLayout       ' X-axis layout information
DIM YTitleLayout  AS TitleLayout       ' Y-axis layout information
DIM TTitleLayout  AS TitleLayout       ' Main/Sub layout information

DIM LLayout       AS LegendLayout      ' Legend layout information

DIM GFI           AS FontInfo          ' Global font information
DIM GE            AS ChartEnvironment  ' An internal global chart environment
DIM GP            AS GlobalParams      ' Holds a number of global parameters
													' used in the charting routines.  See
													' TYPE definition for details.

'$DYNAMIC
DIM V1(1, 1), V2(1, 1)                 ' Internal dynamic data arrays.
'$STATIC

'============================================================
'==============      Main Level Code     ====================
'============================================================

' This error trap is set in the ChartScreen routine and will
' be evoked if an invalid screen mode is used:
ScreenErr:
	clSetError cBadScreen
	RESUME NEXT

' This error trap should catch all errors that arise in using
' the charting library that are not expected:
UnexpectedErr:
	clSetError cCLUnexpectedOff + ERR
	RESUME NEXT

'=== AnalyzeChart - Sets up scales and data window sizes
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     Cat$(1)    - One-dimensional array of category labels
'
'     Value(1)   - One-dimensional array of values to chart
'
'     N%         - The number of data values in data series
'
'  Return Values:
'     Scale and Data-Window values are changed as appropriate.
'
'=================================================================
SUB AnalyzeChart (Env AS ChartEnvironment, Cat$(), value(), N AS INTEGER)

SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED V1()
REDIM V1(1 TO N%, 1 TO 1)
DIM Dum$(1 TO 1)

	' Check initialization and fonts:
	clClearError
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB
  
	' Set a global flag to indicate that this isn't a multiple-series chart:
	GP.MSeries = cNo

	' Check for obvious parameter and ChartEnvironment errors:
	clChkForErrors Env, 1, 3, N, 0, 0
	IF ChartErr > 100 THEN EXIT SUB

	' Make a copy of the user's ChartEnvironment variable to the library's
	' global environment variable:
	GE = Env

	' Set the correct axis modes for the type of chart specified in the
	' chart environment:
	clSetAxisModes

	' Transfer the input data to the dynamic working data array.  Do this
	' for each axis because, depending on the chart type, either one may be
	' the value axis.  The Filter routine automatically ignores the call if
	' the axis is a category axis:
	clFilter GE.XAxis, GP.XMode, value(), V1(), N
	clFilter GE.YAxis, GP.YMode, value(), V1(), N

	' Analyze the data for scale-maximum and -minimum and set the scale-
	' factor, etc. depending on the options set in the chart environment:
	clAnalyzeC Cat$(), N, Dum$(), 1, 1
  
	' Copy the global chart environment back to the user's ChartEnvironment
	' variable so that the settings that were calculated by the library are
	' accessible.  Then, if this routine wasn't called by the library itself,
	' in the course of drawing a bar, column or line chart, deallocate the
	' working data array:
	Env = GE
	IF GP.SysFlag = cNo THEN ERASE V1

END SUB

'=== AnalyzeChartMS - Analyzes multiple-series data for scale/window size.
'
'  Arguments:
'     Env             - ChartEnvironment variable
'
'     Cat$(1)         - One-dimensional array of category labels
'
'     Value(2)        - Two-dimensional array of values to chart.  First
'                       dimension (rows) represents different values within
'                       a series.  Second dimension (columns) represents
'                       different series.
'
'     N%              - Number of values (beginning with 1) to chart per
'                       series.
'
'     First%          - First series to analyze
'
'     Last%           - Last series to analyze
'
'     SeriesLabel$(1) - Labels for the different series
'
'  Return Values:
'     Various settings in the Env variable are altered in accordance with
'     the analysis.
'
'=================================================================
SUB AnalyzeChartMS (Env AS ChartEnvironment, Cat$(), value() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())

SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED V1()
REDIM V1(1 TO N, 1 TO Last - First + 1)

	' Check initialization and fonts:
	clClearError
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB
 
	' Set a global flag to indicate that this is a multiple-series chart:
	GP.MSeries = cYes

	' Check for obvious parameter and ChartEnvironment errors:
	clChkForErrors Env, 1, 3, N, 0, 0
	IF ChartErr > 100 THEN EXIT SUB

	' Make a copy of the user's ChartEnvironment variable to the library's
	' global environment variable:
	GE = Env

	' Set the correct axis modes for the type of chart specified in the
	' chart environment:
	clSetAxisModes

	' Transfer the input data to the dynamic working data array.  Do this
	' for each axis because, depending on the chart type, either one may be
	' the value axis.  The Filter routine automatically ignores the call if
	' the axis is a category axis:
	clFilterMS GE.XAxis, GP.XMode, value(), V1(), N, First, Last
	clFilterMS GE.YAxis, GP.YMode, value(), V1(), N, First, Last

	' Analyze the data for scale maximums and minimums and set the scale
	' factor, etc. depending on the options set in the chart environment:
	clAnalyzeC Cat$(), N, SeriesLabel$(), First, Last

	' Copy the global chart environment back to the user's ChartEnvironment
	' variable so that the settings that were calculated by the library are
	' accessible.  Then, if this routine wasn't called by the library itself,
	' in the course of drawing a bar, column or line chart, deallocate the
	' working data array:
	Env = GE
	IF GP.SysFlag = cNo THEN ERASE V1

END SUB

'=== AnalyzePie - Analyzes data for a pie chart
'
'  Arguments:
'     Env      - A ChartEnvironment variable
'
'     Cat$()   - One-dimensional array of category names
'
'     Value()  - One-dimensional array of values to chart
'
'     Expl()   - One dimensional array of flags indicating whether slices
'                are to be "exploded" (0 means no, 1 means yes).
'                Ignored if Env.ChartStyle <> 1.
'
'     N        - The number of values to chart
'
'  Return Values:
'     None.
'
'=================================================================
SUB AnalyzePie (Env AS ChartEnvironment, Cat$(), value() AS SINGLE, Expl() AS INTEGER, N AS INTEGER)
SHARED GE AS ChartEnvironment
SHARED GP AS GlobalParams
SHARED TTitleLayout AS TitleLayout
SHARED XTitleLayout AS TitleLayout
SHARED YTitleLayout AS TitleLayout
SHARED V1()
DIM EmptyTitle AS TitleType

	' Check initialization and fonts:
	clClearError
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB
 
	' This is a multiple series chart (a pie chart is treated as a
	' multiple series chart with each series having one value):
	GP.MSeries = cYes
	GP.NSeries = N

	' Check for obvious parameter and ChartEnvironment errors:
	clChkForErrors Env, cPie, cPie, 2, 1, N
	IF ChartErr > 100 THEN EXIT SUB

	' Make a copy of the user's ChartEnvironment variable to the library's
	' global environment variable:
	GE = Env

	' Set the correct axis modes for the type of chart specified in the
	' chart environment:
	clSetAxisModes

	' Set global parameters and layout main title:
	clSetGlobalParams
  
	' Layout titles (ignore X and Y axis titles):
	clLayoutTitle TTitleLayout, GE.MainTitle, GE.SubTitle
	EmptyTitle.Title = ""
	clLayoutTitle XTitleLayout, EmptyTitle, EmptyTitle
	clLayoutTitle YTitleLayout, EmptyTitle, EmptyTitle

	' Calculate the size for LegendWindow and DataWindow:
	clLayoutLegend Cat$(), 1, N
	IF ChartErr > 100 THEN EXIT SUB
	clSizeDataWindow Cat$()
	IF ChartErr > 100 THEN EXIT SUB

	' Copy the global chart environment back to the user's ChartEnvironment
	' variable so that the settings that were calculated by the library are
	' accessible.  Then, if this routine wasn't called by the library itself,
	' in the course of drawing a pie chart, deallocate the working data array:
	Env = GE

END SUB

'=== AnalyzeScatter - Sets up scales and data-window sizes for scatter chart
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     ValX(1)    - One-dimensional array of values for X axis
'
'     ValY(1)    - One-dimensional array of values for Y axis
'
'     N%         - The number of data values in data series
'
'  Return Values:
'     Scale and data-window values are changed as appropriate.
'
'=================================================================
SUB AnalyzeScatter (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER)

SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED V1(), V2()
REDIM V1(1 TO N, 1 TO 1), V2(1 TO N, 1 TO 1)
DIM Dum$(1 TO 1)

	' Check initialization and fonts:
	clClearError
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB
 
	' Set a global flag to indicate that this isn't a multiple-series chart:
	GP.MSeries = cNo

	' Check for obvious parameter and ChartEnvironment errors:
	clChkForErrors Env, 4, 4, N%, 0, 0
	IF ChartErr > 100 THEN EXIT SUB

	' Make a copy of the user's ChartEnvironment variable to the library's
	' global environment variable:
	GE = Env

	' Set the correct axis modes for the type of chart specified in the
	' chart environment:
	clSetAxisModes

	' Transfer the input data to the dynamic working data arrays (one
	' for each axis):
	clFilter GE.XAxis, GP.XMode, ValX(), V1(), N
	clFilter GE.YAxis, GP.YMode, ValY(), V2(), N

	' Analyze the data for scale-maximum and -minimum and set the scale-
	' factor, etc. depending on the options set in the chart environment:
	clAnalyzeS N, Dum$(), 1, 1

	' Copy the global chart environment back to the user's ChartEnvironment
	' variable so that the settings that were calculated by the library are
	' accessible.  Then, if this routine wasn't called by the library itself,
	' in the course of drawing a scatter chart, deallocate the working
	' data arrays:
	Env = GE
	IF GP.SysFlag = cNo THEN ERASE V1, V2

END SUB

'=== AnalyzeScatterMS - Analyzes multiple-series data for scale/window size
'
'  Arguments:
'     Env             - A ChartEnvironment variable
'
'     ValX(2)         - Two-dimensional array of values for X axis.  First
'                       dimension (rows) represents different values within
'                       a series.  Second dimension (columns) represents
'                       different series.
'
'     ValY(2)         - Two-dimensional array of values for Y axis.  Above
'                       comments apply
'
'     N%              - Number of values (beginning with 1) to chart per
'                       series
'
'     First%          - First series to analyze
'
'     Last%           - Last series to analyze
'
'     SeriesLabel$(1) - Labels for the different series
'
'  Return Values:
'     Various settings in the Env variable are altered in accordance with
'     the analysis.
'
'=================================================================
SUB AnalyzeScatterMS (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())

SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED V1(), V2()
REDIM V1(1 TO N, 1 TO Last - First + 1), V2(1 TO N, 1 TO Last - First + 1)
DIM Dum$(1 TO 1)

	' Check initialization and fonts:
	clClearError
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB
 
	' Set a global flag to indicate that this is a multiple-series chart:
	GP.MSeries = cYes

	' Check for obvious parameter and ChartEnvironment errors:
	clChkForErrors Env, 4, 4, N%, 0, 0
	IF ChartErr > 100 THEN EXIT SUB

	' Make a copy of the user's ChartEnvironment variable to the library's
	' global environment variable:
	GE = Env

	' Set the correct axis modes for the type of chart specified in the
	' chart environment:
	clSetAxisModes

	' Transfer the input data to the dynamic working data arrays (one
	' for each axis):
	clFilterMS GE.XAxis, GP.XMode, ValX(), V1(), N, First, Last
	clFilterMS GE.YAxis, GP.YMode, ValY(), V2(), N, First, Last

	' Analyze the data for scale-maximum and -minimum and set the scale-
	' factor, etc. depending on the options set in the chart environment:
	clAnalyzeS N, SeriesLabel$(), First%, Last%

	' Copy the global chart environment back to the user's ChartEnvironment
	' variable so that the settings that were calculated by the library are
	' accessible.  Then, if this routine wasn't called by the library itself,
	' in the course of drawing a scatter chart, deallocate the working
	' data arrays:
	Env = GE
	IF GP.SysFlag = cNo THEN ERASE V1, V2

END SUB

'=== Chart - Draws a single-series category/value chart
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     Cat$(1)    - One-dimensional array of category labels
'
'     Value(1)   - One-dimensional array of values to plot
'
'     N          - The number of data values in data series
'
'  Return Values:
'     Some elements of the Env variable are altered by plotting routines
'
'  Remarks:
'     This routine takes all of the parameters set in the Env variable
'     and draws a single-series chart of type Bar, Column, or Line
'     depending on the chart type specified in the Env variable.
'
'=================================================================
SUB Chart (Env AS ChartEnvironment, Cat$(), value() AS SINGLE, N AS INTEGER)

SHARED V1()

	' Analyze data for scale and window settings:
	clFlagSystem
	AnalyzeChart Env, Cat$(), value(), N
	clUnFlagSystem
	IF ChartErr < 100 THEN

		' Draw the different elements of the chart:
		clDrawChartWindow
		clDrawTitles
		clDrawDataWindow
		clDrawAxes Cat$()
  
		' Call appropriate Draw...Data routine for chart type:
		SELECT CASE Env.ChartType
			CASE 1: clDrawBarData
			CASE 2: clDrawColumnData
			CASE 3: clDrawLineData
		END SELECT

	END IF

	' Deallocate the data array:
	ERASE V1

END SUB

'=== ChartMS - Draws a multiple-series category/value chart
'
'  Arguments:
'     Env               - A ChartEnvironment variable
'
'     Cat$(1)           - A one-dimensional array of category names for the
'                         different data values
'
'     Value(2)          - A two-dimensional array of values--one column for
'                         each series of data
'
'     N%                - The number of data points in each series of data
'
'     First%            - The first series to be plotted
'
'     Last%             - The last series to be plotted
'
'     SeriesLabel$(1)   - Labels used for each series in the legend
'
'  Return Values:
'     Some elements of the Env variable are altered by plotting routines
'
'  Remarks:
'     This routine takes all of the parameters set in the Env variable
'     and draws a multiple-series chart of type Bar, Column, or Line
'     depending on the chart type specified in the Env variable.
'
'=================================================================
SUB ChartMS (Env AS ChartEnvironment, Cat$(), value() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())

SHARED V1()

	' Analyze data for scale settings:
	clFlagSystem
	AnalyzeChartMS Env, Cat$(), value(), N, First, Last, SeriesLabel$()
	clUnFlagSystem
	IF ChartErr < 100 THEN

		' Draw the different elements of the chart:
		clDrawChartWindow
		clDrawTitles
		clDrawDataWindow
		clDrawAxes Cat$()

		' Call appropriate Draw...DataMS routine for chart type:
		SELECT CASE Env.ChartType
			CASE 1: clDrawBarData
			CASE 2: clDrawColumnData
			CASE 3: clDrawLineData
		END SELECT

		' Lastly, add the legend:
		clDrawLegend SeriesLabel$(), First, Last

	END IF

	' Deallocate the data array:
	ERASE V1

END SUB

'=== ChartPie - Draws a pie chart
'
'  Arguments:
'     Env      - A ChartEnvironment variable
'
'     Cat$()   - One-dimensional array of category names
'
'     Value()  - One-dimensional array of values to chart
'
'     Expl%()  - One-dimensional array of flags indicating whether slices
'                are to be "exploded" or not (0 means no, 1 means yes),
'                ignored if ChartStyle <> 1
'
'     N%       - The number of values to chart
'
'  Return Values:
'     No return values
'
'=================================================================
SUB ChartPie (Env AS ChartEnvironment, Cat$(), value() AS SINGLE, Expl() AS INTEGER, N AS INTEGER)
SHARED GP AS GlobalParams
	' Set the global system flag to tell the AnalyzePie routine that it
	' is being called by the system and not the user:
	clFlagSystem

	' Calculate the size of the Data- and Legend-window:
	AnalyzePie Env, Cat$(), value(), Expl(), N
 
	' Remove the system flag:
	clUnFlagSystem

	' If there were no errors during analysis draw the chart:
	IF ChartErr < 100 THEN

		' Draw the different chart elements:
		clDrawChartWindow
		clDrawTitles
		clDrawDataWindow
		clDrawPieData value(), Expl(), N
		IF ChartErr <> 0 THEN EXIT SUB
		clDrawLegend Cat$(), 1, N

	END IF

END SUB

'=== ChartScatter - Draws a single-series scatter chart
'
'  Arguments:
'     Env      - A ChartEnvironment variable
'
'     ValX(1)  - One-dimensional array of values for X axis
'
'     ValY(1)  - One-dimensional array of values for Y axis
'
'     N%       - The number of values to chart
'
'
'  Return Values:
'     Some elements of Env variable may be changed by drawing routines
'
'  Remarks:
'     ChartScatter should be called when a chart with two value axes is
'     desired
'
'=================================================================
SUB ChartScatter (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER)
DIM Dum$(1 TO 1)
SHARED V1(), V2()

	' Set the global system flag to tell the AnalyzeScatter routine that it
	' is being called by the system and not the user:
	clFlagSystem
  
	' Calculate the scale maximums and minimums and scale factor. Also
	' calculate the sizes for the Data- and Legend-windows:
	AnalyzeScatter Env, ValX(), ValY(), N

	' Remove the system flag:
	clUnFlagSystem

	' If there were no errors during analysis draw the chart:
	IF ChartErr < 100 THEN

		' Draw the different elements of the chart:
		clDrawChartWindow
		clDrawTitles
		clDrawDataWindow
		clDrawAxes Dum$()
		clDrawScatterData

	END IF

	' Deallocate the dynamic working data arrays:
	ERASE V1, V2

END SUB

'=== ChartScatterMS - Draws a multiple-series scatter chart
'
'  Arguments:
'     Env            - A ChartEnvironment variable
'
'     ValX(2)        - Two-dimensional array of values for X axis
'
'     ValY(2)        - Two-dimensional array of values for Y axis
'
'     N%             - The number of values in each series
'
'     First%         - First series to chart (first column)
'
'     Last%          - Last series to chart (last column)
'
'     SeriesLabel$() - Label used for each series in legend
'
'
'  Return Values:
'     Some elements in Env variable may be changed by drawing routines
'
'  Remarks:
'     A scatter chart uses two value axes so it must have values for both
'     the X and Y axes (ValX(), ValY()).  The first dimension denotes
'     the different values within a series.  The second dimension specifies
'     different data series (e.g. ValX(4,3) would represent the fourth value
'     in the third series of data).
'
'=================================================================
SUB ChartScatterMS (Env AS ChartEnvironment, ValX() AS SINGLE, ValY() AS SINGLE, N AS INTEGER, First AS INTEGER, Last AS INTEGER, SeriesLabel$())
DIM Dum$(1 TO 1)
SHARED V1(), V2()

	' Set the global system flag to tell the AnalyzeScatterMS routine that it
	' is being called by the system and not the user:
	clFlagSystem
 
	' Calculate the scale maximums and minimums and scale factor. Also
	' calculate the sizes for the Data- and Legend-windows:
	AnalyzeScatterMS Env, ValX(), ValY(), N, First, Last, SeriesLabel$()
 
	' Remove the system flag:
	clUnFlagSystem

	' If there were no errors during analysis draw the chart:
	IF ChartErr < 100 THEN

		' Draw the different elements of the chart:
		clDrawChartWindow
		clDrawTitles
		clDrawDataWindow
		clDrawAxes Dum$()
		clDrawScatterData
		clDrawLegend SeriesLabel$(), First, Last

	END IF

	' Deallocate the dynamic working data arrays:
	ERASE V1, V2

END SUB

'=== ChartScreen - Sets the SCREEN mode and default palettes
'
'  Arguments:
'     N%    - A valid BASIC graphic mode, or mode 0
'
'  Return Values:
'     All palettes may be altered
'
'=================================================================
SUB ChartScreen (N AS INTEGER)
SHARED GP AS GlobalParams

	' Check initialization and fonts:
	clClearError
	clChkInit

	' Set up branch to error processor and attempt to set the specified
	' screen mode and draw to it:
	ON ERROR GOTO ScreenErr
	SCREEN N
	IF N <> 0 THEN PRESET (0, 0)
	ON ERROR GOTO UnexpectedErr

	' If the above PRESET failed, then the TestScreen error processor will
	' have set the ChartErr error variable to a nonzero value.  If the last
	' call to ChartScreen used the same mode, GP.PaletteScrn will equal N; and
	' there is no need to rebuild palettes.  In either case there is no need
	' to do anything else, so exit:
	IF ChartErr <> 0 OR (GP.PaletteScrn = N AND GP.PaletteSet) THEN EXIT SUB
  
	' This is a new screen mode so use the SELECT CASE statement below
	' to handle it.  It sets the number of bits per pixel for a screen
	' mode so that the palettes can be built properly:
	SELECT CASE N

		' Screen mode 0 is not a graphics mode and is included mainly for
		' completeness.  The actual screen mode has been set above, so exit:
		CASE 0:
			EXIT SUB

		CASE 1:  Bits% = 2
		CASE 2:  Bits% = 1
		CASE 3:  Bits% = 1
		CASE 4:  Bits% = 1
		CASE 7:  Bits% = 4
		CASE 8:  Bits% = 4
		CASE 9:
					' For screen mode 9, assume a 256K EGA and try setting
					' a color to 63.  If that fails, assume it is a 64K EGA
					' (the number of bit planes is four for 256K and two for
					' 64K):
					Bits% = 4
					ON ERROR GOTO ScreenErr
					clClearError
					COLOR 15
					IF ChartErr <> 0 THEN Bits% = 2
					clClearError
					ON ERROR GOTO UnexpectedErr

		CASE 10: Bits% = 2
		CASE 11: Bits% = 1
		CASE 12: Bits% = 4
		CASE 13: Bits% = 8

		' If none of the above match then a valid screen mode was specified;
		' however the mode is un-supported so set error and exit:
		CASE ELSE: clSetError cBadScreen
					  EXIT SUB
	END SELECT

	' The screen aspect is 4/3 * MaxY/MaxX:
	VIEW
	WINDOW (0, 0)-(1, 1)
	GP.MaxXPix% = PMAP(1, 0) + 1
	GP.MaxYPix% = PMAP(0, 1) + 1
	GP.Aspect = 1.33333 * (GP.MaxYPix% - 1) / (GP.MaxXPix% - 1)
	WINDOW

	' The number of colors available:
	GP.MaxColor = 2 ^ Bits% - 1
  
	' Specify which color to use for white:
	SELECT CASE N
		CASE 13: GP.White = 15
		CASE ELSE: GP.White = GP.MaxColor
	END SELECT

	' Build palette for this screen mode:
	clBuildPalette N, Bits%

END SUB

'=== clAdjustScale - Calculates scaling factor for an axis and adjusts max-min
'                  as appropriate for scale factor and log base if log axis:
'
'  Arguments:
'     Axis  -  AxisType variable describing axis to be scaled.
'
'  Return Values:
'     May set the ScaleFactor and ScaleTitle elements and alter
'     ScaleMin and ScaleMax elements of the Axis variable.
'
'=================================================================
SUB clAdjustScale (Axis AS AxisType)

	' Don't try to scale a log axis:
	IF Axis.RangeType = cLog THEN

		Axis.ScaleFactor = 1
		Axis.ScaleTitle.Title = "Log" + STR$(Axis.LogBase)

	' For a linear axis, choose a scale factor up to Trillions depending
	' on the size of the axis limits:
	ELSE

		' Choose the largest ABS from Max and Min for the axis:
		IF ABS(Axis.ScaleMax) > ABS(Axis.ScaleMin) THEN
			Max = ABS(Axis.ScaleMax)
		ELSE
			Max = ABS(Axis.ScaleMin)
		END IF

		' Find out power of three by which to scale:
		Power% = INT((LOG(Max) / LOG(10)) / 3)

		' And, choose the correct title to go with it:
		SELECT CASE Power%
			CASE -4:     Axis.ScaleTitle.Title = "Trillionths"
			CASE -3:     Axis.ScaleTitle.Title = "Billionths"
			CASE -2:     Axis.ScaleTitle.Title = "Millionths"
			CASE -1:     Axis.ScaleTitle.Title = "Thousandths"
			CASE 0:     Axis.ScaleTitle.Title = ""
			CASE 1:     Axis.ScaleTitle.Title = "Thousands"
			CASE 2:     Axis.ScaleTitle.Title = "Millions"
			CASE 3:     Axis.ScaleTitle.Title = "Billions"
			CASE 4:     Axis.ScaleTitle.Title = "Trillions"
			CASE ELSE:  Axis.ScaleTitle.Title = "10^" + LTRIM$(STR$(Power% * 3))
		END SELECT

		' Calculate the actual scale factor:
		Axis.ScaleFactor = 10 ^ (3 * Power%)

		' Finally, scale Max and Min by ScaleFactor:
		Axis.ScaleMin = Axis.ScaleMin / Axis.ScaleFactor
		Axis.ScaleMax = Axis.ScaleMax / Axis.ScaleFactor

	END IF

END SUB

'=== clAnalyzeC - Does analysis of category/value data
'
'  Arguments:
'     Cat$(1)     -  List of category names
'
'     N%          -  Number of data values per series
'
'     SLabels$    -  Labels for the different data series
'
'     First%      -  First series to chart
'
'     Last%       -  Last series to chart
'
'  Return Values:
'     Some values in GE are altered.
'
'=================================================================
SUB clAnalyzeC (Cat$(), N%, SLabels$(), First%, Last%)
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED TTitleLayout AS TitleLayout
SHARED XTitleLayout AS TitleLayout
SHARED YTitleLayout AS TitleLayout
SHARED V1()

	' Save the number of values and the number of series in the chart in
	' the global parameter variables:
	GP.NVals = N%
	GP.NSeries = Last% - First% + 1

	' Analyze data for scale-maximim and -minimum and scale-factor:
	clScaleAxis GE.XAxis, GP.XMode, V1()
	IF ChartErr > 100 THEN EXIT SUB

	clScaleAxis GE.YAxis, GP.YMode, V1()
	IF ChartErr > 100 THEN EXIT SUB

	' Format tic labels (needed for sizing routines) and set global
	' parameters (again used by sizing and other routines):
	clFormatTics GE.XAxis
	clFormatTics GE.YAxis
	clSetGlobalParams

	' Layout Titles
	clLayoutTitle TTitleLayout, GE.MainTitle, GE.SubTitle
	clLayoutTitle YTitleLayout, GE.YAxis.AxisTitle, GE.YAxis.ScaleTitle
	clLayoutTitle XTitleLayout, GE.XAxis.AxisTitle, GE.XAxis.ScaleTitle

	' If this is a multiple-series chart, calculate the legend size:
	IF GP.MSeries = cYes THEN clLayoutLegend SLabels$(), First%, Last%
	IF ChartErr > 100 THEN EXIT SUB

	' Calculate the data-window size:
	clSizeDataWindow Cat$()
	IF ChartErr > 100 THEN EXIT SUB

	' Finally, figure out the distance between tic marks:
	clSpaceTics

END SUB

'=== clAnalyzeS - Does actual analysis of scatter data
'
'  Arguments:
'     N%          -  Number of values per data series
'
'     SLabels$(1) -  Labels for the data series
'
'     First%      -  First series to analyze
'
'     Last%       -  Last series to analyze
'
'  Return Values:
'     Values in GE are altered.
'
'=================================================================
SUB clAnalyzeS (N%, SLabels$(), First%, Last%)
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED TTitleLayout AS TitleLayout
SHARED XTitleLayout AS TitleLayout
SHARED YTitleLayout AS TitleLayout
SHARED V1(), V2()
DIM Dum$(1 TO 1)

	' Save the number of values and the number of series in the chart in
	' the global parameter variables:
	GP.NVals = N%
	GP.NSeries = Last% - First% + 1

	' Analyze data for scale-maximim and -minimum and scale-factor:
	clScaleAxis GE.XAxis, GP.XMode, V1()
	IF ChartErr > 100 THEN EXIT SUB

	clScaleAxis GE.YAxis, GP.YMode, V2()
	IF ChartErr > 100 THEN EXIT SUB

	' Format tic labels (needed for sizing routines) and set global
	' parameters (again used by sizing and other routines):
	clFormatTics GE.XAxis
	clFormatTics GE.YAxis
	clSetGlobalParams
  
	' Layout Titles
	clLayoutTitle TTitleLayout, GE.MainTitle, GE.SubTitle
	clLayoutTitle YTitleLayout, GE.YAxis.AxisTitle, GE.YAxis.ScaleTitle
	clLayoutTitle XTitleLayout, GE.XAxis.AxisTitle, GE.XAxis.ScaleTitle

	' If this is a multiple-series chart, calculate the legend size:
	IF GP.MSeries = cYes THEN clLayoutLegend SLabels$(), First%, Last%
	IF ChartErr > 100 THEN EXIT SUB

	' Calculate the data window size:
	clSizeDataWindow Dum$()
	IF ChartErr > 100 THEN EXIT SUB

	' Finally, figure out the distance between tic marks:
	clSpaceTics

END SUB

'=== clBuildBitP$ - Builds a pattern tile for a one bit-plane screen mode
'
'  Arguments:
'     Bits%    =  Number of bits per pixel in this screen mode
'
'     C%       =  The color used to make the pattern.
'
'     InP$     =  Reference pattern
'
'  Return Values:
'     Returns the specified pattern in specified color.
'
'  Remarks:
'     In screen modes where a pixel on the screen is represented by 1 or
'     more bits that are adjacent in memory, a byte of memory represents
'     one or more pixels depending on the number of bits per pixel the
'     mode uses (e.g. screen mode 1 uses 2 bits per pixel so each byte
'     contains 4 pixels).  To make a pattern tile in a specific color
'     you first decide which pixels should be on and which ones off.
'     Then, you set the corresponding two-bit pixels in the tile bytes
'     to the value of the color you want the pattern to be.  This routine
'     does this semi-automatically.  First it inputs a reference pattern that
'     contains the pattern defined in the highest color available for a
'     screen mode (all bits in a pixel set to one).  Then a color mask byte
'     is prepared with each pixel set to the color that was specified as
'     input to the routine.  When these two components (reference pattern
'     and color mask) are combined using a logical "AND" any pixel in the
'     reference pattern that was black (all zero) will remain black and any
'     pixel that was white will be of the input color.  The nice feature of
'     this scheme is that you can use one pattern set for any color
'     available for the screen mode.
'
'     Example: Screen mode 1; 2 bits per pixel; to build a pattern
'              with pixels alternating on and off in color 2:
'
'     Reference pattern:   11 00 11 00    (8 bits = 1 byte)
'     Color mask:          10 10 10 10    (each pixel set to color 2)
'                         -------------
'     Result of "AND"      10 00 10 00    (pattern in color 2)
'
'=================================================================
FUNCTION clBuildBitP$ (Bits%, C%, InP$)

	' First get color mask to match this color and pixel size (bits per pixel):
	CMask% = clColorMaskL%(Bits%, C%)
  
	' Initialize the output pattern to empty then combine the color
	' mask with each byte in the input tile using a logical "AND":
	OutP$ = ""
	FOR i% = 1 TO LEN(InP$)
		NxtCH% = CMask% AND ASC(MID$(InP$, i%, 1))
		OutP$ = OutP$ + CHR$(NxtCH%)
	NEXT i%

	' Return the completed pattern:
	clBuildBitP$ = OutP$

END FUNCTION

'=== clBuildPalette - Builds the five chart palettes
'
'  Arguments:
'     N           -  Screen mode for which to build palettes
'
'  Return Values:
'     Values in chart palettes set to standard ones for this mode
'
'  Remarks:
'     The following code sets up the palettes that are referenced when the
'     different chart elements are drawn.  See the charting library
'     documentation for a complete description of how these palettes are
'     used in drawing different portions of a chart.
'
'=================================================================
SUB clBuildPalette (ScrnMode AS INTEGER, Bits AS INTEGER)
SHARED PaletteC%(), PaletteS%(), PaletteP$(), PaletteCh%(), PaletteB%()
SHARED StdChars%()
SHARED GP AS GlobalParams

	' Flag palette set and record the screen mode:
	GP.PaletteSet = cYes
	GP.PaletteScrn = ScrnMode
	GP.PaletteBits = Bits

	' The first palettes to set are the character palette and the border
	' style palette:
	PaletteCh%(0) = 0
	PaletteB%(0) = &HFFFF
	FOR i% = 1 TO cPalLen
		PaletteCh%(i%) = StdChars%(i%)
		PaletteB%(i%) = clGetStyle(i%)
	NEXT i%

	' The next palette to set is the color palette, which is made up of
	' a list of 10 (maybe repeating) colors.  Begin by setting the first
	' two colors.  The first color (position 0) is always black and the
	' second color is always white (or whatever the maximum color number
	' is mapped to in the graphics-card palette).  Cycle through setting
	' other colors.  They will be entered in order starting with color 1
	' until the maximum number of colors is reached or the palette is filled
	' (size governed by the cPalLen CONST).  If the maximum color is reached
	' before the palette is filled then repeat the cycle again excluding
	' color 0, and so on, until the color palette is filled:

	PaletteC%(0) = 0        ' Black
	PaletteC%(1) = GP.White ' White

	FOR i% = 2 TO cPalLen
		MappedI% = ((i% - 2) MOD GP.MaxColor) + 1
		PaletteC%(i%) = MappedI%
	NEXT i%

	' Setting the line styles is almost the inverse of setting the colors
	' in that each color within a cycle has the same line style.  When a
	' new cycle of colors begins, though, the line style changes to
	' differentiate the new cycle from previous ones.  The line style
	' begins as &HFFFF or a solid line:

	' The pattern component of the palette contains fill patterns for use in
	' filling bars and pie slices.  Fill patterns are "bit" oriented whereas
	' line styles are "pixel" oriented.  What this means is that a fill
	' pattern of CHR$(&HFF) will be white regardless of what the current
	' color is.  If you know that each pixel on the screen is represented by
	' 2 bits in RAM and you want a solid fill with color 2, the corresponding
	' definition would be CHR$(&HAA) (in binary 10 10 10 10 -- notice, four
	' pixels of two bits each set to 2).  The following code automatically
	' takes a fill pattern defined in terms of pixels, and by masking it
	' with the current color generates the same fill pattern in the
	' specified color.  Start with solid black (color 0):

	PaletteS%(0) = &HFFFF
	PaletteP$(0) = CHR$(0)

	FOR i% = 1 TO cPalLen

		' The cycle number starts at one and is incremented each time
		' the maximum number of colors for the current screen mode is reached:
		Cycle% = ((i% - 1) \ GP.MaxColor) + 1

		' Set the style palette from the standard styles (which have
		' previously been placed in the border palette):
		PaletteS%(i%) = PaletteB%(Cycle%)
	  
		' Get the default pattern and put it into the palette:
		SELECT CASE ScrnMode
	
			' One bit plane modes:
			CASE 1, 2, 11, 13: RefPattern$ = GetPattern$(Bits, Cycle%)
		  
			' Multiple bit plane modes:
			CASE ELSE: RefPattern$ = GetPattern$(1, Cycle%)

		END SELECT
		PaletteP$(i%) = MakeChartPattern$(RefPattern$, PaletteC%(i%), 0)
	 
	NEXT i%

END SUB

'=== clBuildPlaneP$ - Builds a pattern tile for multiple bit-plane screen modes
'
'  Arguments:
'     Bits%    =  Number of planes in this screen mode
'
'     C%       =  The color used to make the pattern
'
'     InP$     =  Reference pattern
'
'  Return Values:
'     Returns the specified pattern in specified color
'
'  Remarks:
'     PAINT tiles are different for screen modes that use 2 or more
'     bit-planes than for the modes that use only one (see remarks for
'     clBuildBitP$()).  When bit-planes are used each pixel requires only
'     one bit per byte, but, there needs to be one byte for each bit-
'     plane.  The process for building a pattern from a reference pattern
'     and color mask are logically the same as in the one bit-plane modes
'     the only difference is that a color mask requires several bytes
'     (one for each bit-plane) rather than one.
'
'  Example: Screen mode 9 with 2 bit planes; pattern with alternating
'           pixels on and off; color 2:
'
'           Reference pattern:   1 0 1 0 1 0 1 0
'           Color mask:          0 0 0 0 0 0 0 0   (plane 1)
'                                1 1 1 1 1 1 1 1   (plane 2)
'                               -----------------
'           Result of "AND"      0 0 0 0 0 0 0 0   (plane 1)
'                                1 0 1 0 1 0 1 0   (plane 2)
'
'
'=================================================================
FUNCTION clBuildPlaneP$ (Bits%, C%, InP$)
DIM CMask%(1 TO 4)

	' First get color mask to match this color and pixel size (bits per pixel):
	clColorMaskH Bits%, C%, CMask%()

	' Initialize the output pattern to empty then combine the color
	' mask with each byte in the input tile using a logical "AND":
	OutP$ = ""
	FOR TileByte% = 1 TO LEN(InP$)
		RefTile% = ASC(MID$(InP$, TileByte%, 1))

		' Combine each bit-plane in the color mask with the pattern byte:
		FOR Plane% = 1 TO Bits%
			OutP$ = OutP$ + CHR$(RefTile% AND CMask%(Plane%))
		NEXT Plane%
	NEXT TileByte%

	' Return the completed pattern:
	clBuildPlaneP$ = OutP$
  
END FUNCTION

'=== clChkChartWindow - Makes sure the chart window is valid
'
'  Arguments:
'     Env   -  A ChartEnvironment variable
'
'  Return Values:
'     Changes global parameters for chart window
'
'  Remarks:
'     This routine forces the chart window to be valid.  If the input
'     values are invalid a full screen is chosen.  The valid chart window
'     is stored in the global parameter set and used by other charting
'     routines.  The last valid screen set by ChartScreen is used as
'     reference.
'
'=================================================================
SUB clChkChartWindow (Env AS ChartEnvironment)
SHARED GP AS GlobalParams

	' Make sure X1 < X2:
	IF Env.ChartWindow.X1 < Env.ChartWindow.X2 THEN
		GP.CwX1 = Env.ChartWindow.X1
		GP.CwX2 = Env.ChartWindow.X2
	ELSE
		GP.CwX1 = Env.ChartWindow.X2
		GP.CwX2 = Env.ChartWindow.X1
	END IF

	' Make sure Y1 < Y2:
	IF Env.ChartWindow.Y1 < Env.ChartWindow.Y2 THEN
		GP.CwY1 = Env.ChartWindow.Y1
		GP.CwY2 = Env.ChartWindow.Y2
	ELSE
		GP.CwY1 = Env.ChartWindow.Y2
		GP.CwY2 = Env.ChartWindow.Y1
	END IF

	' If the X coordinates of the chart window are invalid,
	' set them to full screen:
	IF GP.CwX1 < 0 OR GP.CwX2 >= GP.MaxXPix OR GP.CwX1 = GP.CwX2 THEN
		GP.CwX1 = 0
		GP.CwX2 = GP.MaxXPix - 1
	END IF

	' If the Y coordinates of the chart window are invalid,
	' set them to full screen:
	IF GP.CwY1 < 0 OR GP.CwY2 >= GP.MaxYPix OR GP.CwY1 = GP.CwY2 THEN
		GP.CwY1 = 0
		GP.CwY2 = GP.MaxYPix - 1
	END IF

	' Set chart height and width for use later:
	GP.ChartWid = GP.CwX2 - GP.CwX1 + 1
	GP.ChartHgt = GP.CwY2 - GP.CwY1 + 1

	' Put the valid coordinates in Env:
	Env.ChartWindow.X1 = GP.CwX1
	Env.ChartWindow.Y1 = GP.CwY1
	Env.ChartWindow.X2 = GP.CwX2
	Env.ChartWindow.Y2 = GP.CwY2

END SUB

'=== clChkFonts - Checks that there is at least one loaded font
'
'  Arguments:
'     none
'
'  Return Values:
'     Chart error set if no room for a font
'
'=================================================================
SUB clChkFonts

	' See if a font is loaded:
	GetTotalFonts Reg%, Load%
  
	' If not then find out the maximum number of fonts allowed and if
	' there's room, then load the default font:
	IF Load% <= 0 THEN
		GetMaxFonts MReg%, MLoad%
		IF Reg% < MReg% AND Load% < MLoad% THEN
			DefaultFont Segment%, Offset%
			FontNum% = RegisterMemFont(Segment%, Offset%)
			FontNum% = LoadFont("N" + STR$(Load% + 1))
	  
		' If there's no room, then set an error:
		ELSE
			clSetError cNoFontSpace
		END IF
	END IF
END SUB

'=== CheckForErrors - Checks for and tries to fix a variety of errors
'
'  Arguments:
'     Env      -  ChartEnvironment variable
'
'     TypeMin% -  Minimum allowable ChartType
'
'     TypeMax% -  Maximum allowable ChartType
'
'     N%       -  Number of data values per series
'
'     First%   -  Column of data representing first series
'
'     Last%    -  Column of data representing last series
'
'  Return Values:
'     This routine is the main one that checks for errors of input in
'     the ChartEnvironment variable and routine parameters.
'
'=================================================================
SUB clChkForErrors (Env AS ChartEnvironment, TypeMin%, TypeMax%, N%, First%, Last%)

	' Clear any previous error:
	clClearError

	' Check for correct chart type:
	IF Env.ChartType < TypeMin% OR Env.ChartType > TypeMax% THEN
		clSetError cBadType
		EXIT SUB
	END IF

	' Check for valid chart style:
	IF Env.ChartStyle < 1 OR Env.ChartStyle > 2 THEN
		clSetError cBadStyle
		Env.ChartStyle = 1
	END IF

	' The following things are not relevant for pie charts:
	IF Env.ChartType <> cPie THEN

		' Check LogBase for the X axis (default to 10):
		IF Env.XAxis.RangeType = cLog AND Env.XAxis.LogBase <= 0 THEN
			clSetError cBadLogBase
			Env.XAxis.LogBase = 10
		END IF

		' Check LogBase for the Y axis (default to 10):
		IF Env.YAxis.RangeType = cLog AND Env.YAxis.LogBase <= 0 THEN
			clSetError cBadLogBase
			Env.YAxis.LogBase = 10
		END IF

		' Check X axis ScaleFactor:
		IF Env.XAxis.AutoScale <> cYes AND Env.XAxis.ScaleFactor = 0 THEN
			clSetError cBadScaleFactor
			Env.XAxis.ScaleFactor = 1
		END IF

		' Check Y axis ScaleFactor:
		IF Env.YAxis.AutoScale <> cYes AND Env.YAxis.ScaleFactor = 0 THEN
			clSetError cBadScaleFactor
			Env.YAxis.ScaleFactor = 1
		END IF
	END IF

	' Make sure N > 0:
	IF N% <= 0 THEN
		clSetError cTooSmallN
		EXIT SUB
	END IF

	' Check that First series <= Last one:
	IF First% > Last% THEN
		clSetError cTooFewSeries
		EXIT SUB
	END IF

	' Force ChartWindow to be valid:
	clChkChartWindow Env

END SUB

'=== clChkInit - Check that chartlib has been initialized
'
'  Arguments:
'     none
'
'  Return Values:
'     none
'
'=================================================================
SUB clChkInit
SHARED GP AS GlobalParams

	IF NOT GP.Initialized THEN clInitChart

END SUB

'=== clChkPalettes - Makes sure that palettes are dimensioned correctly
'
'  Arguments:
'     C%()     -  Color palette array
'
'     S%()     -  Style palette array
'
'     P$()     -  Pattern palette array
'
'     Char%()  -  Plot character palette array
'
'     B%()     -  Border pattern palette array
'
'  Return Values:
'     Chart error may be set to cBadPalette
'
'=================================================================
SUB clChkPalettes (C() AS INTEGER, s() AS INTEGER, P$(), Char() AS INTEGER, B() AS INTEGER)

	' Check each palette array to be sure it is dimensioned from 0
	' to cPalLen:
	FOR i% = 1 TO 5
		SELECT CASE i%
			CASE 1:  L% = LBOUND(C, 1): U% = UBOUND(C, 1)
			CASE 2:  L% = LBOUND(s, 1): U% = UBOUND(s, 1)
			CASE 3:  L% = LBOUND(P$, 1): U% = UBOUND(P$, 1)
			CASE 4:  L% = LBOUND(Char, 1): U% = UBOUND(Char, 1)
			CASE 5:  L% = LBOUND(B, 1): U% = UBOUND(B, 1)
		END SELECT

		' If incorrectly dimensioned then set error:
		IF (L% <> 0) OR (U% <> cPalLen) THEN
			clSetError cBadPalette
			EXIT SUB
		END IF
	NEXT i%

END SUB

'=== clClearError - Clears ChartErr, the ChartLib error variable
'
'  Arguments:
'     None
'
'  Return Values:
'     Sets ChartErr to 0
'
'=================================================================
SUB clClearError

	ChartErr = 0

END SUB

'=== clColorMaskH% - Function to generate a byte with each pixel set to
'                  some color - for high-res modes (7,8,9,10)
'
'  Arguments:
'     Bits%    -  Number of bits per pixel in current screen mode
'
'     Colr%    -  Color to make the mask
'
'     CMask%() -  One dimensional array to place mask values in
'
'  Return Values:
'     Screen modes 7, 8, 9 and 10 use bit planes.  Rather than using
'     adjacent bits in one byte to determine a color, they use bits
'     "stacked" on top of each other in different bytes.  This routine
'     generates one byte of a particular color by setting the different
'     levels of the stack to &H00 and &HFF to represent eight pixels
'     of a particular color.
'
'=================================================================
SUB clColorMaskH (Bits%, Colr%, CMask%())

	' Copy the color to a local variable:
	RefColor% = Colr%

	' Bits% is the number of bit planes, set a mask for each one:
	FOR i% = 1 TO Bits%

		' Check rightmost bit in color, if it is set to 1 then this plane is
		' "on" (it equals &HFF):
		IF RefColor% MOD 2 <> 0 THEN
			CMask%(i%) = &HFF

		' If the bit is 0, the plane is off (it equals &H0):
		ELSE
			CMask%(i%) = &H0
		END IF

		' Shift the reference color right by one bit:
		RefColor% = RefColor% \ 2
	NEXT i%
			
END SUB

'=== clColorMaskL% - Function to generate a byte with each pixel set to
'                 some color.
'
'  Arguments:
'     Bits%    -  Number of bits per pixel in current screen mode
'
'     Colr%    -  Color to make the mask
'
'  Return Values:
'     Returns integer with low byte that contains definitions for
'     pixels of specified color.
'
'=================================================================
FUNCTION clColorMaskL% (Bits%, Colr%)

	' Initialize the mask to zero:
	M% = 0

	' Multiplying a number by (2 ^ Bits%) will shift it left by "Bits%" bits:
	LShift% = 2 ^ Bits%

	' Create a byte in which each pixel (of "Bits%" bits) is set to
	' Colr%.  This is done by setting the mask to "Colr%" then shifting
	' it left by "Bits%" and repeating until the byte is full:
	FOR i% = 0 TO 7 \ Bits%
		M% = M% * LShift% + Colr%
	NEXT i%

	' Return the mask as the value of the function:
	clColorMaskL% = M% MOD 256

END FUNCTION

'=== clDrawAxes - Draws the axes for a chart
'
'  Arguments:
'     Cat$(1)  -  One-dimensional array or category names for use in
'                 labeling the category axis (ignored if category
'                 axis not used)
'
'  Return Values:
'     No return values
'
'=================================================================
SUB clDrawAxes (Cat$())
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED GFI AS FontInfo
SHARED PaletteC%(), PaletteB%()

	' Use temporary variables to refer to axis limits:
	X1 = GE.XAxis.ScaleMin
	X2 = GE.XAxis.ScaleMax
	Y1 = GE.YAxis.ScaleMin
	Y2 = GE.YAxis.ScaleMax

	' To draw the tic/grid lines it is necessary to know where the line
	' starts and ends.  If tic marks are specified (by setting
	' the "labeled" flag in the axis definition) then the
	' tic lines start "ticwidth" below or to the left of the X and
	' Y axis respectively.  If grid lines are specified then the
	' tic/grid line ends at ScaleMax for the respective axis.  The
	' case statements below calculate where the tic/grid lines start
	' based on the above criteria.

	' Check for tic marks first (X Axis):
	SELECT CASE GE.XAxis.Labeled
		CASE cNo:  XTicMinY = Y1
		CASE ELSE
			XTicMinY = Y1 - cTicSize * (Y2 - Y1)
			IF GP.XStagger = cYes THEN
				clSetChartFont GE.XAxis.TicFont
				XTicDropY = GFI.PixHeight * (Y2 - Y1) / (GE.DataWindow.Y2 - GE.DataWindow.Y1)
			ELSE
				XTicDropY = 0
			END IF
	END SELECT

	' (Y Axis):
	SELECT CASE GE.YAxis.Labeled
		CASE cNo:  YTicMinX = X1
		CASE ELSE: YTicMinX = X1 - cTicSize * (X2 - X1)
	END SELECT

	' Now for the other end of the tic/grid lines check for
	' the grid flag (X axis):
	SELECT CASE GE.XAxis.grid
		CASE cNo:  XTicMaxY = Y1
		CASE ELSE: XTicMaxY = Y2
	END SELECT

	' (Y Axis):
	SELECT CASE GE.YAxis.grid
		CASE cNo:  YTicMaxX = X1
		CASE ELSE: YTicMaxX = X2
	END SELECT

	' Now that the beginning and end of the tic/grid lines has been
	' calculated, it is necessary to figure out where they fall along the
	' axes.  This depends on the type of axis: category or value.  On a
	' category axis the tic/grid lines should fall in the middle of each
	' bar set.  This is calculated by adding 1/2 of TicInterval to
	' the beginning of the axis.  On a value axis the tic/grid line
	' falls at the beginning of the axis.  It is also necessary to know
	' the total number of tics per axis.  The following CASE statements
	' calculate this.  Once the first tic/grid location on an axis is
	' calculated the others can be calculated as they are drawn by adding
	' TicInterval each time to the position of the previous tic mark:

	' Location of the first (leftmost) tic/grid line on the X axis:
	TicTotX% = CINT((X2 - X1) / GE.XAxis.TicInterval)
	SELECT CASE GP.XMode
		CASE cCategory: TicX = X1 + GE.XAxis.TicInterval / 2
		CASE ELSE
			TicX = X1
			TicTotX% = TicTotX% + 1
	END SELECT

	' Location of the first (top) tic/grid line on the Y axis:
	TicTotY% = CINT((Y2 - Y1) / GE.YAxis.TicInterval)
	SELECT CASE GP.YMode
		CASE cCategory: TicY = Y1 + GE.YAxis.TicInterval / 2
		CASE ELSE
			TicY = Y1
			TicTotY% = TicTotY% + 1
	END SELECT

	' Now it's time to draw the axes; first the X then the Y axis.
	' There's a small complexity that has to be dealt with first, though.
	' The tic/grid lines are specified in "world" coordinates since that
	' is easier to calculate but the current VIEW (the DataWindow) would
	' clip them since tic marks (and also labels) lie outside of that
	' region.  The solution is to extrapolate the DataWindow "world" to the
	' ChartWindow region and set our VIEW to the ChartWindow.  This will
	' clip labels if they are too long and try to go outside the Chart
	' Window but still allow use of world coordinates for specifying
	' locations.  To extrapolate the world coordinates to the ChartWindow,
	' PMAP can be used.  This works since PMAP can take pixel coordinates
	' outside of the current VIEW and map them to the appropriate world
	' coordinates.  The DataWindow coordinates (calculated in the routine
	' clSizeDataWindow) are expressed relative to the ChartWindow so
	' it can be somewhat complicated trying to understand what to use with
	' PMAP.  If you draw a picture of it things will appear more straight
	' forward.
  
	' To make sure that bars and columns aren't drawn over the axis lines
	' temporarily move the left DataWindow border left by one and the bottom
	' border down by one pixel:
	GE.DataWindow.X1 = GE.DataWindow.X1 - 1
	GE.DataWindow.Y2 = GE.DataWindow.Y2 + 1

	' Select the DataWindow view and assign the "world" to it:
	clSelectRelWindow GE.DataWindow
	WINDOW (X1, Y1)-(X2, Y2)
	GTextWindow X1, Y1, X2, Y2, cFalse

	' Next, use PMAP to extrapolate to ChartWindow:
	WorldX1 = PMAP(-GE.DataWindow.X1, 2)
	WorldX2 = PMAP(GP.ChartWid - 1 - GE.DataWindow.X1, 2)

	WorldY1 = PMAP(GP.ChartHgt - 1 - GE.DataWindow.Y1, 3)
	WorldY2 = PMAP(-GE.DataWindow.Y1, 3)
  
	' Reset the DataWindow borders back to their original settings:
	GE.DataWindow.X1 = GE.DataWindow.X1 + 1
	GE.DataWindow.Y2 = GE.DataWindow.Y2 - 1

	' Finally, select the ChartWindow VIEW and apply the extrapolated
	' window to it:
	clSelectChartWindow
	WINDOW (WorldX1, WorldY1)-(WorldX2, WorldY2)
	GTextWindow WorldX1, WorldY1, WorldX2, WorldY2, cFalse

	 ' Draw the X and Y axes (one pixel to left and bottom of window):
	CX% = PaletteC%(clMap2Pal%(GE.XAxis.AxisColor))  ' Color of X axis
	CY% = PaletteC%(clMap2Pal%(GE.YAxis.AxisColor))  ' Color of Y axis

	SX% = PaletteB%(clMap2Pal%(GE.XAxis.GridStyle)) ' Line styles; X grid
	SY% = PaletteB%(clMap2Pal%(GE.YAxis.GridStyle)) ' Line styles; Y grid

	LINE (X1, Y1)-(X2, Y1), CX%
	LINE (X1, Y1)-(X1, Y2), CY%

	' X-Axis...Draw styled grid line then solid tic mark:
	TicLoc = TicX
	Stagger% = cFalse
	FOR i% = 1 TO TicTotX%
		LINE (TicLoc, Y1)-(TicLoc, XTicMaxY), CY%, , SX%
		IF Stagger% THEN
			LINE (TicLoc, XTicMinY - XTicDropY)-(TicLoc, Y1), CX%
			Stagger% = cFalse
		ELSE
			LINE (TicLoc, XTicMinY)-(TicLoc, Y1), CX%
			Stagger% = cTrue
		END IF
		TicLoc = TicLoc + GE.XAxis.TicInterval
	NEXT i%

	' Y-Axis...Draw styled grid line then solid tic mark:
	TicLoc = TicY
	FOR i% = 1 TO TicTotY%
		LINE (X1, TicLoc)-(YTicMaxX, TicLoc), CX%, , SY%
		LINE (YTicMinX, TicLoc)-(X1, TicLoc), CY%
		TicLoc = TicLoc + GE.YAxis.TicInterval
	NEXT i%

	' Label X tic marks and print titles:
	clLabelXTics GE.XAxis, Cat$(), TicX, TicTotX%, XTicMinY, YBoundry%
	clTitleXAxis GE.XAxis, GE.DataWindow.X1, GE.DataWindow.X2, YBoundry%

	' Label Y tic marks and print titles:
	clLabelYTics GE.YAxis, Cat$(), YTicMinX, TicY, TicTotY%
	clTitleYAxis GE.YAxis, GE.DataWindow.Y1, GE.DataWindow.Y2

END SUB

'=== clDrawBarData - Draws data portion of multi-series bar chart
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clDrawBarData
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED PaletteC%()
SHARED V1()

	' Set the VIEW to the DataWindow:
	clSelectRelWindow GE.DataWindow

	' Set the WINDOW to match:
	WINDOW (GE.XAxis.ScaleMin, GE.YAxis.ScaleMin)-(GE.XAxis.ScaleMax, GE.YAxis.ScaleMax)
	GTextWindow GE.XAxis.ScaleMin, GE.YAxis.ScaleMin, GE.XAxis.ScaleMax, GE.YAxis.ScaleMax, cFalse
  
	' If this is a linear axis then determine where the bars should grow from:
	IF GE.XAxis.RangeType = cLinear THEN

		' If the scale minimum and maximum are on opposite sides of zero
		' set the bar starting point to zero:
		IF GE.XAxis.ScaleMin < 0 AND GE.XAxis.ScaleMax > 0 THEN
			BarMin = 0

		' If the axis range is all negative the the bars should grow from
		' the right to the left so make the bar starting point the scale
		' maximum:
		ELSEIF GE.XAxis.ScaleMin < 0 THEN
			BarMin = GE.XAxis.ScaleMax

		' The axis range is all positive so the bar starting point is the
		' scale minimum:
		ELSE
			BarMin = GE.XAxis.ScaleMin
		END IF
	  
	' The bar starting point for log axes should always be the scale minimum
	' since only positive numbers are represented on a log axis (even though
	' the log of small numbers is negative):
	ELSE
		BarMin = GE.XAxis.ScaleMin
	END IF

	' Calculate the width of a bar.  Divide by the number of
	' series if it's a plain (not stacked) chart:
	BarWid = GE.YAxis.TicInterval * cBarWid
	IF GE.ChartStyle = cPlain THEN BarWid = BarWid / GP.NSeries

	' Calculate the beginning Y value of first bar then loop drawing
	' all the bars:
	SpaceWid = GE.YAxis.TicInterval * (1 - cBarWid)
	StartLoc = GE.YAxis.ScaleMax - SpaceWid / 2

	FOR i% = 1 TO GP.NVals

		' Reset sum variables for positive and negative stacked bars:
		RSumPos = 0
		RSumNeg = 0

		' Reset the bar starting points:
		BarStartPos = BarMin
		BarStartNeg = BarMin

		' Reset starting Y location of this bar set:
		BarLoc = StartLoc

		' Now, chart the different series for this category:
		FOR J% = 1 TO GP.NSeries

			' Get the value to chart from the data array:
			V = V1(i%, J%)

			' If the value isn't a missing one then try to chart it:
			IF V <> cMissingValue THEN

				' If the X-axis has the AutoScale flag set then divide
				' the value by the axis' ScaleFactor variable:
				IF GE.XAxis.AutoScale = cYes THEN V = V / GE.XAxis.ScaleFactor

				' If this is a plain chart then calculate the bar's location
				' and draw it:
				IF GE.ChartStyle = cPlain THEN

					BarLoc = StartLoc - (J% - 1) * BarWid
					clRenderBar BarMin, BarLoc, V, BarLoc - BarWid, J%

				' If the bars should be stacked then draw either a positive or
				' negative portion of a bar depending on whether the data value
				' is positive or negative:
				ELSE

					' If the value is positive:
					IF V > 0 THEN

						' Add the value to the current sum for the bar and draw
						' the bar from the top of the last portion:
						RSumPos = RSumPos + V
						clRenderBar BarStartPos, BarLoc, RSumPos, BarLoc - BarWid, J%
						BarStartPos = RSumPos

					' If the value is negative:
					ELSE

						' Add the value to the current sum for the bar and draw
						' the bar from the bottom of the last portion:
						RSumNeg = RSumNeg + V
						clRenderBar BarStartNeg, BarLoc, RSumNeg, BarLoc - BarWid, J%
						BarStartNeg = RSumNeg

					END IF
				END IF
			END IF

		NEXT J%

		' Update the bar cluster's starting location:
		StartLoc = StartLoc - GE.YAxis.TicInterval

	NEXT i%

	' If BarMin isn't the axis minimum then draw a reference line:
	IF BarMin <> GE.XAxis.ScaleMin THEN
		LINE (BarMin, GE.YAxis.ScaleMin)-(BarMin, GE.YAxis.ScaleMax), PaletteC%(clMap2Pal%(GE.YAxis.AxisColor))
	END IF

END SUB

'=== clDrawChartWindow - Draws the Chart window
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine erases any previous viewport
'
'=================================================================
SUB clDrawChartWindow
SHARED GE AS ChartEnvironment

	' Define viewport then render window:
	clSelectChartWindow
	clRenderWindow GE.ChartWindow

END SUB

'=== clDrawColumnData - Draws data portion of MS Column chart
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clDrawColumnData
SHARED GP AS GlobalParams, GE AS ChartEnvironment
SHARED PaletteC%(), V1()

	' First, set the VIEW to DataWindow:
	clSelectRelWindow GE.DataWindow

	' Set the WINDOW to match:
	WINDOW (GE.XAxis.ScaleMin, GE.YAxis.ScaleMin)-(GE.XAxis.ScaleMax, GE.YAxis.ScaleMax)
	GTextWindow GE.XAxis.ScaleMin, GE.YAxis.ScaleMin, GE.XAxis.ScaleMax, GE.YAxis.ScaleMax, cFalse

	' If this is a linear axis then determine where the bars should grow from:
	IF GE.YAxis.RangeType = cLinear THEN

		' Draw 0 reference line if the scale minimum and maximum are on
		' opposite sides of zero.  Also set the bar starting point to zero
		' so that bars grow from the zero line:
		IF GE.YAxis.ScaleMin < 0 AND GE.YAxis.ScaleMax > 0 THEN
			BarMin = 0

		' If the axis range is all negative the the bars should grow from
		' the right to the left so make the bar starting point the scale
		' maximum:
		ELSEIF GE.YAxis.ScaleMin < 0 THEN
			BarMin = GE.YAxis.ScaleMax

		' The axis range is all positive so the bar starting point is the
		' scale minimum:
		ELSE
			BarMin = GE.YAxis.ScaleMin
		END IF
	  
	' The bar starting point for log axes should always be the scale minimum
	' since only positive numbers are represented on a log axis (even though
	' the log of small numbers is negative):
	ELSE
		BarMin = GE.YAxis.ScaleMin
	END IF
	  
	' Calculate the width of a bar.  Divide by the number of
	' series if it's a plain (not stacked) chart:
	BarWid = GE.XAxis.TicInterval * cBarWid
	IF GE.ChartStyle = cPlain THEN BarWid = BarWid / GP.NSeries

	' calculate the beginning X value of first bar and loop, drawing all
	' the bars:
	SpaceWid = GE.XAxis.TicInterval * (1 - cBarWid)
	StartLoc = GE.XAxis.ScaleMin + SpaceWid / 2

	FOR i% = 1 TO GP.NVals

		' Reset sum variables for positive and negative stacked bars:
		RSumPos = 0
		RSumNeg = 0

		BarStartPos = BarMin
		BarStartNeg = BarMin

		' Reset starting Y location of this bar set:
		BarLoc = StartLoc

		' Now, go across the rows charting the different series for
		' this category:
		FOR J% = 1 TO GP.NSeries

			' Get the value to chart from the data array:
			V = V1(i%, J%)

			' If the value isn't a missing one then try to chart it:
			IF V <> cMissingValue THEN

				' If the Y-axis has the AutoScale flag set then divide
				' the value by the axis' ScaleFactor variable:
				IF GE.YAxis.AutoScale = cYes THEN V = V / GE.YAxis.ScaleFactor

				' If this is a plain chart then calculate the bar's location
				' and draw it:
				IF GE.ChartStyle = cPlain THEN

					BarLoc = StartLoc + (J% - 1) * BarWid
					clRenderBar BarLoc, BarMin, BarLoc + BarWid, V, J%

				' If the bars should be stacked then draw either a positive or
				' negative portion of a bar depending on whether the data value
				' is positive or negative:
				ELSE

					' If the value is positive:
					IF V > 0 THEN

						' Add the value to the current sum for the bar and draw
						' the bar from the top of the last portion:
						RSumPos = RSumPos + V
						clRenderBar BarLoc, BarStartPos, BarLoc + BarWid, RSumPos, J%
						BarStartPos = RSumPos

					' If the value is negative:
					ELSE

						' Add the value to the current sum for the bar and draw
						' the bar from the bottom of the last portion:
						RSumNeg = RSumNeg + V
						clRenderBar BarLoc, BarStartNeg, BarLoc + BarWid, RSumNeg, J%
						BarStartNeg = RSumNeg

					END IF
				END IF
			END IF

		NEXT J%

		' Update the bar cluster's starting location:
		StartLoc = StartLoc + GE.XAxis.TicInterval

	NEXT i%

	' If BarMin isn't the axis minimum then draw a reference line:
	IF BarMin <> GE.YAxis.ScaleMin THEN
		LINE (GE.XAxis.ScaleMin, BarMin)-(GE.XAxis.ScaleMax, BarMin), PaletteC%(clMap2Pal%(GE.XAxis.AxisColor))
	END IF

END SUB

'=== clDrawDataWindow - Draws the Data window
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine erases any previous viewport or window specification.
'
'=================================================================
SUB clDrawDataWindow
SHARED GE AS ChartEnvironment

	' Define viewport then render window:
	clSelectRelWindow GE.DataWindow
	clRenderWindow GE.DataWindow

END SUB

'=== clDrawLegend - Draws a legend
'
'  Arguments:
'     SeriesLabel$(1)   - Array of labels for the legend
'
'     First%            - Label number corresponding to first series
'
'     Last%             - Label number corresponding to last series
'
'  Return Values:
'     None.
'
'=================================================================
SUB clDrawLegend (SeriesLabel$(), First AS INTEGER, Last AS INTEGER)

SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED PaletteC%(), PaletteP$(), PaletteCh%()
SHARED GFI AS FontInfo
SHARED LLayout AS LegendLayout

	' If legend flag is No then exit:
	IF GE.Legend.Legend = cNo THEN EXIT SUB

	' Select and render the legend window:
	clSelectRelWindow GE.Legend.LegendWindow
	clRenderWindow GE.Legend.LegendWindow
	WINDOW
	GTextWindow 0, 0, 0, 0, cFalse

	' Start with the first label, set the Y position of the first line
	' of labels and loop through all the rows in the legend:
	clSetChartFont GE.Legend.TextFont
	LabelNum% = First
	YPos% = LLayout.HorizBorder
	FOR i% = 1 TO LLayout.NumRow

		' Set position of beginning of row:
		XPos% = LLayout.VertBorder

		FOR J% = 1 TO LLayout.NumCol

			' Map the label number to a valid palette reference:
			MJ% = clMap2Pal%(LabelNum% - First + 1)

			' Depending on ChartType draw either a filled box or the
			' plot character used for plotting:
			XStep% = LLayout.SymbolSize / GP.Aspect
			SELECT CASE GE.ChartType

				CASE cBar, cColumn, cPie:
					LINE (XPos%, YPos%)-STEP(XStep%, LLayout.SymbolSize), 0, BF
					LINE (XPos%, YPos%)-STEP(XStep%, LLayout.SymbolSize), 1, B
					PAINT (XPos% + 1, YPos% + 1), PaletteP$(MJ%), 1
					LINE (XPos%, YPos%)-STEP(XStep%, LLayout.SymbolSize), PaletteC%(MJ%), B

				CASE cLine, cScatter:
					clSetCharColor MJ%
					PlotChr$ = CHR$(PaletteCh%(MJ%))
					clHPrint XPos% + XStep% - GFI.AvgWidth, YPos% - GFI.Leading, PlotChr$

			END SELECT

			' Print the label for this entry in the legend:
			clSetCharColor GE.Legend.TextColor
			clHPrint XPos% + LLayout.LabelOffset, YPos% - GFI.Leading, SeriesLabel$(LabelNum%)

			' Increment the label count and check count has finished:
			LabelNum% = LabelNum% + 1
			IF LabelNum% > Last THEN EXIT SUB

			' Move over to the next column:
			XPos% = XPos% + LLayout.ColSpacing

		NEXT J%

		' Move position to the next row:
		YPos% = YPos% + LLayout.RowSpacing

	NEXT i%

END SUB

'=== clDrawLineData - Draws data portion line chart
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clDrawLineData
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED PaletteC%(), PaletteS%(), PaletteCh%()
SHARED GFI AS FontInfo
SHARED V1()

	' First, set the appropriate font and make text horizontal:
	clSetChartFont GE.DataFont
	SetGTextDir 0
  
	' Then, set the view to DataWindow:
	clSelectRelWindow GE.DataWindow

	' Set the window to match:
	WINDOW (GE.XAxis.ScaleMin, GE.YAxis.ScaleMin)-(GE.XAxis.ScaleMax, GE.YAxis.ScaleMax)
	GTextWindow GE.XAxis.ScaleMin, GE.YAxis.ScaleMin, GE.XAxis.ScaleMax, GE.YAxis.ScaleMax, cFalse

	' Loop through the series:
	FOR J% = 1 TO GP.NSeries

		' Map the series number into a valid palette reference:
		MJ% = clMap2Pal%(J%)

		' Calculate starting X location of first point and set
		' last value to missing (since this is the first value in the
		' series the last value wasn't there):
		StartLoc = GE.XAxis.ScaleMin + GE.XAxis.TicInterval / 2
		LastMissing% = cYes

		FOR i% = 1 TO GP.NVals

			' Get a value from the data array:
			V = V1(i%, J%)

			' If the value is missing, set the LastMissing flag to Yes and
			' go to the next value:
			IF V = cMissingValue THEN
				LastMissing% = cYes

			' If the value is not missing then try to chart it:
			ELSE

				' Scale the value (and convert it to a log if this is a
				' Log axis):
				IF GE.YAxis.AutoScale = cYes THEN V = V / GE.YAxis.ScaleFactor

				' If the style dictates lines and the last point wasn't
				' missing then draw a line between the last point and this one:
				IF GE.ChartStyle = cLines AND LastMissing% <> cYes THEN
					LINE -(StartLoc, V), PaletteC%(MJ%), , PaletteS%(MJ%)
				END IF

				' Position and print character:
				CX% = PMAP(StartLoc, 0) - GetGTextLen(CHR$(PaletteCh%(MJ%))) / 2
				CY% = PMAP(V, 1) - GFI.Ascent / 2
				clSetCharColor MJ%
				clHPrint CX%, CY%, CHR$(PaletteCh%(MJ%))

				PSET (StartLoc, V), POINT(StartLoc, V)

				LastMissing% = cNo
			END IF

			' Move to next category position:
			StartLoc = StartLoc + GE.XAxis.TicInterval
		NEXT i%
	NEXT J%

END SUB

'=== clDrawPieData - Draws data part of a pie chart
'
'  Arguments:
'     Value(1)    -  One-dimensional array of data values
'
'     Expl(1)     -  One-dimensional array of explode flags (1=explode, 0=no)
'
'     N%          -  The number of data values to plot
'  Return Values:
'     None
'
'=================================================================
SUB clDrawPieData (value() AS SINGLE, Expl() AS INTEGER, N AS INTEGER)
SHARED GE AS ChartEnvironment
SHARED GP AS GlobalParams
SHARED GFI AS FontInfo
SHARED PaletteC%(), PaletteP$()

	' Set the font to use for percent labels:
	clSetChartFont GE.DataFont

	' Set up some reference variables:
	Pi2 = 2 * cPiVal                  ' 2*PI for radians conversions
	MinAngle = Pi2 / 120              ' Smallest wedge to try to paint
	A1 = -.0000001                    ' Starting and ending angle (set
	A2 = A1                           ' to very small negative to get
												 ' radius line for first wedge)

	' Size the pie.
	' Choose the point in the middle of the data window for the pie center:
	WINDOW (0, 0)-(1, 1)
	X = PMAP(.5, 0)                  ' Distance: left to center
	Y = PMAP(.5, 1)                  ' Distance: bottom to center
	WINDOW                           ' Now, use physical coordinates (pixels)
	GTextWindow 0, 0, 0, 0, cFalse

	' Adjust radii for percent labels if required:
	clSetChartFont GE.DataFont
	IF GE.ChartStyle = cPercent THEN
		RadiusX = (X - 6 * GFI.AvgWidth) * GP.Aspect
		RadiusY = Y - 2 * GFI.PixHeight
	ELSE
		RadiusX = X * GP.Aspect
		RadiusY = Y
	END IF

	' Pick the smallest radius (adjusted for screen aspect) then reduce
	' it by 10% so the pie isn't too close to the window border:
	IF RadiusX < RadiusY THEN
		Radius = RadiusX
	ELSE
		Radius = RadiusY
	END IF
	Radius = (.9 * Radius) / GP.Aspect

	' If radius is too small then error:
	IF Radius <= 0 THEN
		clSetError cBadDataWindow
		EXIT SUB
	END IF

	' Find the sum of the data values (use double precision Sum variable to
	' protect against overflow if summing large data values):
	Sum# = 0
	FOR i% = 1 TO GP.NSeries
		IF value(i%) > 0 THEN Sum# = Sum# + value(i%)
	NEXT i%

	' Loop through drawing and painting the wedges:
	FOR i% = 1 TO N

		' Map I% to a valid palette reference:
		MappedI% = clMap2Pal(i%)

		' Draw wedges for positive values only:
		IF value(i%) > 0 THEN

			' Calculate wedge percent and wedge ending angle:
			Percent = value(i%) / Sum#
			A2 = A1 - Percent * Pi2

			' This locates the angle through the center of the pie wedge and
			' calculates X and Y components of the vector headed in that
			' direction:
			Bisect = (A1 + A2) / 2
			BisectX = Radius * COS(Bisect)
			BisectY = Radius * SIN(Bisect) * GP.Aspect

			' If the piece is exploded then offset it 1/10th of a radius
			' along the bisecting angle calculated above:
			IF Expl(i%) <> 0 THEN
				CX = X + .1 * BisectX
				CY = Y + .1 * BisectY
			ELSE
				CX = X
				CY = Y
			END IF

			' If the angle is large enough, paint the wedge (if wedges of
			' smaller angles are painted, the "paint" will sometimes spill out):
			IF (A1 - A2) > MinAngle THEN
				PX = CX + .8 * BisectX
				PY = CY + .8 * BisectY

				' Outline the wedge in color 1 and paint it black.
				CIRCLE (CX, CY), Radius, 1, A1, A2, GP.Aspect
				PAINT (PX, PY), 0, 1
				' Paint with the appropriate pattern:
				PAINT (PX, PY), PaletteP$(MappedI%), 1
			END IF
			' draw the wedge in the correct color:
			CIRCLE (CX, CY), Radius, PaletteC%(MappedI%), A1, A2, GP.Aspect
	  
			' Label pie wedge with percent if appropriate:
			IF GE.ChartStyle = cPercent THEN
				Label$ = clVal2Str$(Percent * 100, 1, 1) + "%"
				LabelX% = CX + BisectX + (GFI.AvgWidth * COS(Bisect))
				LabelY% = CY + BisectY + (GFI.AvgWidth * SIN(Bisect)) * GP.Aspect

				' Adjust label location for the quadrant:
				Quadrant% = FIX((ABS(Bisect / Pi2)) * 4)
				IF Quadrant% = 0 OR Quadrant% = 1 THEN
					LabelY% = LabelY% - GFI.Ascent
				END IF
				IF Quadrant% = 1 OR Quadrant% = 2 THEN
					LabelX% = LabelX% - GetGTextLen(Label$)
				END IF

				clSetCharColor GE.Legend.TextColor
				clHPrint LabelX%, LabelY%, Label$
			END IF
		END IF

		' Set the beginning of next wedge to the end of this one:
		A1 = A2

	NEXT i%

END SUB

'=== clDrawScatterData - Draws data portion of Scatter chart
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clDrawScatterData
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED PaletteC%(), PaletteS%(), PaletteCh%()
SHARED GFI AS FontInfo
SHARED V1(), V2()

	' Select the chart font and make text output horizontal:
	clSetChartFont GE.DataFont
	SetGTextDir 0
  
	' Now, loop through all the points charting them:
	FOR Series% = 1 TO GP.NSeries

		' Set LastMissing flag to Yes for first point in series:
		LastMissing% = cYes
		MS% = clMap2Pal%(Series%)

		' Loop through all the points, charting them:
		FOR DataPoint% = 1 TO GP.NVals

			' Get the X-value and Y-values from the data arrays:
			VX = V1(DataPoint%, Series%)
			VY = V2(DataPoint%, Series%)

			' If either of the values to chart is missing set LastMissing
			' flag to Yes to indicate a missing point and go to the next point:
			IF VX = cMissingValue OR VY = cMissingValue THEN
				LastMissing% = cYes

			ELSE

				' Otherwise, scale the X and Y values if AutoScale is set for
				' their respective axes:
				IF GE.XAxis.AutoScale = cYes THEN VX = VX / GE.XAxis.ScaleFactor
				IF GE.YAxis.AutoScale = cYes THEN VY = VY / GE.YAxis.ScaleFactor

				' If this is a lined chart and the last point wasn't missing,
				' then draw a line from last point to the current point:
				IF GE.ChartStyle = cLines AND LastMissing% <> cYes THEN
					LINE -(VX, VY), PaletteC%(MS%), , PaletteS%(MS%)
				END IF

				' In any case draw the plot character.  Start by getting the
				' screen coordinates of the character relative to the point
				' just charted:
				CX% = PMAP(VX, 0) - GetGTextLen(CHR$(PaletteCh%(MS%))) / 2
				CY% = PMAP(VY, 1) - GFI.Ascent / 2

				' Now, set the character color and print it:
				clSetCharColor MS%
				clHPrint CX%, CY%, CHR$(PaletteCh%(MS%))

				' Finally, reset the graphics cursor, since printing the
				' character changed it:
				PSET (VX, VY), POINT(VX, VY)

				LastMissing% = cNo
			END IF

		NEXT DataPoint%
	NEXT Series%
END SUB

'=== clDrawTitles - Draws the main and subtitles on a chart
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clDrawTitles
SHARED GE AS ChartEnvironment
SHARED TTitleLayout AS TitleLayout

	' Bottom of main title line is 1-1/2 character heights from the
	' top of the chart window:
	YPos% = TTitleLayout.Top
	clPrintTitle GE.MainTitle, YPos%

	' Add 1.5 * character height to y position for subtitle line position:
	YPos% = YPos% + TTitleLayout.TitleOne + TTitleLayout.Middle
	clPrintTitle GE.SubTitle, YPos%

END SUB

'=== clFilter - Filters input data into dynamic working data array
'
'  Arguments:
'     Axis     -  An AxisType variable
'
'     AxisMode%-  Mode for this axis
'
'     D1(1)    -  One-dimensional array of input data
'
'     D2(2)    -  Two-dimensional array for filtered data
'
'     N%       -  The number of values to transfer
'
'  Return Values:
'     Alters values in D2()
'
'=================================================================
SUB clFilter (Axis AS AxisType, AxisMode%, D1(), D2(), N%)

	' If the axis is a category one then exit:
	IF AxisMode% = cCategory THEN EXIT SUB

	' Transfer the data from the input data array to the working data
	' array:
	FOR i% = 1 TO N%
		D2(i%, 1) = D1(i%)
	NEXT i%

	' Call FilterMS to go through the data again scaling it and taking
	' logs depending on the settings for this axis:
	clFilterMS Axis, AxisMode%, D2(), D2(), N%, 1, 1

END SUB

'=== clFilterMS - Filters two-dimensional input data into the dynamic working
'               data array
'
'  Arguments:
'     Axis     -  An AxisType variable
'
'     AxisMode%-  Axis mode for the axis
'
'     D1(2)    -  Two-dimensional array of input data
'
'     D2(2)    -  Two-dimensional array for filtered data
'
'     N%       -  The number of values to transfer
'
'     First%   -  First data series to filter
'
'     Last%    -  Last data series to filter
'
'  Return Values:
'     Alters values in D2()
'
'=================================================================
SUB clFilterMS (Axis AS AxisType, AxisMode%, D1(), D2(), N%, First%, Last%)

	' If the axis is a category axis then exit:
	IF AxisMode% = cCategory THEN EXIT SUB

	' If this isn't an autoscale axis, use the scale factor from the
	' environment.  If it is an autoscale axis don't scale at all now
	' it will be done when the data is drawn on the screen:
	IF Axis.AutoScale = cNo THEN
		ScaleFactor = Axis.ScaleFactor
	ELSE
		ScaleFactor = 1
	END IF

	' If this a log axis calculate the log base:
	IF AxisMode% = cLog THEN LogRef = LOG(Axis.LogBase)

	' Loop through the data series:
	FOR J% = First% TO Last%

		' Loop through the values within the series:
		FOR i% = 1 TO N%

			' Get a data value and if it isn't missing, then scale it:
			V = D1(i%, J%)
			IF V <> cMissingValue THEN V = V / ScaleFactor

			' If the axis is a log axis, then if the value is greater than
			' it is safe to take it's log.  Otherwise, set the data value to
			' missing:
			IF Axis.RangeType = cLog THEN
				 IF V > 0 THEN
					V = LOG(V) / LogRef
				ELSE
					V = cMissingValue
				END IF
			END IF

			' Place the value in the output data array:
			D2(i%, J% - First% + 1) = V

		NEXT i%

	NEXT J%

END SUB

'=== clFlagSystem - Sets GP.SysFlag to cYes
'
'  Arguments:
'     None
'
'  Return Values:
'     Alters the value of GP.SysFlag
'
'=================================================================
SUB clFlagSystem
SHARED GP AS GlobalParams

	GP.SysFlag = cYes

END SUB

'=== clFormatTics - Figures out tic label format and TicDecimals.
'
'  Arguments:
'     Axis     -  AxisType variable for which to format tics.
'
'  Return Values:
'     The TicFormat and Decimals elements may be changed for an axis
'     if AutoTic is cYes.
'
'=================================================================
SUB clFormatTics (Axis AS AxisType)

	' If AutoScale isn't Yes then exit
	IF Axis.AutoScale <> cYes THEN EXIT SUB

	' If the size of the largest value is bigger than seven decimal
	' places then set TicFormat to exponential.  Otherwise, set it
	' to normal:
	IF ABS(Axis.ScaleMin) >= 10 ^ 8 OR ABS(Axis.ScaleMax) >= 10 ^ 8 THEN
		Axis.TicFormat = cExpFormat
	ELSE
		Axis.TicFormat = cNormFormat
	END IF

	' Pick the largest of the scale max and min (in absolute value) and
	' use that to decide how many decimals to use when displaying the tic
	' labels:
	Range = ABS(Axis.ScaleMax)
	IF ABS(Axis.ScaleMin) > Range THEN Range = ABS(Axis.ScaleMin)
	IF Range < 10 THEN
		TicResolution = -INT(-ABS(LOG(Range) / LOG(10!))) + 1
		IF TicResolution > 9 THEN TicResolution = 9
		Axis.TicDecimals = TicResolution
	ELSE
		Axis.TicDecimals = 0
	END IF

END SUB

'=== clGetStyle - Returns a predefined line-style definition
'
'  Arguments:
'     StyleNum%   -  A number identifying the entry to return
'
'  Return Values:
'     Returns the line-style for the specified style number
'
'=================================================================
FUNCTION clGetStyle% (StyleNum%)

	SELECT CASE StyleNum%
		CASE 1: Style% = &HFFFF
		CASE 2: Style% = &HF0F0
		CASE 3: Style% = &HF060
		CASE 4: Style% = &HCCCC
		CASE 5: Style% = &HC8C8
		CASE 6: Style% = &HEEEE
		CASE 7: Style% = &HEAEA
		CASE 8: Style% = &HF6DE
		CASE 9: Style% = &HF6F6
		CASE 10: Style% = &HF56A
		CASE 11: Style% = &HCECE
		CASE 12: Style% = &HA8A8
		CASE 13: Style% = &HAAAA
		CASE 14: Style% = &HE4E4
		CASE 15: Style% = &HC88C
	END SELECT
	clGetStyle% = Style%

END FUNCTION

'=== clHPrint - Prints text Horizontally on the screen
'
'  Arguments:
'     X     -  X position for the lower left of the first character to be
'              printed (in absolute screen coordinates)
'
'     Y     -  Y position for the lower left of the first character to be
'              printed (in absolute screen coordinates)
'
'     Txt$  -  Text to print
'
'  Return Values:
'     None
'
'=================================================================
SUB clHPrint (X%, Y%, Txt$)

	' Map the input coordinates relative to the current viewport:
	X = PMAP(X%, 2)
	Y = PMAP(Y%, 3)
  
	' Output the text horizontally:
	SetGTextDir 0
	TextLen% = OutGText(X, Y, Txt$)

END SUB

'=== clInitChart - Initializes the charting library.
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine initializes some default data structures and is
'     called automatically by charting routines if the variable
'     GP.Initialized is cNo (or zero).
'
'=================================================================
SUB clInitChart
SHARED StdChars%(), GP AS GlobalParams

	' Clear any previous errors
	clClearError
  
	ON ERROR GOTO UnexpectedErr

	' Initialize PaletteSet to no so palettes will be initialized properly
	' when ChartScreen is called:
	GP.PaletteSet = cNo

	' Set up the list of plotting characters:
	PlotChars$ = "*ox=+/:&#@%![$^"
	StdChars%(0) = 0
	FOR i% = 1 TO cPalLen
		StdChars%(i%) = ASC(MID$(PlotChars$, i%, 1))
	NEXT i%

	' Initialize standard structures for title, axis, window and legend:
	clInitStdStruc

	GP.Initialized = cYes

END SUB

'=== clInitStdStruc - Initializes structures for standard titles, axes, etc.
'
'  Arguments:
'     None
'
'  Return Values:
'     None
'
'=================================================================
SUB clInitStdStruc
SHARED DAxis AS AxisType, DWindow AS RegionType
SHARED DLegend AS LegendType, DTitle AS TitleType
  
' Set up default components of the default chart
' environment; start with default title:

' Default title definition:
DTitle.Title = ""                ' Title text is blank
DTitle.TitleFont = 1             ' Title font is first one
DTitle.TitleColor = 1            ' Title color is white
DTitle.Justify = cCenter         ' Center justified

' Default axis definition:
DAxis.grid = cNo                 ' No grid
DAxis.GridStyle = 1              ' Solid lines for grid
DAxis.AxisTitle = DTitle         ' Use above to initialize axis title
DAxis.AxisColor = 1              ' Axis color is white
DAxis.Labeled = cYes             ' Label and tic axis
DAxis.RangeType = cLinear        ' Linear axis
DAxis.LogBase = 10               ' Logs to base 10
DAxis.AutoScale = cYes           ' Automatically scale numbers if needed
DAxis.ScaleTitle = DTitle        ' Scale title
DAxis.TicFont = 1                ' Tic font is first one
DAxis.TicDecimals = 0            ' No decimals

' Default window definition:
DWindow.Background = 0           ' Black background
DWindow.Border = cNo             ' Window will have no border
DWindow.BorderColor = 1          ' Make the borders white
DWindow.BorderStyle = 1          ' Solid-line borders

' Default legend definition:
DLegend.Legend = cYes            ' Draw a legend if multi-series chart
DLegend.Place = cRight           ' On the right side
DLegend.TextColor = 1            ' Legend text is white on black
DLegend.TextFont = 1             ' Legend text font is first one
DLegend.AutoSize = cYes          ' Figure out size automatically
DLegend.LegendWindow = DWindow   ' Use the default window specification

END SUB

'=== clLabelXTics - Labels tic marks for X axis
'
'  Arguments:
'     Axis     -  An AxisType variable containing axis specification
'
'     Cat$(1)  -  One-dimensional array of category labels.  Ignored
'                 if axis not category axis
'
'     TicX     -  X coordinate of first tic mark
'
'     TicY     -  Y coordinate of tic tip (portion away from axis)
'
'     YBoundry% -  Y coordinate of bottom of tic labels
'
'  Return Values:
'     None
'
'=================================================================
SUB clLabelXTics (Axis AS AxisType, Cat$(), TicX, TicTotX%, TicY, YBoundry%)
SHARED GFI AS FontInfo
SHARED GP AS GlobalParams
SHARED GE AS ChartEnvironment

	' If this axis isn't supposed to be labeled then exit:
	IF Axis.Labeled <> cYes THEN EXIT SUB

	' Set the appropriate color, font, and orientation for tic labels:
	clSetCharColor Axis.AxisColor
	clSetChartFont Axis.TicFont
	SetGTextDir 0
  
	' The Y coordinate of the labels will be a constant .5 character
	' heights below the end of the tic marks (TicY):
	Y% = PMAP(TicY, 1) + (GFI.Ascent - GFI.Leading) / 2
	IF GP.XStagger = cYes THEN
		YDrop% = (3 * GFI.Ascent - GFI.Leading) / 2
	ELSE
		YDrop% = 0
	END IF
	YBoundry% = Y% + YDrop% + GFI.PixHeight

	' Loop through and write labels
	TX = TicX
	CatNum% = 1
	Stagger% = cFalse
	FOR i% = 1 TO TicTotX%

		' The label depends on axis mode (category, value):
		SELECT CASE GP.XMode
			CASE cCategory: Txt$ = Cat$(CatNum%)
			CASE ELSE:      Txt$ = clVal2Str$(TX, Axis.TicDecimals, Axis.TicFormat)
		END SELECT
		TxtLen% = GetGTextLen(Txt$)
		IF GP.XMode = cCategory THEN
			MaxLen% = 2 * (GE.DataWindow.X2 - GE.DataWindow.X1) / GP.NVals - GFI.AvgWidth
			IF MaxLen% < 0 THEN MaxLen% = 0
			DO UNTIL TxtLen% <= MaxLen%
				Txt$ = LEFT$(Txt$, LEN(Txt$) - 1)
				TxtLen% = GetGTextLen(Txt$)
			LOOP
		END IF

		' Center the label under the tic mark and print it:
		X% = PMAP(TX, 0) - (TxtLen%) / 2

		IF Stagger% THEN
			clHPrint X%, Y% + YDrop%, Txt$
			Stagger% = cFalse
		ELSE
			clHPrint X%, Y%, Txt$
			Stagger% = cTrue
		END IF

		' Move to the next tic mark:
		TX = TX + Axis.TicInterval
		CatNum% = CatNum% + 1
	NEXT i%

END SUB

'=== clLabelYTics - Labels tic marks and draws Y axis title
'
'  Arguments:
'     Axis     -  An AxisType variable containing axis specification
'
'     Cat$(1)  -  One-dimensional array of category labels.  Ignored
'                 if axis not category axis
'
'     TicX     -  X coordinate of first tic's tip (away from axis)
'
'     TicY     -  Y coordinate of first tic
'
'  Return Values:
'     None
'
'=================================================================
SUB clLabelYTics (Axis AS AxisType, Cat$(), TicX, TicY, TicTotY%)
SHARED GFI AS FontInfo
SHARED GP AS GlobalParams

	' If axis isn't supposed to be labeled then exit:
	IF Axis.Labeled <> cYes THEN EXIT SUB

	' Set the appropriate color, font, and orientation for tic labels:
	clSetCharColor Axis.AxisColor
	clSetChartFont Axis.TicFont
	SetGTextDir 0

	' Loop through and write labels
	TY = TicY
	CatNum% = 1
	FOR i% = 1 TO TicTotY%

		' The label depends on axis mode (category, value):
		SELECT CASE GP.YMode
			CASE cCategory: Txt$ = Cat$(GP.NVals - CatNum% + 1)
			CASE ELSE:      Txt$ = clVal2Str$(TY, Axis.TicDecimals, Axis.TicFormat)
		END SELECT
		TxtLen% = GetGTextLen(Txt$)

		' Space the label 1/2 character width to the left of the tic
		' mark and center it vertically on the tic mark (round vertical
		' location to the next highest integer):
		X% = PMAP(TicX, 0) - TxtLen% - (.5 * GFI.MaxWidth)
		Y% = -INT(-(PMAP(TY, 1) - (GFI.Ascent + GFI.Leading) / 2))

		' Print the label:
		clHPrint X%, Y%, Txt$

		' Go to the next tic mark:
		TY = TY + Axis.TicInterval
		CatNum% = CatNum% + 1
	NEXT i%

END SUB

'=== clLayoutLegend - Calculates size of the legend
'
'  Arguments:
'     SeriesLabel$(1) - The labels used in the legend
'
'     First%   - The first series (label) to process
'
'     Last%    - The last series (label) to process
'
'  Return Values:
'     The coordinates in the legend window portion of Env are altered
'
'  Remarks:
'     Sizing the legend window requires finding out where it goes (right
'     or bottom) and determining how big the labels are and how big
'     the legend needs to be to hold them.
'
'=================================================================
SUB clLayoutLegend (SeriesLabel$(), First%, Last%)
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED GFI AS FontInfo
SHARED LLayout AS LegendLayout
SHARED TTitleLayout AS TitleLayout
DIM W AS RegionType

	' If "no legend" is specified, then exit:
	IF GE.Legend.Legend = cNo THEN EXIT SUB

	' This may be an auto legend or not, but, in either case we're
	' going to need the following information:
	clSetChartFont GE.Legend.TextFont

	LLayout.SymbolSize = GFI.Ascent - GFI.Leading - 1
	LLayout.HorizBorder = GFI.Ascent
	LLayout.VertBorder = GFI.AvgWidth
	LLayout.RowSpacing = 1.75 * (LLayout.SymbolSize + 1)
	LLayout.LabelOffset = LLayout.SymbolSize / GP.Aspect + GFI.AvgWidth

	'RowLeading% = LLayout.RowSpacing - LLayout.SymbolSize
	RowLeading% = .75 * LLayout.SymbolSize + 1.75

	ColWid% = clMaxStrLen(SeriesLabel$(), First%, Last%) + LLayout.LabelOffset
	LLayout.ColSpacing = ColWid% + GFI.AvgWidth

	' If this isn't an autosize legend:
	IF GE.Legend.AutoSize = cNo THEN

		' Check the legend coordinates supplied by the user to make
		' sure that they are valid.  If they are, exit:
		W = GE.Legend.LegendWindow
		LWid% = W.X2 - W.X1
		LHgt% = W.Y2 - W.Y1
		IF LWid% > 0 AND LHgt% > 0 THEN

			' Calculate the number of columns and rows of labels that will
			' fit in the legend:
			NumCol% = INT((LWid% - LLayout.VertBorder) / (LLayout.ColSpacing))
			IF NumCol% <= 0 THEN NumCol% = 1
			IF NumCol% > GP.NSeries THEN NumCol% = GP.NSeries
			NumRow% = -INT(-GP.NSeries / NumCol%)
			LLayout.NumRow = NumRow%
			LLayout.NumCol = NumCol%

			' Re-calculate the column and row spacing:
			LLayout.ColSpacing = INT((LWid% - LLayout.VertBorder) / NumCol%)
			LLayout.RowSpacing = INT((LHgt% - 2 * LLayout.HorizBorder + RowLeading%) / NumRow%)

			EXIT SUB

		' If invalid legend coordinates are discovered set an error and
		' go on to calculate new ones:
		ELSE

		  clSetError cBadLegendWindow

		END IF
	END IF

	' Do remaining calculations according to the legend placement specified
	' (right, bottom, overlay):
	SELECT CASE GE.Legend.Place

		CASE cRight, cOverlay:

			' Leave room at top for chart titles:
			Top% = TTitleLayout.TotalSize

			' Figure out the maximum number of legend rows that will
			' fit in the amount of space you have left for the legend
			' height.  Then, see how many columns are needed.  Once
			' the number of columns is set refigure how many rows are
			' required:
			NumRow% = INT((GP.ChartHgt - Top% - 2 * LLayout.HorizBorder) / LLayout.RowSpacing)
			IF NumRow% > GP.NSeries THEN NumRow% = GP.NSeries
			NumCol% = -INT(-GP.NSeries / NumRow%)
			NumRow% = -INT(-GP.NSeries / NumCol%)

			' Set the width and height:
			LWid% = NumCol% * LLayout.ColSpacing - GFI.AvgWidth + 2 * LLayout.VertBorder
			LHgt% = (NumRow% * LLayout.RowSpacing - RowLeading% + 2 * LLayout.HorizBorder)

			' Place the legend one character width from right and even with
			' what will be the top of the data window:
			LLft% = GP.ChartWid - 1 - LWid% - GFI.AvgWidth
			LTop% = Top%

		CASE cBottom:

			' The number of label columns that will fit (using the same
			' procedure as above except figure columns first):
			NumCol% = INT((GP.ChartWid - 2 * LLayout.HorizBorder) / LLayout.ColSpacing)
			IF NumCol% > GP.NSeries THEN NumCol% = GP.NSeries
			NumRow% = -INT(-GP.NSeries / NumCol%)
			NumCol% = -INT(-GP.NSeries / NumRow%)

			' Set the width and height:
			LWid% = NumCol% * LLayout.ColSpacing - GFI.AvgWidth + 2 * LLayout.VertBorder
			LHgt% = (NumRow% * LLayout.RowSpacing - RowLeading% + 2 * LLayout.HorizBorder)

			' Center the legend horizontally one character from the bottom:
			LLft% = (GP.ChartWid - 1 - LWid%) / 2
			LTop% = GP.ChartHgt - 1 - LHgt% - GFI.Ascent

	END SELECT

	' Record legend columns and rows:
	LLayout.NumRow = NumRow%
	LLayout.NumCol = NumCol%

	' Finally, place the legend coordinates in GE:
	GE.Legend.LegendWindow.X1 = LLft%
	GE.Legend.LegendWindow.Y1 = LTop%
	GE.Legend.LegendWindow.X2 = LLft% + LWid%
	GE.Legend.LegendWindow.Y2 = LTop% + LHgt%

	' If, after all this, the legend window is invalid, set error:
	IF LLft% < 0 OR LTop% < 0 OR LWid% <= 0 OR LHgt% <= 0 THEN
		clSetError cBadLegendWindow
	END IF

END SUB

'=== clLayoutTitle - Figures out title layouts for Top, X-axis and
'                      Y-axis titles
'
'  Arguments:
'     TL    -  Layout variable into which to place titles
'
'     T1    -  First title
'
'     T2    -  Second Title
'
'  Return Values:
'     none
'
'=================================================================
SUB clLayoutTitle (TL AS TitleLayout, T1 AS TitleType, T2 AS TitleType)
SHARED GFI AS FontInfo

	' Set the title heights initially to 0:
	TL.TitleOne = 0
	TL.TitleTwo = 0
  
	' If the first title is set then get its height:
	Total% = 0
	IF LTRIM$(T1.Title) <> "" THEN
		clSetChartFont T1.TitleFont
		TL.TitleOne = GFI.PixHeight
		Total% = Total% + 1
	END IF
  
	' If the second title is set then get it's height:
	IF LTRIM$(T2.Title) <> "" THEN
		clSetChartFont T2.TitleFont
		TL.TitleTwo = GFI.PixHeight
		Lead2% = GFI.Leading
		Total% = Total% + 1
	END IF
  
	' Set the "leading" values for label spacing depending on how many
	' of the titles were non-blank:
	TotalHeight% = TL.TitleOne + TL.TitleTwo
	SELECT CASE Total%
		CASE 0:
			TL.Top = 8
			TL.Middle = 0
			TL.Bottom = 4

		CASE 1:
			TL.Top = 8 + TotalHeight% / 8
			TL.Middle = 0
			TL.Bottom = TL.Top
	  
		CASE 2:
			TL.Top = 8 + TotalHeight% / 8
			TL.Middle = 0: IF Lead2% = 0 THEN TL.Middle = TL.TitleOne / 2
			TL.Bottom = TL.Top
	END SELECT

	TL.TotalSize = TL.Top + TL.TitleOne + TL.Middle + TL.TitleTwo + TL.Bottom
  
END SUB

'=== clMap2Attrib% - Maps an integer to a screen attribute for current
'                    screen mode
'
'  Arguments:
'     N% - The number to map
'
'  Return Values:
'     The function returns:
'        0 is mapped to 0, all other numbers are mapped to the range
'        1 to GP.MaxColor
'
'=================================================================
FUNCTION clMap2Attrib% (N%)
SHARED GP AS GlobalParams

	AbsN% = ABS(N%)
	IF AbsN% = 0 THEN
		clMap2Attrib% = AbsN%
	ELSE
		clMap2Attrib% = (AbsN% - 1) MOD GP.MaxColor + 1
	END IF

END FUNCTION

'=== clMap2Pal% - Maps an integer into a palette reference
'
'  Arguments:
'     N% - The number to map
'
'  Return Values:
'     The function returns (N%-1) MOD cPalLen + 1
'
'  Remarks:
'     This FUNCTION is used in almost every reference to a palette to ensure
'     that an invalid number doesn't cause a reference outside of a palette
'     array (and thus crash the library).  This FUNCTION maps the first
'     cPalLen values to themselves. Numbers above cPalLen are mapped to
'     the values 2..cPalLen.
'
'=================================================================
FUNCTION clMap2Pal% (N%)

	AbsN% = ABS(N%)
	IF AbsN% > cPalLen THEN
		clMap2Pal% = (AbsN% - 2) MOD (cPalLen - 1) + 2
	ELSE
		clMap2Pal% = AbsN%
	END IF

END FUNCTION

'=== clMaxStrLen% - Finds the length of the longest string in a list
'
'  Arguments:
'     Txt$(1)  -  One-dimensional array of strings to search
'
'     First%   -  First string to consider
'
'     Last%    -  Last string to consider
'
'  Return Values:
'     This FUNCTION returns the length of the longest string
'
'=================================================================
FUNCTION clMaxStrLen% (Txt$(), First%, Last%)

	' Set Max to 0 then loop through each label updating Max if the
	' label is longer:
	Max% = 0
	FOR Row% = First% TO Last%
		L% = GetGTextLen(Txt$(Row%))
		IF L% > Max% THEN Max% = L%
	NEXT Row%

	' Return Max as the value of the FUNCTION:
	clMaxStrLen% = Max%

END FUNCTION

'=== clMaxVal - Returns the maximum of two numbers
'
'  Arguments:
'     A  -  The first number
'
'     B  -  The second number
'
'  Return Values:
'     The function returns the maximum of the two values
'
'=================================================================
FUNCTION clMaxVal (A, B)

	IF A > B THEN clMaxVal = A ELSE clMaxVal = B

END FUNCTION

'=== clPrintTitle - Prints title correctly justified and colored
'
'  Arguments:
'     TitleVar - A TitleType variable containing specifications for the
'                title to be printed
'
'     Y%       - Vertical position in window for bottom of line
'
'  Return Values:
'     None
'
'=================================================================
SUB clPrintTitle (TitleVar AS TitleType, Y%)
SHARED GFI AS FontInfo, GP AS GlobalParams

	' Calculate width of the title text:
	clSetChartFont TitleVar.TitleFont

	Txt$ = RTRIM$(TitleVar.Title)
	TxtLen% = GetGTextLen(Txt$)
	IF TxtLen% = 0 THEN EXIT SUB

	' Calculate horizontal position depending on justification style
	SELECT CASE TitleVar.Justify

		CASE cCenter: X% = (GP.ChartWid - 1 - (TxtLen%)) / 2
		CASE cRight:  X% = GP.ChartWid - 1 - TxtLen% - GFI.AvgWidth
		CASE ELSE:    X% = GFI.AvgWidth

	END SELECT

	' Set color of text and print it:
	clSetCharColor TitleVar.TitleColor
	clHPrint X%, Y%, Txt$

END SUB

'=== clRenderBar - Renders a bar for a bar or column chart
'
'  Arguments:
'     X1    -  Left side of bar (in data world coordinates)
'
'     Y1    -  Top of bar (in data world coordinates)
'
'     X2    -  Right side of bar (in data world coordinates)
'
'     Y2    -  Bottom of bar (in data world coordinates)
'
'     C%    -  Palette entry number to use for border color and fill pattern
'
'  Return Values:
'     None
'
'=================================================================
SUB clRenderBar (X1, Y1, X2, Y2, C%)
SHARED PaletteC%(), PaletteP$()

	' First clear out space for the bar by drawing a bar in black:
	LINE (X1, Y1)-(X2, Y2), 0, BF

	' Put a border around the bar and fill with pattern:
	MC% = clMap2Pal%(C%)

	LINE (X1, Y1)-(X2, Y2), 1, B
	PAINT ((X1 + X2) / 2, (Y1 + Y2) / 2), PaletteP$(MC%), 1
	LINE (X1, Y1)-(X2, Y2), PaletteC%(MC%), B

END SUB

'=== clRenderWindow - Renders a window on the screen
'
'  Arguments:
'     W - A RegionType variable
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine assumes that the viewport is set to the borders of
'     the window to be rendered
'
'=================================================================
SUB clRenderWindow (W AS RegionType)
SHARED PaletteC%(), PaletteB%()

	' Set window since the size of the viewport is unknown and draw
	' a filled box of the background color specified by the window
	' definition:
	WINDOW (0, 0)-(1, 1)
	LINE (0, 0)-(1, 1), PaletteC%(clMap2Pal%(W.Background)), BF

	' Draw a border if specified:
	IF W.Border = cYes THEN
		LINE (0, 0)-(1, 1), PaletteC%(clMap2Pal%(W.BorderColor)), B, PaletteB%(clMap2Pal%(W.BorderStyle))
	END IF

END SUB

'=== clScaleAxis - Calculates minimum, maximum and scale factor for an axis
'
'  Arguments:
'     A        - An AxisType variable
'
'     AxisMode%- cCategory or cValue
'
'     D1(2)    - Two-dimensional array of values to be scaled
'
'  Return Values:
'     ScaleMin, ScaleMax, ScaleFactor, and ScaleTitle elements in
'     axis variable will be altered if it is a category axis or
'     AutoScale is Yes.
'
'=================================================================
SUB clScaleAxis (Axis AS AxisType, AxisMode%, D1())
SHARED GE AS ChartEnvironment, GP AS GlobalParams

	' If this is a category axis then ignore all the flags and force
	' scale parameters to those needed by charting routines:
	IF AxisMode% = cCategory THEN
		Axis.ScaleMin = 0
		Axis.ScaleMax = 1
		Axis.ScaleFactor = 1
		Axis.ScaleTitle.Title = ""
		EXIT SUB
	END IF

	' If AutoScale isn't Yes then exit:
	IF Axis.AutoScale <> cYes THEN EXIT SUB

	' AutoScale was specified, calculate the different scale variables
	' Set maximum and minimum to defaults.

	' Initialize the value- and row-minimum and maximum values to zero:
	VMin = 0
	VMax = 0

	RMin = 0
	RMax = 0

	' Compare data values for minimum and maximum:
	FOR Row% = 1 TO GP.NVals

		' Initialize positive and negative sum variables:
		RSumPos = 0
		RSumNeg = 0

		' Evaluate the value from this row in each series:
		FOR Column% = 1 TO GP.NSeries

			' Get the value from the data array:
			V = D1(Row%, Column%)

			' Process values that aren't missing only:
			IF V <> cMissingValue THEN

				' Add positive values to positive sum and negative ones to
				' negative sum:
				IF V > 0 THEN RSumPos = RSumPos + V
				IF V < 0 THEN RSumNeg = RSumNeg + V

				' Compare the value against current maximum and minimum and
				' replace them if appropriate:
				IF V < VMin THEN VMin = V
				IF V > VMax THEN VMax = V

			END IF

		NEXT Column%

		' Compare the positive and negative sums for this row with the
		' current row maximum and minimum and replace them if appropriate:
		IF RSumNeg < RMin THEN RMin = RSumNeg
		IF RSumPos > RMax THEN RMax = RSumPos

	NEXT Row%

	' If the chart style is one, meaning that the data isn't stacked for
	' bar and column charts, or it is a line or scatter chart then the scale
	' minimum and maximum are the minimum and maximum values found.
	' Each value is adjusted so the data is not drawn on or beyond the
	' border of the data window:
	IF GE.ChartStyle = 1 OR GE.ChartType = cLine OR GE.ChartType = cScatter THEN
		IF VMin < 0 THEN
			Axis.ScaleMin = VMin - .01 * (VMax - VMin)
		END IF
		IF VMax > 0 THEN
			Axis.ScaleMax = VMax + .01 * (VMax - VMin)
		END IF

	' Otherwise, the scale minimum and maximum are the minimum and maximum
	' sums of the data for each row:
	ELSE
		IF RMin < 0 THEN
			Axis.ScaleMin = RMin - .01 * (RMax - RMin)
		END IF
		IF RMax > 0 THEN
			Axis.ScaleMax = RMax + .01 * (RMax - RMin)
		END IF
	END IF

	' If no data then force range to be non-zero:
	IF Axis.ScaleMin = Axis.ScaleMax THEN Axis.ScaleMax = 1

	' Adjust the scale limits by ScaleFactor if required:
	clAdjustScale Axis

END SUB

'=== clSelectChartFont - Selects a font to use and gets info about it
'
'  Arguments:
'     N%    -  Font number to use
'
'  Return Values:
'     none
'
'=================================================================
SUB clSelectChartFont (N%)
SHARED GFI AS FontInfo
  
	' Select the font and get information about it:
	SelectFont N%
	GetFontInfo GFI
END SUB

'=== clSelectChartWindow - Sets viewport to chart window
'
'  Arguments:
'     Env         - A ChartEnvironment variable
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine erases any previous viewport
'
'=================================================================
SUB clSelectChartWindow
SHARED GP AS GlobalParams

	' Set viewport to chart window:
	VIEW (GP.CwX1, GP.CwY1)-(GP.CwX2, GP.CwY2)

END SUB

'=== clSelectRelWindow - Sets viewport to window relative to chart window
'
'  Arguments:
'     Env   - A ChartEnvironment variable
'
'     W     - RegionType variable of window to set
'
'  Return Values:
'     None
'
'  Remarks:
'     This routine erases any previous viewport
'
'=================================================================
SUB clSelectRelWindow (W AS RegionType)
SHARED GP AS GlobalParams

	' New viewport is defined relative to the current one:
	VIEW (GP.CwX1 + W.X1, GP.CwY1 + W.Y1)-(GP.CwX1 + W.X2, GP.CwY1 + W.Y2)

END SUB

'=== clSetAxisModes - Sets axis modes for X- and Y-axis according to
'                   ChartType
'
'  Arguments:
'     None
'
'  Return Values:
'     Alters XAxis and YAxis axis modes
'
'=================================================================
SUB clSetAxisModes
SHARED GE AS ChartEnvironment
SHARED GP AS GlobalParams

	SELECT CASE GE.ChartType

		CASE cBar:
			GP.XMode = cValue
			GP.YMode = cCategory

		CASE cColumn, cLine:
			GP.XMode = cCategory
			GP.YMode = cValue

		CASE cScatter:
			GP.XMode = cValue
			GP.YMode = cValue

		CASE cPie:
			GP.XMode = cCategory
			GP.YMode = cCategory

	END SELECT

END SUB

'=== clSetCharColor - Sets color for DRAW characters
'
'  Arguments:
'     N%    -  Color number
'
'  Return Values:
'     None
'
'=================================================================
SUB clSetCharColor (N%)
SHARED PaletteC%()

	' Check for valid color number then set color if correct:
	SetGTextColor PaletteC%(clMap2Pal%(N%))

END SUB

'=== clSetChartFont - Selects the specified font
'
'  Arguments:
'     N%    -  Number of loaded font to select
'
'  Return Values:
'     none
'
'=================================================================
SUB clSetChartFont (N AS INTEGER)
SHARED GFI AS FontInfo

	' Select font and get information on it:
	SelectFont N%
	GetFontInfo GFI

END SUB

'=== clSetError - Sets the ChartLib error variable
'
'  Arguments:
'     ErrNo    - The error number to set
'
'  Return Values:
'     Sets ChartErr to ErrNo
'
'=================================================================
SUB clSetError (ErrNo AS INTEGER)

	ChartErr = ErrNo

END SUB

'=== clSetGlobalParams - Sets some global parameters that other routines use
'
'  Arguments:
'     None
'
'  Return Values:
'     GP.ValLenX and GP.ValLenY are altered
'
'=================================================================
SUB clSetGlobalParams
SHARED GP AS GlobalParams, GE AS ChartEnvironment

	' Figure out longest label on X axis:
	clSetChartFont GE.XAxis.TicFont
	SF = GE.XAxis.ScaleMin
	Len1 = GetGTextLen(clVal2Str$(SF, GE.XAxis.TicDecimals, GE.XAxis.TicFormat))
	SF = GE.XAxis.ScaleMax
	Len2 = GetGTextLen(clVal2Str$(SF, GE.XAxis.TicDecimals, GE.XAxis.TicFormat))
	GP.ValLenX = clMaxVal(Len1, Len2)

	' Figure out longest label on Y axis:
	clSetChartFont GE.YAxis.TicFont
	SF = GE.YAxis.ScaleMin
	Len1 = GetGTextLen(clVal2Str$(SF, GE.YAxis.TicDecimals, GE.YAxis.TicFormat))
	SF = GE.YAxis.ScaleMax
	Len2 = GetGTextLen(clVal2Str$(SF, GE.YAxis.TicDecimals, GE.YAxis.TicFormat))
	GP.ValLenY = clMaxVal(Len1, Len2)

END SUB

'=== clSizeDataWindow - Calculates general data window size
'
'  Arguments:
'     Cat$(1)  - One-dimensional array of category labels (only
'                used if one of the axes is a category one)
'
'  Return Values:
'     The X1, Y1, X2, Y2 elements of the GE variable will be
'     set to the data window coordinates
'
'=================================================================
SUB clSizeDataWindow (Cat$())
SHARED GE AS ChartEnvironment
SHARED GP AS GlobalParams
SHARED GFI AS FontInfo
SHARED TTitleLayout AS TitleLayout
SHARED XTitleLayout AS TitleLayout
SHARED YTitleLayout AS TitleLayout

	' *** TOP
	' Adjust the top of the data window:
	DTop% = TTitleLayout.TotalSize

	' *** LEFT
	' Do left side:
	DLeft% = YTitleLayout.TotalSize

	' Add room for axis labels if the axis is labeled and not a pie chart:
	IF GE.ChartType <> cPie THEN
		IF GE.YAxis.Labeled = cYes THEN

			' Get the correct font:
			clSetChartFont GE.YAxis.TicFont

			' If it is a category axis then add longest category label:
			IF GP.YMode = cCategory THEN
				DLeft% = DLeft% + clMaxStrLen%(Cat$(), 1, GP.NVals) + .5 * GFI.MaxWidth

			' If it a value axis just add characters for label (plus 1/2 for
			' spacing):
			ELSE
				DLeft% = DLeft% + GP.ValLenY + (.5 * GFI.MaxWidth)
			END IF

		ELSEIF GP.XMode = cValue AND GE.XAxis.Labeled = cYes THEN

			' Then space over 1/2 of the leftmost label on the X Axis if it's
			' a value axis; if it's a category axis assume the label will be
			' correct:
			DLeft% = DLeft% + GP.ValLenX \ 2
		END IF
	END IF

	' *** RIGHT
	' For the right, space over 8 pixels from the right:
	DRight% = 12

	' Then space over 1/2 of the rightmost label on the X Axis if it's
	' a value axis; if it's a category axis assume the label will be
	' correct:
	IF GP.XMode = cValue AND GE.XAxis.Labeled = cYes THEN
		DRight% = DRight% + (GP.ValLenX) \ 2
	END IF

	DRight% = GP.ChartWid - DRight%

	' *** YTIC MARKS
	' Finally, adjust the window coordinates for tic marks (if it's not a
	' pie chart):
	IF GE.ChartType <> cPie THEN
		IF GE.YAxis.Labeled = cYes THEN
			DLeft% = DRight% - (DRight% - DLeft%) / (1 + cTicSize)
		END IF
	END IF

	' *** LEGEND
	' Account for the legend if its on the right:
	IF GE.Legend.Legend = cYes AND GP.MSeries = cYes THEN
		IF GE.Legend.Place = cRight THEN
			A% = GE.Legend.LegendWindow.X1
			DRight% = DRight% - ABS(GP.ChartWid - A%)
		END IF
	END IF

	' Now we have DLeft%, DRight% we can check if the labels fit on the
	' X axis or if we need to put them on two rows:
	GP.XStagger = cFalse
	IF GP.XMode = cCategory AND GE.ChartType <> cPie THEN
		clSetChartFont GE.XAxis.TicFont
		TicInterval% = (DRight% - DLeft%) \ GP.NVals
		IF clMaxStrLen%(Cat$(), 1, GP.NVals) + .5 * GFI.MaxWidth > TicInterval% THEN
			GP.XStagger = cTrue
		END IF
	END IF

	' If we do have to stagger, check if there is enough space to the
	' left and right for long categories.  Make adjustments as necessary:
	IF GP.XStagger THEN
		LenLeft% = GetGTextLen%(Cat$(1)) + GFI.AvgWidth
		LenRight% = GetGTextLen%(Cat$(GP.NVals)) + GFI.AvgWidth
		SizeRight% = cTrue
		SizeLeft% = cTrue
		OldRight% = DRight%
		OldLeft% = DLeft%
		DO WHILE SizeRight% OR SizeLeft%
			IF LenRight% - TicInterval% > 2 * (GP.ChartWid - DRight%) AND 2 * (GP.ChartWid - DRight%) < TicInterval% THEN
				SizeRight% = cTrue
			ELSE
				SizeRight% = cFalse
			END IF
			IF SizeRight% THEN
				TicInterval% = (2 * (GP.ChartWid - DLeft%) - LenRight%) \ (2 * GP.NVals - 1)
				IF LenRight% > 2 * TicInterval% THEN
					TicInterval% = (GP.ChartWid - DLeft%) / (GP.NVals + .5)
				END IF
				DRight% = DLeft% + GP.NVals * TicInterval%
			END IF
			IF LenLeft% - TicInterval% > 2 * DLeft% AND 2 * DLeft% < TicInterval% THEN
				SizeLeft% = cTrue
			ELSE
				SizeLeft% = cFalse
			END IF
			IF SizeLeft% THEN
				TicInterval% = (2 * DRight% - LenLeft%) \ (2 * GP.NVals - 1)
				IF LenLeft% > 2 * TicInterval% THEN
					TicInterval% = DRight% / (GP.NVals + .5)
				END IF
				DLeft% = DRight% - GP.NVals * TicInterval%
			END IF

			' Make sure we haven't gone too far on either side:
			IF DRight% > OldRight% THEN
				DRight% = OldRight%
			END IF
			IF DLeft% < OldLeft% THEN
				DLeft% = OldLeft%
			END IF

			' Check if there has been a change, if not, we are done:
			IF ABS(ChangeRight% - DRight%) + ABS(ChangeLeft% - DLeft%) > 0 THEN
				EXIT DO
			ELSE
				ChangeRight% = DRight%
				ChangeLeft% = DLeft%
			END IF
		LOOP
	END IF

	' *** BOTTOM
	DBot% = XTitleLayout.TotalSize

	' If axis is labeled (and not a pie chart), add row for tic
	' labels + 1/2 row spacing:
	IF GE.XAxis.Labeled = cYes AND GE.ChartType <> cPie THEN
		IF GP.XStagger = cTrue THEN
			DBot% = DBot% + 3 * GFI.PixHeight
		ELSE
			DBot% = DBot% + 1.5 * GFI.PixHeight
		END IF
	END IF

	' Make the setting relative to the chart window:
	DBot% = GP.ChartHgt - 1 - DBot%


	' *** XTIC MARKS
	' Finally, adjust the window coordinates for tic marks (if it's not a
	' pie chart):
	IF GE.ChartType <> cPie THEN
		IF GE.XAxis.Labeled = cYes THEN
			DBot% = DTop% + (DBot% - DTop%) / (1 + cTicSize)
		END IF

	END IF

	' *** LEGEND
	' Account for the legend if its on the bottom:
	IF GE.Legend.Legend = cYes AND GP.MSeries = cYes THEN
		IF GE.Legend.Place = cBottom THEN
			A% = GE.Legend.LegendWindow.Y1
			DBot% = DBot% - ABS(GP.ChartHgt - A%)
		END IF
	END IF

	' Install values in the DataWindow definition:
	GE.DataWindow.X1 = DLeft%
	GE.DataWindow.X2 = DRight%
	GE.DataWindow.Y1 = DTop%
	GE.DataWindow.Y2 = DBot%

	' If the window is invalid then set error:
	IF DLeft% >= DRight% OR DTop% >= DBot% THEN
		clSetError cBadDataWindow
	END IF

END SUB

'=== clSpaceTics - Calculates TicInterval
'
'  Arguments:
'     None
'
'  Return Values:
'     The TicInterval will be altered
'
'  Remarks:
'     The TicInterval is the distance between tic marks in WORLD
'     coordinates (i.e. the coordinates your data are in)
'
'=================================================================
SUB clSpaceTics
SHARED GE AS ChartEnvironment, GP AS GlobalParams
SHARED GFI AS FontInfo

	' X-Axis:
	' Calculate the length of the axis and of the longest tic label.  Then,
	' use that information to calculate the number of tics that will fit:
	clSetChartFont GE.XAxis.TicFont
	AxisLen% = GE.DataWindow.X2 - GE.DataWindow.X1 + 1
	TicWid% = GP.ValLenX + GFI.MaxWidth
	clSpaceTicsA GE.XAxis, GP.XMode, AxisLen%, TicWid%

	' Y-Axis:
	' Same procedure as above:
	clSetChartFont GE.YAxis.TicFont
	AxisLen% = GE.DataWindow.Y2 - GE.DataWindow.Y1 + 1
	TicWid% = 2 * GFI.Ascent
	clSpaceTicsA GE.YAxis, GP.YMode, AxisLen%, TicWid%

END SUB

'=== clSpaceTicsA - Figures out TicInterval for an axis
'
'  Arguments:
'     Axis     -  An AxisType variable to space tics for
'
'     AxisMode%-  cCategory or cValue
'
'     AxisLen% -  Length of the axis in pixels
'
'  Return Values:
'     The TicInterval value may be changed for an axis
'
'  Remarks:
'     The TicInterval is the distance between tic marks in adjusted world
'     coordinates (i.e. the coordinates your data are in scaled by
'     ScaleFactor and adjusted by LogBase if it is a log axis).
'
'=================================================================
SUB clSpaceTicsA (Axis AS AxisType, AxisMode%, AxisLen%, TicWid%)
SHARED GP AS GlobalParams

	' If this is a category axis the tic interval is 1
	' divided by the number-of-categories:
	IF AxisMode% = cCategory THEN
		Axis.TicInterval = 1 / GP.NVals
		EXIT SUB
	END IF

	' Otherwise, if we're supposed to scale this axis then the tic interval
	' depends on how many will fit and some aesthetic considerations:
	IF Axis.AutoScale = cYes THEN
	  
		' Figure which is bigger in absolute value between scale maximum
		' and minimum:
		MaxRange = ABS(Axis.ScaleMax)
		IF ABS(Axis.ScaleMin) > MaxRange THEN MaxRange = ABS(Axis.ScaleMin)

		' Calculate the maximum number of tic marks that will fit:
		MaxTics% = INT(AxisLen% / TicWid%)

		' If the maximum number of tics is one or less set the tic
		' interval to the axis range and the number of tics to one:
		IF MaxTics% <= 1 THEN
			NumTics% = 1
			TicInterval = Axis.ScaleMax - Axis.ScaleMin
	  
		ELSE
			' Guess that the tic interval is equal to 1/10th of the order
			' of magnitude of the largest of the scale max or min:
			TicInterval = .1 * 10 ^ INT(LOG(MaxRange) / LOG(10!))

			' If this doesn't result in too many tic marks then OK. Otherwise
			' multiply the tic interval by 2 and 5 alternatively until the
			' number of tic marks falls into the acceptable range.
			NextStep% = 2
			ScaleRange = Axis.ScaleMax - Axis.ScaleMin
			DO
				NumTics% = -INT(-ScaleRange / TicInterval)
				IF (NumTics% <= MaxTics%) THEN EXIT DO
				TicInterval = TicInterval * NextStep%
				NextStep% = 7 - NextStep%
			LOOP UNTIL NumTics% <= MaxTics%
		END IF

		' Set Axis.TicInterval and adjust scale maximum and minimum:
		Axis.TicInterval = TicInterval
		IF ABS(TicInterval) < 1 THEN
			Axis.TicDecimals = -INT(-ABS(LOG(1.1 * TicInterval) / LOG(10!)))
		END IF

		Axis.ScaleMax = -INT(-Axis.ScaleMax / TicInterval) * TicInterval
		Axis.ScaleMin = INT(Axis.ScaleMin / TicInterval) * TicInterval
	END IF
  
END SUB

'=== clTitleXAxis - Draws titles on X axis (AxisTitle and ScaleTitle)
'
'  Arguments:
'     Axis  -  AxisType variable describing axis
'
'     X1%   -  Left of DataWindow
'
'     X2%   -  Right of DataWindow
'
'     YBoundry%   -  Top boundry of title block
'
'=================================================================
SUB clTitleXAxis (Axis AS AxisType, X1%, X2%, YBoundry%)
SHARED GFI AS FontInfo
SHARED XTitleLayout AS TitleLayout

	CH% = GFI.PixHeight
	CW% = GFI.MaxWidth

	' Set position of first title:
	Y% = YBoundry% + XTitleLayout.Top

	' Loop through the two titles (AxisTitle and ScaleTitle), printing
	' them if they aren't blank:
	FOR i% = 1 TO 2

		' Get the test, color, and justification for the title to be printed:
		SELECT CASE i%

			CASE 1:  ' AxisTitle
				Txt$ = Axis.AxisTitle.Title
				C% = Axis.AxisTitle.TitleColor
				J% = Axis.AxisTitle.Justify
				F% = Axis.AxisTitle.TitleFont
				Lead% = XTitleLayout.Middle
				
			CASE 2:  ' ScaleTitle
				Txt$ = Axis.ScaleTitle.Title
				C% = Axis.ScaleTitle.TitleColor
				J% = Axis.ScaleTitle.Justify
				F% = Axis.ScaleTitle.TitleFont
				Lead% = XTitleLayout.Bottom

		END SELECT
		clSetChartFont F%
		Txt$ = RTRIM$(Txt$)
		TxtLen% = GetGTextLen(Txt$)

		' If the title isn't all blank:
		IF TxtLen% <> 0 THEN

			' Set the title's color:
			clSetCharColor C%

			' Calculate x position of title's first character depending on
			' the justification flag:
			SELECT CASE J%
				CASE cLeft:   X% = X1%
				CASE cCenter: X% = ((X1% + X2%) - TxtLen%) / 2
				CASE ELSE:    X% = X2% - TxtLen%
			END SELECT

			' Write out the text:
			clHPrint X%, Y%, Txt$

			' Move down to the next title position:
			Y% = Y% + GFI.PixHeight + XTitleLayout.Middle

		END IF

	NEXT i%

END SUB

'=== clTitleYAxis - Draws titles on Y axis (AxisTitle and ScaleTitle)
'
'  Arguments:
'     Axis  -  AxisType variable describing axis
'
'     Y1%   -  Top of DataWindow
'
'     Y2%   -  Bottom of DataWindow
'
'  Return Values:
'
'=================================================================
SUB clTitleYAxis (Axis AS AxisType, Y1%, Y2%) STATIC
SHARED GFI AS FontInfo
SHARED YTitleLayout AS TitleLayout

  
	' Set position for first title:
	X% = YTitleLayout.Top

	' Loop through the two titles (AxisTitle and ScaleTitle), printing
	' them if they aren't blank:
	FOR i% = 1 TO 2

		' Get the test, color, and justification for the title to be printed:
		SELECT CASE i%

			CASE 1:  ' AxisTitle
				Txt$ = Axis.AxisTitle.Title
				C% = Axis.AxisTitle.TitleColor
				J% = Axis.AxisTitle.Justify
				F% = Axis.AxisTitle.TitleFont
				Lead% = YTitleLayout.TitleOne + YTitleLayout.Middle

			CASE 2:  ' ScaleTitle
				Txt$ = Axis.ScaleTitle.Title
				C% = Axis.ScaleTitle.TitleColor
				J% = Axis.ScaleTitle.Justify
				F% = Axis.ScaleTitle.TitleFont
				Lead% = 0

		END SELECT
		clSetChartFont F%
		Txt$ = RTRIM$(Txt$)
		TxtLen% = GetGTextLen(Txt$)

		IF TxtLen% <> 0 THEN

			' Set title's color:
			clSetCharColor C%

			' Calculate y position of title's first character depending on
			' the justification flag:
			SELECT CASE J%
				CASE cLeft:   Y% = Y2%
				CASE cCenter: Y% = ((Y1% + Y2%) + TxtLen%) / 2
				CASE ELSE:    Y% = Y1% + (TxtLen% - 1)
			END SELECT

			' Write out the text:
			clVPrint X%, Y%, Txt$

			' Move to next title position:
			X% = X% + Lead%

		END IF

	NEXT i%

END SUB

'=== clUnFlagSystem - Sets GP.SysFlag to cNo
'
'  Arguments:
'     None
'
'  Return Values:
'     Alters the value of GP.SysFlag
'
'=================================================================
SUB clUnFlagSystem
SHARED GP AS GlobalParams

	GP.SysFlag = cNo

END SUB

'=== clVal2Str$ - Converts a single precision value to a string
'
'  Arguments:
'     X        -  The value to convert
'
'     Places%  -  The number of places after the decimal to produce
'
'     Format%  -  1 For normal, other than 1 for exponential
'
'  Return Values:
'     Returns a string representation of the input number
'
'=================================================================
FUNCTION clVal2Str$ (X, Places%, Format%)

	' Make a local copy of the value:
	XX = ABS(X)

	' Force format to exponential if that is specified or number is
	' bigger than a long integer will hold (2^31-1):
	IF Format% <> cNormFormat OR XX >= 2 ^ 31 THEN

		' For exponential format calculate the exponent that will make
		' one decimal to left of decimal place.  This is done by truncating
		' the log (base 10) of XX:
		IF XX = 0 THEN ExpX = 0 ELSE ExpX = INT(LOG(XX) / LOG(10))
		XX = XX / (10 ^ ExpX)

		' If no decimals are specified then a number of 9.5x will be
		' rounded up to 10 leaving two places to left of decimal so check
		' for that and if that occurs divide number by 10 and add 1 to exponent:
		IF Places% <= 0 AND CLNG(XX) > 9 THEN
			XX = XX / 10
			ExpX = ExpX + 1
		END IF

	END IF

	' If no decimal places are specified then generate a rounded integer:
	IF Places% <= 0 THEN
		ValStr$ = LTRIM$(STR$(CLNG(XX)))

	' If decimal places are called for, round number to requisite number of
	' decimals and generate string:
	ELSE

		' Limit places after decimal to six:
		DP% = Places%
		IF DP% > 6 THEN DP% = 6
		RF% = 10 ^ DP%

		' Figure out integer portion:
		IntX = FIX(XX)

		' Round the fractional part to correct number of decimals.  If
		' the fraction carries to the 1's place in the rounding
		' adjust IntX by adding 1:
		FracX = CLNG((1 + XX - IntX) * RF%)
		IF FracX >= 2 * RF% THEN
			IntX = IntX + 1
		END IF

		'Finally, generate the output string:
		ValStr$ = LTRIM$(STR$(IntX)) + "." + MID$(STR$(FracX), 3)

	END IF

	' Add exponent ending if format is exponent:
	IF Format% <> cNormFormat OR ABS(X) > 2 ^ 31 THEN
		ValStr$ = ValStr$ + "E"
		IF ExpX >= 0 THEN ValStr$ = ValStr$ + "+"
		ValStr$ = ValStr$ + LTRIM$(STR$(ExpX))
	END IF

	' Add minus sign if appropriate:
	IF X < 0 AND VAL(ValStr$) <> 0 THEN ValStr$ = "-" + ValStr$
	clVal2Str$ = ValStr$

END FUNCTION

'=== clVPrint - Prints text vertically on the screen
'
'  Arguments:
'     X     -  X position of lower left of first char (in absolute screen
'              coordinates)
'
'     Y     -  Y position of lower left of first char (in absolute screen
'              coordinates)
'
'     Txt$  -  Text to print
'
'  Return Values:
'     None
'
'=================================================================
SUB clVPrint (X%, Y%, Txt$)

	' Map the input coordinates relative to the current viewport:
	X = PMAP(X%, 2)
	Y = PMAP(Y%, 3)

	' Print text out vertically:
	SetGTextDir 1
	TextLen% = OutGText(X, Y, Txt$)
	SetGTextDir 0
  
END SUB

'=== DefaultChart - Sets up the ChartEnvironment variable to generate a
'                   default chart of the type and style specified
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     ChartType  - The chart type desired: 1=Bar, 2=Column, 3=Line,
'                  4=Scatter, 5=Pie
'
'     ChartStyle - The chart style (depends on type, see README file)
'
'
'  Return Values:
'     Elements of Env variable are set to default values
'
'  Remarks:
'     This subprogram should be called to initialize the ChartEnvironment
'     variable before a charting routine is called.
'
'=================================================================
SUB DefaultChart (Env AS ChartEnvironment, ChartType AS INTEGER, ChartStyle AS INTEGER)

SHARED DTitle AS TitleType, DWindow AS RegionType
SHARED DAxis AS AxisType, DLegend AS LegendType

	' Clear any previous chart errors:
	clClearError

	' Check initialization:
	clChkInit
 
  ' Put type in environment:
	IF ChartType < 1 OR ChartType > 5 THEN
		clSetError cBadType
		EXIT SUB
	END IF
	Env.ChartType = ChartType

	' Put chart style in environment:
	IF ChartStyle < 1 OR ChartStyle > 2 THEN
		clSetError cBadStyle
		ChartStyle = 1
	END IF
	Env.ChartStyle = ChartStyle

	' Set elements of chart to default:
	Env.DataFont = 1

	Env.MainTitle = DTitle
	Env.SubTitle = DTitle

	Env.ChartWindow = DWindow           ' Chart window is default window
	Env.ChartWindow.Border = cYes       ' with a border.

	Env.DataWindow = DWindow

	Env.XAxis = DAxis
	Env.YAxis = DAxis

	Env.Legend = DLegend

END SUB

'=== GetPaletteDef - Changes an entry in the internal palette
'
'  Arguments:
'     C%()     -  Color palette array
'
'     S%()     -  Style palette array
'
'     P$()     -  Pattern palette array
'
'     Char%()  -  Plot character palette array
'
'     B%()     -  Border style palette array
'
'  Return Values:
'     Chart error may be set
'
'=================================================================
SUB GetPaletteDef (C() AS INTEGER, s() AS INTEGER, P$(), Char() AS INTEGER, B() AS INTEGER)
SHARED GP AS GlobalParams
SHARED PaletteC%(), PaletteS%(), PaletteP$(), PaletteCh%(), PaletteB%()

	' Reset any outstanding errors:
	clClearError

	' Make sure palettes have been initialized:
	IF NOT GP.PaletteSet THEN
		clSetError cPalettesNotSet
		EXIT SUB
	END IF

	' Make sure the user's palettes are the correct size:
	clChkPalettes C(), s(), P$(), Char(), B()
	IF (ChartErr <> 0) THEN EXIT SUB

	' Replace the palette values with input variables (making sure that
	' the color and character numbers are in range):
	FOR N% = 0 TO cPalLen
		C(N%) = PaletteC%(N%)
		s(N%) = PaletteS%(N%)
		P$(N%) = PaletteP$(N%)
		Char(N%) = PaletteCh%(N%)
		B(N%) = PaletteB%(N%)
	NEXT N%

END SUB

'=== GetPattern - Returns a pattern from among 3 pattern palettes
'
'  Arguments:
'     Bits%       -  The number of bits per pixel for the pattern
'
'     PatternNum% -  The pattern number to return
'
'  Return Values:
'     Returns a pattern tile from the list below.
'
'  Remarks:
'     Below are three pattern sets.  There is a set of patterns for one, two
'     and eight bit-per-pixel screens.
'
'=================================================================
FUNCTION GetPattern$ (Bits%, PatternNum%)

	SELECT CASE Bits%

		' One bit-per-pixel patterns:
		CASE 1:
			SELECT CASE PatternNum%
				CASE 1: P$ = CHR$(&HFF)
				CASE 2: P$ = CHR$(&H55) + CHR$(&HAA)
				CASE 3: P$ = CHR$(&H33) + CHR$(&HCC)
				CASE 4: P$ = CHR$(&H0) + CHR$(&HE7)
				CASE 5: P$ = CHR$(&H7F) + CHR$(&HBF) + CHR$(&HDF) + CHR$(&HEF) + CHR$(&HF7) + CHR$(&HFB) + CHR$(&HFD) + CHR$(&HFE)
				CASE 6: P$ = CHR$(&H7E) + CHR$(&HBD) + CHR$(&HDB) + CHR$(&HE7) + CHR$(&HE7) + CHR$(&HDB) + CHR$(&HBD) + CHR$(&H7E)
				CASE 7: P$ = CHR$(&HFE) + CHR$(&HFD) + CHR$(&HFB) + CHR$(&HF7) + CHR$(&HEF) + CHR$(&HDF) + CHR$(&HBF) + CHR$(&H7F)
				CASE 8: P$ = CHR$(&H33) + CHR$(&HCC) + CHR$(&HCC) + CHR$(&H33)
				CASE 9: P$ = CHR$(&H0) + CHR$(&HFD) + CHR$(&H0) + CHR$(&HF7) + CHR$(&H0) + CHR$(&HDF) + CHR$(&H0) + CHR$(&H7F)
				CASE 10: P$ = CHR$(&HF) + CHR$(&H87) + CHR$(&HC3) + CHR$(&HE1) + CHR$(&HF0) + CHR$(&H78) + CHR$(&H3C) + CHR$(&H1E)
				CASE 11: P$ = CHR$(&HA8) + CHR$(&H51) + CHR$(&HA2) + CHR$(&H45) + CHR$(&H8A) + CHR$(&H15) + CHR$(&H2A) + CHR$(&H54)
				CASE 12: P$ = CHR$(&HAA) + CHR$(&H55) + CHR$(&H0) + CHR$(&H0) + CHR$(&HAA) + CHR$(&H55) + CHR$(&H0) + CHR$(&H0)
				CASE 13: P$ = CHR$(&H2A) + CHR$(&H15) + CHR$(&H8A) + CHR$(&H45) + CHR$(&HA2) + CHR$(&H51) + CHR$(&HA8) + CHR$(&H54)
				CASE 14: P$ = CHR$(&H88) + CHR$(&H0) + CHR$(&H22) + CHR$(&H0) + CHR$(&H88) + CHR$(&H0) + CHR$(&H22) + CHR$(&H0)
				CASE 15: P$ = CHR$(&HFF) + CHR$(&H0) + CHR$(&HFF) + CHR$(&H0) + CHR$(&HFF) + CHR$(&H0) + CHR$(&HFF) + CHR$(&H0)
			END SELECT

		' Two bit-per-pixel patterns:
		CASE 2:
			SELECT CASE PatternNum%
				CASE 1: P$ = CHR$(&HFF)
				CASE 2: P$ = CHR$(&HCC) + CHR$(&H33)
				CASE 3: P$ = CHR$(&HF0) + CHR$(&H3C) + CHR$(&HF) + CHR$(&HC3)
				CASE 4: P$ = CHR$(&HF0) + CHR$(&HF)
				CASE 5: P$ = CHR$(&H3) + CHR$(&HC) + CHR$(&H30) + CHR$(&HC0)
				CASE 6: P$ = CHR$(&HFF) + CHR$(&HC)
				CASE 7: P$ = CHR$(&HF0) + CHR$(&HF0) + CHR$(&HF) + CHR$(&HF)
				CASE 8: P$ = CHR$(&HFF) + CHR$(&HC) + CHR$(&H30) + CHR$(&HC0)
				CASE 9: P$ = CHR$(&HC0) + CHR$(&H30) + CHR$(&HC) + CHR$(&H3)
				CASE 10: P$ = CHR$(&HC0) + CHR$(&HC)
				CASE 11: P$ = CHR$(&HCC) + CHR$(&HCC) + CHR$(&H33) + CHR$(&H33)
				CASE 12: P$ = CHR$(&HCC) + CHR$(&HCC) + CHR$(&H0) + CHR$(&H0)
				CASE 13: P$ = CHR$(&HFF) + CHR$(&H33) + CHR$(&H33)
				CASE 14: P$ = CHR$(&HFF) + CHR$(&H0)
				CASE 15: P$ = CHR$(&HCC) + CHR$(&H30) + CHR$(&H0)
			END SELECT

		' Eight bit-per-pixel patterns:
		CASE 8:
			P$ = CHR$(&HFF)

	END SELECT

	' Return the pattern as the value of the function:
	GetPattern$ = P$

END FUNCTION

'=== LabelChartH - Prints horizontal text on a chart
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     X          - Horizontal position of text relative to the left of
'                  the Chart window (in pixels)
'
'     Y          - Vertical position of text relative to the top of
'                  the Chart window (in pixels)
'
'     Font%      - Font number to use for the text
'
'     TxtColor   - Color number (in internal color palette) for text
'
'     TxtString$ - String variable containing text to print
'
'  Return Values:
'     None
'
'=================================================================
SUB LabelChartH (Env AS ChartEnvironment, X AS INTEGER, Y AS INTEGER, Font AS INTEGER, TxtColor AS INTEGER, TxtString$)

	' Reset any outstanding errors:
	clClearError

	' Check initialization and fonts:
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB

	' Select ChartWindow as reference viewport:
	clSelectChartWindow

	' Select font and set color:
	SelectFont Font
	clSetCharColor TxtColor

	' Call internal print routine to print text:
	clHPrint X, Y, TxtString$

END SUB

'=== LabelChartV - Prints vertical text on a chart
'
'  Arguments:
'     Env        - A ChartEnvironment variable
'
'     X          - Horizontal position of text relative to the left of
'                  the Chart window (in pixels)
'
'     Y          - Vertical position of text relative to the top of
'                  the Chart window (in pixels)
'
'     Font%      - Font number to use for the text
'
'     TxtColor   - Color number (in internal color palette) for text
'
'     TxtString$ - String variable containing text to print
'
'  Return Values:
'     None
'
'=================================================================
SUB LabelChartV (Env AS ChartEnvironment, X AS INTEGER, Y AS INTEGER, Font AS INTEGER, TxtColor AS INTEGER, TxtString$)

	' Reset any outstanding errors:
	clClearError

	' Check initialization and fonts:
	clChkInit
	clChkFonts
	IF ChartErr >= 100 THEN EXIT SUB

	' Select ChartWindow as reference viewport:
	clSelectChartWindow

	' Select font and set color:
	SelectFont Font%
	clSetCharColor TxtColor

	' Call internal print routine to print text:
	clVPrint X, Y, TxtString$

END SUB

'=== MakeChartPattern$ - Makes a pattern given reference pattern and
'                        foreground and background colors
'
'  Arguments:
'     RefPattern$ -  Reference pattern
'
'     FG%         -  Foreground color
'
'     BG%         -  Background color
'
'  Return Values:
'     Returns a pattern in standard PAINT format
'     Sets error cBadScreen if ChartScreen hasn't been called
'
'=================================================================
FUNCTION MakeChartPattern$ (RefPattern$, FG AS INTEGER, BG AS INTEGER)
SHARED GP AS GlobalParams

	' Reset any outstanding errors:
	clClearError

	' Check initialization:
	clChkInit
	IF ChartErr >= 100 THEN EXIT FUNCTION
	IF NOT GP.PaletteSet THEN
		clSetError cBadScreen
		EXIT FUNCTION
	END IF

	FGColor% = clMap2Attrib%(FG%)
	BGColor% = clMap2Attrib%(BG%)

	' Screens 1, 2, 11 and 13 are 1 bit plane modes and require one method
	' of generating pattern tiles.  The other modes supported are multiple
	' bit plane modes and require another method of generating pattern
	' tiles.  Select the appropriate method for this screen mode:
	SELECT CASE GP.PaletteScrn
		
		' One bit plane modes:
		CASE 1, 2, 11, 13: SinglePlane% = cTrue
		CASE ELSE: SinglePlane% = cFalse
  
	END SELECT

	' Do foreground part of pattern:
	IF SinglePlane% THEN
			FGPattern$ = clBuildBitP$(GP.PaletteBits, FGColor%, RefPattern$)
	ELSE
			FGPattern$ = clBuildPlaneP$(GP.PaletteBits, FGColor%, RefPattern$)
	END IF

	' Do background part of pattern (if background color is black then
	' the pattern is just the foreground pattern):
	IF BGColor% = 0 THEN
		Pattern$ = FGPattern$

	ELSE
		' Background reference pattern is inverted foreground pattern:
		BGPattern$ = ""
		FOR i% = 1 TO LEN(RefPattern$)
			BGPattern$ = BGPattern$ + CHR$(ASC(MID$(RefPattern$, i%, 1)) XOR &HFF)
		NEXT i%
	  
		' Build the corresponding PAINT style pattern:
		IF SinglePlane% THEN
				BGPattern$ = clBuildBitP$(GP.PaletteBits, BGColor%, BGPattern$)
		ELSE
				BGPattern$ = clBuildPlaneP$(GP.PaletteBits, BGColor%, BGPattern$)
		END IF

		' Put foreground and background patterns back together:
		Pattern$ = ""
		FOR i% = 1 TO LEN(FGPattern$)
			Pattern$ = Pattern$ + CHR$(ASC(MID$(FGPattern$, i%, 1)) OR ASC(MID$(BGPattern$, i%, 1)))
		NEXT i%

	END IF

	MakeChartPattern$ = Pattern$

END FUNCTION

'=== ResetPaletteDef - Resets charting palettes for last screen
'                      mode set with ChartScreen.
'
'=================================================================
SUB ResetPaletteDef
SHARED GP AS GlobalParams

	' Clear outstanding errors:
	clClearError

	' Check initialization:
	clChkInit

	' Make sure that ChartScreen has been called at least once:
	IF NOT GP.PaletteSet THEN
		clSetError cBadScreen
		EXIT SUB
	END IF

	' Now rebuild the palette with the last set screen mode:
	clBuildPalette GP.PaletteScrn, GP.PaletteBits

END SUB

'=== SetPaletteDef - Changes an entry in the internal palette
'
'  Arguments:
'     C%()     -  Color palette array
'
'     S%()     -  Style palette array
'
'     P$()     -  Pattern palette array
'
'     Char%()  -  Plot character palette array
'
'     B%()     -  Border style palette array
'
'  Return Values:
'     Internal chart palettes may be modified or ChartErr set
'
'=================================================================
SUB SetPaletteDef (C() AS INTEGER, s() AS INTEGER, P$(), Char() AS INTEGER, B() AS INTEGER)
SHARED PaletteC%(), PaletteS%(), PaletteP$(), PaletteCh%(), PaletteB%()

	' Reset any outstanding errors and check that palettes are dimesioned
	' correctly:
	clClearError
	clChkPalettes C(), s(), P$(), Char(), B()
	IF (ChartErr <> 0) THEN EXIT SUB
  
	' Check initialization:
	clChkInit

	' Replace the palette values with input variables (making sure that
	' the color and character numbers are in range):
	FOR N% = 0 TO cPalLen
		PaletteC%(N%) = clMap2Attrib%(C%(N%))
		PaletteS%(N%) = s(N%)
		PaletteP$(N%) = P$(N%)
		PaletteCh%(N%) = ABS(Char(N%)) MOD (cMaxChars + 1)
		PaletteB%(N%) = B(N%)
	NEXT N%

END SUB

