' PGLINEMS.BAS - Program to generate a simple multi-data series line chart

DEFINT A-Z
'$INCLUDE: 'CHRTB.BI'                 ' Declarations and Definitions
DIM Env AS ChartEnvironment           ' Variable to hold environment structure
DIM AxisLabels(1 TO 4) AS STRING      ' Array of categories
DIM LegendLabels(1 TO 2) AS STRING    ' Array of series labels
DIM Values(1 TO 4, 1 TO 3) AS SINGLE  ' 2-dimentsion array of values to plot

DIM Col%(0 TO cPalLen)          ' Define arrays to hold values retrieved with
DIM Lines%(0 TO cPalLen)        ' call to GetPaletteDef. By modifying these
DIM Fill$(0 TO cPalLen)         ' values, then calling ResetPaletteDef, you
DIM Char%(0 TO cPalLen)         ' can change colors, plot characters, borders,
DIM Bord%(0 TO cPalLen)         ' and even the line styles and fill patterns

' Read the data to display into the arrays

FOR index = 1 TO 2: READ LegendLabels(index): NEXT index
FOR index = 1 TO 4: READ AxisLabels(index): NEXT index

FOR columnindex = 1 TO 2                ' The array has 2 columns, each of
  FOR rowindex = 1 TO 4                 ' which has 4 rows. Each column rep-
    READ Values(rowindex, columnindex)  ' resents 1 full data series. First,
  NEXT rowindex                         ' fill column 1, then fill column 2
NEXT columnindex                        ' with values from the last DATA
                                        ' statement (below).
CLS

ChartScreen 2                           ' Set a common graphics mode

' Retrieve current palette settings, then assign some new values

GetPaletteDef Col%(), Lines%(), Fill$(), Char%(), Bord%()

 Col%(2) = (15)          '  Assign white as color for second-series plot line
 Char%(1) = (4)          '  Assign  "" as plot character for 1st plot line
 Char%(2) = (18)         '  Assign  "" as plot character for 2nd plot line

' Reset the palettes with modified arrays

SetPaletteDef Col%(), Lines%(), Fill$(), Char%(), Bord%()   ' Enter the changes

DefaultChart Env, cLine, cLines         ' Set up multi-series line chart

' Display the chart

ChartMS Env, AxisLabels(), Values(), 4, 1, 2, LegendLabels()

SLEEP                                   ' Keep it onscreen until user presses
                                        ' a key
END

' Simulated data to be shown on chart
DATA "Qtr 1","Qtr 2"
DATA "Admn","Markg","Prodn","Devel"
DATA 38,30,40,32,18,40,20,12

