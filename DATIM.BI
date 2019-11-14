'***
' DATIM.BI - Include file for Date/Time functions
'
'       Copyright <C> 1989 Microsoft Corporation
'
' Purpose - This file is included in user's Basic program to declare
'           Date and Time functions that are in Add-On libraries.
'
'***********************************************************************

DECLARE FUNCTION DateSerial# (Years%, Months%, Days%)
DECLARE FUNCTION DateValue#  (DateText$)
DECLARE FUNCTION Year&	( SerialNumber#)
DECLARE FUNCTION Month&  ( SerialNumber#)
DECLARE FUNCTION Day&  ( SerialNumber#)
DECLARE FUNCTION Weekday&  ( SerialNumber#)
DECLARE FUNCTION Hour&	( SerialNumber#)
DECLARE FUNCTION Minute&  ( SerialNumber#)
DECLARE FUNCTION Now#  ()
DECLARE FUNCTION Second&  ( SerialNumber#)
DECLARE FUNCTION TimeSerial# (Hours%, Minutes%, Seconds%)
DECLARE FUNCTION TimeValue# (TimeText$)
