'***
' FORMAT.BI - Include file for Format functions
'
'       Copyright <C> 1989 Microsoft Corporation
'
' Purpose - This file is included in user's Basic program to declare
'           Format functions that are in Add-On libraries.
'
'***********************************************************************

DECLARE FUNCTION FormatI$ (byval a%, b$)
DECLARE FUNCTION FormatL$ (byval a&, b$)
DECLARE FUNCTION FormatS$ (byval a!, b$)
DECLARE FUNCTION FormatD$ (byval a#, b$)
DECLARE FUNCTION FormatC$ (byval a@, b$)
DECLARE SUB SetFormatCC (byval a%)
