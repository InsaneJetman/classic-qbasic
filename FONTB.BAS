'*** FONTB.BAS - Font Routines for the Presentation Graphics Toolbox in
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
'  PURPOSE:  These are the toolbox routines to handle graphics text using
'            Windows format raster font files:
'
'  To create a library and QuickLib containing the font routines found
'  in this file, follow these steps:
'       BC /X/FS fontb.bas
'       LIB fontb.lib + fontb + fontasm + qbx.lib;
'       LINK /Q fontb.lib, fontb.qlb,,qbxqlb.lib;
'  If you are going to use this FONTB.QLB QuickLib in conjunction with
'  the charting source code (CHRTB.BAS) or the UI toobox source code
'  (GENERAL.BAS, WINDOW.BAS, MENU.BAS and MOUSE.BAS), you need to
'  include the assembly code routines referenced in these files.  For the
'  charting routines, create FONTB.LIB as follows before you create the
'  QuickLib:
'       LIB fontb.lib + fontb + fontasm + chrtasm + qbx.lib;
'  For the UI toolbox routines, create the library as follows:
'       LIB fontb.lib + fontb + fontasm + uiasm + qbx.lib;
'**************************************************************************

' $INCLUDE: 'QBX.BI'
' $INCLUDE: 'FONTB.BI'

CONST cFALSE = 0              ' Logical False
CONST cTRUE = NOT cFALSE      ' Logical True

CONST cDefaultColor = 15      ' Default character color (white in all modes)
CONST cDefaultDir = 0         ' Default character direction
CONST cDefaultFont = 1        ' Default font selected in LoadFont

CONST cMaxFaceName = 32       ' Maximum length of a font name
CONST cMaxFileName = 66       ' Maximum length of a font file name
CONST cFontResource = &H8008  ' Identifies a font resource
CONST cBitMapType = 0         ' Bitmap font type

CONST cFileFont = 0           ' Font comes from file
CONST cMemFont = 1            ' Font comes from memory

CONST cSizeFontHeader = 118   ' Size of Windows font header

' *********************************************************************
' Data Types:

' Some global variables used:
TYPE GlobalParams
	MaxRegistered     AS INTEGER     ' Max number of registered fonts allowed
	MaxLoaded         AS INTEGER     ' Max number of loaded fonts allowed
	TotalRegistered   AS INTEGER     ' Number of fonts actually registered
	TotalLoaded       AS INTEGER     ' Number of fonts actually loaded

	NextDataBlock     AS INTEGER     ' Next available block in font buffer

	CurrentFont       AS INTEGER     ' Current font number in loaded fonts
	CHeight           AS INTEGER     ' Character height of current font
	FChar             AS INTEGER     ' First char in font
	LChar             AS INTEGER     ' Last char in font
	DChar             AS INTEGER     ' Default char for font
	DSeg              AS INTEGER     ' Segment of current font
	DOffset           AS INTEGER     ' Offset of current font
	FontSource        AS INTEGER     ' Source of current font (File or Mem)

	CharColorInit     AS INTEGER     ' cFALSE (0) means color not initialized
	CharColor         AS INTEGER     ' Character color
	CharDirInit       AS INTEGER     ' cFALSE (0) means dir not initialized
	CharDir           AS INTEGER     ' Character direction
	CharSet           AS INTEGER     ' Character mappings to use

	XPixInc           AS INTEGER     ' X increment direction (0, 1, -1)
	YPixInc           AS INTEGER     ' Y increment direction (0, 1, -1)

	WindowSet         AS INTEGER     ' cTRUE if GTextWindow has been called
	WX1               AS SINGLE      ' Minimum WINDOW X
	WY1               AS SINGLE      ' Minimum WINDOW Y
	WX2               AS SINGLE      ' Maximum WINDOW X
	WY2               AS SINGLE      ' Maximum WINDOW Y
	WScrn             AS INTEGER     ' cTRUE means Y increases top to bottom

END TYPE

' The following 3 types are needed to read .FON files. They are documented
' in chapter 7 of the MS Windows Programmer's Reference:

' Windows font file header:
TYPE WFHeader
	dfVersion         AS INTEGER
	dfSize            AS LONG
	dfCopyright       AS STRING * 60
	dfType            AS INTEGER
	dfPoints          AS INTEGER
	dfVertRes         AS INTEGER
	dfHorizRes        AS INTEGER
	dfAscent          AS INTEGER
	dfInternalLeading AS INTEGER
	dfExternalLeading AS INTEGER
	dfItalic          AS STRING * 1
	dfUnderline       AS STRING * 1
	dfStrikeOut       AS STRING * 1
	dfWeight          AS INTEGER
	dfCharSet         AS STRING * 1
	dfPixWidth        AS INTEGER
	dfPixHeight       AS INTEGER
	dfPitchAndFamily  AS STRING * 1
	dfAvgWidth        AS INTEGER
	dfMaxWidth        AS INTEGER
	dfFirstChar       AS STRING * 1
	dfLastChar        AS STRING * 1
	dfDefaultChar     AS STRING * 1
	dfBreakChar       AS STRING * 1
	dfWidthBytes      AS INTEGER
	dfDevice          AS LONG
	dfFace            AS LONG
	dfBitsPointer     AS LONG
	dfBitsOffset      AS LONG
	pad               AS STRING * 1  ' To ensure word boundry
END TYPE

' Structure for reading resource type and number from a resource
' table:
TYPE ResType
	TypeID            AS INTEGER
	NumResource       AS INTEGER
	Reserved          AS LONG
END TYPE

' Structure for reading an actual resource entry:
TYPE ResEntry
	AddrOffset        AS INTEGER
	Length            AS INTEGER
	ResourceKeywd     AS INTEGER
	ResID             AS INTEGER
	Reserved1         AS LONG
END TYPE

' Internal font header data type:
TYPE IFontInfo
	Status            AS INTEGER  ' Processing status. 0=unproc. else <>0
	FontHeader        AS WFHeader ' The Windows font header
	FaceName          AS STRING * cMaxFaceName   ' Font name
	FileName          AS STRING * cMaxFileName   ' File name
	FontSource        AS INTEGER  ' 0=file, 1=memory
	FileLoc           AS LONG     ' Location in resource file of font file
	DataSeg           AS INTEGER  ' FontData index or Segment address of font
	DataOffset        AS INTEGER  ' Offset  address of font if in memory
	BitsOffset        AS INTEGER  ' Offset from beginning of data to bitmaps
END TYPE

' Type for selecting registered fonts via LoadFont:
TYPE FontSpec
	FaceName    AS STRING * cMaxFaceName
	Pitch       AS STRING * 1
	PointSize   AS INTEGER     ' Fonts point size
	HorizRes    AS INTEGER     ' Horizontal resolution of font
	VertRes     AS INTEGER     ' Vertical resolution of font
	ScrnMode    AS INTEGER     ' Screen mode
	Height      AS INTEGER     ' Pixel height of font
  
	Best        AS INTEGER     ' "Best" flag (true/false)
  
	RegNum      AS INTEGER     ' Number of font in registered list
  
	InMemory    AS INTEGER     ' Whether font is in memory (true/false)
	HdrSeg      AS INTEGER     ' Segment of font in memory
	HdrOff      AS INTEGER     ' Offset of font in segment
	DataSeg     AS INTEGER     ' Segment of data in memory
	DataOff     AS INTEGER     ' Offset of data in segment
END TYPE

' *********************************************************************
' Routine Declarations:

DECLARE SUB flSetFontErr (ErrNum AS INTEGER)
DECLARE SUB flClearFontErr ()
DECLARE SUB flRegisterFont (FileName$, FileNum%)
DECLARE SUB flReadFont (I%)
DECLARE SUB flSizeFontBuffer (NFonts%)
DECLARE SUB flInitSpec (Spec AS ANY)
DECLARE SUB flClearFontStatus ()
DECLARE SUB flGetCurrentScrnSize (XPixels%, YPixels%)
DECLARE SUB flGetBASICScrnSize (ScrnMode%, XPixels%, YPixels%)
DECLARE SUB flInitMask ()
DECLARE SUB flPSET (X%, Y%, Colr%)
DECLARE SUB flChkMax ()

DECLARE FUNCTION flGetFonts! (NFonts%)
DECLARE FUNCTION flMatchFont! (FSpec AS ANY)
DECLARE FUNCTION flGetNum! (Txt$, ChPos%, Default!, ErrV!)
DECLARE FUNCTION flGetNextSpec! (SpecTxt$, ChPos%, Spec AS ANY)
DECLARE FUNCTION flDoNextResource! (Align%, FileName$, FileNum%)
DECLARE FUNCTION flOutGChar% (X%, Y%, ChVal%)

' -- Assembly language routines
DECLARE SUB flMovMem ALIAS "fl_MovMem" (SEG dest AS ANY, BYVAL SrcSeg AS INTEGER, BYVAL SrcOffset AS INTEGER, BYVAL Count AS INTEGER)
DECLARE FUNCTION flANSI% ALIAS "fl_ansi" (BYVAL I%)

DECLARE SUB flSetBltDir ALIAS "fl_SetBltDir" (BYVAL XPixInc%, BYVAL YPixInc%, BYVAL XRowInc%, BYVAL YRowInc%)
DECLARE SUB flSetBltColor ALIAS "fl_SetBltColor" (BYVAL CharColor%)
DECLARE SUB flSetBltParams ALIAS "fl_SetBltParams" (BYVAL HdrLen%, BYVAL CharHgt%, BYVAL FirstChar%, BYVAL LastChar%, BYVAL DefaultChar%)
DECLARE FUNCTION flbltchar% ALIAS "fl_BltChar" (BYVAL FASeg%, BYVAL FAOffset%, BYVAL Char%, BYVAL X%, BYVAL Y%)

' *********************************************************************
' Variable Definitions:

' The following arrays hold font headers and font data as fonts are
' registered and loaded. They are dynamically allocated so they can be
' changed in size to accomodate the number of fonts a program will be
' using:

' $DYNAMIC

' Array to hold header information for registered fonts:
DIM SHARED FontHdrReg(1 TO 10)  AS IFontInfo

' Arrays to hold header information and registered font numbers
' for loaded fonts:
DIM SHARED FontHdrLoaded(1 TO 10) AS IFontInfo
DIM SHARED FontLoadList(1 TO 10) AS INTEGER

' Array to hold font data information:
DIM SHARED FontData(1 TO 1) AS FontDataBlock

' $STATIC

' Structure holding global parameters:
DIM SHARED FGP AS GlobalParams

' Error handler for flChkMax so these arrays will be dimensioned
' to 10 by default:
SetMax:
	REDIM FontHdrLoaded(1 TO 10) AS IFontInfo
	REDIM FontHdrReg(1 TO 10) AS IFontInfo
	REDIM FontLoadList(1 TO 10) AS INTEGER
	RESUME

' Error handler for out of memory error:
MemErr:
	flSetFontErr cNoFontMem
	RESUME NEXT

' Error handler for unexpected errors:
UnexpectedErr:
	flSetFontErr cFLUnexpectedErr + ERR
	RESUME NEXT

' File not found error: RegisterFonts
NoFileErr:
	flSetFontErr cFileNotFound
	RESUME NEXT

'=== flChkMax - Makes sure that max font settings are correct and
'                enforces default of 10 for max loaded and registered
'
'  Arguments:
'     none
'
'  Return Values:
'     none
'
'=================================================================
SUB flChkMax STATIC
SHARED FontHdrLoaded() AS IFontInfo
SHARED FontHdrReg() AS IFontInfo
SHARED FGP AS GlobalParams

' Make sure that GP.MaxLoaded and GP.MaxRegistered match array dimensions
' this will only happen if user hasn't used SetMaxFonts and allows Fontlib
' to set a default of 10 since that is what the arrays are first DIM'd
' to:

ON ERROR GOTO SetMax
FGP.MaxLoaded = UBOUND(FontHdrLoaded)
FGP.MaxRegistered = UBOUND(FontHdrReg)
ON ERROR GOTO UnexpectedErr

END SUB

'=== flClearFontErr - Sets the FontErr variable to 0
'
'  Arguments:
'     none
'
'  Return Values:
'     none
'
'=================================================================
SUB flClearFontErr STATIC

	FontErr = 0

END SUB

'=== flClearFontStatus - Clears the status field in the registered font list
'
'  Arguments:
'     none
'
'=================================================================
SUB flClearFontStatus STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo

FOR I% = 1 TO FGP.TotalRegistered
	FontHdrReg(I%).Status = 0
NEXT I%

END SUB

'=== flDoNextResource - Processes resource from resource table:
'
'  Arguments:
'     Align%      - Alignment shift count for finding resource data
'
'     FileName$   - Name of font file (passed to routine that actually
'                   registers resource entry)
'
'     FileNum%    - File number for reading
'
'  Return Value:
'     The number of fonts actually registered
'
'=================================================================
FUNCTION flDoNextResource (Align%, FileName$, FileNum%) STATIC
DIM ResID AS ResType, Entry AS ResEntry

' Get the first few bytes identifying the resource type and the number
' of this type:
GET FileNum%, , ResID

' If this is not the last resource then process it:
IF ResID.TypeID <> 0 THEN

	' Loop through the entries of this resource and if an entry happens to be
	' a font resource then register it. The file location must be saved
	' for each entry in the resource table since the flRegisterFont
	' routine may go to some other part of the file to read the resource:
	FOR ResourceEntry = 1 TO ResID.NumResource
	  
		GET FileNum%, , Entry
		NextResLoc# = SEEK(FileNum%)
		IF ResID.TypeID = cFontResource THEN
			 
			' Seek to font information, register it, then seek back to
			' the next resource table entry:
			SEEK FileNum%, Entry.AddrOffset * 2 ^ Align% + 1
			flRegisterFont FileName$, FileNum%
			SEEK FileNum%, NextResLoc#
			IF FontErr <> 0 THEN EXIT FUNCTION

		END IF
		
	NEXT ResourceEntry
END IF

' Return the current resource type so that RegisterFonts knows when the
' last resource has been read:
flDoNextResource = ResID.TypeID

END FUNCTION

'=== flGetBASICScrnSize - Returns screen size for specified BASIC screen mode
'
'  Arguments:
'
'     ScrnMode%   -  BASIC screen mode
'
'     XPixels%    -  Number of pixels in horizontal direction
'
'     YPixels%    -  Number of pixels in vertical direction
'
'=================================================================
SUB flGetBASICScrnSize (ScrnMode%, XPixels%, YPixels%) STATIC
	SELECT CASE ScrnMode%
		CASE 1: XPixels% = 320: YPixels% = 200
		CASE 2: XPixels% = 640: YPixels% = 200
		CASE 3: XPixels% = 720: YPixels% = 348
		CASE 4: XPixels% = 640: YPixels% = 400
		CASE 7: XPixels% = 320: YPixels% = 200
		CASE 8: XPixels% = 640: YPixels% = 200
		CASE 9: XPixels% = 640: YPixels% = 350
		CASE 10: XPixels% = 640: YPixels% = 350
		CASE 11: XPixels% = 640: YPixels% = 480
		CASE 12: XPixels% = 640: YPixels% = 480
		CASE 13: XPixels% = 320: YPixels% = 200
		CASE ELSE: XPixels% = 0: YPixels% = 0
	END SELECT
END SUB

'=== flGetCurrentScrnSize - Returns screen size for current screen mode
'
'  Arguments:
'
'     XPixels%    -  Number of pixels in horizontal direction
'
'     YPixels%    -  Number of pixels in vertical direction
'
'=================================================================
SUB flGetCurrentScrnSize (XPixels%, YPixels%) STATIC
DIM Regs AS RegType

' Use DOS interrupt to get current video display mode:
Regs.ax = &HF00
CALL INTERRUPT(&H10, Regs, Regs)

' Set screen size based on mode:
SELECT CASE Regs.ax MOD 256
	CASE &H4: XPixels% = 320: YPixels% = 200
	CASE &H5: XPixels% = 320: YPixels% = 200
	CASE &H6: XPixels% = 640: YPixels% = 200
	CASE &H7: XPixels% = 720: YPixels% = 350
	CASE &H8: XPixels% = 720: YPixels% = 348     ' Hercules
	CASE &HD: XPixels% = 320: YPixels% = 200
	CASE &HE: XPixels% = 640: YPixels% = 200
	CASE &HF: XPixels% = 640: YPixels% = 350
	CASE &H10: XPixels% = 640: YPixels% = 350
	CASE &H11: XPixels% = 640: YPixels% = 480
	CASE &H12: XPixels% = 640: YPixels% = 480
	CASE &H13: XPixels% = 320: YPixels% = 200
	CASE &H40: XPixels% = 640: YPixels% = 400    ' Olivetti
	CASE ELSE: XPixels% = 0: YPixels = 0
END SELECT
END SUB

'=== flGetFonts - Gets fonts specified in FontLoadList
'
'  Arguments:
'     NFonts%  -  Number of fonts to load
'
'  Return Values:
'     Number of fonts successfully loaded
'
'=================================================================
FUNCTION flGetFonts (NFonts%) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo
SHARED FontHdrLoaded() AS IFontInfo
SHARED FontLoadList() AS INTEGER

' Re-dimension font data buffer to fit all the fonts:
flSizeFontBuffer (NFonts%)
IF FontErr = cNoFontMem THEN EXIT FUNCTION

' Clear the font status variables then load the fonts (the status variable
' is used to record which ones have already been loaded so they aren't
' loaded more than once):
flClearFontStatus
FOR Font% = 1 TO NFonts%
	FontNum% = FontLoadList(Font%)

	' If font already loaded then just copy the already-filled-out header
	' to the new slot:
	IF FontHdrReg(FontNum%).Status <> 0 THEN
		FontHdrLoaded(Font%) = FontHdrLoaded(FontHdrReg(FontNum%).Status)
  
	' Otherwise, read the font and update status in registered version
	' to point to the first slot it was loaded into (so we can go get
	' an already-filled-out header from there):
	ELSE
		FontHdrLoaded(Font%) = FontHdrReg(FontNum%)
	  
		' Hold any existing errors:
		HoldErr% = FontErr
		flClearFontErr

		flReadFont Font%
	  
		' If there was an error in reading font, exit. Otherwise,
		' reset the error to what it was before and continue:
		IF FontErr <> 0 THEN
			flGetFonts = FontNum% - 1
			EXIT FUNCTION
		ELSE
			flSetFontErr HoldErr%
		END IF

		FontHdrReg(FontNum%).Status = Font%
	END IF
NEXT Font%

flGetFonts = NFonts%
END FUNCTION

'=== flGetNextSpec - Parses the next spec from the spec string
'
'  Arguments:
'     SpecTxt$ -  String containing font specifications
'
'     ChPos%   -  Current position in string (updated in this routine)
'
'     Spec     -  Structure to contain parsed values
'
'
'  Return Values:
'     0    -  Spec was found
'
'     1    -  No spec found
'
'     2    -  Invalid spec found
'=================================================================
FUNCTION flGetNextSpec (SpecTxt$, ChPos%, Spec AS FontSpec) STATIC

' Initialize some things:
SpecErr = cFALSE
SpecLen% = LEN(SpecTxt$)

' If character pos starts past end of spec then we're done:
IF ChPos% > SpecLen% THEN
	flGetNextSpec = 1
	EXIT FUNCTION
END IF

DO UNTIL ChPos% > SpecLen%

	Param$ = UCASE$(MID$(SpecTxt$, ChPos%, 1))
	ChPos% = ChPos% + 1

	SELECT CASE Param$

		' Skip blanks:
		CASE " ":

		' Font title:
		CASE "T":
		  
			' Scan for font title until blank or end of string:
			StartPos% = ChPos%
			DO UNTIL ChPos% > SpecLen%
				Char$ = MID$(SpecTxt$, ChPos%, 1)
				ChPos% = ChPos% + 1
			LOOP
		  
			' Extract the title:
			TitleLen% = ChPos% - StartPos%
			IF TitleLen% <= 0 THEN
				SpecErr = cTRUE
			ELSE
				Spec.FaceName = MID$(SpecTxt$, StartPos%, TitleLen%)
			END IF

		' Fixed or Proportional font:
		CASE "F", "P":
			Spec.Pitch = Param$

		' Font Size (default to 12 points):
		CASE "S":
			Spec.PointSize = flGetNum(SpecTxt$, ChPos%, 12, SpecErr)

		' Screen Mode:
		CASE "M":
			Spec.ScrnMode = flGetNum(SpecTxt$, ChPos%, -1, SpecErr)

		' Pixel Height:
		CASE "H":
			Spec.Height = flGetNum(SpecTxt$, ChPos%, 0, SpecErr)

		' Best fit:
		CASE "B":
			Spec.Best = cTRUE

		' Registered font number:
		CASE "N":
			Spec.RegNum = flGetNum(SpecTxt$, ChPos%, 0, SpecErr)

		' Font in memory:
		CASE "R":
			Spec.InMemory = cTRUE

		' Spec separator:
		CASE "/":
			EXIT DO

		' Anything else is an error:
		CASE ELSE:
			SpecErr = cTRUE
			ChPos% = ChPos% + 1
	END SELECT
LOOP

' Spec is parsed, make sure a valid screen mode has been specified and
' adjust point sizes for 320x200 screens if necessary:
IF Spec.PointSize <> 0 THEN
  
	' Get screen size for specified mode (with "M" param) or current
	' screen mode:
	IF Spec.ScrnMode < 0 THEN
		flGetCurrentScrnSize XPixels%, YPixels%
	ELSE
		flGetBASICScrnSize Spec.ScrnMode, XPixels%, YPixels%
	END IF

	' If this isn't a graphics mode then set an error and skip the rest:
	IF XPixels% = 0 THEN
		SpecErr = cTRUE
		Spec.PointSize = 0

	' If this is a 320x200 screen mode adjust point sizes to the
	' equivalent EGA font point sizes. Also set the horizontal
	' a vertical resolutions to search for in fonts (horizontal is
	' 96 for all modes, vertical varies):
	ELSE
	  
		' Use a horizontal resolution of 96 for all screens:
		Spec.HorizRes = 96

		IF XPixels% = 320 THEN
			Spec.VertRes = 72
		  
			' In a 320x200 mode scale point sizes to their equivalent
			' EGA fonts (special case 14 and 24 point fonts to map them
			' to the closest EGA font otherwise multiply point size by
			' 2/3:
			SELECT CASE Spec.PointSize
				CASE 14: Spec.PointSize = 10
				CASE 24: Spec.PointSize = 18
				CASE ELSE: Spec.PointSize = Spec.PointSize * 2 / 3
			END SELECT

		ELSE
		  
			' Other screen modes vary only in vertical resolution:
			SELECT CASE YPixels%
				CASE 200: Spec.VertRes = 48
				CASE 350: Spec.VertRes = 72
				CASE 480: Spec.VertRes = 96
			END SELECT
		END IF
	END IF
END IF

' If an error was found somewhere then pass it on and set-up to load
' first font:
IF SpecErr THEN
	flGetNextSpec = 2
	Spec.RegNum = 1
ELSE
	flGetNextSpec = 0
END IF

END FUNCTION

'=== flGetNum - Parses number from string
'
'  Arguments:
'     Txt$     -  String from which to parse number
'
'     ChPos%   -  Character position on which to start
'
'     Default  -  Default value if number not found
'
'     ErrV     -  Returns error as cTrue or cFalse
'
'  Return Values:
'     Returns value found or default
'
'  Notes:
'     Simple state machine:
'        state 0: Looking for first char
'        state 1: Found start (+, -, or digit)
'        state 2: Done
'        state 3: Error
'
'=================================================================
FUNCTION flGetNum (Txt$, ChPos%, Default, ErrV) STATIC

' Start in state 0
State = 0

' Loop until done
DO
	Char$ = MID$(Txt$, ChPos%, 1)
	SELECT CASE Char$

		' Plus and minus are only OK at the beginning:
		CASE "+", "-":
			SELECT CASE State
				CASE 0: Start% = ChPos%: State = 1
				CASE ELSE: State = 3
			END SELECT

		' Digits are OK at the beginning of after plus and minus:
		CASE "0" TO "9":
			SELECT CASE State
				CASE 0: Start% = ChPos%: State = 1
				CASE ELSE:
			END SELECT

		' Spaces are skipped:
		CASE " ":

		' Anything else is an error at the beginning or marks the end:
		CASE ELSE:
			SELECT CASE State
				CASE 0: State = 3
				CASE 1: State = 2
			END SELECT
	END SELECT

	' Go to next character:
	ChPos% = ChPos% + 1
LOOP UNTIL State = 2 OR State = 3

' Scanning is complete; adjust ChPos% to mark last character processed:
ChPos% = ChPos% - 1

' If error then set default number:
IF State = 3 THEN
	flGetNum = Default
	ErrV = cTRUE

' Otherwise, extract number and get its value:
ELSE
	EndPos% = ChPos% - 1
	flGetNum = VAL(MID$(Txt$, Start%, EndPos%))
	ErrV = cFALSE
END IF
END FUNCTION

'=== flInitSpec - Initializes font specification structure
'
'  Arguments:
'     Spec     -  FontSpec variable to initialize
'
'=================================================================
SUB flInitSpec (Spec AS FontSpec) STATIC

	Spec.FaceName = ""
	Spec.Pitch = ""
	Spec.PointSize = 0
	Spec.ScrnMode = -1
	Spec.Height = 0
	Spec.Best = cFALSE
	Spec.RegNum = 0
	Spec.InMemory = cFALSE

END SUB

'=== flMatchFont - Finds first registered font that matches FontSpec
'
'  Arguments:
'     FSpec -  FontSpec variable containing specification to match
'
'  Return Values:
'     Number of registered font matched, -1 if no match.
'
'=================================================================
FUNCTION flMatchFont (FSpec AS FontSpec) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo

' Match a specific registered font:
IF FSpec.RegNum > 0 AND FSpec.RegNum <= FGP.TotalRegistered THEN
	flMatchFont = FSpec.RegNum
	EXIT FUNCTION
END IF

' If this is an invalid spec. then no fonts matched:
IF FontErr <> 0 THEN
	flMatchFont = -1
	EXIT FUNCTION
END IF

' Scan font for first one that matches the rest of the specs:
SelectedFont% = -1
BestSizeDiff = 3.402823E+38
BestFontNum% = -1
FOR FontNum% = 1 TO FGP.TotalRegistered

	' Match a font from memory:
	MemOK% = cTRUE
	IF FSpec.InMemory AND FontHdrReg(FontNum%).FontSource <> cMemFont THEN
		MemOK% = cFALSE
	END IF
  
	' Match name:
	IF FSpec.FaceName = FontHdrReg(FontNum%).FaceName OR LTRIM$(FSpec.FaceName) = "" THEN
		NameOK% = cTRUE
	ELSE
		NameOK% = cFALSE
	END IF

	' Match pitch (fixed or proportional):
	Pitch$ = "F"
	IF FontHdrReg(FontNum%).FontHeader.dfPixWidth = 0 THEN Pitch$ = "P"
	IF FSpec.Pitch = Pitch$ OR FSpec.Pitch = " " THEN
		PitchOK% = cTRUE
	ELSE
		PitchOK% = cFALSE
	END IF

	' Match font size (if neither point or pixel size specified then
	' this font is OK):
	IF FSpec.PointSize = 0 AND FSpec.Height = 0 THEN
		SizeOK% = cTRUE
  
	' Otherwise, if point size specified (note that point size overrides
	' the pixel height if they were both specified)...
	ELSEIF FSpec.PointSize <> 0 THEN
	  
		' Make sure the font resolution matches the screen resolution
		' (pass over this font if not):
		IF FSpec.HorizRes <> FontHdrReg(FontNum%).FontHeader.dfHorizRes THEN
			SizeOK% = cFALSE
		ELSEIF FSpec.VertRes <> FontHdrReg(FontNum%).FontHeader.dfVertRes THEN
			SizeOK% = cFALSE
	  
		' Font has made it past the resolution check, now try to match size:
		ELSE
			SizeDiff = ABS(FSpec.PointSize - FontHdrReg(FontNum%).FontHeader.dfPoints)
			IF SizeDiff = 0 THEN
				SizeOK% = cTRUE
			ELSE
				SizeOK% = cFALSE
			END IF
		END IF


	' Now, the case where height was specified and not point size:
	ELSEIF FSpec.Height <> 0 THEN
		SizeDiff = ABS(FSpec.Height - FontHdrReg(FontNum%).FontHeader.dfPixHeight)
		IF SizeDiff = 0 THEN
			SizeOK% = cTRUE
		ELSE
			SizeOK% = cFALSE
		END IF
	END IF

	' Do record keeping if best-fit was specified:
	IF NOT SizeOK% AND PitchOK% AND FSpec.Best AND SizeDiff < BestSizeDiff THEN
		BestSizeDiff = SizeDiff
		BestFontNum% = FontNum%
	END IF

	' See if this font is OK:
	IF MemOK% AND NameOK% AND PitchOK% AND SizeOK% THEN
		SelectedFont% = FontNum%
		EXIT FOR
	END IF
NEXT FontNum%

' If no font was matched and best-fit was specified then select the
' best font:
IF SelectedFont% < 0 AND FSpec.Best THEN SelectedFont% = BestFontNum%

' Return the font matched:
flMatchFont = SelectedFont%

END FUNCTION

'=== flReadFont - Reads font data and sets up font header
'
'  Arguments:
'     I%    -  Slot in loaded fonts to process
'
'=================================================================
SUB flReadFont (I%) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrLoaded() AS IFontInfo
SHARED FontData() AS FontDataBlock

ON ERROR GOTO UnexpectedErr

' If memory font then it's already in memory:
IF FontHdrLoaded(I%).FontSource = cMemFont THEN
	EXIT SUB

' For a font from a file, read it in:
ELSE
	DataSize# = FontHdrLoaded(I%).FontHeader.dfSize - cSizeFontHeader
	NumBlocks% = -INT(-DataSize# / cFontBlockSize)
	FontHdrLoaded(I%).DataSeg = FGP.NextDataBlock

	' Get next available file number and open file:
	FileNum% = FREEFILE
	OPEN FontHdrLoaded(I%).FileName FOR BINARY AS FileNum%

	' Read blocks from the font file:
	DataLoc# = FontHdrLoaded(I%).FileLoc + cSizeFontHeader
	SEEK FileNum%, DataLoc#
	FOR BlockNum% = 0 TO NumBlocks% - 1
		GET FileNum%, , FontData(FGP.NextDataBlock + BlockNum%)
	NEXT BlockNum%
  
	' Close the file:
	CLOSE FileNum%

	' Update the next data block pointer:
	FGP.NextDataBlock = FGP.NextDataBlock + NumBlocks%
END IF

END SUB

'=== flRegisterFont - Actually registers a font resource:
'
'  Arguments:
'     FileName$   - Name of font file (passed to routine that actually
'                   registers resource entry)
'
'     FileNum%    - File number for reading
'
'=================================================================
SUB flRegisterFont (FileName$, FileNum%) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo

DIM Byte AS STRING * 1, FontHeader AS WFHeader

' Read the font header:
FontLoc# = SEEK(FileNum%)
GET FileNum%, , FontHeader

' Only register vector fonts:
IF FontHeader.dfType AND &H1 <> cBitMapType THEN EXIT SUB

' See that we're still within MaxRegistered limits:
IF FGP.TotalRegistered >= FGP.MaxRegistered THEN
	flSetFontErr cTooManyFonts
	EXIT SUB
END IF

' Go to next "registered" font slot:
FGP.TotalRegistered = FGP.TotalRegistered + 1

' Set font source and save the header and file location:
FontHdrReg(FGP.TotalRegistered).FontSource = cFileFont
FontHdrReg(FGP.TotalRegistered).FontHeader = FontHeader
FontHdrReg(FGP.TotalRegistered).FileLoc = FontLoc#

' Get the face name (scan characters until zero byte):
SEEK FileNum%, FontLoc# + FontHeader.dfFace
FaceName$ = ""
FOR Char% = 0 TO cMaxFaceName - 1
	GET FileNum%, , Byte
	IF ASC(Byte) = 0 THEN EXIT FOR
	FaceName$ = FaceName$ + Byte
NEXT Char%
FontHdrReg(FGP.TotalRegistered).FaceName = FaceName$

' Finally, save the file name:
FontHdrReg(FGP.TotalRegistered).FileName = FileName$

END SUB

'=== flSetFontErr - Sets the FontErr variable to an error value:
'
'  Arguments:
'     ErrNum   -  The error number to set FontErr variable to
'
'=================================================================
SUB flSetFontErr (ErrNum AS INTEGER) STATIC

	 FontErr = ErrNum

END SUB

'=== flSizeFontBuffer - Calculate the FontBuffer size required for all fonts
'
'  Arguments:
'     NFonts%  -  Number of font to be loaded
'
'  Notes:
'     The use of -INT(-N) in the following code rounds N to the next
'     larger integer
'
'=================================================================
SUB flSizeFontBuffer (NFonts%) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo
SHARED FontLoadList() AS INTEGER
SHARED FontData() AS FontDataBlock


ON ERROR GOTO UnexpectedErr
IF NFonts% = 0 THEN EXIT SUB

' Clear font status variables so we know what has been processed:
flClearFontStatus

' Add sizes of all unique fonts together to get total size (each font
' begins on a new font block so the size of each font is calculated in
' terms of the number of font blocks it will take up):
Size = 0
FOR I% = 1 TO NFonts%
	FontNum% = FontLoadList(I%)
	IF FontHdrReg(FontNum%).Status = 0 THEN
		FontSize = FontHdrReg(FontNum%).FontHeader.dfSize - cSizeFontHeader
		Size = Size - INT(-FontSize / cFontBlockSize)
		FontHdrReg(FontNum%).Status = 1
	END IF
NEXT I%

' Dimension the FontData array to hold everything:
ON ERROR GOTO MemErr
REDIM FontData(1 TO Size) AS FontDataBlock
ON ERROR GOTO UnexpectedErr

' Set the next font block to the start for when flReadFont begins
' putting data in the font buffer:
FGP.NextDataBlock = 1

END SUB

'=== GetFontInfo - Returns useful information about current font
'
'  Arguments:
'     FI    -  FontInfo type variable to receive info
'
'=================================================================
SUB GetFontInfo (FI AS FontInfo) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrLoaded() AS IFontInfo

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' Check that some fonts are loaded:
IF FGP.TotalLoaded <= 0 THEN
	flSetFontErr cNoFonts
	EXIT SUB
END IF

' All OK, assign values from internal font header:
FI.FontNum = FGP.CurrentFont
FI.Ascent = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfAscent
FI.Points = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfPoints
FI.PixWidth = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfPixWidth
FI.PixHeight = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfPixHeight
FI.Leading = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfInternalLeading
FI.MaxWidth = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfMaxWidth
FI.AvgWidth = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfAvgWidth
FI.FileName = FontHdrLoaded(FGP.CurrentFont).FileName
FI.FaceName = FontHdrLoaded(FGP.CurrentFont).FaceName

END SUB

'=== GetGTextLen - Returns bit length of string
'
'  Arguments:
'     Text$ -  String for which to return length
'
'  Return Values:
'     -1    -  Error (No fonts loaded, probably)
'
'     >=0   -  Length of string
'
'=================================================================
FUNCTION GetGTextLen% (Text$) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrLoaded() AS IFontInfo
SHARED FontData() AS FontDataBlock

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' Make sure some fonts are loaded:
IF FGP.TotalLoaded <= 0 THEN
	flSetFontErr cNoFonts
	GetGTextLen = -1
	EXIT FUNCTION
END IF

' Assume this is a memory font (may override this later):
CharTblPtr% = FontHdrLoaded(FGP.CurrentFont).DataOffset
CharTblSeg% = FontHdrLoaded(FGP.CurrentFont).DataSeg

' Index into font data array:
CharTable% = FontHdrLoaded(FGP.CurrentFont).DataSeg

' Add together the character lengths from the character table:
TextLen% = 0
FOR I% = 1 TO LEN(Text$)
  
	' Get character code and translate to Ansi if IBM char set is specified:
	ChVal% = ASC(MID$(Text$, I%, 1))
	IF FGP.CharSet = cIBMChars THEN ChVal% = flANSI(ChVal%)
  
	' Convert to default char if out of range:
	IF ChVal% < FGP.FChar OR ChVal% > FGP.LChar THEN ChVal% = FGP.DChar%
  
	' Offset into character table for length word:
	CharOffset% = (ChVal% - FGP.FChar) * 4

	' Peek the data and add it to the text length:
	IF FontHdrLoaded(FGP.CurrentFont).FontSource = cFileFont THEN
		CharTblPtr% = VARPTR(FontData(CharTable%))
		CharTblSeg% = VARSEG(FontData(CharTable%))
	END IF
	DEF SEG = CharTblSeg%
	CharLen% = PEEK(CharTblPtr% + CharOffset%) + PEEK(CharTblPtr% + CharOffset% + 1) * 256
	TextLen% = TextLen% + CharLen%
NEXT I%

GetGTextLen = TextLen%

END FUNCTION

'=== GetMaxFonts - Gets the maximum number of fonts that can be registered
'                  and loaded by the font library:
'
'  Arguments:
'     Registered  -  The maximum number of fonts that can be registered
'                    by the font library
'
'     Loaded      -  The maximum number of fonts that can be loaded by
'                    by the font library
'
'=================================================================
SUB GetMaxFonts (Registered AS INTEGER, Loaded AS INTEGER)
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' If SetMaxFonts hasn't been called then make sure the default is
' correct:
flChkMax

' Simply return the values of the internal variables for maximum
' fonts registered and loaded:
Registered = FGP.MaxRegistered
Loaded = FGP.MaxLoaded

END SUB

'=== GetFontInfo - Returns useful information about current font
'
'  Arguments:
'     Font  -  Font number (in list of registered fonts) on which to get
'              information
'
'     FI    -  FontInfo type variable to receive info
'
'=================================================================
SUB GetRFontInfo (Font AS INTEGER, FI AS FontInfo) STATIC
SHARED FontHdrReg() AS IFontInfo

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' See that they've specified a valid font:
IF Font < 0 OR Font > FGP.TotalRegistered THEN
	flSetFontErr cBadFontNumber
	EXIT SUB
END IF

' All OK, assign values from internal font header:
FI.FontNum = Font
FI.Ascent = FontHdrReg(Font).FontHeader.dfAscent
FI.Points = FontHdrReg(Font).FontHeader.dfPoints
FI.PixWidth = FontHdrReg(Font).FontHeader.dfPixWidth
FI.PixHeight = FontHdrReg(Font).FontHeader.dfPixHeight
FI.Leading = FontHdrReg(Font).FontHeader.dfInternalLeading
FI.MaxWidth = FontHdrReg(Font).FontHeader.dfMaxWidth
FI.AvgWidth = FontHdrReg(Font).FontHeader.dfAvgWidth
FI.FileName = FontHdrReg(Font).FileName
FI.FaceName = FontHdrReg(Font).FaceName

END SUB

'=== GetTotalFonts - Gets the total number of fonts that currently registered
'                    and loaded by the font library:
'
'  Arguments:
'     Registered  -  The total number of fonts registered by the font
'                    library
'
'     Loaded      -  The total number of fonts loaded by the font library
'
'=================================================================
SUB GetTotalFonts (Registered AS INTEGER, Loaded AS INTEGER)
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' Simply return the values of internal variables:
Registered = FGP.TotalRegistered
Loaded = FGP.TotalLoaded

END SUB

'=== GTextWindow - Communicates the current WINDOW to fontlib
'
'  Arguments:
'     X1    -  Minimum X value
'
'     Y1    -  Minimum Y value
'
'     X2    -  Maximum X value
'
'     Y2    -  Maximum Y value
'
'     Scrn% -  cTRUE means that window Y values increase top to bottom
'
'  Remarks:
'     Calling this with X1=X2 or Y1=Y2 will clear the current
'     window.
'
'=================================================================
SUB GTextWindow (X1 AS SINGLE, Y1 AS SINGLE, X2 AS SINGLE, Y2 AS SINGLE, Scrn%)
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' Save the window values in global variable:
FGP.WX1 = X1
FGP.WY1 = Y1
FGP.WX2 = X2
FGP.WY2 = Y2
FGP.WScrn = Scrn%

' If window is valid then flag it as set:
FGP.WindowSet = ((X2 - X1) <> 0) AND ((Y2 - Y1) <> 0)

END SUB

'=== LoadFont - Loads one or more fonts according to specification string
'
'  Arguments:
'     SpecTxt$ -  String containing parameters specifying one or more
'                 fonts to load (see notes below)
'
'  Return Values:
'     The number of fonts loaded
'
'  Notes:
'     A spec. can contain the following parameters in any order.
'     Parameters are each one character immediately followed by a value
'     if called for. Multiple specifications may be entered separated
'     by slash (/) characters. Loadfont will search for the FIRST font in
'     the list of registered fonts that matches each spec. and load it. If
'     no font matches a specification registered font number one will be
'     used. If a given font is selected by more than one spec in the list
'     it will only be loaded once. When this routine is called all
'     previous fonts will be discarded:
'
'        T  -  followed by a blank-terminated name loads font by
'              specified name
'
'        F  -  No value. Selects only fixed pitch fonts
'
'        P  -  No value. Selects only proportional fonts
'
'        S  -  Followed by number specifies desired point size
'
'        M  -  Followed by number specifies the screen mode font will be
'              used on. This is used in conjunction with the "S" parameter
'              above to select appropriately sized font.
'
'        H  -  Followed by number specifies the pixel height of
'              font to select. "S" overrides this.
'
'        N  -  Followed by number selects specific font number
'              from the list of currently registered fonts.
'
'        R  -  Selects font stored in RAM memory
'
'=================================================================
FUNCTION LoadFont% (SpecTxt$) STATIC
SHARED FGP AS GlobalParams
DIM FSpec AS FontSpec

ON ERROR GOTO UnexpectedErr

' Clear outstanding errors and check for valid max limits:
flClearFontErr

flChkMax

' Make sure there's room to load a font:
IF FGP.TotalLoaded >= FGP.MaxLoaded THEN
	flSetFontErr cTooManyFonts
	EXIT FUNCTION
END IF

' Make sure there are some registered fonts to look through:
IF FGP.TotalRegistered <= 0 THEN
	flSetFontErr cNoFonts
	EXIT FUNCTION
END IF

' Process each spec in the spec string:
Slot% = 1
ChPos% = 1
DO UNTIL Slot% > FGP.MaxLoaded

	' Initialize the spec structure:
	flInitSpec FSpec

	' Get next spec from string (Found will be false if no spec found):
	SpecStatus% = flGetNextSpec(SpecTxt$, ChPos%, FSpec)
	SELECT CASE SpecStatus%
		CASE 0:
		CASE 1: EXIT DO
		CASE 2: flSetFontErr cBadFontSpec
	END SELECT

	' Try to match font. Set font to one if none match:
	FontNum% = flMatchFont(FSpec)
	IF FontNum% < 1 THEN
		flSetFontErr cFontNotFound
		FontNum% = 1
	END IF

	' Record font in font load list:
	FontLoadList(Slot%) = FontNum%
	Slot% = Slot% + 1
LOOP

' Now actually get the fonts in the load list:
FGP.TotalLoaded = flGetFonts(Slot% - 1)
FGP.CurrentFont = 1

' Select the first font by default (pass outstanding font errors around
' it):
HoldErr% = FontErr
SelectFont cDefaultFont
IF HoldErr% <> 0 THEN flSetFontErr HoldErr%

LoadFont = FGP.TotalLoaded

END FUNCTION

'=== OutGText - Outputs graphics text to the screen
'
'  Arguments:
'     X        -  X location of upper left of char box
'
'     Y        -  Y location of upper left of char box
'
'     Text$    -  Text string to output
'
'  Return Values:
'     Length of text output, Values of X and Y are updated
'
'=================================================================
FUNCTION OutGText% (X AS SINGLE, Y AS SINGLE, Text$) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrLoaded() AS IFontInfo

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' Make sure fonts are loaded:
IF FGP.TotalLoaded <= 0 THEN
	flSetFontErr cNoFonts
	EXIT FUNCTION
END IF

IF NOT FGP.CharColorInit THEN SetGTextColor cDefaultColor
IF NOT FGP.CharDirInit THEN SetGTextDir cDefaultDir

' Make sure a graphic mode is set:
flGetCurrentScrnSize XP%, YP%
IF XP% = 0 THEN EXIT FUNCTION

' Save input location to working variables and erase any window setting:
IX% = PMAP(X, 0)
IY% = PMAP(Y, 1)
WINDOW

' Map chars to valid ones and output them adding their lengths:
TextLen% = 0
FOR Char% = 1 TO LEN(Text$)
	ChVal% = ASC(MID$(Text$, Char%, 1))
	IF FGP.CharSet = cIBMChars THEN ChVal% = flANSI(ChVal%)
  
	IF FGP.FontSource = cFileFont THEN
		BitMapPtr% = VARPTR(FontData(FGP.DSeg))
		BitMapSeg% = VARSEG(FontData(FGP.DSeg))
	ELSE
		BitMapPtr% = FGP.DOffset
		BitMapSeg% = FGP.DSeg
	END IF

	CharLen% = flbltchar%(BitMapSeg%, BitMapPtr%, ChVal%, IX%, IY%)

	IX% = IX% + FGP.XPixInc * CharLen%
	IY% = IY% + FGP.YPixInc * CharLen%

	TextLen% = TextLen% + CharLen%
NEXT Char%

' Reset window:
IF FGP.WindowSet THEN
	IF FGP.WScrn% THEN
		WINDOW SCREEN (FGP.WX1, FGP.WY1)-(FGP.WX2, FGP.WY2)
	ELSE
		WINDOW (FGP.WX1, FGP.WY1)-(FGP.WX2, FGP.WY2)
	END IF
END IF

' Update locations
X = PMAP(IX%, 2)
Y = PMAP(IY%, 3)

' Return total character length:
OutGText = TextLen%

END FUNCTION

'=== RegisterFonts - Loads header information from font resources:
'
'  Arguments:
'     FileName$   -  Path name for font file to register
'
'  Return Value:
'     The number of fonts actually registered
'
'  Notes:
'     Offsets documented in Windows document assume the file's first
'     byte is byte 0 (zero) and GET assumes the first byte is byte 1 so
'     many GET locations are expressed in the following code as
'     a documented offset + 1.
'
'=================================================================
FUNCTION RegisterFonts% (FileName$) STATIC
SHARED FGP AS GlobalParams
DIM Byte AS STRING * 1

ON ERROR GOTO UnexpectedErr

' Clear errors and make sure things are initialized:
flClearFontErr

flChkMax

' Get next available file number:
FileNum% = FREEFILE

' Try to open the file for input first to make sure the file exists. This
' is done to avoid creating a zero length file if the file doesn't exist.
ON ERROR GOTO NoFileErr
OPEN FileName$ FOR INPUT AS FileNum%
ON ERROR GOTO UnexpectedErr
IF FontErr <> 0 THEN
	RegisterFonts = 0
	EXIT FUNCTION
END IF
CLOSE FileNum%

' File seems to exist, so open it in binary mode:
OPEN FileName$ FOR BINARY ACCESS READ AS FileNum%

' Get the byte that indicates whether this file has a new-style
' header on it. If not, then error:
GET FileNum%, &H18 + 1, Byte
IF ASC(Byte) <> &H40 THEN
	flSetFontErr cBadFontFile
	CLOSE FileNum%
	EXIT FUNCTION
END IF

' Save the number of fonts currently registered for use later in
' calculating the number of fonts registered by this call:
OldTotal = FGP.TotalRegistered

' Get the pointer to the new-style header:
GET FileNum%, &H3C + 1, Word%
NewHdr% = Word%

' Get pointer to resource table:
GET FileNum%, Word% + &H22 + 1, Word%
ResourceEntry# = NewHdr% + Word% + 1

' Get the alignment shift count from beginning of table:
GET FileNum%, ResourceEntry#, Align%

' Loop, registering font resources until they have run out:
DO
	ResType% = flDoNextResource(Align%, FileName$, FileNum%)
	IF FontErr <> 0 THEN EXIT DO
LOOP UNTIL ResType% = 0

CLOSE FileNum%

' Finally, return number of fonts actually registered:
RegisterFonts = FGP.TotalRegistered - OldTotal

END FUNCTION

'=== RegisterMemFont - Loads header information from a memory-resident font
'
'  Arguments:
'     FontSeg%    -  Segment address of font to register
'
'     FontOffset% -  Offset address of font to register
'
'  Return Value:
'     The number of fonts actually registered (0 or 1)
'
'  Notes:
'     Memory resident fonts cannot be stored in BASIC relocatable data
'     structures (like arrays or non-fixed strings).
'
'=================================================================
FUNCTION RegisterMemFont% (FontSeg AS INTEGER, FontOffset AS INTEGER) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo
DIM FontHeader AS WFHeader

ON ERROR GOTO UnexpectedErr

' Clear error and check max limits:
flClearFontErr
flChkMax

' Get the font header:
flMovMem FontHeader, FontSeg, FontOffset, cSizeFontHeader

' Only register vector fonts:
IF FontHeader.dfType AND &H1 <> cBitMapType THEN
	flSetFontErr cBadFontType
	RegisterMemFont = 0
	EXIT FUNCTION
END IF

' See that we're still within MaxRegistered limits:
IF FGP.TotalRegistered >= FGP.MaxRegistered THEN
	flSetFontErr cTooManyFonts
	RegisterMemFont = 0
	EXIT FUNCTION
END IF

' Go to next "registered" font slot:
FGP.TotalRegistered = FGP.TotalRegistered + 1

' Set font source and save the header:
FontHdrReg(FGP.TotalRegistered).FontSource = cMemFont
FontHdrReg(FGP.TotalRegistered).FontHeader = FontHeader

' Set font location in memory:
FontHdrReg(FGP.TotalRegistered).DataSeg = FontSeg
FontHdrReg(FGP.TotalRegistered).DataOffset = FontOffset + cSizeFontHeader

' Get the face name (scan characters until zero byte):
FaceLoc% = FontOffset + FontHeader.dfFace
FaceName$ = ""
DEF SEG = FontSeg
FOR Char% = 0 TO cMaxFaceName - 1
	Byte% = PEEK(FaceLoc% + Char%)
	IF Byte% = 0 THEN EXIT FOR
	FaceName$ = FaceName$ + CHR$(Byte%)
NEXT Char%
FontHdrReg(FGP.TotalRegistered).FaceName = FaceName$

' Finally, return number of fonts actually registered:
RegisterMemFont = 1

END FUNCTION

'=== SelectFont - Selects current font from among loaded fonts
'
'  Arguments:
'     FontNum% -  Font number to select
'
'=================================================================
SUB SelectFont (FontNum AS INTEGER) STATIC
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

' If no fonts are loaded then error:
IF FGP.TotalLoaded <= 0 THEN
	flSetFontErr cNoFonts
	EXIT SUB
END IF

' Now, map the font number to an acceptable one and select it:
IF FontNum <= 0 THEN
	FGP.CurrentFont = 1
ELSE
	FGP.CurrentFont = (ABS(FontNum - 1) MOD (FGP.TotalLoaded)) + 1
END IF

' Get First, Last and Default character params from header:
FGP.FChar = ASC(FontHdrLoaded(FGP.CurrentFont).FontHeader.dfFirstChar)
FGP.LChar = ASC(FontHdrLoaded(FGP.CurrentFont).FontHeader.dfLastChar)
FGP.DChar = ASC(FontHdrLoaded(FGP.CurrentFont).FontHeader.dfDefaultChar)
FGP.CHeight = FontHdrLoaded(FGP.CurrentFont).FontHeader.dfPixHeight
flSetBltParams cSizeFontHeader, FGP.CHeight, FGP.FChar, FGP.LChar, FGP.DChar

' Set some other commonly used elements of font info:
FGP.DSeg = FontHdrLoaded(FGP.CurrentFont).DataSeg
FGP.DOffset = FontHdrLoaded(FGP.CurrentFont).DataOffset
FGP.FontSource = FontHdrLoaded(FGP.CurrentFont).FontSource

END SUB

'=== SetGCharset - Specifies IBM or Windows char set
'
'  Arguments:
'     Charset%    -  cIBMChars for IBM character mappings
'                    cWindowsChars for Windows character mappings
'
'=================================================================
SUB SetGCharset (CharSet AS INTEGER) STATIC
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

IF CharSet = cWindowsChars THEN
	FGP.CharSet = cWindowsChars
ELSE
	FGP.CharSet = cIBMChars
END IF

END SUB

'=== SetGTextColor - Sets color for drawing characters
'
'  Arguments:
'     FColor   -  Color number
'
'=================================================================
SUB SetGTextColor (FColor AS INTEGER) STATIC
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr
  
FGP.CharColor = ABS(FColor)
flSetBltColor FGP.CharColor
FGP.CharColorInit = cTRUE

END SUB

'=== SetGTextDir - Sets character direction for OutGText
'
'  Arguments:
'     Dir   -  Character direction:
'              0 = Horizontal-Right
'              1 = Vertical-Up
'              2 = Horizontal-Left
'              3 = Vertical-Down
'
'=================================================================
SUB SetGTextDir (Dir AS INTEGER) STATIC
SHARED FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

SELECT CASE Dir

	' Vertical - up
	CASE 1:  FGP.XPixInc% = 0
				FGP.YPixInc% = -1
				XRowInc% = 1
				YRowInc% = 0
				FGP.CharDir = 1
  
	' Horizontal -left
	CASE 2:  FGP.XPixInc% = -1
				FGP.YPixInc% = 0
				XRowInc% = 0
				YRowInc% = -1
				FGP.CharDir = 2
  
	' Vertical - down
	CASE 3:  FGP.XPixInc% = 0
				FGP.YPixInc% = 1
				XRowInc% = -1
				YRowInc% = 0
				FGP.CharDir = 3
  
	' Horizontal - right
	CASE ELSE:  FGP.XPixInc% = 1
					FGP.YPixInc% = 0
					XRowInc% = 0
					YRowInc% = 1
					FGP.CharDir = 0
	END SELECT

	' Call routine to set these increments in the char output routine
	flSetBltDir FGP.XPixInc%, FGP.YPixInc%, XRowInc%, YRowInc%
	FGP.CharDirInit = cTRUE

END SUB

'=== SetMaxFonts - Sets the maximum number of fonts that can be registered
'                  and loaded by the font library:
'
'  Arguments:
'     Registered  -  The maximum number of fonts that can be registered
'                    by the font library
'
'     Loaded      -  The maximum number of fonts that can be loaded by
'                    by the font library
'
'  Return Values:
'     Sets error if values are not positive. Adjusts MaxReg and MaxLoad
'     internal values and resets the length of FontHdrReg and FontHdrLoad
'     arrays if the new value is different from previous one
'
'=================================================================
SUB SetMaxFonts (Registered AS INTEGER, Loaded AS INTEGER) STATIC
SHARED FGP AS GlobalParams
SHARED FontHdrReg() AS IFontInfo
SHARED FontHdrLoaded() AS IFontInfo
SHARED FontLoadList() AS INTEGER
SHARED FontData() AS FontDataBlock

ON ERROR GOTO UnexpectedErr

' Clear errors:
flClearFontErr

' Check to see that values are within range:
IF Registered <= 0 OR Loaded <= 0 THEN
	flSetFontErr cBadFontLimit
	EXIT SUB
END IF

' Values are ostensibly OK. Reset values and redimension arrays:
' Reset values for registered fonts:
FGP.TotalRegistered = 0
FGP.MaxRegistered = Registered
  
ON ERROR GOTO MemErr
REDIM FontHdrReg(1 TO FGP.MaxRegistered) AS IFontInfo
ON ERROR GOTO UnexpectedErr

' Reset values for loaded fonts:
FGP.TotalLoaded = 0
FGP.MaxLoaded = Loaded
  
ON ERROR GOTO MemErr
REDIM FontLoadList(1 TO FGP.MaxLoaded) AS INTEGER
REDIM FontHdrLoaded(1 TO FGP.MaxLoaded) AS IFontInfo
ON ERROR GOTO UnexpectedErr

' Clear font data array:
ERASE FontData

END SUB

'=== UnRegisterFonts - Erases registered font header array and resets
'                      total registered fonts to 0:
'
'  Arguments:
'     ErrNum   -  The error number to set FontErr variable to
'
'=================================================================
SUB UnRegisterFonts STATIC
SHARED FontHdrReg() AS IFontInfo, FGP AS GlobalParams

ON ERROR GOTO UnexpectedErr

' Clear outstanding font errors:
flClearFontErr

REDIM FontHdrReg(1 TO 1)  AS IFontInfo
FGP.MaxRegistered = UBOUND(FontHdrReg, 1)
FGP.TotalRegistered = 0

END SUB

