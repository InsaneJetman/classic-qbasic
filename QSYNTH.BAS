'
'                                QSYNTH.BAS
'
'        Copyright (C) 1990 Microsoft Corporation. All Rights Reserved.
'
' This program records and plays back songs. To enter a song, use the
' piano keyboard displayed on the screen. You can save, change, and
' delete the songs and change the speed of the song play back.
'
' To run this game, press SHIFT+F5.
'
' To exit this program, press ALT, F, X.
'
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'
' To view suggestions on changing this game, press Page Down.
'
'
'                             Suggested Changes
'                             -----------------
'
' There are many ways that you can modify this BASIC game.  The CONST
' statements below these comments and the DATA statements at the end
' of this screen can be modified to change the following:
'    Pitch of song playback
'    Background color
'    Text color
'    Pressed piano key color
'    System songs
'
' On the right side of each CONST statement, there is a comment that tells
' you what it does and how big or small you can set the value.  Above the
' DATA statements, there are comments that tell you the format of the
' information stored there.
'
' On your own, you can also add exciting sound and visual effects or make any
' other changes that your imagination can dream up.  By reading the
' Learn BASIC Now book, you'll learn the techniques that will enable you
' to fully customize this game and to create games of your own.
'
'
' If the game won't run after you have changed it, you can exit without
' saving your changes by pressing Alt, F, X and choosing NO.
'
' If you do want to save your changes, press Alt, F, A and enter a filename
' for saving your version of the program.  Before you save your changes,
' however, you should make sure they work by running the program and
' verifying that your changes produce the desired results.  Also, always
' be sure to keep a backup of the original program.
'
DEFINT A-Z

' These constants can be modified to change certain aspects of the game.
CONST PRESSEDKEYCOLOR = 15               ' Pressed keyboard key color. Range 1-6, 8-15.
CONST BACKGROUNDCOLOR = 3                ' Background color. Range 0-7
CONST TEXTCOLOR = 0                      ' Menu text color. Range 0-15, but not same as BACKGROUNDCOLOR.
CONST TITLECOLOR = 15                    ' Mode and title color. Range 0-15, but not same as BACKGROUNDCOLOR.
CONST PITCH = 4                          ' Pitch of songs (higher number means lower pitch)- MUST BE BETWEEN 0 AND 4!!!
CONST XTNOTELENGTH = 4                   ' Length of the note sound on all PC/XT type machines.  Range 1 to 24.
CONST INTROSONG = "T100O2L16CCDFDEL4DL16FEDO1BO2L4C"  ' Song played at game introduction


' The following are general constants and their values should not be changed.

' Musical note constants.
CONST C = 1
CONST DF = 2
CONST D = 3
CONST EF = 4
CONST E = 5
CONST F = 6
CONST GF = 7
CONST G = 8
CONST AF = 9
CONST A = 10
CONST BF = 11
CONST B = 12
' Menu constants
CONST MAIN = 0
CONST PRACTICE = 1
CONST RECORD = 2
CONST PLAYBACK = 3
CONST PLAYING = 6
CONST SAVING = 7
CONST EDITOR = 8
CONST EDITMENU = 9
CONST GETNAME = 10
' Other constants
CONST MAXSONG = 50                      ' # of recordable selections
CONST MAXNOTE = 2000                    ' # notes available per song
CONST FALSE = 0                         ' Constant for FALSE value
CONST TRUE = NOT FALSE                  ' Constant for TRUE value
CONST UP = FALSE                        ' Up as in key released
CONST DOWN = TRUE                       ' Down as in key pressed
CONST CANCEL = 255
CONST NOTETICK = .6                     ' Length of time rests and notes are
CONST RESTTICK = .7                     ' played per duration increment
CONST CR = 13                           ' Carriage Return (Enter)
CONST ESC = 27                          ' Esc key
CONST TABCHAR = 9                       ' Tab key

' Structure definitions
TYPE KeyMap
	Note  AS INTEGER                    ' An array of this type is used to map
	Oct   AS INTEGER                    ' keyboard keys to Note/Oct values.
END TYPE

TYPE SongElement                        ' An array of this type is used to
	Note  AS INTEGER                    ' hold all the information in a song
	Oct   AS INTEGER                    ' (Note, Octave, and Duration of each
	Dur   AS INTEGER                    ' note in the song)
END TYPE

TYPE SongIndexCard                      ' Elements of this type make up the
	naym          AS STRING * 25        ' SongIndex section of the song file,
	desc          AS STRING * 40        ' describing the name/description of
	Size          AS INTEGER            ' a song, the length in notes, and the
	Offset        AS LONG               ' position of the song in the file.
END TYPE

TYPE FileHeader                         ' This structure is the file header
	Count         AS INTEGER            ' for the song file, indicating the
	NextNote      AS LONG               ' number of songs saved, and position
END TYPE                                ' of the next unused byte in the file.

'Declaration of all the FUNCTION and SUB procedures called in this program.
DECLARE FUNCTION ConfirmDelete% ()
DECLARE FUNCTION GetNote$ (Note%)
DECLARE FUNCTION SaveChanges% ()
DECLARE FUNCTION SimpleEdit% (row%, col%, text$, MaxLen%)
DECLARE SUB Center (text$, row%)
DECLARE SUB ChangeTempo (Inc%)
DECLARE SUB ClearMenuScreen (Title$)
DECLARE SUB CreateSongFile ()
DECLARE SUB DeleteSong (SongNo%)
DECLARE SUB DisplayChanges ()
DECLARE SUB DisplayGameTitle ()
DECLARE SUB DisplayIntro ()
DECLARE SUB DisplayMenuText (Menu%)
DECLARE SUB DrawBox (r1%, c1%, r2%, c2%, Title$)
DECLARE SUB DrawKeyboard ()
DECLARE SUB DrawNote (Note%, Octave%, Action%)
DECLARE SUB EditSong (SongNo%)
DECLARE SUB ErrorMessage (msg$)
DECLARE SUB GetNameAndSave ()
DECLARE SUB InitFreq ()
DECLARE SUB LoadDefaultSong (num%)
DECLARE SUB LoadSong (SongNo%)
DECLARE SUB MainMenu ()
DECLARE SUB PlayNote (Note%, Octave%, Duration#)
DECLARE SUB PlaySong ()
DECLARE SUB RecordMenu (NoSave%)
DECLARE SUB RecordMode (NoSave%)
DECLARE SUB SaveSong ()
DECLARE SUB TimeDelay (Dur#)

' SHARED (global) variable declarations for use in this program
DIM SHARED Freq(1 TO 12)      AS INTEGER     ' Base array of frequencies
DIM SHARED Kyb(1 TO 127)      AS KeyMap      ' Keyboard key to piano key array
DIM SHARED Song(1 TO MAXNOTE) AS SongElement ' Array to hold the actual song
DIM SHARED Counter            AS INTEGER     ' Number of notes in Song()
DIM SHARED SongName           AS STRING * 25 ' Name of Song()
DIM SHARED SongDesc           AS STRING * 40 ' Description of Song()
DIM SHARED TEMPO              AS INTEGER     ' Tempo of song playback (control)
DIM SHARED TFACTOR            AS SINGLE      ' Tempo factor (for actual speed)
DIM SHARED FileError          AS INTEGER     ' Keeps file error status.
DIM SHARED SongRecorded       AS INTEGER     ' Flag used to decide whether or
											 ' not a song was actually recorded
DIM KeyFlags                  AS INTEGER     ' Used to turn NUM LOCK off
DIM BadMode                   AS INTEGER     ' Used to validate screen mode

' Use error trap to test for graphics capability.
ON ERROR GOTO ScreenError              ' Set up an error trap.
BadMode = FALSE                        ' ScreenError will change to TRUE if...
SCREEN 1                               ' this statement fails, which means...

IF BadMode = TRUE THEN                 ' no graphic was found.
	CLS
	LOCATE 11, 13
	PRINT "CGA, EGA Color, or VGA graphics required to run QSYNTH.BAS"

ELSE

	DEF SEG = 0
	KeyFlags = PEEK(1047)              ' Keep current keyboard flags.
	IF KeyFlags AND 32 THEN
		POKE 1047, KeyFlags AND 223    ' Force the NUM LOCK state to OFF.
	END IF
	DEF SEG

	ON ERROR GOTO ErrorTrap            ' Set the main error trap.
	DisplayIntro                       ' Display the intro screen.
	FileError = 0

StartAgain:                            ' If an error occurs, we start again here.
	DrawKeyboard                       ' Draw the keyboard on the screen.
	InitFreq                           ' Map keyboard to piano, and set frequencies.
	MainMenu                           ' Go to the main menu.
	DisplayChanges                     ' Display the final screen.

	IF KeyFlags AND 32 THEN            ' Restore the keyboard
		DEF SEG = 0                    ' to the state in which
		POKE 1047, KeyFlags OR 32      ' we found it originally.
		DEF SEG
	END IF

END IF

END                                    ' The end of module-level control flow.


' Error handling routine
ErrorTrap:
	errnum = ERR
	SELECT CASE errnum
	CASE 52 TO 76
		IF FileError = 0 THEN
			ErrorMessage "Cannot access QSYNTH.DAT file."
			FileError = 1
		END IF
		RESUME NEXT
	CASE ELSE
		ErrorMessage "Sorry, an unexpected error has occurred."
		RESUME StartAgain
	END SELECT

' Error handler for screen test
ScreenError:
	BadMode = TRUE
	RESUME NEXT


' Data statements for the default hard-coded songs
' The format for the DATA statements for a song is:
'
'     DATA "Song's Name", "Song's Description", Length of song (in "notes")
'     DATA note,octave,duration        ' <- first "note"
'     DATA note,octave,duration        ' <- second "note"
'     DATA note,octave,duration        ' <- third "note", etc.
SONG1:
	DATA "Yankee","Yankee Doodle Dandy", 107
	DATA 1,0,18,0,0,3,1,0,18,0,0,3,3,0,18,0,0,3,5,0,18,0,0,3,1,0,18,0,0,3
	DATA 5,0,18,0,0,3,3,0,18,0,0,3,8,-1,18,0,0,3,1,0,18,0,0,3,1,0,18,0,0,3
	DATA 3,0,18,0,0,3,5,0,18,0,0,3,1,0,38,0,0,6,12,-1,38,0,0,6,1,0,18,0,0,3
	DATA 1,0,18,0,0,3,3,0,18,0,0,3,5,0,18,0,0,3,6,0,18,0,0,3,5,0,18,0,0,3
	DATA 3,0,18,0,0,3,1,0,18,0,0,3,12,-1,18,0,0,3,8,-1,18,0,0,3,10,-1,18
	DATA 0,0,3,12,-1,18,0,0,3,1,0,38,0,0,6,1,0,38,0,0,6,10,-1,38,0,0,3
	DATA 12,-1,15,10,-1,18,0,0,3,8,-1,18,0,0,3,10,-1,18,0,0,3,12,-1,18
	DATA 0,0,3,1,0,38,0,0,6,8,-1,38,0,0,3,10,-1,15,8,-1,18,0,0,3,6,-1,18
	DATA 0,0,3,5,-1,38,0,0,6,8,-1,38,0,0,6,10,-1,38,0,0,3,12,-1,15,10,-1,18
	DATA 0,0,3,8,-1,18,0,0,3,10,-1,18,0,0,3,12,-1,18,0,0,3,1,0,18,0,0,3
	DATA 10,-1,18,0,0,3,8,-1,18,0,0,3,1,0,18,0,0,3,12,-1,18,0,0,3,3,0,18
	DATA 0,0,3,1,0,38,0,0,6,1,0,38,0,0,6

SONG2:
	DATA "Hat","Mexican Hat Dance",36
	DATA 1,0,16,6,0,10,0,0,5,1,0,16,6,0,10,0,0,5,1,0,16,6,0,10,0,0,12
	DATA 1,0,16,6,0,16,8,0,16,6,0,16,5,0,10,0,0,5,6,0,16,8,0,10,0,0,15
	DATA 1,0,16,5,0,10,0,0,5,1,0,16,5,0,10,0,0,5,1,0,16,5,0,10,0,0,12
	DATA 1,0,16,5,0,16,6,0,16,5,0,16,3,0,10,0,0,5,5,0,16,6,0,10,0,0,15

'-------------------------------------------------------------------------
'  Center
'
'    Centers the text string it receives on the indicated row.
'
'             PARAMETERS:   text$   -   the text to print
'                           row     -   the row on which to print text$
'-------------------------------------------------------------------------
SUB Center (text$, row)

	LOCATE row%, 40 - LEN(text$) \ 2 + 1    ' Calculate column to start at.
	PRINT text$;

END SUB

'-------------------------------------------------------------------------
' ChangeTempo
'
'   Changes the current tempo (speed) for song playback.  When the
'   user presses a direction key, this SUB is called with the direction of
'   change as an argument and adjusts the tempo accordingly, and also updates
'   the tempo control on the screen.
'
'             PARAMETERS:   Inc - The amount by which to change the TEMPO
'
'-------------------------------------------------------------------------
SUB ChangeTempo (Inc) STATIC

	COLOR 0, 7                            ' Erase the TEMPO control lever.
	LOCATE 23, 17 + TEMPO
	PRINT CHR$(205)

	IF Inc = UP AND TEMPO < 45 THEN       ' Calculate new TEMPO value.
		TEMPO = TEMPO + 1
	ELSEIF Inc = DOWN AND TEMPO > 1 THEN
		TEMPO = TEMPO - 1
	END IF

	TFACTOR = 1 + (23 - TEMPO) * .03      ' Calculate new TFACTOR value.
	LOCATE 23, 17 + TEMPO                 ' Redisplay the TEMPO control.
	PRINT CHR$(219)

END SUB

'-------------------------------------------------------------------------
' ClearMenuScreen
'
'   Clears the section of the screen below the keyboard, and centers
'   the title given in Title$ at the top.
'
'             PARAMETERS:   Title$ - The text to display at the top of the
'                                    screen or "RETAIN" to keep current title
'-------------------------------------------------------------------------
SUB ClearMenuScreen (Title$) STATIC
	
	COLOR TEXTCOLOR, BACKGROUNDCOLOR
	IF Title$ <> "RETAIN" THEN            ' "RETAIN" means leave the current
		COLOR TITLECOLOR                  ' title on the top of the screen.
		LOCATE 3, 20

		' Print a 40-character string of spaces with Title$ in the center.
		PRINT LEFT$(SPACE$(20 - LEN(Title$) \ 2) + Title$ + SPACE$(20), 40)
		COLOR TEXTCOLOR
	END IF

	VIEW PRINT 13 TO 25                   ' Clear these specific lines.
	CLS
	VIEW PRINT

END SUB

'-------------------------------------------------------------------------
' ConfirmDelete
'
'   Makes sure that the user really wants to delete the song.
'   Returns TRUE if so, or FALSE if not.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
FUNCTION ConfirmDelete STATIC

	COLOR BACKGROUNDCOLOR, BACKGROUNDCOLOR      ' Draw the dialog box.
	DrawBox 13, 38, 24, 78, ""
	COLOR 0, 7
	DrawBox 15, 41, 20, 75, "Delete Song"
	LOCATE 17, 43
	PRINT "Are you sure you want to delete"
	LOCATE , 43, 1
	PRINT "the current song? (Y/N) ";

	DO                                            ' Wait for input.
		i$ = UCASE$(INKEY$)
	LOOP UNTIL i$ <> ""

	LOCATE , , 0                                  ' Turn off cursor.
	IF i$ = "Y" THEN                              ' Return appropriate value.
		ConfirmDelete = TRUE
	ELSE
		ConfirmDelete = FALSE
	END IF

END FUNCTION

'-------------------------------------------------------------------------
' CreateSongFile
'
'   Creates the song file QSYNTH.DAT in the proper format.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB CreateSongFile STATIC

	DIM Hdr AS FileHeader
	DIM S AS SongIndexCard

	Hdr.Count = 0
	Hdr.NextNote = 1                      ' Initialize header structure.
	SEEK #1, 1
	PUT #1, , Hdr                         ' Put the header in the file.

	FOR i = 1 TO MAXSONG                  ' Fill in the blank index records.
		PUT #1, , S
	NEXT i

END SUB

'-------------------------------------------------------------------------
' DeleteSong
'
'   Deletes a song from the song list, and deletes the song index card
'   and song data from the file.  To understand how this procedure works, see the
'   SaveSong SUB for a description of the file structure for QSYNTH.DAT.
'
'             PARAMETERS:   SongNo  -       The song number to delete
'-------------------------------------------------------------------------
SUB DeleteSong (SongNo) STATIC

	DIM HeaderInfo AS FileHeader
	DIM S AS SongIndexCard
	DIM n AS SongElement
	FileError = 0

	OPEN "QSYNTH.DAT" FOR BINARY AS 1
	GET #1, , HeaderInfo
  
	SEEK #1, LEN(HeaderInfo) + (LEN(S) * (SongNo - 1)) + 1
	GET #1, , S                           ' Get info on song to delete.
	BackPtr = S.Offset                    ' BackPtr is the location where the
					  ' collapse starts copying TO (see below).

	IF SongNo = HeaderInfo.Count THEN
		' SongNo was the last song in the list, so we're basically done.  All
		' there is to do is reduce the number of songs by one, and update the
		' NextNote pointer in the file header to point to the first note of
		' the deleted song.  Remember that NextNote always points to the next
		' available note location in the file.  Note that this leaves all the
		' data of the deleted song at the end of the file, which will be over-
		' written the next time a song is saved.
		HeaderInfo.Count = HeaderInfo.Count - 1  ' Reduce # of songs by one
		HeaderInfo.NextNote = S.Offset           ' Point NextNote at first note
							 ' of deleted song.
		SEEK #1, 1                               ' Header belongs at beginning
		PUT #1, , HeaderInfo                     ' Write new header.
		CLOSE 1
		EXIT SUB
	ELSE
		' The song we're deleting is not the last one in the file, so
		' to delete it, we must "collapse" both the song index part and
		' the song data part of the file.  "Collapse" here means to copy
		' all the information appearing in the file after the song we are
		' deleting up in the file, thus writing over the deleted song.  Then,
		' the song count and NextNote values in the file header are updated.
		GET #1, , S                     ' Get info on the next song in the file.
		ForePtr = S.Offset              ' ForePtr is where we are copying from.
	END IF

  ' First, we need to collapse song index part of the file.  This is done by
  ' moving each song index record after the deleted one up one notch, so that
  ' the deleted song's index record get replaced by the one following it, and
  ' so on.  Also, for each record, the Offset field needs to be adjusted by
  ' the size of the deleted song, which at this point can be calculated by
  ' ForePtr - BackPtr, since ForePtr points to the song data immediately after
  ' the deleted song, and BackPtr points to the deleted song's data.  This
  ' adjustment is necessary because the song data will be collapsed as well.
	FOR i = SongNo + 1 TO HeaderInfo.Count
		SEEK #1, LEN(HeaderInfo) + (LEN(S) * (i - 1)) + 1
		GET #1, , S                               ' Get the old song index record.
		S.Offset = S.Offset - (ForePtr - BackPtr) ' Make the adjustment
		SEEK #1, LEN(HeaderInfo) + (LEN(S) * (i - 2)) + 1
		PUT #1, , S                               ' and put it in it's new place.
	NEXT i

  ' Next, we need to collapse the song data part of the file. This is done in
  ' the same way that the song index part was, with the exception that it is
  ' moved up in the file one note at a time.  ALL notes of ALL songs after the
  ' deleted song are copied with the following loop.
	FOR i = ForePtr TO HeaderInfo.NextNote - LEN(n) STEP LEN(n)
		SEEK #1, LEN(HeaderInfo) + (LEN(S) * MAXSONG) + i
		GET #1, , n                             ' Get a note.
		SEEK #1, LEN(HeaderInfo) + (LEN(S) * MAXSONG) + BackPtr
		PUT #1, , n                             ' Put it at it's new location.
		BackPtr = BackPtr + LEN(n)              ' Update BackPtr.
	NEXT i

	' The last step is to update the file header record.  This is the same as
	' if we deleted the last song; reduce the song count by one, and point NextNote
	' at the next AVAILABLE note location in the song data section.  Note that
	' after the above loop completes, i points to one past the last note of the
	' last song (which is exactly what we want for NextNote).
	HeaderInfo.Count = HeaderInfo.Count - 1
	HeaderInfo.NextNote = i
	SEEK #1, 1
	PUT #1, , HeaderInfo                        ' write the new file header
	CLOSE 1                                     '  - song is history!

END SUB

'-------------------------------------------------------------------------
' DisplayChanges
'
'   Displays game characteristics that you can easily change via CONST and DATA.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB DisplayChanges

	DisplayGameTitle                         ' Print game title
						 
	COLOR 7                                  ' Print suggested changes in white.
	Center "The following game characteristics can be easily changed from", 5
	Center "within QuickBASIC Interpreter.  To change the values of these", 6
	Center "characteristics, locate the corresponding CONST or DATA      ", 7
	Center "statements in the source code and change their values, then  ", 8
	Center "restart the program (press Shift + F5).                      ", 9
	COLOR 15
	Center "Pitch of song playback ", 11
	Center "Background color       ", 12
	Center "Text color             ", 13
	Center "Pressed piano key color", 14
	Center "System songs           ", 15
	COLOR 7
	Center "The CONST statements and instructions on changing them are   ", 17
	Center "located at the beginning of the main program.                ", 18

	DO WHILE INKEY$ = "": LOOP               ' Wait for any keypress.
	CLS

END SUB

'-------------------------------------------------------------------------
' DisplayGameTitle
'
'   Displays game title for use in the introduction and suggested changes.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB DisplayGameTitle

	' Set the screen to a normal text, clear it and add blue background.
	SCREEN 0
	WIDTH 80, 25
	COLOR 4, 0
	CLS
	
	LOCATE 1, 2                               ' Draw outline around screen with extended ASCII characters.
	PRINT CHR$(201); STRING$(76, 205); CHR$(187); ' top border
	FOR x% = 2 TO 24                              ' left and right borders
		LOCATE x%, 2
		PRINT CHR$(186); TAB(79); CHR$(186);
	NEXT x%
	LOCATE 25, 2
	PRINT CHR$(200); STRING$(76, 205); CHR$(188); ' bottom border

	' Print game title centered at top of screen
	COLOR 0, 4                                ' Print title in black on red.
	Center "          Microsoft          ", 1 ' Center game title on lines 1 & 2.
	Center "   Q S Y N T H E S I Z E R   ", 2
	Center "   Press any key to continue   ", 25
	COLOR 7, 0

END SUB

'-------------------------------------------------------------------------
' DisplayIntro
'
'   Displays game introduction screen.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB DisplayIntro

	DisplayGameTitle                    ' Display game title.

	COLOR 7
	Center "Copyright (C) 1990 Microsoft Corporation.  All Rights Reserved.", 4
	Center "Microsoft QSynthesizer allows you to record and play back songs", 7
	Center "entered by pressing keys on the keyboard.  You can save up to ", 8
	Center LTRIM$(STR$(MAXSONG)) + " songs on disk and play them back as often as you like.  You", 9
	Center "can also use the song editor to fine-tune your songs.         ", 10
	Center "Just follow the directions in the menus to play, record, save,", 12
	Center "edit, and delete your songs.                                  ", 13

	PLAY INTROSONG                       ' Play melody while waiting to continue.

	DO WHILE INKEY$ = "": LOOP           ' Wait for any keypress.

END SUB

'-------------------------------------------------------------------------
' DisplayMenuText
'
'   Displays almost all the text, boxes, and other info on the screen.  It
'   uses the parameter (Menu) to determine which screen has been requested.
'
'             PARAMETERS:   Menu - The "Menu" number to be displayed
'-------------------------------------------------------------------------
SUB DisplayMenuText (Menu) STATIC

	SELECT CASE Menu
	CASE MAIN                          ' Main menu screen.
		ClearMenuScreen ""
		COLOR 0, 7
		DrawBox 14, 7, 24, 35, "Song List"
		COLOR TEXTCOLOR, BACKGROUNDCOLOR
		LOCATE 14, 46
		PRINT "P - Play current song"
		LOCATE , 46
		PRINT "R - Record a new song"
		LOCATE , 46
		PRINT "E - Edit song from list"
		LOCATE , 46
		PRINT "D - Delete song from list"
		LOCATE , 46
		PRINT "S - Practice QSynthesizer"
		LOCATE , 46
		PRINT "Q - Quit QSynthesizer"
		LOCATE 21, 40
		PRINT "Use the arrow keys to select a song."
		LOCATE , 41
		PRINT "Press the corresponding letter to"
		LOCATE , 44
		PRINT "make a selection from above."
	  
	CASE PRACTICE, RECORD              ' These two are almost the same (PRACTICE and RECORD modes).
		IF Menu = PRACTICE THEN
			ClearMenuScreen "Practice Mode"
			Center "When finished practicing, press the Esc key.", 20
		ELSE
			ClearMenuScreen "Record Mode"
			Center "When finished recording, press the Esc key.", 20
		END IF
		Center "To play a note, press the key on the keyboard", 16
		Center "corresponding to the desired note shown on", 17
		Center "the piano above. ", 18

	CASE PLAYBACK                      ' Song playback screen - with the TEMPO control.
		ClearMenuScreen "Playback Mode: " + RTRIM$(SongName)
		COLOR TITLECOLOR
		LOCATE 14, 40 - (LEN(RTRIM$(SongDesc)) \ 2)
		PRINT RTRIM$(SongDesc)
		COLOR TEXTCOLOR
		Center "Use arrow keys to adjust tempo.", 17
		Center "Press Enter to start song playback.", 18
		Center "Press Esc to exit playback mode.", 19
		COLOR 0, 7
		DrawBox 21, 15, 24, 65, "Tempo Control"
		LOCATE 22, 17
		PRINT "Slow"
		LOCATE 22, 60
		PRINT "Fast"
		LOCATE , 17
		PRINT CHR$(198); STRING$(22, 205); CHR$(219); STRING$(22, 205); CHR$(181)
		TEMPO = 23
		TFACTOR = 1
		COLOR TEXTCOLOR, BACKGROUNDCOLOR

	CASE EDITOR                        ' Song editor screen.
		ClearMenuScreen "Edit Mode: " + RTRIM$(SongName)
		LOCATE 14, 58 - LEN(RTRIM$(SongDesc)) \ 2
		COLOR TITLECOLOR
		PRINT RTRIM$(SongDesc)
		COLOR TEXTCOLOR
		LOCATE 16, 46
		PRINT "C - Change current note"
		LOCATE , 46
		PRINT "I - Insert new note"
		LOCATE , 46
		PRINT "D - Delete current note"
		LOCATE , 46
		PRINT "P - Play song"
		LOCATE 22, 41
		PRINT "Press the corresponding letter to"
		LOCATE , 39
		PRINT "make a selection from the list above."
		LOCATE , 43
		PRINT "Press Esc to exit the editor.";
		COLOR 0, 7
		DrawBox 14, 5, 24, 35, "Note List"
		COLOR 15
		LOCATE 15, 6
		PRINT "Note        Octave   Duration"
		COLOR 0
		LOCATE 22, 5
		PRINT CHR$(195); STRING$(29, 196); CHR$(180)

	CASE GETNAME                       ' This is the dialog box to get the song name.
		ClearMenuScreen "RETAIN"
		COLOR 0, 7
		DrawBox 14, 10, 24, 70, "Save Recorded Song"
		DrawBox 15, 26, 17, 53, ""
		DrawBox 18, 26, 20, 68, ""
		LOCATE 16, 12
		PRINT "Song's Name:"
		LOCATE 19, 12
		PRINT "Description:"
		LOCATE 22, 15
		PRINT "Press Tab to change edit fields, and Enter to save."
		LOCATE 23, 12
		PRINT "Press Esc to return to main menu without saving the song."

	END SELECT

END SUB

'-------------------------------------------------------------------------
' DrawBox
'
'   Draws a box using single line characters at the given coordinates.
'
'        PARAMETERS:    r1,c1  - The row and column location of the upper left corner
'                       r2,c2  - The row and column location of the lower right corner
'                       Title$ - The text to place at the top of the box, if any
'-------------------------------------------------------------------------
SUB DrawBox (r1, c1, r2, c2, Title$) STATIC

	InBoxWidth = c2 - c1 - 1                      ' Calculate box width.

	LOCATE r1, c1                                 ' Draw the top line.
	PRINT CHR$(218); STRING$(InBoxWidth, 196); CHR$(191)

	FOR t = r1 + 1 TO r2 - 1                      ' Draw sides of the box.
	LOCATE t, c1
		PRINT CHR$(179); SPACE$(InBoxWidth); CHR$(179);
	NEXT t

	LOCATE r2, c1                                 ' Draw the bottom line.
	PRINT CHR$(192); STRING$(InBoxWidth, 196); CHR$(217);

	IF Title$ <> "" THEN                          ' Put the title on top.
		LOCATE r1, c1 + (InBoxWidth \ 2) - (LEN(Title$) \ 2)
		PRINT " "; Title$; " ";
	END IF

END SUB

'-------------------------------------------------------------------------
' DrawKeyboard
'
'   Draws the piano keyboard on the screen.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB DrawKeyboard STATIC

	COLOR TITLECOLOR, BACKGROUNDCOLOR
	WIDTH 80, 25
	CLS

	'  - Print top lines of keyboard.
	LOCATE 2, 29
	PRINT "Microsoft QSynthesizer"
	COLOR 7, 0
	LOCATE 5, 4
	PRINT SPACE$(75)
	LOCATE , 4
	PRINT SPACE$(75)

	'    Print middle section (black/white keys).
	COLOR 0, 7
	temp$ = CHR$(179) + " " + CHR$(219) + " " + CHR$(219) + " " + CHR$(179) + " "
	temp$ = temp$ + CHR$(219) + " " + CHR$(219) + " " + CHR$(219) + " "
	temp2$ = CHR$(179) + " " + CHR$(222)
	FOR i = 1 TO 3
		LOCATE , 4
		PRINT CHR$(219); CHR$(221); MID$(temp$, 2); temp$;
		PRINT temp$; temp$; temp$; temp2$; CHR$(219)
	NEXT i

	'    Print middle section (white keys only).
	FOR i = 1 TO 2
		LOCATE , 4
		PRINT CHR$(219); CHR$(221);
		FOR t = 1 TO 35
			PRINT " "; CHR$(179);
		NEXT t
		PRINT " "; CHR$(222); CHR$(219)
	NEXT i
	COLOR 7, 0
	LOCATE , 4
	PRINT SPACE$(75)

END SUB

'-------------------------------------------------------------------------
' DrawNote
'
' Highlights or un-highlights the given note on the piano keyboard.
'
'        PARAMETERS:    Note - The note number to draw
'                       Octave - The octave number (-3 to 3) of the note
'                       Action - What to do (DOWN = un-highlight, UP = highlight)
'-------------------------------------------------------------------------
SUB DrawNote (Note, Octave, Action) STATIC

	IF Note = 0 THEN EXIT SUB             ' Note = 0 means do not draw a note

	SELECT CASE Note
	CASE A, B, C, D, E, F, G              ' Find offset from middle C for a
		SELECT CASE Note                  ' white key.
		CASE A
			Offset = 10
		CASE B
			Offset = 12
		CASE C
			Offset = 0
		CASE D
			Offset = 2
		CASE E
			Offset = 4
		CASE F
			Offset = 6
		CASE G
			Offset = 8
		END SELECT
		col = (34 + Offset) + (Octave * 14) ' Calculate column value.
		bottom = 11                         ' Bottom of key (row)
		keyColor = 7                        ' Color to draw UP key

	CASE ELSE                               ' Find offset from middle C for a
		SELECT CASE Note                    ' black key.
		CASE DF
			Offset = 1
		CASE EF
			Offset = 3
		CASE GF
			Offset = 7
		CASE AF
			Offset = 9
		CASE BF
			Offset = 11
		END SELECT
		col = (34 + Offset) + (Octave * 14) ' Calculate column value.
		bottom = 9                          ' Bottom of key (row)
		keyColor = 0                        ' Color to draw UP key
	END SELECT

	IF Action = DOWN THEN
		COLOR PRESSEDKEYCOLOR               ' Set pressed key color.
	ELSE
		COLOR keyColor
	END IF

	IF col > 5 AND col < 77 THEN            ' Using the calculated row
		FOR row = 7 TO bottom               ' and the bottom found, draw
			LOCATE row, col                 ' a vertical line of blocks
			PRINT CHR$(219)                 ' to fill in key pressed.
		NEXT row
	END IF

END SUB

'-------------------------------------------------------------------------
' EditSong
'
' This is the song editor.  Each of the functions available in the song editor
' are subroutines contained in this subprogram.
'
'        PARAMETERS:    SongNo - The number of the song being edited, indexed
'                                from the beginning of the QSYNTH.DAT file
'-------------------------------------------------------------------------
SUB EditSong (SongNo) STATIC

	DIM EditNote AS SongElement, DrawnNote AS SongElement

	'     - Initialize screen and variables.
	DisplayMenuText EDITOR
	Cursor = 1
	WindowTop = 1
	DrawnNote.Note = 0
	Finished = FALSE
	GOSUB DisplayNoteTable

	'Poll the keyboard, performing requested functions, until the Esc key is pressed.
	WHILE NOT Finished
		DO
			x$ = INKEY$                       '  - Wait for a keypress.
		LOOP UNTIL x$ <> ""

		IF LEN(x$) > 1 THEN                   '  - Key was a function key
			COLOR 0, 7
			GOSUB DrawNoteCursor              ' "undraw" note cursor
			IF RIGHT$(x$, 1) = "H" AND Cursor > 1 THEN                ' UP arrow
				Cursor = Cursor - 1
			ELSEIF RIGHT$(x$, 1) = "P" AND Cursor < Counter THEN      ' DOWN arrow
				Cursor = Cursor + 1
			END IF
			COLOR 7, 0
			GOSUB DrawNoteCursor              ' Redraw note cursor.
			GOSUB PrintEditorStatus           ' Update the current note value.

		ELSE                                '    Key was a standard key
			SELECT CASE UCASE$(x$)
			CASE "C"                        ' - Change note.
				EditNote = Song(Cursor)
				EditTitle$ = "Change a Note"
				GOSUB GetNewNote              ' Do the note edit.
				IF NOT (Cancelled) THEN
					Song(Cursor) = EditNote
				END IF
				GOSUB DisplayNoteTable        ' Update the note list to display change.

			CASE "D"                        ' - Delete note.
				GOSUB DeleteCurrentNote

			CASE "I"                        ' - Insert note
				EditNote.Note = C
				EditNote.Oct = 0
				EditNote.Dur = 4
				EditTitle$ = "Insert a Note"
				GOSUB GetNewNote              ' Do the note edit.
				IF NOT (Cancelled) THEN
					GOSUB InsertEditNote
				END IF
				GOSUB DisplayNoteTable        ' Update the note list.

			CASE "P"                          ' - Play song.
				DrawNote DrawnNote.Note, DrawnNote.Oct, UP
				PlaySong                      ' Play the song.
				DisplayMenuText EDITOR        ' Redraw the editor screen.
				GOSUB DisplayNoteTable        ' Update the note table.

				CASE CHR$(27)                 ' - Esc key was pressed!
				Finished = TRUE               ' time to leave

			END SELECT
		END IF
	WEND

	' - See if the user wants to save his/her changes.
	IF SaveChanges THEN
		DeleteSong SongNo
		SaveSong
	END IF
	DrawNote DrawnNote.Note, DrawnNote.Oct, UP
	EXIT SUB

'     - This subroutine displays the notes in text format in the Note List box.
DisplayNoteTable:
	GOSUB PrintEditorStatus
	FOR i = 16 TO 21
		LOCATE i, 6
		COLOR 0, 7
		CurNote = i + WindowTop - 16
		IF CurNote <= Counter THEN
			GOSUB PrintCurNote
		ELSE
			PRINT SPACE$(29)
		END IF
	NEXT i
	COLOR 7, 0

'     - This subroutine draws the current note again but in a different color to indicate a "cursor."
DrawNoteCursor:
	IF Cursor < WindowTop THEN
		WindowTop = Cursor
		GOTO DisplayNoteTable
	ELSEIF Cursor > WindowTop + 5 THEN
		WindowTop = Cursor - 5
		GOTO DisplayNoteTable
	END IF
	CurNote = Cursor
	LOCATE Cursor - WindowTop + 16, 6
	GOSUB PrintCurNote
	DrawNote DrawnNote.Note, DrawnNote.Oct, UP
	DrawNote Song(CurNote).Note, Song(CurNote).Oct, DOWN
	DrawnNote = Song(CurNote)
	RETURN

'     - This subroutine actually does the conversion of the note into text information and prints it out.
PrintCurNote:
	IF Song(CurNote).Note = 0 THEN
		PRINT USING " \        \           ###### "; "<rest>"; Song(CurNote).Dur
	ELSE
		PRINT USING " \    \       ##      ###### "; GetNote$(Song(CurNote).Note); Song(CurNote).Oct; Song(CurNote).Dur
	END IF
	RETURN

'     - This subroutine displays the number of notes in the song and the number of the current note.
PrintEditorStatus:
	COLOR 7, 0
	LOCATE 23, 6
	PRINT USING " Notes: ####  Current:  #### "; Counter; Cursor
	RETURN

'     - This subroutine deletes the current note.
DeleteCurrentNote:
	IF Counter = 1 THEN
		ErrorMessage "You cannot delete the last note."
		DisplayMenuText EDITOR
	ELSE
		Counter = Counter - 1
		FOR i = Cursor TO Counter
			Song(i) = Song(i + 1)                ' Delete note up on notch.
		NEXT i
		IF Cursor > Counter THEN Cursor = Counter
	END IF
	GOTO DisplayNoteTable

'     - This subroutine controls the input of a new note.  It draws
'     - the box, processes the keys pressed and changes the note
'     - values accordingly, until Esc or Enter is pressed.  Before
'     - this routine is called, the starting values of the new note
'     - are placed in EditNote; when the edit has been completed, the
'     - new note's value is placed back into EditNote.
GetNewNote:
	COLOR 0, 7                                    ' Draw the edit box.
	DrawBox 16, 39, 24, 76, EditTitle$
	LOCATE 18, 42
	PRINT "Use arrow keys to change values"
	LOCATE , 42
	PRINT "Enter when done, Esc to cancel"
	LOCATE 21, 42
	COLOR 7, 0
	PRINT "Note";
	COLOR 0, 7
	PRINT "       Octave      Duration"
	EditField = 1
	EditDone = FALSE
	Cancelled = FALSE

	WHILE NOT EditDone
		Note$ = GetNote$(EditNote.Note)             ' Display the note.
		LOCATE 22, 42
		IF EditNote.Note > 0 THEN
			PRINT USING "\         \  ##         ######"; Note$; EditNote.Oct; EditNote.Dur
		ELSE
			PRINT USING "\               \       ######"; "    <rest>"; EditNote.Dur
		END IF
  
		DO
			k$ = INKEY$
		LOOP WHILE k$ = ""
  
		IF LEN(k$) > 1 THEN
			SELECT CASE RIGHT$(k$, 1)
			CASE "H", "P"                   ' Up arrow or Down arrow key pressed
			IF RIGHT$(k$, 1) = "H" THEN     ' set increment appropriately.
				Increment = 1
			ELSE
				Increment = -1
			END IF
  
			SELECT CASE EditField                 ' Change value according to edit mode.
			CASE 1
				EditNote.Note = EditNote.Note + Increment
				IF EditNote.Note > B THEN
					EditNote.Note = 0
					EditNote.Oct = EditNote.Oct + 1
				ELSEIF EditNote.Note < 0 THEN
					EditNote.Note = B
					EditNote.Oct = EditNote.Oct - 1
				END IF
			CASE 2
				EditNote.Oct = EditNote.Oct + Increment
			CASE 3
				EditNote.Dur = EditNote.Dur + Increment
				IF EditNote.Dur < 1 THEN EditNote.Dur = 0
			END SELECT

			IF EditNote.Oct = 3 THEN              ' Keep octave value in range.
				IF EditNote.Note > C THEN
					EditNote.Oct = 2
				END IF
			ELSEIF EditNote.Oct > 3 THEN
				EditNote.Oct = 3
			ELSEIF EditNote.Oct < -2 THEN
				EditNote.Oct = -2
			END IF
				  
			CASE "K", "M"                        ' Left arrow or Right arrow key pressed.
				IF RIGHT$(k$, 1) = "K" THEN      ' Set increment appropriately.
					Increment = -1
				ELSE
					Increment = 1
				END IF
   
				EditField = EditField + Increment     ' Change edit mode according
				IF EditField > 3 THEN EditField = 1   ' to increment's value.
				IF EditField < 1 THEN EditField = 3

				LOCATE 21, 42                         ' Redraw the field titles to
				COLOR 0, 7                            ' show which one has focus.
				PRINT "Note       Octave      Duration"
				COLOR 7, 0
				SELECT CASE EditField
				CASE 1
					LOCATE 21, 42
					PRINT "Note"
				CASE 2
					LOCATE 21, 53
					PRINT "Octave"
				CASE 3
					LOCATE 21, 65
					PRINT "Duration"
				END SELECT
				COLOR 0, 7
	 
			END SELECT
	 
		ELSEIF k$ = CHR$(13) THEN                   ' Enter key = exit
			EditDone = TRUE
		ELSEIF k$ = CHR$(27) THEN                   ' Esc key = exit/cancel
			Cancelled = TRUE
			EditDone = TRUE
		END IF
	WEND
	DisplayMenuText EDITOR                          ' Redisplay editor screen.
	RETURN

'     - This subroutine inserts EditNote into the song just before the current note.
InsertEditNote:
	Counter = Counter + 1
	FOR i = Counter TO Cursor + 1 STEP -1         ' Move all notes from current
		Song(i) = Song(i - 1)                     ' note down one notch, and put
	NEXT i                                        ' the new note at the current
	Song(Cursor) = EditNote                       ' location.
	Cursor = Cursor + 1
	RETURN

END SUB

'-------------------------------------------------------------------------
' ErrorMessage
'
'   Prints an error message in a box and waits for a keypress.
'
'        PARAMETERS:    msg$ - The message to display in the box.
'-------------------------------------------------------------------------
SUB ErrorMessage (msg$) STATIC

	COLOR 15, 4                                   ' Draw the box.
	DrawBox 15, 15, 19, 65, ""
	LOCATE 16, 40 - LEN(msg$) \ 2
	PRINT msg$                                    ' Print the message
	LOCATE 18, 27
	PRINT "Press any key to continue"

	SOUND 250, 1                                  ' Make some sound.
	SOUND 32767, 2
	SOUND 200, 1

	DO WHILE INKEY$ = "": LOOP                    ' Wait for a keypress.

END SUB

'-------------------------------------------------------------------------
' GetNameAndSave
'
'   Gets a name and description for a newly recorded song.  If the
'   user presses the Esc key, the song is not saved.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB GetNameAndSave STATIC

	DisplayMenuText GETNAME                         ' Display the screen info.

	Finished = FALSE
	EditField = 1
	naym$ = ""
	desc$ = ""

	WHILE Finished = FALSE                          ' Depending on the value of
		SELECT CASE EditField                       ' EditField, call SimpleEdit
		CASE 1                                      ' with either naym$ or desc$
			RetVal = SimpleEdit(16, 27, naym$, 25)  ' as a parameter.  Then, take
			IF RetVal = TABCHAR THEN EditField = 2  ' a look at the return value:
		CASE 2                                      ' If it was a Tab key, then
			RetVal = SimpleEdit(19, 27, desc$, 40)  ' switch to the other edit
			IF RetVal = TABCHAR THEN EditField = 1  ' field   otherwise, we're done.
		END SELECT

		IF RetVal = ESC OR RetVal = CR THEN
			IF naym$ = "" AND RetVal = CR THEN          ' Make sure user enters a name.
				ErrorMessage "You must supply a song name."
				DisplayMenuText GETNAME                 ' Redisplay dialog box.
				LOCATE 16, 27
				PRINT naym$
				LOCATE 19, 27
				PRINT desc$
			ELSE
				Finished = TRUE
			END IF
		END IF
	WEND

	IF RetVal = CR THEN                             ' Only save the song if the
		SongName = naym$                            ' user pressed Enter to
		SongDesc = desc$                            ' terminate the save dialog.
		SaveSong
	END IF

END SUB

'-------------------------------------------------------------------------
' GetNote$
'
' Given a note number, return the text representation of that note.
'
'        PARAMETERS: Note - Value of note to be converted
'-------------------------------------------------------------------------
FUNCTION GetNote$ (Note) STATIC

	SELECT CASE Note
	CASE C
		GetNote$ = "C"
	CASE D
		GetNote$ = "D"
	CASE E
		GetNote$ = "E"
	CASE F
		GetNote$ = "F"
	CASE G
		GetNote$ = "G"
	CASE A
		GetNote$ = "A"
	CASE B
		GetNote$ = "B"
	CASE DF
		GetNote$ = "D flat"
	CASE EF
		GetNote$ = "E flat"
	CASE GF
		GetNote$ = "G flat"
	CASE AF
		GetNote$ = "A flat"
	CASE BF
		GetNote$ = "B flat"
	END SELECT

END FUNCTION

'-------------------------------------------------------------------------
' InitFreq
'
'   Initializes the note frequency table and the keyboard map array.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB InitFreq STATIC

	'                    - Initialize frequency table
	Freq(1) = 4186
	Freq(2) = 4435
	Freq(3) = 4699
	Freq(4) = 4978
	Freq(5) = 5274
	Freq(6) = 5588
	Freq(7) = 5920
	Freq(8) = 6272
	Freq(9) = 6645
	Freq(10) = 7040
	Freq(11) = 7459
	Freq(12) = 7902

	' Initialize keyboard map
	FOR i = 1 TO 127
		Kyb(i).Note = 0                           ' Set all keys to 0 first
		Kyb(i).Oct = 0                            ' (that means no note is
	NEXT i                                        ' played if that key is pressed).

	CurNote = C                                   ' Initialize counter variables.
	CurOct = -1

	'
	' The code below correlates note/octave values with the values
	' returned by the INP(&H60) function, which is the function that reads
	' the keyboard port to see what key is pressed.  The numbers returned by
	' the keys pressed are sequential as follows:
	'
	'      KEY PRESSED             VALUE RETURNED
	'
	'      1 thru 0        ->      2  thru 11
	'      Q thru P        ->      16 thru 25
	'      A thru ;        ->      30 thru 39
	'      Z thru ,        ->      44 thru 51
	'

	'Mapping for Q through P
	Kyb(16).Note = A
	Kyb(16).Oct = -2
	Kyb(17).Note = B
	Kyb(17).Oct = -2
	Kyb(18).Note = C
	Kyb(18).Oct = -1
	Kyb(19).Note = D
	Kyb(19).Oct = -1
	Kyb(20).Note = E
	Kyb(20).Oct = -1
	Kyb(21).Note = F
	Kyb(21).Oct = -1
	Kyb(22).Note = G
	Kyb(22).Oct = -1
	Kyb(23).Note = A
	Kyb(23).Oct = -1
	Kyb(24).Note = B
	Kyb(24).Oct = -1
	Kyb(25).Note = C
	Kyb(25).Oct = 0
	Kyb(26).Note = D
	Kyb(26).Oct = 0
	Kyb(27).Note = E
	Kyb(27).Oct = 0

	'Mapping for 1 through 0
	Kyb(3).Note = BF
	Kyb(3).Oct = -2
	Kyb(5).Note = DF
	Kyb(5).Oct = -1
	Kyb(6).Note = EF
	Kyb(6).Oct = -1
	Kyb(8).Note = GF
	Kyb(8).Oct = -1
	Kyb(9).Note = AF
	Kyb(9).Oct = -1
	Kyb(10).Note = BF
	Kyb(10).Oct = -1
	Kyb(12).Note = DF
	Kyb(12).Oct = 0
	Kyb(13).Note = EF
	Kyb(13).Oct = 0

	'Mapping for Z through .
	Kyb(44).Note = F
	Kyb(44).Oct = 0
	Kyb(45).Note = G
	Kyb(45).Oct = 0
	Kyb(46).Note = A
	Kyb(46).Oct = 0
	Kyb(47).Note = B
	Kyb(47).Oct = 0
	Kyb(48).Note = C
	Kyb(48).Oct = 1
	Kyb(49).Note = D
	Kyb(49).Oct = 1
	Kyb(50).Note = E
	Kyb(50).Oct = 1
	Kyb(51).Note = F
	Kyb(51).Oct = 1
	Kyb(52).Note = G
	Kyb(52).Oct = 1
	Kyb(53).Note = A
	Kyb(53).Oct = 1

	'Mapping for A through ;
	Kyb(31).Note = GF
	Kyb(31).Oct = 0
	Kyb(32).Note = AF
	Kyb(32).Oct = 0
	Kyb(33).Note = BF
	Kyb(33).Oct = 0
	Kyb(35).Note = DF
	Kyb(35).Oct = 1
	Kyb(36).Note = EF
	Kyb(36).Oct = 1
	Kyb(38).Note = GF
	Kyb(38).Oct = 1
	Kyb(39).Note = AF
	Kyb(39).Oct = 1
	Kyb(40).Note = BF
	Kyb(40).Oct = 1

END SUB

'-------------------------------------------------------------------------
' LoadDefaultSong
'
'    Reads the data for either SONG1 or SONG2 into the Song() array.
'
'       PARAMETERS:     num - Indicates which internal song to load
'-------------------------------------------------------------------------
SUB LoadDefaultSong (num) STATIC

	IF num = 1 THEN                               ' Restore appropriate line number.
		RESTORE SONG1
	ELSE
		RESTORE SONG2
	END IF

	READ SongName, SongDesc, Counter              ' Read global song info.
	FOR i = 1 TO Counter                          ' Read in the notes.
		READ n, o, Dur
		Song(i).Note = n
		Song(i).Oct = o
		Song(i).Dur = Dur
	NEXT i

END SUB

'-------------------------------------------------------------------------
' LoadSong
'
'   Given an index into the QSYNTH.DAT file, this SUB loads the song data from
'   the file into the global Song() array.
'
'        PARAMETERS:    SongNo - Index of song to load
'-------------------------------------------------------------------------
SUB LoadSong (SongNo) STATIC

	DIM HeaderInfo AS FileHeader
	DIM S AS SongIndexCard
	FileError = 0

	OPEN "QSYNTH.DAT" FOR BINARY AS 1

	' seek to the beginning of the song index record and read it
	SEEK #1, LEN(HeaderInfo) + (LEN(S) * (SongNo - 1)) + 1
	GET #1, , S
	SongName = S.naym                             ' Set the global song info.
	SongDesc = S.desc
	Counter = S.Size

	' seek to the beginning of the actual song information according to the
	' information in the song index record
	SEEK #1, LEN(HeaderInfo) + (LEN(S) * MAXSONG) + S.Offset

	FOR i = 1 TO Counter                          ' Read the notes in from disk.
		GET #1, , Song(i)
	NEXT i
	CLOSE 1

END SUB

'-------------------------------------------------------------------------
' MainMenu
'
'   Handles the main menu.  It waits for a keypress and acts upon it.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB MainMenu STATIC

	DIM S AS SongIndexCard
	DIM HeaderInfo AS FileHeader                  ' Dimension variables.
	REDIM i$(1 TO 10)

	Cursor = 1
	WindowTop = 1
	SongLoaded = 0
	GOSUB LoadSongList

	Finished = FALSE
	WHILE NOT Finished
		DisplayMenuText MAIN
		GOSUB DisplaySongList
		x$ = ""
		'     Wait for a relevant keypress
		WHILE INSTR("PSERDQ" + CHR$(13), x$) = 0 OR x$ = ""
			x$ = UCASE$(INKEY$)
			IF LEN(x$) > 1 THEN
				IF RIGHT$(x$, 1) = "H" AND Cursor > 1 THEN
					Cursor = Cursor - 1
				ELSEIF RIGHT$(x$, 1) = "P" AND Cursor < UBOUND(i$) THEN
					Cursor = Cursor + 1
				END IF
				GOSUB DisplaySongList
			END IF
		WEND

		SELECT CASE x$
		CASE "S"
			RecordMenu (TRUE)             ' Practice mode.
  
		CASE "P", CHR$(13)                ' Load and play the appropriate song.
			IF Cursor > 2 THEN
				LoadSong Cursor - 2
			ELSE
				LoadDefaultSong Cursor
			END IF
			PlaySong

		CASE "E"                          ' Song editor
			IF Cursor > 2 THEN
				LoadSong Cursor - 2
				EditSong Cursor - 2
			ELSE
				ErrorMessage "You cannot edit a system song."
			END IF

		CASE "R"                          ' Record mode
			SongRecorded = FALSE
			RecordMenu (FALSE)
			IF SongRecorded THEN
				GetNameAndSave            ' Only ask to save if something was recorded.
			END IF
			GOSUB LoadSongList

		CASE "D"                          ' Delete song
			IF Cursor > 2 THEN
				IF ConfirmDelete THEN
					DeleteSong Cursor - 2
					IF Cursor = UBOUND(i$) THEN Cursor = Cursor - 1
					GOSUB LoadSongList
				END IF
			ELSE
				ErrorMessage "You cannot delete a system song."
			END IF

		CASE "Q"                          ' Quit QSynth.
			Finished = TRUE
		END SELECT
	WEND
	EXIT SUB

'       This subroutine loads the names of all the songs in the QSYNTH.DAT file.
LoadSongList:
	OPEN "QSYNTH.DAT" FOR BINARY AS 1
	IF LOF(1) = 0 THEN
		CreateSongFile                            ' Create disk file if not there.
	END IF
	HeaderInfo.Count = 0
	GET #1, , HeaderInfo
	REDIM i$(1 TO 2 + HeaderInfo.Count)           ' Size array appropriately.
	RESTORE SONG1                                 ' First two songs are provided as DATA statements.
	READ i$(1)
	RESTORE SONG2
	READ i$(2)

	FOR i = 3 TO UBOUND(i$)                       ' Read the rest from disk.
		GET #1, , S
		i$(i) = S.naym
	NEXT i
	CLOSE 1
	RETURN

'       This subroutine displays the song list in the Song List box.
DisplaySongList:
	COLOR 0, 7
	FOR i = 15 TO 23
		LOCATE i, 8
		IF i - 15 + WindowTop <= UBOUND(i$) THEN
			PRINT " "; LEFT$(i$(i - 15 + WindowTop) + SPACE$(27), 25); " "
		ELSE
			PRINT SPACE$(27)
		END IF
	NEXT i

'       This subroutine displays the "cursor" by re-displaying the current song's name in a different color.
DisplaySongCursor:
	IF Cursor < WindowTop THEN
		WindowTop = Cursor
		GOTO DisplaySongList
	ELSEIF Cursor > WindowTop + 8 THEN
		WindowTop = Cursor - 8
		GOTO DisplaySongList
	END IF
	COLOR 7, 0
	LOCATE 15 + Cursor - WindowTop, 8
	PRINT " "; LEFT$(i$(Cursor) + SPACE$(27), 25); " "
	COLOR TEXTCOLOR, BACKGROUNDCOLOR
	RETURN

END SUB

'-------------------------------------------------------------------------
' PlayNote
'
'   Calculates the correct frequency of the note given in the octave
'   given for the duration given.
'
'        PARAMETERS:    Note    -       The note to play
'                       Octave  -       The octave to play the note in
'                       Duration# -     The duration (time) to play the note
'-------------------------------------------------------------------------
SUB PlayNote (Note, Octave, Duration#) STATIC

	IF Note <> 0 THEN
		' Perform equation to find frequency value from the Freq() array
		ThisFreq& = Freq(Note) / (2 ^ (PITCH - Octave))

		IF ThisFreq& > 32767 THEN ThisFreq& = 32767
		SOUND ThisFreq&, Duration#
	ELSE
		TimeDelay Duration#
	END IF

END SUB

'-------------------------------------------------------------------------
' PlaySong
'
'   Performs song playback by running through the Song() array playing
'   each note, and highlighting each note on the keyboard.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB PlaySong STATIC

	DisplayMenuText PLAYBACK

	DO                                            ' Wait for the Enter key,
		i$ = INKEY$                               ' Adjust the tempo.
		SELECT CASE i$
		CASE CHR$(27)
			EXIT SUB
		CASE CHR$(0) + "H", CHR$(0) + "M"
			ChangeTempo UP
		CASE CHR$(0) + "P", CHR$(0) + "K"
			ChangeTempo DOWN
		CASE CHR$(13)
			EXIT DO
		END SELECT
	LOOP

	Finished = FALSE
	FOR i = 1 TO Counter
		DrawNote Song(i).Note, Song(i).Oct, DOWN
		FOR j = 1 TO Song(i).Dur
			IF Song(i).Note > 0 THEN
				PlayNote Song(i).Note, Song(i).Oct, NOTETICK * TFACTOR
			ELSE
				PlayNote 0, 0, RESTTICK * TFACTOR
			END IF

			i$ = INKEY$                           ' Adjust the TEMPO if need be.
			SELECT CASE UCASE$(i$)
			CASE CHR$(27)
				Finished = TRUE
				EXIT FOR
			CASE CHR$(0) + "H", CHR$(0) + "M"
				ChangeTempo UP
			CASE CHR$(0) + "P", CHR$(0) + "K"
				ChangeTempo DOWN
			END SELECT

		NEXT j
		DrawNote Song(i).Note, Song(i).Oct, UP    ' "Undraw" the pressed key.
		IF Finished THEN EXIT FOR
	NEXT i

END SUB

'-------------------------------------------------------------------------
' RecordMenu
'
'   Sets up the screen for record/practice mode.
'
'       PARAMETERS: NoSave  -   Indicates whether the user is doing PRACTICE or RECORD
'-------------------------------------------------------------------------
SUB RecordMenu (NoSave) STATIC

	IF NoSave = TRUE THEN
		DisplayMenuText PRACTICE            ' Draw the PRACTICE screen.
	ELSE
		DisplayMenuText RECORD              ' Draw the RECORD screen.
	END IF
   
	COLOR 7, 0                              ' Show the keyboard helper keys.
	LOCATE 6, 17:   PRINT "2   4 5   7 8 9   - =   S D F   H J   L ; '"
	LOCATE 12, 16: PRINT "Q W E R T Y U I O P [ ] Z X C V B N M , . /"

	RecordMode NoSave
   
	COLOR , 0                               ' Print spaces over both rows.
	LOCATE 6, 4: PRINT SPACE$(75)
	LOCATE 12, 4: PRINT SPACE$(75)
	COLOR , BACKGROUNDCOLOR
 
END SUB

'-------------------------------------------------------------------------
' RecordMode
'
'   Handles the keypresses from the user and plays
'   and records notes in the global Song() array.  For keyboard input,
'   it uses the INP function to read from the keyboard port (ADDR = 0x60).
'   This is done to determine what key was pressed and how long it was pressed.
'
'       PARAMETERS: NoSave  -   Indicates whether the user is doing PRACTICE or RECORD
'-------------------------------------------------------------------------
SUB RecordMode (NoSave) STATIC

	Hld$ = ""
	Counter = 1
	Song(Counter).Note = 0
	Song(Counter).Dur = 0
	KeyState = UP
	oi$ = "": n$ = ""
	FOR i = 1 TO 12
		n$ = n$ + CHR$(i)
	NEXT i
	n$ = MID$(n$, 10) + n$ + n$ + n$            ' Used for the XT version.
	o$ = "00011111111111122222222222233333333333"

	IF INP(&H60) = 0 THEN                       ' If 0, we must use INKEY$
		XTKeyboard = TRUE                       ' instead of INP(&H60)
	ELSE                                        ' because of certain keyboard
		XTKeyboard = FALSE                      ' controllers that do not
	END IF                                      ' keep the last keypress in the &h60 port.
	DO
		i$ = INKEY$
	LOOP WHILE i$ = ""                            ' Wait for first keypress.
	IF i$ = CHR$(27) THEN EXIT SUB                ' Esc means no song recorded.

	x = 0
	WHILE i$ <> CHR$(27)
		IF XTKeyboard THEN                        ' Do this block if XT keyboard
			IF i$ = oi$ THEN
				IF Song(Counter).Note = 0 THEN
					PlayNote Song(Counter).Note, Song(Counter).Oct, RESTTICK
					Song(Counter).Dur = Song(Counter).Dur + 1
				ELSE                                      ' a real note
					PlayNote Song(Counter).Note, Song(Counter).Oct, XTNOTELENGTH * NOTETICK
					Song(Counter).Dur = Song(Counter).Dur + XTNOTELENGTH
				END IF
			ELSEIF i$ <> "" THEN
				keyloc = INSTR("Q2WE4R5TY7U8I9OP-[=]ZSXDCFVBHNJM,L.;/'", UCASE$(i$))
				IF keyloc = 0 THEN
					DrawNote Song(Counter).Note, Song(Counter).Oct, UP
					Counter = Counter + 1
					IF Counter > MAXNOTE THEN
						ErrorMessage "Maximum song length reached."
						GOTO GetOut
					END IF
					Song(Counter).Note = 0
					Song(Counter).Oct = 0           ' Rests are counted.
					Song(Counter).Dur = 0
					DrawNote Song(Counter).Note, Song(Counter).Oct, DOWN
				ELSE
					DrawNote Song(Counter).Note, Song(Counter).Oct, UP
					Counter = Counter + 1
					NewNote = ASC(MID$(n$, keyloc, 1))
					NewOct = VAL(MID$(o$, keyloc, 1)) - 2

					IF Counter > MAXNOTE THEN
						ErrorMessage "Maximum song length reached."
						GOTO GetOut
					END IF
					Song(Counter).Note = NewNote
					Song(Counter).Oct = NewOct
					Song(Counter).Dur = XTNOTELENGTH
					DrawNote Song(Counter).Note, Song(Counter).Oct, DOWN
					PlayNote Song(Counter).Note, Song(Counter).Oct, XTNOTELENGTH * NOTETICK
					TimeDelay XTNOTELENGTH * NOTETICK
				END IF
			ELSEIF i$ = "" THEN
				DrawNote Song(Counter).Note, Song(Counter).Oct, UP
				Counter = Counter + 1
				IF Counter > MAXNOTE THEN
					ErrorMessage "Maximum song length reached."
					GOTO GetOut
				END IF
				Song(Counter).Note = 0
				Song(Counter).Oct = 0
				Song(Counter).Dur = 0
				DrawNote Song(Counter).Note, Song(Counter).Oct, DOWN
			END IF
		
		ELSE                                    ' Do this block if not XT keyboard.
			ox = x
			Hld$ = Hld$ + i$
			IF (LEN(Hld$) < 2) AND (Song(Counter).Dur > 8) AND (Song(Counter).Note <> 0) THEN
				x = 1
			ELSE
				x = INP(&H60)
			END IF

			IF ox = x THEN                      ' no change in keypress
				Song(Counter).Dur = Song(Counter).Dur + 1
				IF Song(Counter).Note = 0 THEN            ' a rest
					PlayNote Song(Counter).Note, Song(Counter).Oct, RESTTICK
				ELSE                                      ' a real note
					PlayNote Song(Counter).Note, Song(Counter).Oct, NOTETICK
				END IF
				IF LEN(Hld$) > 1 THEN Hld$ = ""

			ELSEIF x < 128 THEN                           ' it's a KEYDOWN event
				IF KeyState = DOWN THEN
					DrawNote Song(Counter).Note, Song(Counter).Oct, UP
				END IF
				Counter = Counter + 1
				IF Counter > MAXNOTE THEN
					ErrorMessage "Maximum song length reached."
					GOTO GetOut
				END IF
				Song(Counter).Note = Kyb(x).Note
				Song(Counter).Oct = Kyb(x).Oct
				Song(Counter).Dur = 0
				DrawNote Song(Counter).Note, Song(Counter).Oct, DOWN
				Hld$ = ""
				KeyState = DOWN

			ELSEIF (x - 128) <> ox THEN               ' an old key came up
				x = ox
				Song(Counter).Dur = Song(Counter).Dur + 1
				IF Song(Counter).Note = 0 THEN            ' a rest
					PlayNote Song(Counter).Note, Song(Counter).Oct, RESTTICK
				ELSE                                      ' a real note
					PlayNote Song(Counter).Note, Song(Counter).Oct, NOTETICK
				END IF
				IF LEN(Hld$) > 1 THEN Hld$ = ""
				 
			ELSEIF KeyState = DOWN THEN               ' it's a KEYUP event
				KeyState = UP
				DrawNote Song(Counter).Note, Song(Counter).Oct, UP
				Counter = Counter + 1
				IF Counter > MAXNOTE THEN
					ErrorMessage "Maximum song length reached."
					GOTO GetOut
				END IF
				Song(Counter).Note = 0
				Song(Counter).Dur = 0
				Hld$ = ""
			END IF
		END IF
		oi$ = i$
		i$ = INKEY$
	WEND

GetOut:
	DrawNote Song(Counter).Note, Song(Counter).Oct, UP  ' Make sure that the last key pressed is released.

	IF Counter = 1 OR NoSave = TRUE THEN EXIT SUB       ' No notes recorded or Practice mode.

	WHILE Song(Counter).Note = 0 AND Counter > 0   ' Get rid of any rests at the end of the song.
		Counter = Counter - 1
	WEND

	SongRecorded = TRUE                            ' Let MainMenu know we have recorded a new song.

END SUB

'-------------------------------------------------------------------------
' SaveChanges
'
'   Asks the user if the changes just made to the current song with the song
'   editor should be saved or not.  It returns TRUE if so, or FALSE if not.
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
FUNCTION SaveChanges STATIC

	COLOR BACKGROUNDCOLOR, BACKGROUNDCOLOR
	DrawBox 13, 38, 24, 78, ""
	COLOR 0, 7
	DrawBox 15, 41, 20, 75, "Save Changes"
	LOCATE 17, 43
	PRINT "Do you want to save the changes"
	LOCATE , 43, 1
	PRINT "made to this song? (Y/N) ";

	DO                                    ' Wait for keypress.
		i$ = UCASE$(INKEY$)
	LOOP UNTIL i$ = "Y" OR i$ = "N"

	LOCATE , , 0                          ' Return appropriate value.
	IF i$ = "Y" THEN
		SaveChanges = TRUE
	ELSE
		SaveChanges = FALSE
	END IF

END FUNCTION

'-------------------------------------------------------------------------
' SaveSong
'
' Saves the current song in the QSYNTH.DAT file, thus adding
' the song to the Song List.  The QSYNTH.DAT file has the following format:
'
'  o A file header having a fixed length (LEN(HeaderInfo), where HeaderInfo
'    is a variable of type FileHeader)
'
'  o A "Song Index Card" area having a fixed length (LEN(S) * MAXSONG, where
'    S is a variable of type SongIndexCard, and MAXSONG is the constant
'    which defines the maximum number of songs which can be saved in the file)
'
'  o A "Song Data" area which is of variable length, since the songs can be of
'    variable length.  Thus, the last MEANINGFUL byte in the file is the last
'    byte of song data of the last song in the file.  It is important to
'    understand that this last note of the last song may not be the last
'    physical byte of the file.  If a song is deleted from the file, the other
'    songs in the file are "moved up", to fill in the "hole" left by the song
'    that was deleted.  However, the file does not decrease in size.  Once the
'    remaining songs are moved up in the file, there exists "old" data starting
'    1 byte after the last note of the last song, and extending to the end of
'    the physical file.
'
'    The end of the meaningful data in the file can always be found using the
'    NextNote field of the FileHeader structure.  This value ALWAYS points to
'    the next AVAILABLE note position in the song data area of the file.  Thus,
'    the last byte of the last song in the file is at location NextNote-1.
'
'    IMPORTANT NOTE:  NextNote is relative to the beginning of the song data
'                     area, NOT the physical beginning of the file!  So, the
'                     physical location of the next available note can be found
'                     by:  LEN(Header) + LEN(S)*MAXSONG + Header.NextNote
'
'    Understanding this file structure is crucial to understanding how the
'    SaveSong and DeleteSong procedures work.  Here in SaveSong, the Song Index
'    Card is written to the FileHeader.Count+1 record of the SongIndexCard part
'    of the file, and the actual song data is written to the file starting at
'    the NextNote byte of the file.  (See the DeleteNote procedure for details
'    on how songs are deleted from the file).
'
'             PARAMETERS:   None
'-------------------------------------------------------------------------
SUB SaveSong STATIC

	DIM S AS SongIndexCard
	DIM HeaderInfo AS FileHeader
	FileError = 0

	OPEN "QSYNTH.DAT" FOR BINARY AS 1             ' Open the file.
	GET #1, , HeaderInfo                          ' Get header info.

	' Seek to the next available song index record.  The Count field of the
	' header record is 1-based, so LEN(S)*HeaderInfo.Count + 1 is the first
	' byte of song index record #(Count+1)
	SEEK #1, LEN(HeaderInfo) + (LEN(S) * HeaderInfo.Count) + 1

	S.naym = SongName                           ' Insert data for current song.
	S.desc = SongDesc
	S.Size = Counter
	S.Offset = HeaderInfo.NextNote              ' PUT it into the file at the
	PUT #1, , S                                 ' file position found by the SEEK statement.

	' Seek to the next available note position in the song data section.  This
	' is found using the NextNote field of the file header structure.  Remember
	' the NextNote pointer is relative to the start of the song data area,
	' and not the physical beginning of the file, so we have to add the size of
	' the file header and the song index card sections as well.
	SEEK #1, LEN(HeaderInfo) + (LEN(S) * MAXSONG) + HeaderInfo.NextNote

	FOR i = 1 TO Counter                        ' Write each note of the song
		PUT #1, , Song(i)                       ' to the file.
	NEXT i

	' Update the header information (increment the song count, and point
	' NextNote at the next available note in the file by adding the size of the
	' newly saved song to the current value of NextNote.
	HeaderInfo.Count = HeaderInfo.Count + 1
	HeaderInfo.NextNote = HeaderInfo.NextNote + (Counter * LEN(Song(1)))
	SEEK #1, 1                                  ' Seek to the start.
	PUT #1, , HeaderInfo                        ' Write the new header.
	CLOSE 1                                     ' The song is saved!

END SUB

'-------------------------------------------------------------------------
' SimpleEdit
'
' This function is a very simple one-line edit routine.  It allows entry of
' text up to a given maximum length, and the editing capabilities are
' limited to backspace (to delete the last character typed).  It returns a
' code indicating which terminator key was pressed:  Esc, Enter or Tab.
'
'        PARAMETERS:    row,col - Row and column position of first character
'                                 in text
'                       Text$   - String variable to place edited text (can
'                                 contain the initial value of the text)
'                       MaxLen  - Maximum length allowed for the entered text
'-------------------------------------------------------------------------
FUNCTION SimpleEdit (row, col, text$, MaxLen) STATIC

	LOCATE row, col, 1                    ' Turn on the cursor.
	PRINT text$;

	Finished = FALSE
	WHILE NOT Finished
		i$ = INKEY$
		SELECT CASE i$
		CASE CHR$(13)
			Finished = TRUE                 ' Enter key pressed.
			SimpleEdit = CR

		CASE CHR$(27)
			Finished = TRUE                 ' Esc key pressed.
			SimpleEdit = ESC

		CASE CHR$(9)
			Finished = TRUE                 ' Tab key pressed.
			SimpleEdit = TABCHAR

		CASE CHR$(8)                      ' Backspace key pressed.
			IF LEN(text$) > 0 THEN
				text$ = LEFT$(text$, LEN(text$) - 1)
				LOCATE row, col
				PRINT text$; " ";
				LOCATE row, col + LEN(text$)
			END IF

		CASE CHR$(32) TO CHR$(126)
			IF LEN(text$) < MaxLen THEN     ' Normal key - add it to text$.
				text$ = text$ + i$
				PRINT i$;
			END IF
		END SELECT
	WEND

	LOCATE , , 0                          ' Turn cursor off.

END FUNCTION

'-------------------------------------------------------------------------
' TimeDelay
'
'   Waits for a length of time equal to a SOUND 0, Cur# statement.
'
'        PARAMETERS:    Dur#    -       Duration of delay
'-------------------------------------------------------------------------
SUB TimeDelay (Dur#) STATIC

	x# = TIMER                                    ' Wait until Dur# seconds pass.
	WHILE (TIMER - x#) < (Dur# / 18.2): WEND

END SUB

