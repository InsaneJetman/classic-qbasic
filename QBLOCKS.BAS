'                                QBLOCKS.BAS
'
' Press Page Down for information on running and modifying QBlocks.
'
' To run this game, press Shift+F5.
'
' To exit this program, press ALT, F, X.
'
' To get help on a BASIC keyword, move the cursor to the keyword and press
' F1 or click the right mouse button.
'
'                             Suggested Changes
'                             -----------------
'
' There are many ways that you can modify this BASIC game.  The CONST
' statements below these comments and the DATA statements at the end
' of this screen can be modified to change the following:
'
'    Block shapes
'    Block rotation
'    Number of different block shapes
'    Score needed to advance to next level
'    Width of the game well
'    Height of the game well
'    Songs played during game
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

' Here are the BASIC CONST statements you can change.  The comments tell
' you the range that each CONST value can be changed, or any limitations.
CONST WELLWIDTH = 10        ' Width of playing field (well).   Range 5 to 13.
CONST WELLHEIGHT = 21       ' Height of playing field.  Range 4 to 21.
CONST NUMSTYLES = 7         ' Number of unique shapes.  Range 1 to 20.  Make sure you read the notes above the DATA statements at the end of the main program before you change this number!
CONST WINGAME = 1000000     ' Points required to win the game.  Range 200 to 9000000.
CONST NEXTLEVEL = 300       ' Helps determine when the game advances to the next level.  (Each cleared level gives player 100 points) Range 100 to 2000.
CONST BASESCORE = 1000      ' Number of points needed to advance to first level.
CONST ROTATEDIR = 1         ' Control rotation of blocks. Can be 1 for clockwise, or 3 for counterclockwise.
' The following sound constants are used by the PLAY command to
' produce music during the game.  To change the sounds you hear, change
' these constants.  Refer to the online help for PLAY for the correct format.
' To completely remove sound from the game set the constants equal to null.
' For example:  PLAYINTRO = ""
CONST PLAYCLEARROW = "MBT255L16O4CDEGO6C"         ' Tune played when a row is cleared.  Range unlimited.
CONST PLAYINTRO = "MBT170O1L8CO2CO1CDCA-A-FGFA-F" ' Song played at game start.  Range unlimited.
CONST PLAYGAMEOVER = "MBT255L16O6CO4GEDC"         ' Song when the game is lost.  Range unlimited.
CONST PLAYNEWBLOCK = "MBT160L28N20L24N5"          ' Song when a new block is dropped.  Range unlimited.
CONST PLAYWINGAME = "T255L16O6CO4GEDCCDEFGO6CEG"  ' Song when game is won.  Range unlimited.

' The following CONST statements should not be changed like the ones above
' because the program relies on them being this value.
CONST FALSE = 0             ' 0 means FALSE.
CONST TRUE = NOT FALSE      ' Anything but 0 can be thought of as TRUE.
CONST SPACEBAR = 32         ' ASCII value for space character. Drops the shape.
CONST DOWNARROW = 80        ' Down arrow key.  Drops the shape.
CONST RIGHTARROW = 77       ' Right arrow key.  Moves the shape right.
CONST UPARROW = 72          ' Up arrow key.  Rotates the shape.
CONST LEFTARROW = 75        ' Left arrow key.  Moves the shape left.
CONST DOWNARROW2 = 50       ' 2 key.  Drops the shape.
CONST RIGHTARROW2 = 54      ' 6 key.  Moves the shape right.
CONST UPARROW2 = 56         ' 8 key.  Rotates the shape.
CONST LEFTARROW2 = 52       ' 4 key.  Moves the shape left.
CONST UPARROW3 = 53         ' 5 key.  Rotates the shape.
CONST QUIT = "Q"            ' Q key.  Quits the game.
CONST PAUSE = "P"           ' P key.  Pauses the game.
CONST XMATRIX = 3           ' Width of the matrix that forms each falling unit.  See the discussions in Suggested Changes #2 and #3.
CONST YMATRIX = 1           ' Depth of the matrix that forms each falling unit.
CONST BYTESPERBLOCK = 76    ' Number of bytes required to store one block in Screen mode 7.
CONST BLOCKVOLUME = (XMATRIX + 1) * (YMATRIX + 1)   ' Number of blocks in each shape.
CONST ELEMENTSPERBLOCK = BLOCKVOLUME * BYTESPERBLOCK \ 2    ' Number of INTEGER array elements needed to store an image of a shape.
CONST XSIZE = 13            ' Width, in pixels, of each block.  QBlocks assumes that the entire screen is 25 blocks wide.  Since the screen is 320 pixels wide, each block is approximately 13 pixels wide.
CONST YSIZE = 8             ' Height, in pixels, of each block.  Again, QBlocks assumes that screen is 25 blocks high.  At 200 pixels down, each block is exactly 8 pixels high.
CONST XOFFSET = 10          ' X position, in blocks, of the well.
CONST YOFFSET = 2           ' Y position, in blocks, of the well.
CONST WELLX = XSIZE * XOFFSET   ' X position, in pixels, of the start of the well.
CONST WELLY = YSIZE * YOFFSET   ' Y position.
CONST TILTVALUE = 9999000   ' Points required for QBlocks to tilt.
CONST WELLCOLOR7 = 0        ' Well color for SCREEN 7.
CONST WELLCOLOR1 = 0        ' Well color for SCREEN 1.
CONST BORDERCOLOR1 = 8      ' Border color for SCREEN 1.
CONST BORDERCOLOR7 = 15     ' Border color for SCREEN 7.

TYPE BlockType              ' Block datatype.
    X AS INTEGER            ' Horizontal location within the well.
    Y AS INTEGER            ' Vertical location within the well.
    Style AS INTEGER        ' Define shape (and color, indirectly).
    Rotation AS INTEGER     ' 4 possible values (0 to 3).
END TYPE

' SUB and FUNCTION declarations
DECLARE FUNCTION CheckFit ()
DECLARE FUNCTION GameOver ()
DECLARE SUB AddBlockToWell ()
DECLARE SUB CheckForFullRows ()
DECLARE SUB Center (M$, Row)
DECLARE SUB DeleteChunk (Highest%, Lowest%)
DECLARE SUB DisplayIntro ()
DECLARE SUB DisplayGameTitle ()
DECLARE SUB DisplayChanges ()
DECLARE SUB DrawBlock (X, Y, FillColor)
DECLARE SUB InitScreen ()
DECLARE SUB MakeInfoBox ()
DECLARE SUB NewBlock ()
DECLARE SUB PerformGame ()
DECLARE SUB RedrawControls ()
DECLARE SUB Show (b AS BlockType)
DECLARE SUB UpdateScoring ()
DECLARE SUB PutBlock (b AS BlockType)
DECLARE SUB DrawAllShapes ()
DECLARE SUB DrawPattern (Patttern)
DECLARE SUB DrawPlayingField ()

' DIM SHARED indicates that a variable is available to all subprograms.
' Without this statement, a variable used in one subprogram cannot be
' used by another subprogram or the main program.
DIM SHARED Level AS INTEGER                                         ' Difficulty level.  0 is slowest, 9 is fastest.
DIM SHARED WellBlocks(WELLWIDTH, WELLHEIGHT) AS INTEGER             ' 2 dimensional array to hold the falling shapes that have stopped falling and become part of the well.
DIM SHARED CurBlock AS BlockType                                    ' The falling shape.
DIM SHARED BlockShape(0 TO XMATRIX, 0 TO YMATRIX, 1 TO NUMSTYLES)   ' Holds the data required to make each shape.  Values determined by the DATA statements at the end of this window.
DIM SHARED PrevScore AS LONG                                        ' Holds the previous level for scoring purposes.
DIM SHARED Score AS LONG                                            ' Score.
DIM SHARED ScreenWidth AS INTEGER                                   ' Width of the screen, in character-sized units.
DIM SHARED ScreenMode AS INTEGER                                    ' Value of the graphics screen mode used.
DIM SHARED WellColor AS INTEGER                                     ' Color inside the well.
DIM SHARED BorderColor AS INTEGER                                   ' Color of well border and text.
DIM SHARED OldBlock AS BlockType                                    ' An image of the last CurBlock.  Used to erase falling units when they move.
DIM SHARED TargetTime AS SINGLE                                     ' Time to move the shape down again.
DIM SHARED GameTiltScore AS LONG                                    ' Holds the value that this game will tilt at.
DIM SHARED Temp(11175)  AS INTEGER                                  ' Used by several GET and PUT statements to store temporary screen images.
DIM SHARED BlockColor(1 TO NUMSTYLES) AS INTEGER                    ' Block color array
DIM SHARED BlockImage((NUMSTYLES * 4 + 3) * ELEMENTSPERBLOCK) AS INTEGER    ' Holds the binary image of each rotation of each shape for the PutBlock subprogram to use.
DIM KeyFlags AS INTEGER                                             ' Internal state of the keyboard flags when game starts.  Hold the state so it can be restored when the games ends.
DIM BadMode AS INTEGER                                              ' Store the status of a valid screen mode.


ON ERROR GOTO ScreenError               ' Set up a place to jump to if an error occurs in the program.
BadMode = FALSE
ScreenMode = 8
SCREEN ScreenMode                       ' Attempt to go into SCREEN 7 (EGA screen).
IF BadMode = TRUE THEN                  ' If this attempt failed.
    ScreenMode = 1
    BadMode = FALSE
    SCREEN ScreenMode                   ' Attempt to go into SCREEN 1 (CGA screen).
END IF
ON ERROR GOTO 0                         ' Turn off error handling.

IF BadMode = TRUE THEN                  ' If no graphics adapter.
    CLS
    LOCATE 10, 12: PRINT "CGA, EGA Color, or VGA graphics required to run QBLOCKS.BAS"
ELSE
    RANDOMIZE TIMER                     ' Create a new sequence of random numbers based on the clock.
    DisplayIntro                        ' Show the opening screen.

    DEF SEG = 0                         ' Set the current segment to the low memory area.
    KeyFlags = PEEK(1047)               ' Read the location that holds the keyboard flag.
    IF (KeyFlags AND 32) = 0 THEN       ' If the NUM LOCK key is off
        POKE 1047, KeyFlays OR 32       ' set the NUM LOCK key to on.
    END IF
    DEF SEG                             ' Restore the default segment.
    
    ' Read the pattern for each QBlocks shape.
    FOR i = 1 TO NUMSTYLES                  ' Loop for the each shape
        FOR j = 0 TO YMATRIX                ' and for the Y and X dimensions of
            FOR k = 0 TO XMATRIX            ' each shape.
                READ BlockShape(k, j, i)    ' Actually read the data.
            NEXT k
        NEXT j
    NEXT i
    DrawAllShapes                       ' Draw all shapes in all four rotations.
    PerformGame                         ' Play the game until the player quits.
    DisplayChanges                      ' Show the suggested changes.
   
    DEF SEG = 0                         ' Set the current segment back to low memory where the keyboard flags are.
    POKE 1047, KeyFlags AND 233         ' Set the NUM LOCK key back to where it was at the game start.
    DEF SEG                             ' Restore the current segment back to BASIC's data group area.

    IF ScreenMode = 7 THEN PALETTE      ' Restore the default color palette if SCREEN 7 was used.

END IF

END                                     ' End of the main program code.


' The DATA statements below define the block shapes used in the game.
' Each shape contains 8 blocks (4 x 2).  A "1" means that there
' is a block in that space; "0" means that the block is blank.  The pattern
' for Style 1, for example, creates a shape that is 4 blocks wide.
' To change an existing block's shape, change a "0" to a "1" or a "1" to
' a "0".  To add new shapes, insert new DATA statements with the same format
' as those below, after the last group of DATA statements (style 7).  Be sure
' to change the NUMSTYLES constant at the beginning of this program to reflect
' the new number of block shapes for the game.
' IMPORTANT! Creating a completely blank block will cause QBlocks to fail.

' Data for Style 1: Long
DATA 1,1,1,1
DATA 0,0,0,0

' Data for Style 2: L Right
DATA 1,1,1,0
DATA 0,0,1,0

' Data for Style 3: L Left
DATA 0,1,1,1
DATA 0,1,0,0

' Data for Style 4: Z Right
DATA 1,1,0,0
DATA 0,1,1,0

' Data for Style 5: Z Left
DATA 0,1,1,0
DATA 1,1,0,0

' Data for Style 6: T
DATA 1,1,1,0
DATA 0,1,0,0

' Data for Style 7: Square
DATA 0,1,1,0
DATA 0,1,1,0


ScreenError:                            ' QBlocks uses this error handler to determine the highest available video mode.
    BadMode = TRUE
    RESUME NEXT

'----------------------------------------------------------------------------
' AddBlockToWell
'
'    After a shape stops falling, put it into the WellBlocks array
'    so later falling shapes know where to stop.
'
'           PARAMETERS:    None.
'----------------------------------------------------------------------------
SUB AddBlockToWell
   
    FOR i = 0 TO XMATRIX                                    ' Loop through all elements in the array.
        FOR j = 0 TO YMATRIX
            IF BlockShape(i, j, CurBlock.Style) = 1 THEN    ' If there is a block in that space.
                SELECT CASE CurBlock.Rotation               ' Use the Rotation to determine how the blocks should map into the WellBlocks array.
                    CASE 0              ' No rotation.
                        WellBlocks(CurBlock.X + i, CurBlock.Y + j) = CurBlock.Style
                    CASE 1              ' Rotated 90 degrees clockwise.
                        WellBlocks(CurBlock.X - j + 2, CurBlock.Y + i - 1) = CurBlock.Style
                    CASE 2              ' Rotated 180 degrees.
                        WellBlocks(CurBlock.X - i + 3, CurBlock.Y - j + 1) = CurBlock.Style
                    CASE 3               ' Rotated 270 degrees clockwise.
                        WellBlocks(CurBlock.X + j + 1, CurBlock.Y - i + 2) = CurBlock.Style
                END SELECT
            END IF
        NEXT j
    NEXT i
END SUB

'----------------------------------------------------------------------------
' Center
'
'    Centers a string of text on a specified row.
'
'           PARAMETERS:    Text$ - Text to display on the screen.
'                          Row   - Row on the screen where the text$ is
'                                  displayed.
'----------------------------------------------------------------------------
SUB Center (text$, Row)

  LOCATE Row, (ScreenWidth - LEN(text$)) \ 2 + 1
  PRINT text$;

END SUB

'----------------------------------------------------------------------------
' CheckFit
'
'    Checks to see if the shape will fit into its new position.
'    Returns TRUE if it fits and FALSE if it does not fit.
'
'           PARAMETERS:    None
'
'----------------------------------------------------------------------------
FUNCTION CheckFit

    CheckFit = TRUE                     ' Assume the shape will fit.
   
    FOR i = 0 TO XMATRIX                ' Loop through all the blocks in the
        FOR j = 0 TO YMATRIX            ' shape and see if any would
                                        ' overlap blocks already in the well.
            IF BlockShape(i, j, CurBlock.Style) = 1 THEN    ' 1 means that space, within the falling shape, is filled with a block.
                SELECT CASE CurBlock.Rotation               ' Base the check on the rotation of the shape.
                    CASE 0                         ' No rotation.
                        NewX = CurBlock.X + i
                        NewY = CurBlock.Y + j
                    CASE 1                         ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
                        NewX = CurBlock.X - j + 2
                        NewY = CurBlock.Y + i - 1
                    CASE 2                         ' Rotated 180 degrees.
                        NewX = CurBlock.X - i + 3
                        NewY = CurBlock.Y - j + 1
                    CASE 3                         ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
                        NewX = CurBlock.X + j + 1
                        NewY = CurBlock.Y - i + 2
                END SELECT

                ' Set CheckFit to false if the block would be out of the well.
                IF (NewX > WELLWIDTH - 1 OR NewX < 0 OR NewY > WELLHEIGHT - 1 OR NewY < 0) THEN
                    CheckFit = FALSE
                    EXIT FUNCTION

                ' Otherwise, set CheckFit to false if the block overlaps
                ' an existing block.
                ELSEIF WellBlocks(NewX, NewY) THEN
                    CheckFit = FALSE
                    EXIT FUNCTION
                END IF

            END IF
        NEXT j
    NEXT i

END FUNCTION

'----------------------------------------------------------------------------
' CheckForFullRows
'
'    Checks for filled rows.  If a row is filled, delete it and move
'    the blocks above down to fill the deleted row.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB CheckForFullRows

    DIM RowsToDelete(WELLHEIGHT)    ' Temporary array to track rows that should be deleted.
    NumRowsToDelete = 0
    i = WELLHEIGHT                  ' Begin scanning from the bottom up.
    DO
        DeleteRow = TRUE            ' Assume the row should be deleted.
        j = 0
        DO                          ' Scan within each row for blocks.
            DeleteRow = DeleteRow * SGN(WellBlocks(j, i)) ' If any position is blank, DeleteRow is 0 (FALSE).
            j = j + 1
        LOOP WHILE DeleteRow = TRUE AND j < WELLWIDTH
       
        IF DeleteRow = TRUE THEN
            ' Walk up the rows and copy them down in the WellBlocks array.
            NumRowsToDelete = NumRowsToDelete + 1   ' Number of rows to delete.
            RowsToDelete(i - NumDeleted) = TRUE     ' Mark the rows to be deleted, compensating for rows that have already been deleted below it.
            NumDeleted = NumDeleted + 1             ' Compensates for rows that have been deleted already.
           
            ' Logically delete the row by moving all WellBlocks values down.
            FOR Row = i TO 1 STEP -1
                FOR Col = 0 TO WELLWIDTH
                    WellBlocks(Col, Row) = WellBlocks(Col, Row - 1)
                NEXT Col
            NEXT Row
        ELSE                        ' This row will not be deleted.
            i = i - 1
        END IF
    LOOP WHILE i >= 1                ' Stop looping when the top of the well is reached.
           
    IF NumRowsToDelete > 0 THEN
        Score = Score + 100 * NumRowsToDelete  ' Give 100 points for every row.
       
        ' Set Highest and Lowest such that any deleted row will initially set them.
        Highest = -1
        Lowest = 100
       
        ' Find where the highest and lowest rows to delete are.
        FOR i = WELLHEIGHT TO 1 STEP -1
            IF RowsToDelete(i) = TRUE THEN
                IF i > Highest THEN Highest = i
                IF i < Lowest THEN Lowest = i
            END IF
        NEXT i
        
        IF (Highest - Lowest) + 1 = NumRowsToDelete THEN    ' Only one contiguous group of rows to delete.
            DeleteChunk Highest, Lowest
        ELSE                                                ' Two groups of rows to delete.
            ' Begin at Lowest and scan down for a row NOT to be deleted.
            ' Then delete everything from Lowest to the row not to be deleted.
            i = Lowest
            DO WHILE i <= Highest
                IF RowsToDelete(i) = FALSE THEN
                    DeleteChunk i - 1, Lowest
                    EXIT DO
                ELSE
                    i = i + 1
                END IF
            LOOP
           
            ' Now look for the second group and delete those rows.
            Lowest = i
            DO WHILE RowsToDelete(Lowest) = FALSE
                Lowest = Lowest + 1
            LOOP
            DeleteChunk Highest, Lowest
       
        END IF
    END IF

END SUB

'----------------------------------------------------------------------------
' DeleteChunk
'
'    Deletes a group of one or more rows.
'
'           PARAMETERS:    Highest - Highest row to delete (physically lowest
'                                    on screen).
'                          Lowest  - Lowest row to delete (physically highest
'                                    on screen).
'----------------------------------------------------------------------------
SUB DeleteChunk (Highest, Lowest)
   
    ' GET the image of the row to delete.                           
    GET (WELLX, Lowest * YSIZE + WELLY)-(WELLX + WELLWIDTH * XSIZE, (Highest + 1) * YSIZE + WELLY - 1), Temp
    PLAY PLAYCLEARROW
   
    ' Flash the rows 3 times.
    FOR Flash = 1 TO 3
        PUT (WELLX, Lowest * YSIZE + WELLY), Temp, PRESET
        DelayTime! = TIMER + .02
        DO WHILE TIMER < DelayTime!: LOOP
        PUT (WELLX, Lowest * YSIZE + WELLY), Temp, PSET
        DelayTime! = TIMER + .02
        DO WHILE TIMER < DelayTime!: LOOP
    NEXT Flash
   
    ' Move all the rows above the deleted ones down.
    GET (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, Lowest * YSIZE + WELLY), Temp
    PUT (WELLX, (Highest - Lowest + 1) * YSIZE + WELLY), Temp, PSET
    'Erase the area above the block which just moved down.
    LINE (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, WELLY + (Highest - Lowest + 1) * YSIZE), WellColor, BF
END SUB

'----------------------------------------------------------------------------
' DisplayChanges
'
'    Displays list of changes that the player can easily make.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayChanges

    DisplayGameTitle                            ' Print game title.
    
    COLOR 7
    Center "The following game characteristics can be easily changed from", 5
    Center "within the QuickBASIC Interpreter.  To change the values of  ", 6
    Center "these characteristics, locate the corresponding CONST or DATA", 7
    Center "statements in the source code and change their values, then  ", 8
    Center "restart the program (press Shift + F5).                      ", 9

    COLOR 15
    Center "Block shapes                         ", 11
    Center "Block rotation                       ", 12
    Center "Number of different block shapes     ", 13
    Center "Score needed to advance to next level", 14
    Center "Width of the game well               ", 15
    Center "Height of the game well              ", 16
    Center "Songs played during game             ", 17

    COLOR 7
    Center "The CONST statements and instructions on changing them are   ", 19
    Center "located at the beginning of the main program.                ", 20

    DO WHILE INKEY$ = "": LOOP                  ' Wait for any key to be pressed.
    CLS                                         ' Clear screen.

END SUB

'----------------------------------------------------------------------------
' DisplayGameTitle
'
'    Displays title of the game.
'
'           PARAMETERS:    None.
'----------------------------------------------------------------------------
SUB DisplayGameTitle

    SCREEN 0
    WIDTH 80, 25                                  ' Set width to 80, height to 25.
    COLOR 4, 0                                    ' Set colors for red on black.
    CLS                                           ' Clear the screen.
    ScreenWidth = 80                              ' Set screen width variable to match current width.

    ' Draw outline around screen with extended ASCII characters.
    LOCATE 1, 2
    PRINT CHR$(201); STRING$(76, 205); CHR$(187);
    FOR i% = 2 TO 24
        LOCATE i%, 2
        PRINT CHR$(186); TAB(79); CHR$(186);
    NEXT i%
    LOCATE 25, 2
    PRINT CHR$(200); STRING$(76, 205); CHR$(188);

    'Print game title centered at top of screen
    COLOR 0, 4
    Center "      Microsoft      ", 1
    Center "    Q B L O C K S    ", 2
    Center "   Press any key to continue   ", 25  ' Center prompt on line 25.
    COLOR 7, 0

END SUB

'----------------------------------------------------------------------------
' DisplayIntro
'
'    Explains the object of the game and how to play.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayIntro
    
    CLS
    DisplayGameTitle
   
    Center "QBlocks challenges you to keep the well from filling.  Do this by", 5
    Center "completely filling rows with blocks, making the rows disappear.  ", 6
    Center "Move and rotate the falling shapes to get them into the best     ", 7
    Center "position.  The game will get faster as you score more points.    ", 8

    COLOR 4                                 ' Change foreground color for line to red.
    Center STRING$(74, 196), 11             ' Put horizontal red line on screen.
    COLOR 7     ' White (7) letters.        ' Change foreground color back to white
    Center " Game Controls ", 11            ' Display game controls.
    Center "     General                             Block Control      ", 13
    Center "                                     (Rotate)", 15
    Center "   P - Pause                                 " + CHR$(24) + " (or 5)   ", 16
    Center "      Q - Quit                         (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)   ", 17
    Center "                                    " + CHR$(25), 18
    Center "                                          (Drop)      ", 19
    
    DO                                      ' Wait for any key to be pressed.
        kbd$ = UCASE$(INKEY$)
    LOOP WHILE kbd$ = ""
    IF kbd$ = "Q" THEN                      'Allow player to quit now
        CLS
        LOCATE 10, 30: PRINT "Really quit? (Y/N)";
        DO
            kbd$ = UCASE$(INKEY$)
        LOOP WHILE kbd$ = ""
        IF kbd$ = "Y" THEN
            CLS
            END
        END IF
    END IF

END SUB

'----------------------------------------------------------------------------
' DrawAllShapes
'
'    Quickly draws all shapes in all four rotations.  Uses GET
'    to store the images so they can be PUT onto the screen
'    later very quickly.
'
'           PARAMETERS:    None.
'----------------------------------------------------------------------------
SUB DrawAllShapes

    DIM b AS BlockType
    SCREEN ScreenMode                   ' Set the appropriate screen mode.
   
    ' On EGA and VGA systems, appear to blank the screen.
    IF ScreenMode = 7 THEN
        DIM Colors(0 TO 15)             ' DIM an array of 16 elements.  By default, all elements are 0.
        PALETTE USING Colors            ' Redefine the colors all to 0.
        FOR i = 1 TO NUMSTYLES          ' Set block colors EGA, VGA
            BlockColor(i) = ((i - 1) MOD 7) + 1
        NEXT i
    ELSE
        FOR i = 1 TO NUMSTYLES          'Set block colors for CGA
            BlockColor(i) = ((i - 1) MOD 3) + 1
        NEXT i
    END IF

    CLS
    Count = 0                           ' Count determines how many shapes have been drawn on the screen and vertically where.
    FOR shape = 1 TO NUMSTYLES          ' Loop through all shapes.

        RtSide = 4
        DO
            IF BlockShape(RtSide - 1, 0, shape) = 1 OR BlockShape(RtSide - 1, 1, shape) = 1 THEN EXIT DO
            RtSide = RtSide - 1
        LOOP UNTIL RtSide = 1

        LtSide = 0
        DO
            IF BlockShape(LtSide, 0, shape) = 1 OR BlockShape(LtSide, 1, shape) = 1 THEN EXIT DO
            LtSide = LtSide + 1
        LOOP UNTIL LtSide = 3

        FOR Rotation = 0 TO 3           ' Loop through all rotations.
            b.X = Rotation * 4 + 2      ' Determine where to put the shape.
            b.Y = Count + 2
            b.Rotation = Rotation
            b.Style = shape
            Show b                      ' Draw the shape.
           
            X = b.X: Y = b.Y
            SELECT CASE Rotation        ' Based on Rotation, determine where the shape really is on the screen.
                CASE 0                  ' No rotation.
                    x1 = X: x2 = X + RtSide: y1 = Y: y2 = Y + 2
                CASE 1                  ' Rotated 90 degrees clockwise.
                    x1 = X + 1: x2 = X + 3: y1 = Y - 1: y2 = Y + RtSide - 1
                CASE 2                  ' 180 degrees.
                    x1 = X: x2 = X + 4 - LtSide: y1 = Y: y2 = Y + 2
                CASE 3                  ' Rotated 270 degrees clockwise.
                    x1 = X + 1: x2 = X + 3: y1 = Y - 1: y2 = Y + 3 - LtSide
            END SELECT
           
            ' Store the image of the rotated shape into an array for fast recall later.
            GET (x1 * XSIZE, y1 * YSIZE)-(x2 * XSIZE, y2 * YSIZE), BlockImage(((shape - 1) * 4 + Rotation) * ELEMENTSPERBLOCK)
       
        NEXT Rotation
       
        Count = Count + 5               ' Increase Count by 5 to leave at least one blank line between shapes.
        IF Count = 20 THEN              ' No space for any more shapes.
            CLS
            Count = 0
        END IF
   
    NEXT shape
   
    CLS
   
    ' Changes the color palette if SCREEN is used.
    IF ScreenMode = 7 THEN
        PALETTE                         ' Restore default color settings.
        PALETTE 6, 14                   ' Make brown (6) look like yellow (14).
        PALETTE 14, 15                  ' Make yellow (14) look like bright white (15).
    END IF

END SUB

'----------------------------------------------------------------------------
' DrawBlock
'
'    Draws one block of a QBlocks shape.
'
'           PARAMETERS:    X         - Horizontal screen location.
'                          Y         - Vertical screen location.
'                          FillColor - The primary color of the block.
'                                      The top and left edges will be the
'                                      brighter shade of that color.
'----------------------------------------------------------------------------
SUB DrawBlock (X, Y, FillColor)

    LINE (X * XSIZE + 2, Y * YSIZE + 2)-((X + 1) * XSIZE - 2, (Y + 1) * YSIZE - 2), FillColor, BF
    LINE (X * XSIZE + 1, Y * YSIZE + 1)-((X + 1) * XSIZE - 1, Y * YSIZE + 1), FillColor + 8
    LINE (X * XSIZE + 1, Y * YSIZE + 1)-(X * XSIZE + 1, (Y + 1) * YSIZE - 1), FillColor + 8

END SUB

'----------------------------------------------------------------------------
' DrawPattern
'
'    Draws a background pattern that is 32 pixels wide by 20 pixels
'    deep.  Gets the pattern and duplicates it to fill the screen.
'
'           PARAMETERS:    Pattern - Which of the 10 available patterns to
'                                    draw.
'----------------------------------------------------------------------------
SUB DrawPattern (Pattern)

    CLS
    X = 1: Y = 1
    DIM Temp2(215) AS INTEGER           ' Create an array to store the image.
   
    ' Draw the pattern specified.
    SELECT CASE Pattern
    CASE 0
        j = Y + 21
        FOR i = X TO X + 27 STEP 3
            j = j - 2
            LINE (i, j)-(i, Y + 19), 12, BF
        NEXT i
        LINE (X, Y)-(X + 30, Y + 19), 4, B
        LINE (X + 1, Y + 1)-(X + 31, Y + 18), 4, B
    CASE 1
        LINE (X, Y)-(X + 8, Y + 12), 1, BF
        LINE (X + 9, Y + 8)-(X + 24, Y + 20), 2, BF
        LINE (X + 25, Y)-(X + 32, Y + 12), 3, BF
    CASE 2
        LINE (X, Y)-(X + 29, Y + 18), X / 32 + 1, B
        LINE (X + 1, Y + 1)-(X + 28, Y + 17), X / 32 + 2, B
    CASE 3
        FOR i = 0 TO 9 STEP 2
            LINE (X + i, Y + i)-(X + 29 - i, Y + 18 - i), i, B
        NEXT i
    CASE 4
        j = 0
        FOR i = 1 TO 30 STEP 3
            LINE (X + i, Y + j)-(X + 30 - i, Y + j), i
            LINE (X + i, Y + 19 - j)-(X + 30 - i, Y + 19 - j), i
            j = j + 2
        NEXT i
    CASE 5
        LINE (X, Y)-(X + 29, Y + 4), 1, BF
        LINE (X, Y)-(X + 4, Y + 18), 1, BF
        LINE (X + 7, Y + 7)-(X + 29, Y + 11), 5, BF
        LINE (X + 7, Y + 7)-(X + 11, Y + 18), 5, BF
        LINE (X + 14, Y + 14)-(X + 29, Y + 18), 4, BF
    CASE 6
        LINE (X + 15, Y)-(X + 17, Y + 19), 1
        LINE (X, Y + 9)-(X + 31, Y + 11), 2
        LINE (X, Y + 1)-(X + 31, Y + 18), 9
        LINE (X + 30, Y)-(X + 1, Y + 19), 10
    CASE 7
        FOR i = 1 TO 6
            CIRCLE (X + 16, Y + 10), i, i
        NEXT i
    CASE 8
        FOR i = X TO X + 30 STEP 10
            CIRCLE (i, Y + 9), 10, Y / 20 + 1
        NEXT i
    CASE 9
        LINE (X + 1, Y)-(X + 1, Y + 18), 3
        LINE (X + 1, Y)-(X + 12, Y + 18), 3
        LINE (X + 1, Y + 18)-(X + 12, Y + 18), 3
        LINE (X + 30, Y)-(X + 30, Y + 18), 3
        LINE (X + 30, Y)-(X + 19, Y + 18), 3
        LINE (X + 30, Y + 18)-(X + 19, Y + 18), 3
        LINE (X + 4, Y)-(X + 26, Y), 1
        LINE (X + 4, Y)-(X + 15, Y + 18), 1
        LINE (X + 26, Y)-(X + 15, Y + 18), 1
    END SELECT
   
    GET (0, 0)-(31, 19), Temp2  ' GET the image.
   
    ' Duplicate the image 10 times across by 10 times down.
    FOR H = 0 TO 319 STEP 32
        FOR V = 0 TO 199 STEP 20
            PUT (H, V), Temp2, PSET
        NEXT V
    NEXT H

END SUB

'----------------------------------------------------------------------------
' DrawPlayingField
'
'    Draws the playing field, including the well, the title, the
'    score/level box, etc.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DrawPlayingField
   
    SELECT CASE ScreenMode               ' Choose the screen colors based on the current mode.
        CASE 7
            WellColor = WELLCOLOR7
            BorderColor = BORDERCOLOR7

        CASE ELSE                         ' Setup for SCREEN 1.
            WellColor = WELLCOLOR1
            BorderColor = BORDERCOLOR1
    END SELECT
   
    ScreenWidth = 40                      ' Set to proper width and colors.
   
    ' Draw the background pattern.
    DrawPattern Level
  
    ' Draw the well box.
    LINE (WELLX - 1, WELLY - 5)-(WELLX + WELLWIDTH * XSIZE + 1, WELLY + WELLHEIGHT * YSIZE + 1), WellColor, BF
    LINE (WELLX - 1, WELLY - 5)-(WELLX + WELLWIDTH * XSIZE + 1, WELLY + WELLHEIGHT * YSIZE + 1), BorderColor, B
   
    ' Draw the title box.
    LINE (XSIZE, WELLY - 5)-(XSIZE * 8, WELLY + 12), WellColor, BF
    LINE (XSIZE, WELLY - 5)-(XSIZE * 8, WELLY + 12), BorderColor, B
   
    ' Draw the scoring box.
    LINE (XSIZE, WELLY + 20)-(WELLX - 2 * XSIZE, 78), WellColor, BF
    LINE (XSIZE, WELLY + 20)-(WELLX - 2 * XSIZE, 78), BorderColor, B
                                         
    MakeInfoBox                     ' Draw the Information Box.

    COLOR 12
    LOCATE 3, 5: PRINT "QBLOCKS"     ' Center the program name on line 2.
    COLOR BorderColor

    ' Draw the scoring area.
    LOCATE 6, 4: PRINT "Score:";
    LOCATE 7, 4: PRINT USING "#,###,###"; Score
    LOCATE 9, 4: PRINT USING "Level: ##"; Level

END SUB

'----------------------------------------------------------------------------
' GameOver
'
'    Ends the game and asks the player if he/she wants to play
'    again.  GameOver returns TRUE if the player wishes to stop
'    or FALSE if the player wants another game.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
FUNCTION GameOver
   
    PLAY PLAYGAMEOVER                           ' Play the game over tune.
    MakeInfoBox

    DO WHILE INKEY$ <> "": LOOP                 ' Clear the keyboard buffer.

    ' Put Game Over messages into the InfoBox.
    LOCATE 14, 4: PRINT "Game Over"
    LOCATE 17, 6: PRINT "Play"
    LOCATE 18, 5: PRINT "again?"
    LOCATE 20, 6: PRINT "(Y/N)"
    
    ' Wait for the player to press either Y or N.
    DO
        a$ = UCASE$(INKEY$)                     ' UCASE$ assures that the key will be uppercase.  This eliminates the need to check for y and n in addition to Y and N.
    LOOP UNTIL a$ = "Y" OR a$ = "N"
   
    IF a$ = "Y" THEN                            ' If player selects "Y",
        GameOver = FALSE                        ' game is not over,
    ELSE                                        ' otherwise
        GameOver = TRUE                         ' the game is over.
    END IF
  
END FUNCTION

'----------------------------------------------------------------------------
' InitScreen
'
'    Draws the playing field and ask for the desired starting level.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB InitScreen

    DrawPlayingField                ' Draw playing field assuming Level 0.

    ' Prompt for starting level.
    COLOR 12                        ' Change foreground color to bright red.
    LOCATE 14, 5: PRINT "Select";
    LOCATE 16, 5: PRINT "start";
    LOCATE 18, 5: PRINT "level?";
    LOCATE 20, 5: PRINT "(0 - 9)";
    COLOR BorderColor               ' Restore the default text color to BorderColor (white).
    Level = TRUE                    ' Use level as flag as well as a real value.  Level remain TRUE if Q (Quit) is pressed instead of a level.
   
    ' Get a value for Level or accept a Q.
    DO
        a$ = UCASE$(INKEY$)
    LOOP WHILE (a$ > "9" OR a$ < "0") AND a$ <> "Q"
   
    IF a$ = "Q" THEN
        EXIT SUB
    ELSE
        Level = VAL(a$)
    END IF

    IF Level > 0 THEN DrawPlayingField    ' Draw new playing field because the background pattern depends on the level.
    RedrawControls      ' Draw the controls.

END SUB

'----------------------------------------------------------------------------
' MakeInfoBox
'
'    Draws the information box.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB MakeInfoBox

    LINE (WELLX - 9 * XSIZE, 90)-(WELLX - 2 * XSIZE, 185), WellColor, BF    ' Clear the Info area.
    LINE (WELLX - 9 * XSIZE, 90)-(WELLX - 2 * XSIZE, 185), BorderColor, B   ' Draw a border around it.

END SUB

'----------------------------------------------------------------------------
' NewBlock
'
'    Initializes a new falling shape about to enter the well.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB NewBlock

    CurBlock.Style = INT(RND(1) * NUMSTYLES) + 1    ' Randomly pick a block style.
    CurBlock.X = (WELLWIDTH \ 2) - 1                ' Put the new shape in the horizontal middle of the well
    CurBlock.Y = 0                                  ' and at the top of the well.
    CurBlock.Rotation = 0                           ' Begin with no rotation.

    PLAY PLAYNEWBLOCK

END SUB

'----------------------------------------------------------------------------
' PerformGame
'
'    Continues to play the game until the player quits.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB PerformGame

    DO                                          ' Loop for repetitive games
        a$ = ""
        ERASE WellBlocks                        ' Set all the elements in the WellBlocks array to 0.
        Score = 0                               ' Clear initial score.
        Level = 0                               ' Assume Level 0.
        PrevScore = BASESCORE - NEXTLEVEL       ' Set score needed to get to first level
        GameTiltScore = WINGAME                 ' Set the initial win game value.
       
        InitScreen                              ' Prepare the screen and get the difficulty level.
        IF Level = -1 THEN EXIT SUB             ' Player pressed Q instead of a level.
       
        TargetTime = TIMER + 1 / (Level + 1)    ' TargetTime is when the falling shape will move down again.
        DO                                      ' Create new falling shapes until the game is over.
            DoneWithThisBlock = FALSE           ' This falling shape is not done falling yet.
            NewBlock                            ' Create a new falling unit.
            IF CheckFit = FALSE THEN EXIT DO    ' If it does not fit, then the game is over.
            PutBlock CurBlock                   ' Display the new shape.
           
            DO                                  ' Continue dropping the falling shape.
                OldBlock = CurBlock             ' Save current falling shape for possible later use.
                DO                              ' Loop until enough time elapses.
                   
                    ValidEvent = TRUE           ' Assume a key was pressed.
                    ans$ = UCASE$(INKEY$)

                    IF ans$ = PAUSE OR ans$ = QUIT THEN
                        MakeInfoBox
                   
                        ' SELECT CASE will do different actions based on the
                        ' value of the SELECTED variable.
                        SELECT CASE ans$
                            CASE PAUSE
                                SOUND 1100, .75
                                LOCATE 16, 6: PRINT "GAME";
                                LOCATE 18, 5: PRINT "PAUSED";
                                DO WHILE INKEY$ = "": LOOP  ' Wait until another key is pressed.
                            CASE QUIT
                                ' Play sounds to tell the player that Q was pressed.
                                SOUND 1600, 1
                                SOUND 1000, .75
                               
                                ' Confirm that the player really wants to quit.
                                LOCATE 15, 5: PRINT "Really";
                                LOCATE 17, 6: PRINT "quit?";
                                LOCATE 19, 6: PRINT "(Y/N)";
                                DO
                                    a$ = UCASE$(INKEY$)
                                LOOP UNTIL a$ <> ""
                                IF a$ = "Y" THEN EXIT SUB
                        END SELECT
                        RedrawControls  ' Redraw controls if either Q or P is pressed.
                   
                    ELSE    ' A key was pressed but not Q or P.
                        ans = ASC(RIGHT$(CHR$(0) + ans$, 1))    ' Convert the key press to an ASCII code for faster processing.
                        SELECT CASE ans
                        CASE DOWNARROW, DOWNARROW2, SPACEBAR    ' Drop shape immediately.
                            DO                                  ' Loop to drop the falling unit one row at a time.
                                CurBlock.Y = CurBlock.Y + 1
                            LOOP WHILE CheckFit = TRUE          ' Keep looping while the falling unit isn't stopped.
                            CurBlock.Y = CurBlock.Y - 1         ' Went one down too far, restore to previous.
                            TargetTime = TIMER - 1              ' Ensure that the shape falls immediately.
                        CASE RIGHTARROW, RIGHTARROW2
                            CurBlock.X = CurBlock.X + 1         ' Move falling unit right.
                        CASE LEFTARROW, LEFTARROW2
                            CurBlock.X = CurBlock.X - 1         ' Move falling unit left.
                        CASE UPARROW, UPARROW2, UPARROW3
                            CurBlock.Rotation = ((CurBlock.Rotation + ROTATEDIR) MOD 4)  ' Rotate falling unit.
                        CASE ELSE
                            ValidEvent = FALSE
                    END SELECT

                    IF ValidEvent = TRUE THEN
                        IF CheckFit = TRUE THEN         ' If the move is valid and the shape fits in the new position,
                            PutBlock OldBlock           ' erase the shape from its old position
                            PutBlock CurBlock           ' and display it in the new position.
                            OldBlock = CurBlock
                        ELSE
                            CurBlock = OldBlock         ' If it does not fit then reset CurBlock to the OldBlock.
                        END IF
                    END IF
                END IF

                LOOP UNTIL TIMER >= TargetTime       ' Keep repeating the loop until it is time to drop the shape.  This allows many horizontal movements and rotations per vertical step.
               
                TargetTime = TIMER + 1 / (Level + 1) ' The player has less time between vertical movements as the skill level increases.
                CurBlock.Y = CurBlock.Y + 1          ' Try to drop the falling unit one row.

                IF CheckFit = FALSE THEN             ' Cannot fall any more.
                    DoneWithThisBlock = TRUE         ' Done with this block.
                    CurBlock = OldBlock
                END IF
               
                PutBlock OldBlock                    ' Erase the falling shape from the old position,
                PutBlock CurBlock                    ' and display it in the new position.
                OldBlock = CurBlock

            LOOP UNTIL DoneWithThisBlock             ' Continue getting keys and moving shapes until the falling shape stops.
           
            AddBlockToWell                           ' Shape has stopped so logically add it to the well.
            CheckForFullRows                         ' Check to see if a row(s) is now full.  If so, deletes it.
            UpdateScoring                            ' Use the UpdateScoring subprogram to add to the score.

            IF Score >= GameTiltScore THEN           ' See if the score has hit the tilt score.

                PLAY PLAYWINGAME
                MakeInfoBox
                LOCATE 13, 5: PRINT USING "#######"; Score
                PLAY PLAYWINGAME

                IF GameTiltScore = TILTVALUE THEN    ' If the player has tilted the game.
                    LOCATE 15, 4: PRINT "GAME TILT"
                    LOCATE 17, 5: PRINT "You are"
                    LOCATE 18, 4: PRINT "Awesome!"
                    LOCATE 20, 4: PRINT "Press any"
                    LOCATE 21, 6: PRINT "key..."
                    PLAY PLAYWINGAME
                    DO WHILE INKEY$ = "": LOOP
                    EXIT SUB
                ELSE                                 ' If they just met the WINGAME value.
                    LOCATE 15, 4: PRINT "YOU WON!"
                    LOCATE 17, 5: PRINT "Want to"
                    LOCATE 18, 4: PRINT "continue"
                    LOCATE 20, 6: PRINT "(Y/N)"

                    DO                               ' DO loop to wait for the player to press anything.
                        a$ = UCASE$(INKEY$)          ' The UCASE$ function assures that a$ always has an uppercase letter in it.
                    LOOP UNTIL a$ <> ""
        
                    IF a$ <> "Y" THEN EXIT DO        ' Exit this main loop if the player pressed anything but Y.

                    GameTiltScore = TILTVALUE        ' Reset to the tilt value.

                    RedrawControls
                END IF
            END IF

        LOOP                                         ' Unconditional loop.  Each game is stopped by the EXIT DO command at the top of this loop that executes when a new block will not fit in the well.
    LOOP UNTIL GameOver                              ' GameOver is always TRUE (-1) unless the user presses X or the well is full.

END SUB

'----------------------------------------------------------------------------
' PutBlock
'
'    Uses very fast graphics PUT command to draw the shape.
'
'           PARAMETERS:    B - Block to be put onto the screen.
'----------------------------------------------------------------------------
SUB PutBlock (b AS BlockType)
   
    SELECT CASE b.Rotation          ' Base exact placement on the rotation.
        CASE 0                      ' No rotation.
            x1 = b.X: y1 = b.Y
        CASE 1                      ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
            x1 = b.X + 1: y1 = b.Y - 1
        CASE 2                      ' Rotated 180 degrees.
            x1 = b.X: y1 = b.Y
        CASE 3                      ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
            x1 = b.X + 1: y1 = b.Y - 1
    END SELECT
   
    ' Actually PUT the rotated shape on the screen.  The XOR option makes the
    ' new image blend with whatever used to be there in such a way that
    ' identical colors cancel each other out.  Therefore, one PUT with the XOR
    ' option can draw an object while the second PUT to that same location
    ' erases it without affecting anything else near it.  Often used for animation.

    PUT (x1 * XSIZE + WELLX, y1 * YSIZE + WELLY), BlockImage(((b.Style - 1) * 4 + b.Rotation) * ELEMENTSPERBLOCK), XOR  ' XOR mixes what used to be there on the screen with the new image.  Two identical colors cancel each other.

END SUB

'----------------------------------------------------------------------------
' RedrawControls
'
'    Puts control keys information into the information box.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB RedrawControls
  
    ' Draw the InfoBox and erase anything that used to be in it.
    MakeInfoBox

    ' Print the key assignments within the Info Box.
    COLOR BorderColor
    LOCATE 13, 4: PRINT "Controls"
    LOCATE 14, 4: PRINT "--------"
    LOCATE 15, 4: PRINT CHR$(24) + " = Turn"
    LOCATE 16, 4: PRINT CHR$(27) + " = Left"
    LOCATE 17, 4: PRINT CHR$(26) + " = Right"
    LOCATE 18, 4: PRINT CHR$(25) + " = Drop"
    LOCATE 20, 4: PRINT "P = Pause"
    LOCATE 21, 4: PRINT "Q = Quit"

END SUB

'----------------------------------------------------------------------------
' Show
'
'    Draws the falling shape one block at a time.  Only used by
'    DisplayAllShapes.  After that, PutBlock draws all falling
'    shapes.
'
'           PARAMETERS:    B - Block to be put onto the screen.
'----------------------------------------------------------------------------
SUB Show (b AS BlockType)
                                                 
    ' Loop through all possible block locations.
    FOR i = 0 TO XMATRIX
        FOR j = 0 TO YMATRIX
           
            IF BlockShape(i, j, b.Style) = 1 THEN   ' 1 means there is a block there.
                 SELECT CASE b.Rotation             ' Exact screen position is determined by the rotation.
                    CASE 0                          ' No rotation.
                        DrawBlock b.X + i, b.Y + j, BlockColor(b.Style)
                    CASE 1                          ' Rotated 90 degrees clockwise, or 270 degrees counterclockwise.
                        DrawBlock b.X - j + 2, b.Y - 1 + i, BlockColor(b.Style)
                    CASE 2                          ' Rotated 180 degrees.
                        DrawBlock b.X + 3 - i, b.Y - j + 1, BlockColor(b.Style)
                    CASE 3                          ' Rotated 270 degrees clockwise, or 90 degrees counterclockwise.
                        DrawBlock b.X + j + 1, b.Y - i + 2, BlockColor(b.Style)
                END SELECT
            END IF
        NEXT j
    NEXT i

END SUB

'---------------------------------------------------------------------------
' UpdateScoring
'
'    Puts the new score on the screen.  Checks if the new score forces
'    a new level.  If so, change the background pattern to match the
'    new level.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB UpdateScoring
   
    ' Increase the level if the score is high enough and the level is not
    ' maximum already.
    IF Level < 9 AND Score >= (NEXTLEVEL * (Level + 1) + PrevScore) THEN
   
        ' Store the entire well image to quickly PUT it back after the
        ' background changes.
        GET (WELLX, WELLY)-(WELLX + WELLWIDTH * XSIZE, WELLY + WELLHEIGHT * YSIZE), Temp
       
        PrevScore = Score           ' Save previous Score for next level.
        Level = Level + 1
        DrawPlayingField            ' Draw playing field again, this time with the new background pattern.
        PUT (WELLX, WELLY), Temp    ' Restore the image of the old well.
   
        RedrawControls              ' Show the controls again.
    END IF

    LOCATE 7, 4: PRINT USING "#,###,###"; Score   ' Print the score and level.
    
END SUB

