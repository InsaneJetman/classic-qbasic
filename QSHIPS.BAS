'
'                                QSHIPS.BAS
'
'         Copyright (C) 1990 Microsoft Corporation. All Rights Reserved.
'
' QShips puts two players in a ship-to-ship cannon duel.  Adjust your cannon
' fire to the correct angle and velocity to sink your opponent.  An island,
' the shifting wind, and a moving opponent make each shot a new challenge.
'
' To run this game, press Shift+F5.
'
' To exit this program, press Alt, F, X.
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
' statements below these comments can be modified to change the following:
'    Size of the island
'    Number of trees on the island
'    Strength of gravity that affects your shots
'    Default number of hits needed to win the game.
'    Maximum velocity your cannons can fire.
'    Sounds of the cannon firing and the explosions
'
' On the right side of each CONST statement, there is a comment that tells
' you what it does and how big or small you can set the value.
'
' On your own, you can also add exciting sound and visual effects or make
' any other changes that your imagination can dream up.  By reading the
' Learn BASIC Now book, you'll learn the techniques that will enable you
' to fully customize this game and to create games of your own.
'
' If the game won't run after you have changed it, you can exit without
' saving your changes by pressing Alt, F, X and choosing NO.
'
' If you do want to save your changes, press Alt, F, A and enter a filename
' for saving your version of the program.  Before saving your changes,
' however, make sure the program still works by running the program and
' verifying that your changes produce the desired results.  Also, always
' be sure to keep a backup of the original program.
'
DEFINT A-Z

CONST ISLLENGTH = 80               ' Length of the island.  Range is 40 to 240.  If you make ISLLENGTH too large there will not be any water for your ships!
CONST NUMTREES = 3                 ' Number of trees on the island.  Range 0 to 10.  If you make this large (or make ISLLENGTH small) there may not be enough room on the island for all the trees.
CONST GRAVITY = 9.8                ' Gravity. Range is 1.0 to 50.0. The higher or lower you go, the more difficult to hit your opponent.
CONST INITNUMGAMES = 3             ' Default number of hits needed to win the game.  Range from 1 to 99.
CONST MAXVELOCITY = 150            ' Maximum velocity of cannon.  Range 50 to 300 for best results.  You may need to increase this if you have also increased GRAVITY above 9.8.
' The following sound constants are used by the PLAY command to
' produce music during the game.  To change the sounds you hear, change
' these constants.  Refer to the online help for PLAY for the correct format.
' To completely remove sound from the game set the constants equal to null.
' For example:  INTROSOUND = ""
CONST INTROSOUND = "T160O1L8CDEDCDL4ECC"          ' Sound played at the start of the game.
CONST CANNONFIRESOUND = "MBo0L32A-L64CL16BL64A+"  ' Sound made when a cannon is fired.
CONST CANNONHITSOUND = "MBO0L32EFGEFDC"           ' Sound made when a cannon ball hits an object.
CONST SHIPEXPLOSIONSOUND = "MBO0L16EFGEFDC"       ' Sound made when a ship is exploded.

' The following are general constants and their values should not be changed.
CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST SHOOTSELF = 1             ' Used to indicate that a player shot him/her self.
CONST TREEWIDTH = 16            ' Width of the tree picture (do NOT change).
CONST SHIPWIDTH = 16            ' Width of the Ship picture (do NOT change).
CONST MOVESHIPBY = 16           ' Distance to move the ship per key press.
CONST UP = 0 + 72               ' Two bytes representing Up arrow key. Elevate cannon by 1 degree.
CONST DOWN = 0 + 80             ' Two bytes representing Down arrow key. Depress cannon by 1.
CONST LEFT = 0 + 75             ' Two bytes representing Left arrow key. Move ship left / speed change.
CONST RIGHT = 0 + 77            ' Two bytes representing Right arrow key. Move ship right / speed change.
CONST ENTERKEY = 13             ' Single byte ASCII code for Carriage Return (Enter key). Confirm selections.
CONST BACKGROUNDCOLOR = 0       ' Black.
CONST WATERCOLOR = 1            ' Palette 1, color 1 cyan.
CONST ISLANDCOLOR = 2           ' Palette 1, color 2 purple.
CONST OBJECTCOLOR = 3           ' Palette 1, color 3 white.

'Declarations of all the FUNCTION and SUB procedures called in this program.
DECLARE FUNCTION MoveShip (PlayerNum, SeaLevel())
DECLARE FUNCTION SinkShip (x, y)
DECLARE FUNCTION PlotShot (startX, startY, angle#, velocity)
DECLARE FUNCTION GetPlayerCommand (PlayerNum, SeaLevel())
DECLARE SUB Center (text$, row)
DECLARE SUB ClearArea (startRow, startCol, endRow, endCol)
DECLARE SUB CyclePalette ()
DECLARE SUB DisplayChanges ()
DECLARE SUB DisplayGameTitle ()
DECLARE SUB DisplayIntro ()
DECLARE SUB DrawIsland (SeaLevel())
DECLARE SUB DrawWaves (offset, hmult, tmult, SeaLevel())
DECLARE SUB DrawWind ()
DECLARE SUB GetGameOptions ()
DECLARE SUB GetShotParams (PlayerNum, NewAngle#, NewVelocity)
DECLARE SUB InitializeVariables ()
DECLARE SUB MakeBattleField (SeaLevel())
DECLARE SUB PlaceShips (SeaLevel())
DECLARE SUB PlayGame ()
DECLARE SUB PlotAngle (col, angle#, PlayerNum)
DECLARE SUB PlotBattleField (SeaLevel())
DECLARE SUB PlotVelocity (col, velocity, PlayerNum)
DECLARE SUB CannonHit (x, y, theColor)
DECLARE SUB UpdateScores (Record(), PlayerNum, Results)

' SHARED (global) variable declarations for use in this program.
DIM SHARED ShipX(1 TO 2) AS INTEGER       ' x coordinate for ships.
DIM SHARED ShipY(1 TO 2) AS INTEGER       ' y coordinate for ships.
DIM SHARED TotalShots(1 TO 2) AS INTEGER  ' Total shots fired for each player.
DIM SHARED TotalWins(1 TO 2) AS INTEGER   ' Total points for each player.
DIM SHARED TheAngle#(1 TO 2)              ' Angle each player used in last shot.
DIM SHARED TheVelocity(1 TO 2) AS INTEGER ' Velocity each player used in last shot.
DIM SHARED Player$(1 TO 2)                ' Player name of each player.
DIM SHARED Pi#                            ' Pi (3.1415....) used in calculations
DIM SHARED TreePic1(1 TO 20) AS INTEGER   ' Holds the bottom of the palm tree picture.
DIM SHARED TreePic2(1 TO 36) AS INTEGER   ' Holds the top of the palm tree picture.
DIM SHARED ShipPic(1 TO 36) AS INTEGER    ' Holds the ship picture.
DIM SHARED Shot&(1 TO 2)                  ' Holds the cannon ball picture.
DIM SHARED ScreenHeight AS INTEGER        ' Screen height in pixels.
DIM SHARED ScreenWidth AS INTEGER         ' Screen width in pixels.
DIM SHARED NumGames AS INTEGER            ' Number of games to play.
DIM SHARED Wind AS INTEGER                ' Wind speed; used in shot calculations.
DIM SHARED ScreenMode AS INTEGER          ' BASIC screen ScreenMode number.
DIM SHARED MaxCol AS INTEGER              ' Screen maximum number of columns.
DIM SHARED IStart AS INTEGER              ' Island starting x coordinate.
DIM SHARED IEnd AS INTEGER                ' Island ending x coordinate.
DIM SHARED Delay AS INTEGER               ' Delay factor for explosions
' Module-level variables (not known in procedures)
DIM KeyFlags  AS INTEGER
DIM BadMode AS INTEGER

' The values below are loaded into arrays, then used in graphics PUT
' statement to display pictures of ships, trees, cannon shots.
' DO NOT CHANGE THE VALUE OF THIS DATA
ShipPicData:
DATA 32,16,0,192,0,192,768,192,3840,192,16128,195
DATA -256,195,-253,-16189,-241,-3901,-193,-829,-1,-61,0,195
DATA 0,195,-21846,-21846,-21926,-23126,-21931,21930,23125,21925,0,0

TreePic1Data:
DATA 16,16,-32760,-32760,-32758,-32760,-32760,-32758,-32760,-32760
DATA -32758,-32760,-32760,-32758,-24024,-30584,8226,-30072,0,0

TreePic2Data:
DATA 32,16,0,2,0,-32758,0,10272,-32766,2720,-24566,640
DATA 10792,-32766,10912,-24566,2688,10280,2176,10400,512,2688,2562,512
DATA -30198,522,-24024,-32598,10784,-24416,2208,10368,2176,640,0,0

ShotData:
DATA 196614,3210288&


' The module-level code of the QSHIPS program begins here!

' Use error trap to test for Screen Mode 1 (320x200 graphics, 40-column text)
ON ERROR GOTO ScreenError               ' If mode 1 not available, BadMode
BadMode = FALSE                         ' is set TRUE in error handler. Other-
ScreenMode = 1                          ' wise, screen mode 1 is set.
SCREEN ScreenMode                       ' Attempt to go into SCREEN 1.

ON ERROR GOTO 0                         ' Turn off error trapping for now.

IF BadMode = TRUE THEN                  ' If mode 1 wasn't found...
	CLS
	LOCATE 11, 13
	PRINT "CGA, EGA Color, or VGA graphics required to run QSHIPS.BAS"
ELSE                                    ' Make sure NUM LOCK isn't on.
	DEF SEG = 0                         ' Set segment to low memory
	KeyFlags = PEEK(1047)               ' Check address of NUM LOCK status
	IF KeyFlags AND 32 THEN             ' If it was turned on,
		POKE 1047, KeyFlags AND 223     ' Turn it off
	END IF
	DEF SEG                             ' Reset segment to DGROUP (default data segment)

	DisplayIntro                ' Display game rules.
	GetGameOptions              ' Get player's names and length of game.
	InitializeVariables         ' Initialize starting variables.
	PlayGame
	DisplayChanges              ' Reset normal screen mode and end.

	IF KeyFlags AND 32 THEN     ' Restore the previous flag settings.
		DEF SEG = 0
		POKE 1047, KeyFlags OR 32
		DEF SEG
	END IF
END IF

END


ScreenError:                ' Screen mode error handler starts here.
	BadMode = TRUE          ' Set the flag indicating there was an error.
	RESUME NEXT             ' Ignore the error, by executing next statement.

'--------------------------------------------------------------------------
' CannonHit
'
'    A cannon shot has hit the water, island, or a ship.  What has been
'    hit is determined;  if the shot hit a solid object there is a small
'    explosion, then water / debris is thrown into the air.
'
'             PARAMETERS:   x        - x coordinate of the hit
'                           y        - y coordinate of the hit
'                           theColor - Indicates what has been hit
'--------------------------------------------------------------------------
SUB CannonHit (x, y, theColor)

	fragments = 11                      ' Assume shell hit water - splash.
	SELECT CASE Delay                   ' Base number of cycles in explosion
		CASE 500: cycles = 8            '   on machine speed
		CASE 200: cycles = 5
		CASE 50: cycles = 3
	END SELECT
	offset = ScreenHeight / 10

	IF theColor <> WATERCOLOR THEN      ' If shell hit solid object - explode.
		PLAY CANNONHITSOUND
		radius = ScreenHeight / 70      ' Set up and create explosion illusion.
		increment# = 1.2
		FOR Counter# = 0 TO radius STEP increment#
			CIRCLE (x, y), Counter#, ISLANDCOLOR
		NEXT Counter#
		FOR Counter# = radius TO 0 STEP (-1 * increment#) ' Repaint with the
			CIRCLE (x, y), Counter#, BACKGROUNDCOLOR    ' object now missing.
		NEXT Counter#
	END IF

	'Throw water or debris into the air.
	DIM xpos(1 TO fragments), ypos(1 TO fragments)
	radius# = .5
	PLAY CANNONHITSOUND
	FOR j = -3 TO 3 STEP 3
		LINE (x, y)-(x + j, y - offset), theColor   ' Create debris
	NEXT j

	' water / debris flies around for a short time
	FOR kt = 1 TO cycles
		FOR i = 1 TO fragments
			xpos(i) = x + (((10 * RND) - 5) / 5) * (offset / 2) + (2 * kt) / cycles
			ypos(i) = y - (RND + 1) * offset + (3 * kt) / cycles
		NEXT i
		FOR i = 1 TO fragments
			CIRCLE (xpos(i), ypos(i)), radius#, theColor
		NEXT i
		FOR j = -3 TO 3 STEP 3
			LINE (x, y)-(x + j, y - offset), BACKGROUNDCOLOR
		NEXT j
		FOR i = 1 TO fragments
			CIRCLE (xpos(i), ypos(i)), radius#, BACKGROUNDCOLOR
		NEXT i
	NEXT kt

END SUB

'----------------------------------------------------------------------------
' Center
'
'    Centers the given text string on the indicated row.
'
'             PARAMETERS:   text$   - The text to center
'                           row     - The screen row to print on
'----------------------------------------------------------------------------
SUB Center (text$, row)

	LOCATE row%, 40 - LEN(text$) \ 2 + 1    'Calculate column to start at.
	PRINT text$;                            'Print the string.

END SUB

'----------------------------------------------------------------------------
' ClearArea
'
'    Prints spaces over the a rectangular area of the screen to
'      clear any text that may be in that area.
'
'             PARAMETERS:   startRow  - Top row of rectangle to clear
'                           startCol  - Left side
'                           endRow    - Bottom row, must be <= startRow
'                           endCol    - Right side, must be <= endCol
'----------------------------------------------------------------------------
SUB ClearArea (startRow, startCol, endRow, endCol)

	FOR row = startRow TO endRow
		LOCATE row, startCol                  ' Set spot for printing to
		PRINT SPACE$(endCol - startCol + 1)   ' begin, then print blank spaces.
	NEXT row

END SUB

'--------------------------------------------------------------------------
' CyclePalette
'
'    Changes the screen colors to make the flashing effect when
'    a cannon ball explodes.  If you wish you can try different
'    colors to change the way the flashing effect looks.
'
'             PARAMETERS:   None
'--------------------------------------------------------------------------
SUB CyclePalette

	' If you wish to change the colors used by the screen flash, change only
	' the first argument (the foreground) in the COLOR statements below -
	' leave the second argument (background) as "ISLANDCOLOR".
	COLOR 12, ISLANDCOLOR
	FOR g! = 1 TO Delay: NEXT g!    'delay loop
	COLOR 14, ISLANDCOLOR
	FOR g! = 1 TO Delay: NEXT g!    'delay loop
	COLOR 0, 1                    'Return the screen colors to normal.

END SUB

'----------------------------------------------------------------------------
' DisplayChanges
'
'   Displays game characteristics that you can easily change via
'   CONST and DATA statements.
'
'             PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayChanges

	DisplayGameTitle                        'Print game title.
	
	COLOR 7
	Center "The following game characteristics can be easily changed from", 5
	Center "within the QuickBASIC Interpreter.  To change the values of  ", 6
	Center "these characteristics, locate the corresponding CONST or DATA", 7
	Center "statements in the source code and change their values, then  ", 8
	Center "restart the program (press Shift + F5).                      ", 9
	COLOR 15
	Center "Size of the island                            ", 11
	Center "Number of trees on the island                 ", 12
	Center "Strength of gravity that affects your shots   ", 13
	Center "Maximum velocity your cannons can fire        ", 14
	Center "Default number of hits needed to win the game ", 15
	Center "Sounds of the cannon firing and the explosions", 16
	COLOR 7
	Center "The CONST statements and instructions on changing them are   ", 18
	Center "located at the beginning of the main program.                ", 19

	DO WHILE INKEY$ = "": LOOP              'Wait for any keypress.
	CLS

END SUB

'----------------------------------------------------------------------------
' DisplayGameTitle
'
'    Displays game title and draws a screen border for use in the
'    introduction and suggested changes screens.
'
'             PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayGameTitle

	SCREEN 0                           ' Set the screen to a normal 80x25 text mode, clear it and add blue background.
	WIDTH 80, 25
	COLOR 4, 0
	CLS

	' Draw outline around screen with extended ASCII characters.
	LOCATE 1, 2
	PRINT CHR$(201); STRING$(76, 205); CHR$(187); ' top border
	FOR x% = 2 TO 24                              ' left and right borders
		LOCATE x%, 2
		PRINT CHR$(186); TAB(79); CHR$(186);
	NEXT x%
	LOCATE 25, 2
	PRINT CHR$(200); STRING$(76, 205); CHR$(188); 'bottom border

	'Print game title centered at top of screen
	COLOR 0, 4
	Center "      Microsoft      ", 1
	Center "     Q S H I P S     ", 2
	Center "   Press any key to continue   ", 25
	COLOR 7, 0

END SUB

'----------------------------------------------------------------------------
' DisplayIntro
'
'    Displays game introduction screen which explains game objective and
'    game keyboard controls
'
'             PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayIntro
	
	DisplayGameTitle                        ' Display game title.

	COLOR 7                                 ' Print game introduction and objectives.
	Center "Copyright (C) 1990 Microsoft Corporation.  All Rights Reserved.", 4
	Center "Each player's mission is to destroy the opponent's ship by varying the     ", 6
	Center "angle and speed of your cannon, taking into account wind speed and terrain.", 7
	Center "The wind speed is shown by a directional arrow on the playing field, its   ", 8
	Center "length relative to its strength.  With each turn, you may EITHER move your ", 9
	Center "ship to avoid your opponent's cannon fire OR shoot your cannon.            ", 10
	
	COLOR 4                                 'Print game controls.
	Center STRING$(74, 196), 12
	COLOR 7
	Center " Game Controls ", 12
	Center "    General              Shooting Cannon                  Moving Ship     ", 13
	Center "S - Shoot cannon    " + CHR$(24) + " - Increase cannon angle          " + CHR$(27) + " - Move ship left ", 15
	Center "M - Move ship       " + CHR$(25) + " - Decrease cannon angle          " + CHR$(26) + " - Move ship right", 16
	Center "Q - Quit game       " + CHR$(26) + " - Increase cannon velocity                          ", 17
	Center "                        for Player 1, decrease                            ", 18
	Center "                        cannon velocity for Player 2                      ", 19
	Center "                    " + CHR$(27) + " - Decrease cannon velocity                          ", 20
	Center "                        for Player 1, increase                            ", 21
	Center "                        cannon velocity for Player 2                      ", 22
	Center "                    Enter - Fire cannon                                   ", 23
	
	PLAY INTROSOUND                         'Play melody while waiting to continue.
						
	DO                                      'Wait for key press before continuing
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

'--------------------------------------------------------------------------
' DrawIsland
'
'    Draws an island between the two ships.  The island is made
'    by drawing increasingly taller lines (based on the sea level)
'    for the left side of the island, and then increasingly shorter
'    lines for the right side.  Trees are then placed. Note that the
'    seaLevel() array was loaded with values at the start of PlayGame.
'
'             PARAMETERS:   seaLevel()      - Water level at each point
'--------------------------------------------------------------------------
SUB DrawIsland (SeaLevel())

	' Calculate a random-sized island (iScale#), and place it between the ships (iStart, iEnd).
	iScale# = (INT(((SeaLevel(0) - 60) / 3) * RND) + INT((SeaLevel(0) - 60) / 3)) / (ISLLENGTH / 2)
	IStart = ((ShipX(2) - (ShipX(1) + SHIPWIDTH)) - ISLLENGTH - 6) * RND + ShipX(1) + SHIPWIDTH + 3
	IEnd = IStart + ISLLENGTH

	' Draw the island by extending lines from the water level (seaLevel()) sloping up on the left side and down on the right.
	FOR Counter = IStart TO IEnd
		IF Counter < (IStart + IEnd) / 2 THEN
			yoffset = iScale# * (Counter - IStart)
		ELSE
			yoffset = iScale# * (IEnd - Counter)
		END IF
		y1 = SeaLevel(Counter) + yoffset
		y2 = SeaLevel(Counter) - yoffset
		LINE (Counter, y1)-(Counter, y2), ISLANDCOLOR
	NEXT Counter

	' Place the trees.  Determine position depending on the island size (iScale#).  The pictures that make up the tree image are
	' held in the global arrays treePic1() and treePic2().  These arrays were loaded with treePic DATA in the InitializeVariables
	' SUB procedure. A BASIC graphics PUT statement is used to draw the tree pictures.
	FOR Counter = 1 TO NUMTREES
		xpos = IStart + 6 + (Counter - 1) * (ISLLENGTH / NUMTREES)
		subtot# = 0: trend = 10
		FOR j = xpos TO xpos + trend
			IF j < (IStart + IEnd) / 2 THEN
				yoffset = iScale# * (j - IStart)
			ELSE
				yoffset = iScale# * (IEnd - j)
			END IF
			subtot# = subtot# + SeaLevel(j) - yoffset
		NEXT j
		ypos = subtot# / (trend + 1) - 28
		PUT (xpos, ypos), TreePic2, PSET
		PUT (xpos + 4, ypos + TREEWIDTH), TreePic1, PSET
	NEXT Counter

END SUB

'--------------------------------------------------------------------------
' DrawWaves
'
'    Uses the BASIC CIRCLE statement to draw arcs (partial circles)
'    to make waves in the water.
'
'             PARAMETERS:   offset     - Increasing value move wave left.
'                           hmult      - Increasing value moves wave down.
'                           tmult      - Increasing value moves wave up.
'                           seaLevel() - Ocean level at each point.
'--------------------------------------------------------------------------
SUB DrawWaves (offset, hmult, tmult, SeaLevel())

	radius = ScreenWidth / 80           ' Size of a wave

	' Move across the screen from right to left drawing waves.
	FOR i = ScreenWidth / offset TO ScreenWidth - ScreenWidth / offset STEP ScreenWidth / 5

		' Calculate vertical position of wave and draw the arcs to create it.
		ypos = (hmult * ScreenHeight + tmult * SeaLevel(i)) / (hmult + tmult)
		CIRCLE (i, ypos), radius, ISLANDCOLOR, 5 * Pi# / 4, 2 * Pi#
		CIRCLE (i + 2 * radius, ypos), radius, ISLANDCOLOR, Pi#, Pi# * 2
		CIRCLE (i + 4 * radius, ypos), radius, ISLANDCOLOR, Pi#, 7 * Pi# / 4

	NEXT i

END SUB

'--------------------------------------------------------------------------
' DrawWind
'
'    Draws an arrow in the direction of the wind.  The length of
'    the arrow depends on the strength of the wind.
'
'             PARAMETERS:   None
'--------------------------------------------------------------------------
SUB DrawWind

	WindTmp = Wind
	IF WindTmp = 0 THEN WindTmp = 1   ' Ensure that Wind won't ever be zero.

	WindLineLength = WindTmp * (ScreenWidth / 80) ' Calculate the length of the arrow.
	x1 = ScreenWidth / 2 - WindLineLength / 2
	x2 = x1 + WindLineLength
	' SGN(WindTmp) returns -1 if WindTmp is negative, 1 if it is positive.
	ArrowDir = -2 * SGN(WindTmp)                ' Figure out the arrowhead direction.
	LINE (x1, 16)-(x2, 16), ISLANDCOLOR         ' Draw the wind arrow line.
	LINE (x2, 16)-(x2 + ArrowDir, 14), ISLANDCOLOR
	LINE (x2, 16)-(x2 + ArrowDir, 18), ISLANDCOLOR

END SUB

'--------------------------------------------------------------------------
' GetGameOptions
'
'    Prompts for and saves the Player names for each player and
'    the number of points to play to.  GetGameOptions does not call
'    any other SUB or FUNCTION procedures.
'
'             PARAMETERS:   None
'--------------------------------------------------------------------------
SUB GetGameOptions

	SCREEN 0
	WIDTH 80
	CLS

	' player1$ defaults to "Player 1"
	COLOR 7: Center "Default is 'Player 1'", 9
	COLOR 15: LOCATE 8, 30
	LINE INPUT "Name of Player 1: "; Player$(1)
	IF Player$(1) = "" THEN
		Player$(1) = "Player 1"
	ELSE
		Player$(1) = LEFT$(Player$(1), 10)
	END IF

	' player2$ defaults to "Player 2"
	COLOR 7: Center "Default is 'Player 2'", 12
	COLOR 15: LOCATE 11, 30
	LINE INPUT "Name of Player 2: "; Player$(2)
	IF Player$(2) = "" THEN
		Player$(2) = "Player 2"
	ELSE
		Player$(2) = LEFT$(Player$(2), 10)
	END IF

	' Number of games defaults to INITNUMGAMES
	COLOR 7: Center "Default is" + STR$(INITNUMGAMES), 15
	DO
		COLOR 15: LOCATE 14, 27: PRINT SPACE$(50);
		LOCATE 14, 27
		INPUT "Play to how many points"; NumHold$
		NumGames = VAL(LEFT$(NumHold$, 2))
	LOOP UNTIL NumGames > 0 AND LEN(NumHold$) < 3 OR LEN(NumHold$) = 0
	IF NumGames = 0 THEN NumGames = INITNUMGAMES

	COLOR 7                                         ' Restore color

END SUB

'--------------------------------------------------------------------------
' GetPlayerCommand
'
'     Displays a menu on the active players side of the screen and gets the
'     player's selection.  If the selection is a valid menu option, then
'     the appropriate SUB is called to handle the command.  If the choice
'     is invalid the SUB InvalidKeyHit is called.  This process is repeated
'     until the player actually shoots, moves, or quits the game.
'
'             PARAMETERS:   playerNum  - The active player
'                           seaLevel() - Passed on to other SUB procedures
'--------------------------------------------------------------------------
FUNCTION GetPlayerCommand (PlayerNum, SeaLevel())

	' As long as the player has not shot or moved he/she can keep selecting.
	finished = FALSE
	DO: LOOP WHILE INKEY$ <> ""         ' Flush keyboard buffer before turn

	xpos = ShipX(PlayerNum)             ' Determine this player's ship location.
	ypos = ShipY(PlayerNum)
	IF PlayerNum = 1 THEN               ' Determine where to put the menu
		MenuCol = 3                     ' depending on whose turn it is.
		startCol = 30
		endCol = 40
	ELSE
		MenuCol = 25
		startCol = 1
		endCol = 24
	END IF

	' If the player fires the shot "starts" from above his/her ship, so the
	'   yShotPosition is calculated to enforce this.
	YShotPos = ypos - 3

	DO WHILE NOT finished
		' Print the player's score and label the wind indicator.
		LOCATE 1, 1: PRINT Player$(1)
		LOCATE 1, (MaxCol - LEN(Player$(2))): PRINT Player$(2)
		LOCATE 1, 19: PRINT "Wind";
		LOCATE 2, 1: PRINT "Hits:"; TotalWins(1);
		LOCATE 2, MaxCol - 6 - LEN(STR$(TotalWins(2))): PRINT "Hits:"; TotalWins(2);

		CALL DrawWind                   ' Draw the wind indicator.

		' Clear the area where the menu will appear and print the menu with a box.
		CALL ClearArea(4, MenuCol, 8, MenuCol + 12)
		LINE (MenuCol * 8 - 2, 28)-(MenuCol * 8 + 87, 60), , B
		LOCATE 5, MenuCol + 2: PRINT "S = Shoot"
		LOCATE 6, MenuCol + 2: PRINT "M = Move "
		LOCATE 7, MenuCol + 2: PRINT "Q = Quit "
		
		DO                              ' Get the player's selection.
			kbd$ = UCASE$(INKEY$)
		LOOP UNTIL kbd$ <> ""
		CALL ClearArea(4, MenuCol, 8, MenuCol + 12)

		SELECT CASE kbd$
			CASE "Q"                    ' Q = QUIT
				SOUND 600, .5           ' Make sure the player really wants to quit.
				SOUND 800, .5
				LOCATE 5, 11: PRINT "Really quit? (Y/N)";
				DO
					kbd$ = UCASE$(INKEY$)
				LOOP WHILE kbd$ = ""
				IF kbd$ = "Y" THEN         ' If players want to quit, then
					CALL DisplayChanges    ' remind them of changes they can
					END                    ' make to the program.
				ELSE
					CALL ClearArea(5, 1, 5, 40)
				END IF

			CASE "M"                    ' M = MOVE
				IF MoveShip(PlayerNum, SeaLevel()) THEN
					finished = TRUE     ' Player moved - his/her turn is over.
					GetPlayerCommand = FALSE
				END IF

			CASE "S", CHR$(ENTERKEY)    ' S = SHOOT (or Enter key)
				' Get desired angle & velocity.
				CALL GetShotParams(PlayerNum, angle#, velocity)

				' Player 2 is actually shooting from the other direction so we need to turn the angle 180 degrees.
				IF PlayerNum = 2 THEN angle# = 180 - angle#

				' Clear the text on the upper area of the screen so the cannon shot can go off the top of the screen.
				VIEW PRINT 1 TO 7       ' Set text viewport to rows 1-7.
				CLS 2                   ' Clear the text viewport.
				VIEW PRINT              ' Reset text viewport to whole screen.

				' Plot the shot on the screen.
				playerHit = PlotShot(xpos, YShotPos, angle#, velocity)
				TotalShots(PlayerNum) = TotalShots(PlayerNum) + 1

				' Determine what happened by the value returned from PlotShot FUNCTION..
				IF playerHit = PlayerNum THEN
					GetPlayerCommand = SHOOTSELF ' Player shot himself/herself.
				ELSEIF playerHit <> 0 THEN
					GetPlayerCommand = TRUE      ' Play shot opponent.
				ELSE
					GetPlayerCommand = FALSE     ' Player missed.
				END IF
				finished = TRUE                  ' Player has fired - his/her turn is over.
			CASE ELSE                            ' Some other key was pressed.
				BEEP
		END SELECT
	LOOP

END FUNCTION

'--------------------------------------------------------------------------
' GetShotParams
'
'    Prompts for and records the angle and velocity the player wishes to use
'    for shooting his/her cannon.  The main loop keeps checking for and
'    recording player changes to angle and velocity until the player hits
'    Enter to fire the cannon.  If the player hits an invalid key then a
'    message is displayed about how to get help.  Help will be displayed if
'    the player enters "h" or "H".
'
'             PARAMETERS:   playerNum   - Which player is firing
'                           newAngle#   - The new angle to use
'                           newVelocity - The new shot speed to use
'--------------------------------------------------------------------------
SUB GetShotParams (PlayerNum, NewAngle#, NewVelocity)

	'Clear the upper left and right corners of the screen
	IF PlayerNum = 1 THEN
		locateCol = 1
		locateCol2 = 26
		ArrowAffect = 1               ' Direction wind arrow will point and
		CALL ClearArea(2, 1, 6, 16)   ' direction that velocity meter will extend.
		CALL ClearArea(1, 26, 6, 40)
	ELSE
		locateCol = 30
		locateCol2 = 1
		ArrowAffect = -1
		CALL ClearArea(1, 1, 6, 16)
		CALL ClearArea(2, 26, 6, 40)
	END IF

	' Display the shooting instructions in the upper corner on the non-firing players side of the screen.
	LOCATE 2, locateCol2: PRINT "Change angle "
	LOCATE 3, locateCol2: PRINT "with  and ."
	LOCATE 4, locateCol2: PRINT "Change speed "
	LOCATE 5, locateCol2: PRINT "with " + CHR$(26) + " and " + CHR$(27) + "."
	LOCATE 6, locateCol2: PRINT "ENTER to fire."

	' Show the angle and velocity of the players last shot.  The angle and velocity defaults to 45 degrees and velocity of 50 meters/ second if the player has not fired yet.
	CALL PlotAngle(locateCol, TheAngle#(PlayerNum), PlayerNum)
	CALL PlotVelocity(locateCol, TheVelocity(PlayerNum), PlayerNum)

	' Get the players input.  Either change cannon angle using the Up and
	' Down arrows, change the shot velocity using the Right and Left arrow
	' or fire the cannon with the Enter key.  Note that the left
	' and right arrow key affect is reversed for the second player.
	DO WHILE NOT finished
		DO                                     ' Get key pressed.
			kbd$ = INKEY$
		LOOP WHILE kbd$ = ""

		cursorkey = ASC(RIGHT$(kbd$, 1))
		SELECT CASE cursorkey
			CASE LEFT, RIGHT                   ' Change the velocity level indicator.
				IF cursorkey = RIGHT THEN
					increment = 1 * ArrowAffect
				ELSE
					increment = -1 * ArrowAffect
				END IF
				IF (increment < 0 AND TheVelocity(PlayerNum) > 0) OR (increment > 0 AND TheVelocity(PlayerNum) < MAXVELOCITY) THEN
					TheVelocity(PlayerNum) = TheVelocity(PlayerNum) + increment
					CALL PlotVelocity(locateCol, TheVelocity(PlayerNum), PlayerNum)
				ELSE
					BEEP
					DO
						temp$ = INKEY$
					LOOP WHILE temp$ <> ""
				END IF
			CASE UP, DOWN                     ' Change angle in trajectory display.
				IF cursorkey = UP THEN
					increment = 1
				ELSE
					increment = -1
				END IF
				IF (increment < 0 AND TheAngle#(PlayerNum) > 0) OR (increment > 0 AND TheAngle#(PlayerNum) < 90) THEN
					TheAngle#(PlayerNum) = TheAngle#(PlayerNum) + increment
					CALL PlotAngle(locateCol, TheAngle#(PlayerNum), PlayerNum)
				ELSE
					BEEP
					DO
						temp$ = INKEY$
					LOOP WHILE temp$ <> ""
				END IF
			CASE ENTERKEY
				finished = TRUE         ' Fire cannon when Enter key is pressed.
				NewAngle# = TheAngle#(PlayerNum)
				NewVelocity = TheVelocity(PlayerNum)
			CASE ELSE                   ' Any other key is invalid.
				BEEP
		END SELECT
	LOOP
	  
END SUB

'--------------------------------------------------------------------------
' InitializeVariables
'
'    SHARED variables are initialized and graphics pictures loaded from
'    DATA statements.
'
'             PARAMETERS:   None
'--------------------------------------------------------------------------
SUB InitializeVariables

	Pi# = 4 * ATN(1#)           ' Calculate PI to 14 decimal places.

	ScreenWidth = 320           ' Width and height for SCREEN 1 (320x200).
	ScreenHeight = 200
	MaxCol = 40
	' Loading picture data from the DATA statements into the respective arrays.
	RESTORE ShipPicData
	FOR Counter = 1 TO 36
		READ ShipPic(Counter)   ' ship picture
	NEXT Counter
	FOR Counter = 1 TO 20
		READ TreePic1(Counter)  ' bottom of palm tree
	NEXT Counter
	FOR Counter = 1 TO 36
		READ TreePic2(Counter)  ' top of palm tree
	NEXT Counter
	FOR Counter = 1 TO 2
		READ Shot&(Counter)     ' shot
	NEXT Counter

	'Determine machine performance in a generic manner...
	x! = TIMER
	FOR g! = 1 TO 500
	NEXT g!
	x! = TIMER - x!
	SELECT CASE x!
	CASE 0 TO .18                        'For 386 type machines.
		Delay = 500
	CASE IS < .45                        'For PC/AT type machines.
		Delay = 200
	CASE ELSE                            'For XT type machines.
		Delay = 50
	END SELECT

END SUB

'--------------------------------------------------------------------------
' MakeBattleField
'
'    Generates the sea at each point on the screen, storing
'    the values in the array seaLevel().  The level of
'    the ocean is generated from left side of the screen to
'    the right.  The variable "motion" affects the "trend" of
'    sea level, generating up and down "slopes."
'
'             PARAMETERS:   SeaLevel()     - The height of the sea
'--------------------------------------------------------------------------
SUB MakeBattleField (SeaLevel())

	increment = 1
	range = 5
	SeaLevel(0) = ScreenHeight - (45 + INT((ScreenHeight / 8) * RND + 1))
	' When the following loop ends, each element of the seaLevel() array has a
	' value to be used in drawing the ocean surface.
	FOR Counter = 1 TO ScreenWidth
		Motion = INT(range * RND + 1)
		SELECT CASE Motion
			CASE 1 TO range / 2
				IF Motion < range / 2 - 1 THEN
					trend = trend + increment
				ELSE
					trend = trend - increment
				END IF
			CASE range / 2 + 1 TO range / 2 + 2
				trend = 1 * SGN(Wind)
			CASE range / 2 + 3
				trend = 0
			CASE ELSE
				trend = -1 * SGN(Wind)
		END SELECT
		SELECT CASE trend     ' Set values in seaLevel based on trend generated above
			CASE IS < 0
				SeaLevel(Counter) = SeaLevel(Counter - 1) - 1  ' if trend is negative
			CASE IS > 0
				SeaLevel(Counter) = SeaLevel(Counter - 1) + 1  ' if trend is positive
			CASE ELSE
				SeaLevel(Counter) = SeaLevel(Counter - 1)      ' if trend is zero
		END SELECT
		IF SeaLevel(Counter) > SeaLevel(0) + range THEN SeaLevel(Counter) = SeaLevel(Counter - 1) - 3
		IF SeaLevel(Counter) < SeaLevel(0) - range THEN SeaLevel(Counter) = SeaLevel(Counter - 1) + 3
		IF Counter > range AND Counter < ScreenWidth - range THEN
			diff = SeaLevel(Counter - 4) - SeaLevel(Counter - 1)
			IF ABS(diff) > range THEN
				trend = -increment * SGN(diff)
			END IF
		END IF
	NEXT Counter
' When this SUB ends the values in SeaLevel array are known in the caller. They
' are passed to PlotBattleField and used in LINE statements to draw the ocean
' surface and fill the ocean with color down to the bottom of the screen with
' WATERCOLOR. DrawWaves uses the SeaLevel values in calculating the positions
' for drawing waves.

END SUB

'--------------------------------------------------------------------------
' MoveShip
'
'    Allows player to move ship using Left and Right arrow keys.  As long
'    as they do not actually move the ship, they can change their mind
'    and go back to the main menu to shoot.
'
'             PARAMETERS:   playerNum  - The player who is moving
'                           seaLevel() - Used to fix area after move
'--------------------------------------------------------------------------
FUNCTION MoveShip (PlayerNum, SeaLevel())

	finished = FALSE                    ' TRUE if player presses Enter.
	MoveShip = FALSE                    ' TRUE if player moves ship. If FALSE
										' player can go back and shoot.

	' Print instructions for moving.
	LOCATE 5, 2: PRINT "To move your ship, press either the"
	LOCATE 6, 2: PRINT CHR$(26) + " or " + CHR$(27) + " arrow keys, then press Enter."

	' Loop until the player hits enter to indicate they are done moving.
	'   If they have not actually moved then they can still do something else.
	DO WHILE NOT finished
		DO                              ' Get player's selection.
			kbd$ = INKEY$
		LOOP WHILE kbd$ = ""
		cursorkey = ASC(RIGHT$(kbd$, 1))
		SELECT CASE cursorkey
			CASE LEFT                   ' Move ship to left.
				MoveShip = TRUE
				moveit = -MOVESHIPBY
			CASE RIGHT                  ' Move ship to right.
				MoveShip = TRUE
				moveit = MOVESHIPBY
			CASE ENTERKEY               ' End move by pressing Enter key.
				finished = TRUE
			CASE ELSE                   ' Any other key is invalid.
				BEEP
				moveit = 0
		END SELECT

		IF NOT finished THEN            ' Move ship.
			xpos = ShipX(PlayerNum) + moveit
			' Move ship only if it can be moved in that direction.
			IF (xpos > 3 AND xpos < IStart - MOVESHIPBY - 3) OR (xpos > IEnd + 3 AND xpos < ScreenWidth - SHIPWIDTH - 3) THEN
				ypos = SeaLevel(xpos) - 9
				' blank out current position
				PUT (ShipX(PlayerNum), ShipY(PlayerNum)), ShipPic, XOR
				IF moveit > 0 THEN        ' clean up the water using seaLevel()
					FOR i = ShipX(PlayerNum) TO xpos - 1
						LINE (i, SeaLevel(i))-(i, SeaLevel(i) + SHIPWIDTH), WATERCOLOR
					NEXT i
				ELSE
					FOR i = ShipX(PlayerNum) TO ShipX(PlayerNum) - moveit
						LINE (i, SeaLevel(i))-(i, SeaLevel(i) + SHIPWIDTH), WATERCOLOR
					NEXT i
				END IF
				PUT (xpos, ypos), ShipPic, PSET   ' Put ship in new position.
				ShipX(PlayerNum) = xpos
				ShipY(PlayerNum) = ypos
			ELSE
				BEEP
			END IF
		END IF
	LOOP

	CALL ClearArea(5, 1, 6, 40)

END FUNCTION

'--------------------------------------------------------------------------
' PlaceShips
'
'    Computes new locations and place ships there.
'
'             PARAMETERS:   seaLevel() - Needed to find shipY()
'--------------------------------------------------------------------------
SUB PlaceShips (SeaLevel())
	
	' Calculate random position for ships on either side of the screen.
	ShipX(1) = INT(((ScreenWidth - ISLLENGTH) / 2 - SHIPWIDTH - 6) * RND) + 3
	ShipY(1) = SeaLevel(ShipX(1)) - 12
	ShipX(2) = ScreenWidth - INT(((ScreenWidth - ISLLENGTH) / 2 - SHIPWIDTH - 6) * RND) - SHIPWIDTH - 3
	ShipY(2) = SeaLevel(ShipX(2)) - 12
	
	FOR Counter = 1 TO 2   ' Place the ship images from shipPic() and fix the water to look good.
		PUT (ShipX(Counter), ShipY(Counter)), ShipPic, PSET
		FOR FixTerrain = ShipX(Counter) TO ShipX(Counter) + SHIPWIDTH
			LINE (FixTerrain, ShipY(Counter) + 2 * SHIPWIDTH)-(FixTerrain, ShipY(Counter) + SHIPWIDTH), WATERCOLOR
		NEXT FixTerrain
	NEXT Counter

END SUB

'--------------------------------------------------------------------------
' PlayGame
'
'    This is the main driver for QSHIPS.
'    This SUB procedure contains three nested loops.  The outermost loop
'    will ask the players if they wish to play again each time
'    someone wins.  The second loop will continue to generate a new
'    battle field and wind speed each round until someone scores
'    enough points to win.  The inner most loop gets and executes
'    each players commands for that round, ending when one player is
'    sunk.
'
'             PARAMETERS:   None
'--------------------------------------------------------------------------
SUB PlayGame

	'Set up arrays to hold the height of the water/terrain at each point.
	DIM SeaLevel(0 TO ScreenWidth)
	RANDOMIZE (TIMER)
	Counter = 0

	'The main loop of the game.  Keeps going until users select to quit.
	DO
		Center "Creating game battleground, please wait.", 22

		' Initialize score.
		FOR i = 1 TO 2
			TotalShots(i) = 0
			TotalWins(i) = 0
		NEXT i

		firstPlayer = INT(2 * RND + 1)  ' Randomly determine first player.

		'This loop gets and executes the players commands until the 'play to' score has been met.
		DO UNTIL TotalWins(1) >= NumGames OR TotalWins(2) >= NumGames

			FOR i = 1 TO 2              ' Reset initial cannon angle and speed.
				TheAngle#(i) = 45
				TheVelocity(i) = 50
			NEXT i

			' Calculate wind for this round.  To have the wind speed change after each shot, move the next four lines to down below the statement "DO WHILE directHit = FALSE."
			Wind = INT(11 * RND + 1) - 6
			IF (INT(4 * RND + 1) = 1) THEN   ' Every once in a while, make it 10 times stronger.
				Wind = Wind + SGN(Wind) * 10
			END IF

			CALL MakeBattleField(SeaLevel())    ' Generate and plot a new battle field.
			DO                                  ' Flush keyboard buffer
				kbd$ = INKEY$
			LOOP UNTIL kbd$ = ""
			SCREEN ScreenMode
			COLOR 0, 1                          ' Set CGA palette to palette #1.
			CLS                                 ' Clear screen for new round.
			CALL PlotBattleField(SeaLevel())    ' Draw ocean, waves, ships, island, trees.
			
			DO                                  ' This section will get and execute commands until one player is blown up or the players quit.
				Counter = Counter + 1
										  
				' Get and execute a player's command, returning an integer that tells if someone has been hit.
				directHit = GetPlayerCommand(firstPlayer, SeaLevel())

				IF directHit <> FALSE THEN
					IF directHit = SHOOTSELF THEN       ' SHOOTSELF is constant value 1.
						TotalWins(ABS(firstPlayer - 3)) = TotalWins(ABS(firstPlayer - 3)) + 1
					ELSE
						TotalWins(firstPlayer) = TotalWins(firstPlayer) + 1
					END IF
					IF directHit = TRUE THEN Counter = Counter + 1
				END IF
				
				firstPlayer = ABS(firstPlayer - 3)   ' Change firstPlayer so turns alternate
													 ' between players while there hasn't
			LOOP WHILE directHit = FALSE             ' been a direct hit.
			' When this loop is exited one player or the other has been sunk.  We now go back to the outer loop to make a new battle field.

		LOOP                            ' Repeat until totalwins are finally met

		COLOR 1, 1
		CLS                             ' Display game over info.
		LOCATE 3, 15: PRINT "GAME OVER!"
		diff = TotalWins(1) - TotalWins(2)           ' If diff is negative, player2 won
		IF diff > 0 THEN ofst = 1 ELSE ofst = 2      ' more games, otherwise player1 won more.

		LOCATE 7, 4         ' If player2 won more, display absolute value of difference:
		PRINT Player$(ofst) + " won by" + STR$(ABS(diff)) + " point(s)."
		LOCATE 11, 2: PRINT "Player:       Hits:      Shots Fired:"
		LOCATE 12, 2: PRINT "-------       -----      ------------"

		FOR j = 1 TO 2
			LOCATE 13 + j, 2: PRINT Player$(j)
			LOCATE 13 + j, 17: PRINT TotalWins(j)
			LOCATE 13 + j, 32: PRINT TotalShots(j)
		NEXT j
		
		LOCATE 20, 11: PRINT "Play again? (Y/N) "    'See if the players wish to play again.
		DO
			StillPlay$ = UCASE$(INKEY$)
		LOOP WHILE StillPlay$ <> "Y" AND StillPlay$ <> "N"

	LOOP WHILE StillPlay$ = "Y"      'Repeat while players still want to play game.

END SUB

'--------------------------------------------------------------------------
' PlotAngle
'
'    Plots the angle (from 0-90 degrees) indicator that is displayed while
'    a player is adjusting the angle of his/her shot.
'
'             PARAMETERS:   col       - What column to start at
'                           newAngle# - The angle to plot
'                           OldAngle# - The old angle plotted
'                           playerNum - Which player is adjusting
'--------------------------------------------------------------------------
SUB PlotAngle (col, NewAngle#, PlayerNum)
	
	radius = 28.57143                  ' Assumes a screen height of 200 and width of 320.
	YCenter = 34.28572

	IF PlayerNum = 1 THEN              ' Set placement of drawing according
		XCenter = 78.57143             ' to whose turn it is.
		XIncrement = radius
		xoffset = -1
	ELSE
		XCenter = 228.5714
		XIncrement = -radius
		xoffset = 1
	END IF
	
	' Erase the previous arc.
	LINE (XCenter, YCenter)-(XCenter + XIncrement, YCenter - radius * .8333333), BACKGROUNDCOLOR, BF
	' Draw new pie-slice representing trajectory.
	IF PlayerNum = 1 THEN
		CIRCLE (XCenter, YCenter), radius, OBJECTCOLOR, -.00001, -Pi# / 2
	ELSE
		CIRCLE (XCenter, YCenter), radius, OBJECTCOLOR, -Pi# / 2, -Pi#
	END IF
	' Draw line representing the angle of the shot.
	LINE (XCenter, YCenter)-(XCenter + COS((NewAngle# * (Pi# / 180))) * XIncrement, YCenter - SIN((NewAngle# * (Pi# / 180))) * (radius * .8333333)), OBJECTCOLOR
	' Fill bottom with magenta if appropriate.
	IF NewAngle# > 1 THEN PAINT (XCenter + XIncrement + xoffset, YCenter - 1), ISLANDCOLOR, OBJECTCOLOR

	LOCATE 3, col                  ' Print the angle beside the indicator.
	PRINT USING "Angle: ##"; NewAngle#

END SUB

'--------------------------------------------------------------------------
' PlotBattlefield
'
'    Plots the sea level stored in the array seaLevel().  A line of color
'    WATERCOLOR is drawn from the bottom of the screen straight up to the
'    y coordinate stored in seaLevel().  This process is repeated for each
'    x coordinate on the screen.  PlotBattleField then adds waves, ships,
'    and an island to complete the battle field.
'
'             PARAMETERS:   seaLevel()      - The height of the sea
'--------------------------------------------------------------------------
SUB PlotBattleField (SeaLevel())

	FOR Counter = 1 TO ScreenWidth          ' Draw the sea.
		LINE (Counter, ScreenHeight)-(Counter, SeaLevel(Counter)), WATERCOLOR
	NEXT Counter
	
	CALL DrawWaves(10, 4, 1, SeaLevel())    ' Call WaveSub to add in the waves.
	CALL DrawWaves(20, 2, 1, SeaLevel())
	CALL DrawWaves(30, 1, 1, SeaLevel())
	CALL DrawWaves(40, 3, 1, SeaLevel())
	CALL DrawWaves(50, 7, 1, SeaLevel())
	
	CALL PlaceShips(SeaLevel())             ' Place ships in battlefield.
	CALL DrawIsland(SeaLevel())             ' Place the island.

END SUB

'--------------------------------------------------------------------------
' PlotShot
'
'    Plots the trajectory of a shot on the screen based on indicated angle
'    and velocity, adjusting for wind speed and direction.  As the cannon
'    ball trajectory is plotted we check to see if the shot has hit
'    anything.  If the shot goes off the top of the screen we keep track
'    of it in case it comes down back on the screen.  If the shot goes off
'    the side of the screen then we assume it missed and quit plotting.
'
'             PARAMETERS:   startX   - The x coordinate shot from
'                           startY   - The y coordinate shot from
'                           angle#   - The angle of the cannon
'                           velocity - The starting velocity
'--------------------------------------------------------------------------
FUNCTION PlotShot (startX, startY, angle#, velocity)

	CONST cSeconds = 3.5                ' Avg # seconds cannon ball in air

	PLAY CANNONFIRESOUND
	
	IF velocity < 2 THEN      ' If a shot is too slow then the cannon ball blows the shooter up!
		PlotShot = SinkShip(startX, startY)
	ELSE

		' Calculate the starting variables for plotting the trajectory.
		angle# = angle# / 180 * Pi#         ' Convert degree angle to radians.
		InitialXVelocity# = COS(angle#) * velocity
		InitialYVelocity# = SIN(angle#) * velocity

		PlotShot = 0          ' Assume no ship destroyed
		NEEDERASE = FALSE     ' TRUE if we need to erase a previous shot

		t1# = TIMER

		' While the shot is on the screen and has not hit anything we plot its
		' trajectory.
		DO

			' Calculate shot's current position.
			x = startX + (InitialXVelocity# * t#) + (.5 * (Wind / 5) * t# * t#)
			y = startY + (-(InitialYVelocity# * t#) + (.5 * GRAVITY * t# * t#)) * (ScreenHeight / 350)

			' Erase the old image of the shot if we need to do so.
			IF NEEDERASE THEN                     ' Shot& is an array holding data
				PUT (Oldx, Oldy), Shot&, XOR      ' used to draw the projectile.
			END IF                                ' XOR causes erasing by
												  ' redrawing in background color

			' Check to see if shot has gone off the side of the screen.  If
			' so, flag to exit the loop now so we don't try to plot the point
			' or look at a point off of the screen.
			IF (x >= ScreenWidth - 3) OR (x <= 3) OR (y >= ScreenHeight - 3) THEN
				EXIT DO
			END IF

			' Check to see if the shot hit anything: Use the POINT statement
			' to get the color of the points around the shot, then compare the
			' color to ISLANDCOLOR, WATERCOLOR, OBJECTCOLOR to see what was hit.
			' If we get a hit, we have to exit immediately, otherwise we could
			' get several hits at the current location.
			FOR lookX = -1 TO 1
				FOR lookY = -1 TO 1
					IF POINT(x + lookX, y + lookY) <> BACKGROUNDCOLOR THEN
						IF POINT(x + lookX, y + lookY) = OBJECTCOLOR THEN
							PlotShot = SinkShip(x, y)         ' hit a ship
							EXIT DO
						ELSEIF POINT(x + lookX, y + lookY) = ISLANDCOLOR THEN
							CALL CannonHit(x, y, ISLANDCOLOR) ' hit the island
							EXIT DO
						ELSEIF POINT(x + lookX, y + lookY) = WATERCOLOR THEN
							CALL CannonHit(x, y, WATERCOLOR)  ' hit the water
							EXIT DO
						END IF
					END IF
				NEXT
			NEXT

			' If the shot has not hit anything, plot it.
			IF y > 0 THEN
				PUT (x, y), Shot&, PSET
				Oldx = x
				Oldy = y
				NEEDERASE = TRUE
			ELSE
				NEEDERASE = FALSE
			END IF

			' Wait till time for next position to be drawn (.05 time units)
			DO
				' If midnight rollover occurred, adjust starting time
				IF TIMER < t1# THEN
					t1# = t1# - 86400
				END IF

				t2# = (TIMER - t1#) * 8 / cSeconds
			LOOP WHILE t2# - t# < .05
			t# = t2#
		LOOP
	END IF
	
END FUNCTION

'--------------------------------------------------------------------------
' PlotVelocity
'
'    Plots the shot velocity indicator used when a player is entering the
'    parameters for a shot.
'
'             PARAMETERS:   col         - Where to plot
'                           newVelocity - The velocity to indicate
'                           playerNum   - Which player is firing
'--------------------------------------------------------------------------
SUB PlotVelocity (col, NewVelocity, PlayerNum)
	
	margin = 33.33333         ' Assumes a 320x200 screen dimension.
	YHeight = 5
	YCenter = 50
	XWidth = 106.66667#
	
	IF PlayerNum = 1 THEN     ' Put the indicator on the firing players side of the screen.
		XCenter = .66666
		XIncrement = XWidth
		xoffset = -1
	ELSE
		XCenter = 236.6667
		XIncrement = -XWidth
		xoffset = 1
	END IF

	' Plot the current velocity on indicator.  ISLANDCOLOR is used to paint the current velocity.
	LINE (XCenter, YCenter)-(XCenter + XIncrement * (NewVelocity / MAXVELOCITY) + xoffset, YCenter - YHeight), ISLANDCOLOR, BF
	LINE (XCenter + XIncrement * (NewVelocity / MAXVELOCITY), YCenter)-(XCenter + XIncrement + xoffset, YCenter - YHeight), BACKGROUNDCOLOR, BF
	' Draw box around velocity indicator.
	LINE (XCenter + xoffset, YCenter + 1)-(XCenter + XIncrement, YCenter - YHeight - 1), OBJECTCOLOR, B
	' Put strip of grey at current velocity.
	LINE (XCenter + XIncrement * (NewVelocity / MAXVELOCITY) + xoffset, YCenter)-(XCenter + XIncrement * (NewVelocity / MAXVELOCITY) + xoffset, YCenter - YHeight), OBJECTCOLOR
	
	LOCATE 5, col                 ' Print current velocity as numerical value also.
	PRINT USING "Speed:###"; NewVelocity

END SUB

'--------------------------------------------------------------------------
' SinkShip
'
'    Causes the ship that has been hit to explode in a large explosion and
'    then sink.  The explosion is created using a series of circle
'    statements.  The sinking ship is done using the BASIC graphics PUT
'    statement with the XOR option.
'
'             PARAMETERS:   x    -  x coordinate of shot
'                           y    -  y coordinate of shot
'--------------------------------------------------------------------------
FUNCTION SinkShip (x, y)

	XWidth = 8
	YHeight = 10
	
	CALL CannonHit(x, y, ISLANDCOLOR)   ' Do a normal cannon ball explosion.
									   
	CALL CyclePalette                    ' Flash the screen.
	IF x < ScreenWidth / 2 THEN playerHit = 1 ELSE playerHit = 2  ' Determine the player hit.

	PLAY SHIPEXPLOSIONSOUND

	' Create the base of the explosion using expanding circles and the "stem" of the mushroom cloud using lines.
	FOR blast = 1 TO XWidth
		CIRCLE (ShipX(playerHit) + SHIPWIDTH / 2, ShipY(playerHit) + YHeight), blast, ISLANDCOLOR, , , -1.57
		LINE (ShipX(playerHit) + SHIPWIDTH / 2 - 3.5, ShipY(playerHit) + YHeight - blast)-(ShipX(playerHit) + SHIPWIDTH / 2 + 3.5, ShipY(playerHit) + YHeight - blast), ISLANDCOLOR
	NEXT blast
   
	CALL CyclePalette
   
	' Create the top of the mushroom cloud using expanding circles while clearing the bottom of the cloud off.
	FOR Cloud = 1 TO SHIPWIDTH
		IF Cloud < (XWidth) THEN CIRCLE (ShipX(playerHit) + SHIPWIDTH / 2, ShipY(playerHit) + YHeight), (XWidth + 1) - Cloud, BACKGROUNDCOLOR, , , -1.57
		CIRCLE (ShipX(playerHit) + SHIPWIDTH / 2, ShipY(playerHit)), Cloud, ISLANDCOLOR, , , -1.57
	NEXT Cloud
	   
	CALL CyclePalette
	
	FOR Cloud = SHIPWIDTH TO 1 STEP -1    ' Slowly erase the top of the cloud.
		CIRCLE (ShipX(playerHit) + SHIPWIDTH / 2, ShipY(playerHit)), Cloud, BACKGROUNDCOLOR, , , -1.57
	NEXT Cloud
	SinkShip = playerHit

	xcol = ShipX(playerHit)
	origycol = ShipY(playerHit)
	ycol = origycol
	i = 1

	' Sink the ship using the PUT statement to slowly lower the ship image and drawing lines in WATERCOLOR over the image so it seems to submerge.
	DO WHILE i <= SHIPWIDTH AND ycol + SHIPWIDTH < ScreenHeight
		PUT (xcol, ycol), ShipPic, XOR
		ycol = ycol + 1
		IF ycol + SHIPWIDTH < ScreenHeight THEN
			LINE (xcol, ycol - 1)-(xcol + SHIPWIDTH, ycol - 1), BACKGROUNDCOLOR
			PUT (xcol, ycol), ShipPic, PSET
			FOR j = 0 TO i
				LINE (xcol, origycol + j + SHIPWIDTH)-(xcol + SHIPWIDTH, origycol + j + SHIPWIDTH), WATERCOLOR
			NEXT j
		END IF
		i = i + 1
	LOOP

END FUNCTION

