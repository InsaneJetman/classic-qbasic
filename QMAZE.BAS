'
'                                QMAZE.BAS
'
'       Copyright (C) 1990 Microsoft Corporation. All Rights Reserved.
'
' Race through a maze that pits your skill against the clock, or a friend.
' The object is to finish the maze before time runs out or before your
' opponent finishes.
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
' statements below these comments and the DATA statements at the end
' of the main program can be modified to change the following:
'    Songs played during this game
'    Size of the maze game grid
'    How quickly the monsters move
'    Initial time allowed per maze
'    Color of Player 1
'    Color of Player 2
'    Color of the Monsters
'    Maze dimensions at each difficulty level
'    Amount of time path preview is shown
'
' On the right side of each CONST statement, there is a comment that tells
' you what it does and how large or small you can set the value.  Above the
' DATA statements, there are comments that tell you the format of the
' information stored there.
'
' On your own, you can also add exciting sound and visual effects or make any
' other changes that your imagination can dream up.  By reading the
' "Learn BASIC Now" book, you'll learn the techniques that will enable you
' to fully customize this game and to create games of your own.
'
' If the game won't run after you have changed it, you can exit without
' saving your changes by pressing Alt, F, X and choosing NO.
'
' If you do want to save your changes, press Alt, F, A and enter a filename
' for your version of the program.  Before you save your changes,
' however, you should make sure they work by running the program and
' verifying that your changes produce the desired results.  Also, always
' be sure to keep a backup of the original program.
'
DEFINT A-Z

' Here are the BASIC CONST statements you can change.
CONST BLOCKSIZE = 8             ' Maze grid element size.  Range: 4 to 8.  With a smaller block size a larger maze can be created by modifying the DATA statements at the end of the main program.
CONST DEFAULTTIME = 60          ' Seconds allowed to complete a maze at level 1.  Decreases by 5 seconds in each later level.  Range 30 to 100.
CONST DEFAULTMONSTERTIME = .5   ' Range: .2 to 3.0   A lower number makes the monsters move faster.
CONST SHOWDELAY = .2            ' Seconds that the maze solution will be shown.  Range 0 to 10.
CONST DEFAULTPLAYERS = 1        ' Range 1 to 2.
CONST DEFAULTLEVEL = 1          ' Range 1 to 5.
CONST DEFAULTMONSTERS = 0       ' Range 0 to 10.
CONST DEFAULTSHOWMAZE = -1      ' Range 0 (FALSE) or -1 (TRUE).
CONST DEFAULTNUMMAZES = 1       ' Range 1 to 10.
CONST MAZECOLOR7 = 7            ' Color of the maze (EGA, VGA graphics).  Range: 1 to 15, but not the same as GameBkGround7.
CONST PLAYER1COLOR7 = 10        ' Color of player 1 (EGA, VGA graphics).  Range: 1 to 15, but not the same as GameBkGround7.
CONST PLAYER2COLOR7 = 13        ' Color of player 2 (EGA, VGA graphics).  Range: 1 to 15, but not the same as GameBkGround7.
CONST MONSTERCOLOR7 = 12        ' Color of the monsters (EGA, VGA graphics).  Range: 1 to 15, but not the same as GameBkGround7.
CONST MAZECOLOR1 = 3            ' Color of the maze (CGA graphics).  Range 1 to 3, but not the same as GameBkGround1.
CONST PLAYER1COLOR1 = 1         ' Color of player 1 (CGA graphics).  Range: 1 to 3, but not the same as GameBkGround1.
CONST PLAYER2COLOR1 = 2         ' Color of player 2 (CGA graphics).  Range: 1 to 3, but not the same as GameBkGround1.
CONST MONSTERCOLOR1 = 3         ' Color of the monsters (CGA graphics).  Range: 1 to 3, but not the same as GameBkGround1.
' The following sound constants are used by the PLAY command to
' produce music during the game.  To change the sounds you hear, change
' these constants.  Refer to the online help for PLAY for the correct format.
' To completely remove sound from the game set the constants equal to null.
' For example:  STARTOFGAMESOUND = ""
CONST STARTOFGAMESOUND = "MBT145O1L8B-O2DL4E-L8O1A-O2CL4D-L8O2CEFE-DO1B-O2CO1B-"  ' Played during Displayintro subroutine.
CONST ENDOFGAMESOUND = "MBT200O1L6EBAEL7A"           ' Played when all the mazes have been played or 'Q' was chosen when the game was paused.
CONST ENDOFMAZESOUND = "MBT190n70n60n50n40"          ' Played when a maze is completed
CONST PLAYERDEATHSOUND = "MBT255o0l10n10l7n7l10n4"   ' Played when you lose a life or run out of time.
CONST PLAYEROUTSOUND = "MBT140L64n40n35n30"          ' Played when a player exits the maze

'The next section contains CONST statements that are not changeable.
CONST FALSE = 0                 ' FALSE for Boolean operations.
CONST TRUE = -1                 ' TRUE for Boolean operations.
CONST UP2 = 18                  ' Keyboard scan code for the E key.
CONST DOWN2 = 32                ' Keyboard scan code for the D key.
CONST LEFT2 = 31                ' Keyboard scan code for the S key.
CONST RIGHT2 = 33               ' Keyboard scan code for the F key.
CONST CTOP = 1                  ' Keyboard code for up in internal maze directions.
CONST CBOTTOM = 2               ' Keyboard code for down in internal maze directions.
CONST CLEFT = 3                 ' Keyboard code for left in internal maze directions.
CONST CRIGHT = 4                ' Keyboard code for right in internal maze directions.
CONST MAXMONSTERS = 10          ' Maximum number of monsters.
CONST MAXLEVEL = 5              ' Maximum number of levels.
CONST MAXMAZES = 10             ' Maximum number of mazes.
CONST SCREENWIDTH = 80          ' Text mode screen width.
CONST GAMEBKGROUND7 = 1         ' Color to use to erase objects (EGA, VGA graphics).
CONST GAMEBKGROUND1 = 0         ' Color to use to erase objects (CGA graphics).  In CGA mode, 0 means the color of the background.

' DECLARE statement tell the main program that subprograms and
' functions exist and defines what data types they use.
DECLARE SUB BustOut (x%, y%)
DECLARE SUB CancelDirection (move AS ANY, direction%)
DECLARE SUB Center (text$, row%)
DECLARE SUB ChangeWall (x%, y%, direction%, RepCh%)
DECLARE SUB ClosePath (move AS ANY)
DECLARE SUB CompleteMaze ()
DECLARE SUB ConvertDirToXY (direction%, xmove%, ymove%)
DECLARE SUB DisplayChanges ()
DECLARE SUB DisplayGameTitle ()
DECLARE SUB DisplayIntro ()
DECLARE SUB DisplayWinner ()
DECLARE SUB DrawMonster (x%, y%, WhatColor%)
DECLARE SUB DrawPlayer (Player%, WhatColor%)
DECLARE SUB GenerateMaze ()
DECLARE SUB GenerateMove (move AS ANY)
DECLARE SUB GetGameOptions ()
DECLARE SUB InitVariables ()
DECLARE SUB Keys (onoff%)
DECLARE SUB MonsterControl ()
DECLARE SUB PlayGame ()
DECLARE SUB PopMove (move AS ANY)
DECLARE SUB PrintBlock (block AS ANY, x%, y%, WhatColor%)
DECLARE SUB PrintMaze ()
DECLARE SUB ProcessPlayerInput (i%)
DECLARE SUB UpdatePosition (Dir%, Plr%)
DECLARE FUNCTION CheckForClosedArea% (move AS ANY)
DECLARE FUNCTION CreatePath% ()
DECLARE FUNCTION GetMonsterDirection% (x%, y%, direction%)
DECLARE FUNCTION NumberOfWalls% (x%, y%)
DECLARE FUNCTION ValidBustDir% (x%, y%)

' This section contains TYPE definitions:
TYPE MazeType             ' This type contains maze information of a single block.
	top AS INTEGER        ' Tells if top block side exists.
	Bottom AS INTEGER     ' Tells if bottom block side exists.
	Left AS INTEGER       ' Tells if left block side exists.
	Right AS INTEGER      ' Tells if right block side exists.
END TYPE

TYPE MoveType             ' MoveType contains information about moves within the maze.
	x AS INTEGER          ' X coordinate of a move.
	y AS INTEGER          ' Y coordinate of a move.
	direction AS INTEGER  ' Direction of a move 1 to 4.
	Spaces AS INTEGER     ' Number of hexes to move in a row during maze creation.
	top AS INTEGER        ' Tells if top block side exists.
	Bottom AS INTEGER     ' Tells if bottom block side exists.
	Left AS INTEGER       ' Tells if left block side exists.
	Right AS INTEGER      ' Tells if right block side exists.
END TYPE

TYPE PlayerType           ' This type is used to keep track of information about each player.
	x AS INTEGER          ' Player's horizontal position.
	y AS INTEGER          ' Player's vertical position.
	PColor AS INTEGER     ' Player's color.
	Dead AS INTEGER       ' -1 means the player is dead, 0 means the player is alive.
	TimeLeft AS INTEGER   ' Temporary time left for each player.
	Done AS INTEGER       ' -1 means the player is done with current maze, 0 means the player is not done.
	Score AS LONG         ' Player points.
END TYPE

TYPE MonsterType          ' This type is used to keep track of information about each monster.
	x AS INTEGER          ' Monster's horizontal position.
	y AS INTEGER          ' Monster's vertical position.
	direction AS INTEGER  ' Monster's direction of movement.
	Active AS INTEGER     ' -1 means the monster is active, 0 means the monster is frozen and inactive.
END TYPE

CLEAR , , 5120        'Set up a large stack for all key processing.

' This section contains COMMON SHARED variables.
DIM SHARED MazeWidth AS INTEGER          ' Width of maze, in blocks.
DIM SHARED MazeHeight AS INTEGER         ' Height of maze, in blocks.
DIM SHARED PathLength AS INTEGER         ' Minimum number of blocks the valid path must be go before trying to turn.
DIM SHARED TurnRate AS INTEGER           ' Maximum number of blocks the valid path may go before trying to turn
DIM SHARED ShowMaze AS INTEGER           ' -1 cause the full maze to be shown, 0 causes only the outer edge to be shown.
DIM SHARED Level AS INTEGER              ' Play level.
DIM SHARED AvailMonsters AS INTEGER      ' Number of monsters.
DIM SHARED NumOfPlayers AS INTEGER       ' Number of players.
DIM SHARED NumOfMazes AS INTEGER         ' Number of mazes to run before completing a level.
DIM SHARED MazeTime AS INTEGER           ' Time allowed to complete a maze.
DIM SHARED StackPointer AS INTEGER       ' Points to current place on the stack.
DIM SHARED MazeOver AS INTEGER           ' -1 means an event has ended one run through a maze.
DIM SHARED GameOver AS INTEGER           ' -1 means an event has caused the game to end.
DIM SHARED MazesFinished AS INTEGER      ' Number of mazes completed.
DIM SHARED NumMonsters AS INTEGER        ' Number of monsters currently on the screen.
DIM SHARED MazeColor AS INTEGER          ' Color of maze.
DIM SHARED GameBkGround AS INTEGER       ' Background color.
DIM SHARED MonsterColor AS INTEGER       ' Color of the monsters.
DIM SHARED ScreenMode AS INTEGER         ' Screen mode.
DIM SHARED EntryX AS INTEGER             ' X coordinate of the maze entrance.
DIM SHARED EntryY AS INTEGER             ' Y coordinate of the maze entrance.
DIM SHARED ExitX AS INTEGER              ' X coordinate of the maze exit.
DIM SHARED ExitY AS INTEGER              ' Y coordinate of the maze exit.
DIM SHARED StartX AS INTEGER             ' X coordinate of the maze.
DIM SHARED StartY AS INTEGER             ' Y coordinate of the maze.
DIM SHARED CountDown AS INTEGER          ' Number of seconds left to complete a maze.
DIM SHARED MazeError AS INTEGER          ' Indicates if a maze error occurred.
DIM SHARED MonsterUpdateTime AS SINGLE   ' Delay between monster movements.
DIM SHARED ContinueDirection AS SINGLE   ' Probability of the monster continuing in the same direction.
DIM SHARED Player(1 TO 2) AS PlayerType  ' Information about each player.
DIM SHARED PlayerMove(1 TO 2) AS INTEGER ' Temporary user move from the queue.
REDIM SHARED stackVar(0)  AS MoveType    ' All maze creation moves.
REDIM SHARED MazeArray(0, 0) AS MazeType ' Maze information.
REDIM SHARED Monsters(0) AS MonsterType  ' Monster information.
DIM KeyFlags AS INTEGER                  ' Internal state of keyboard flags when game starts. Hold the state so it can be restored when the game ends.
DIM BadMode AS INTEGER                   ' Store the status of a valid screen mode.

' The module-level code of QMAZE begins here!

RANDOMIZE TIMER MOD 32768                ' Causes the mazes to be different.

'Determines which graphics mode to use.
ON ERROR GOTO ScreenError           ' Set an error trap for testing valid screen mode.
BadMode = FALSE                     ' Assume the graphics mode is okay.
ScreenMode = 7                      ' First try mode 7.
SCREEN ScreenMode                   ' Attempt to go into SCREEN 7 (EGA screen).
IF BadMode = TRUE THEN              ' If this attempt failed.
	ScreenMode = 1                  ' Try mode 1 - a CGA screen.
	BadMode = FALSE                 ' Again, assume that graphics mode is okay.
	SCREEN ScreenMode               ' Attempt to go into SCREEN 1.
END IF
ON ERROR GOTO 0                     ' Turn off error handling.

IF BadMode = TRUE THEN              ' If no graphics adapter...
	CLS
	LOCATE 10, 13: PRINT "CGA, EGA Color, or VGA graphics required to run QMAZE.BAS"
ELSE
	DEF SEG = 0                     ' Set the current segment to the low memory area.
	KeyFlags = PEEK(1047)           ' Read the location that stores the keyboard flag.
	POKE 1047, &H0                  ' Force them off.
	DEF SEG                         ' Restore the default segment.

	DisplayIntro                    ' Display the introduction screen now.
	DO
		GetGameOptions              ' Get the user choices for game.
		InitVariables               ' Initialize keys and variables.
		DO
			SCREEN ScreenMode       ' Set appropriate screen mode.
			COLOR GameBkGround, 1
			CLS                     ' Clear the screen.
			GenerateMaze            ' Create a new maze.
			PlayGame                ' Play the current maze.
			DisplayWinner           ' Show who won, etc.
		LOOP WHILE NOT GameOver

		LOCATE 22, 11               ' See if player wants to play again.
		PRINT "Play again? (Y/N)"
		DO
			k$ = UCASE$(INKEY$)     ' Wait for any key to be pressed.
		LOOP WHILE (k$ <> "Y" AND k$ <> "N")
		IF k$ = "Y" THEN GameOver = FALSE
	LOOP WHILE NOT GameOver

	DisplayChanges                  ' Display suggested changes screen.
	CLS

	DEF SEG = 0                     ' Restore the previous flag settings.
	POKE 1047, KeyFlags
	DEF SEG

END IF

END

' The subroutines below are called when the player presses a key to move.

MovePlayer1Up:
	PlayerMove(1) = CTOP
	ProcessPlayerInput 1
	RETURN

MovePlayer1Down:
	PlayerMove(1) = CBOTTOM
	ProcessPlayerInput 1
	RETURN

MovePlayer1Left:
	PlayerMove(1) = CLEFT
	ProcessPlayerInput 1
	RETURN

MovePlayer1Right:
	PlayerMove(1) = CRIGHT
	ProcessPlayerInput 1
	RETURN

MovePlayer2Up:
	PlayerMove(2) = CTOP
	ProcessPlayerInput 2
	RETURN

MovePlayer2Down:
	PlayerMove(2) = CBOTTOM
	ProcessPlayerInput 2
	RETURN

MovePlayer2Left:
	PlayerMove(2) = CLEFT
	ProcessPlayerInput 2
	RETURN

MovePlayer2Right:
	PlayerMove(2) = CRIGHT
	ProcessPlayerInput 2
	RETURN

PauseGame:
	Keys (2)                            ' Ensure that no other interrupts happen at the same time.
	TIMER OFF
	SOUND 1100, .75                     ' Tone at 1100 hertz for 75 clock ticks.
	Center SPACE$(13) + "* PAUSED *" + SPACE$(13), 1   'Display pause message.
	WHILE INKEY$ = "": WEND             ' Wait for a key to be pressed.
	Center SPACE$(36), 1                ' Clear prompt.
	Keys (1)                            ' Allow interrupts to fire normally.
	TIMER ON
	RETURN

QuitGame:
	Keys (2)                            ' Ensure that no other interrupts happen at the same time.
	TIMER OFF
	SOUND 600, .5
	SOUND 800, .5
	Center SPACE$(10) + "Really quit? (Y/N)" + SPACE$(10), 1 ' Display prompt.
	DO
		k$ = UCASE$(INKEY$)             ' Wait for desired key to be pressed.
	LOOP WHILE k$ = ""
	Center SPACE$(39), 1                ' Clear prompt from the screen.

	IF k$ = "Y" THEN GameOver = TRUE
	Keys (1)                            ' Turn keys back on.
	TIMER ON
	RETURN

TimerUpdate:
	SOUND 500, .1
	CountDown = CountDown - 1           ' Reduce the time-left variable by one.
	LOCATE 1, 13: PRINT CountDown
	RETURN

ScreenError:                            ' Screen test error handling routine.
	BadMode = TRUE
	RESUME NEXT

MazeErrorHandler:                       ' Maze creation error handler.
	MazeError = TRUE
	RESUME NEXT


' The following Data Statements contain information about the Mazes at
' different levels.  The Data is read in from GetGameOptions.  Each row of
' DATA is information for a level.  The first row for Level one and so
' on.  The entries are read from left to right into the following variables:
' mazeWidth, mazeHeight, pathLength, TurnRate.
'
'           THESE DATA STATEMENTS MAY BE CHANGED WITHIN LIMITS
'
' The first entry is the width of the maze.  Range 20 to 38
' The second entry is the height of the maze. Range 15 to 20
' The third entry is the minimum length of the path.  Range 25 to 60
' The greater the value the  harder the maze is. The smaller the maze
' the shorter the PathLength should be.
'
' WARNING: A Large PathLength with a small maze can cause the maze
' generator to produce an error.
'
' The fourth entry is the turn rate of the correct path. Range 3 to 7.
'
DATA 20,20,30,3
DATA 25,20,40,3
DATA 30,20,50,5
DATA 35,20,60,7
DATA 35,20,60,5

' The total number of levels can be changed if extra DATA statements are added
' after the existing ones.  They must have the same format as the existing
' DATA statements.  You must also change the constant maxLevel to add
' additional levels.

'---------------------------------------------------------------------------
' BustOut
'
'   Starts at a given block and creates a path (removing walls) until it
'   encounters a block that does not have four walls. Uses the SHARED
'   TurnRate variable to decide how far to go in one direction before turning.
'   This keeps the rest of the maze consistent with the valid path.
'
'       PARAMETERS: X - X coordinate of the starting block
'                   Y - Y coordinate of the starting block
'---------------------------------------------------------------------------
SUB BustOut (x, y)

	BustedOut = FALSE
	currx = x                                   ' Set current X & Y position.
	curry = y
	DO WHILE NOT BustedOut
		direction = ValidBustDir(currx, curry)  ' Set direction.
		ConvertDirToXY direction, xmove, ymove  ' Convert direction to coordinates.
		Spaces = INT(RND(1) * TurnRate) + 1
		ValidMove = TRUE
		BlocksOpened = 1
		DO WHILE BlocksOpened <= Spaces AND ValidMove
			ChangeWall currx, curry, direction, 0
			currx = currx + xmove
			curry = curry + ymove
			IF currx + xmove < 1 OR currx + xmove > MazeWidth OR curry + ymove < 1 OR curry + ymove > MazeHeight THEN
				ValidMove = FALSE
			END IF
			IF MazeArray(currx, curry).top + MazeArray(currx, curry).Bottom + MazeArray(currx, curry).Left + MazeArray(currx, curry).Right <> 3 THEN
				ValidMove = FALSE
				BustedOut = TRUE
			END IF
			BlocksOpened = BlocksOpened + 1
		LOOP
	LOOP

END SUB

'--------------------------------------------------------------------------
'
' CancelDirection
'
'   Takes a direction value and cancels it as a valid move
'   direction in the move type variable which is passed to it.
'
'           PARAMETERS:    move      - Allowable directions to move
'                          direction - Direction to cancel legal move
'--------------------------------------------------------------------------
SUB CancelDirection (move AS MoveType, direction)

	SELECT CASE direction           ' Determine direction.
		CASE CTOP
			move.top = 1            ' Cancel move up.
		CASE CBOTTOM
			move.Bottom = 1         ' Cancel move down.
		CASE CLEFT
			move.Left = 1           ' Cancel move left.
		CASE CRIGHT
			move.Right = 1          ' Cancel move right.
	END SELECT

END SUB

'--------------------------------------------------------------------------
' Center
'
'   Centers the given text string on the indicated row.
'
'           PARAMETERS:   text$ - The text to be centered
'                         row   - The screen row to print on
'--------------------------------------------------------------------------
SUB Center (text$, row)

	' Calculate the starting column. Subtract the string length from
	' ScreenWidth the divide that value by 2.

	LOCATE row%, (SCREENWIDTH - LEN(text$)) \ 2 + 1

	PRINT text$;            ' Print the text.

END SUB

'---------------------------------------------------------------------------
' ChangeWall
'
'   Replaces a wall given the coordinates and the direction to replace.
'   The routine actually has to replace two walls.  It needs to replace
'   the adjoining wall in the adjoining block.
'
'           PARAMETERS:    x         - X coordinate of the block whose wall
'                                      will be replaced
'                          y         - Y coordinate of the block whose wall
'                                      will be replaced
'                          direction - Direction of the wall to be replaced
'                          repch     - Designates whether to replace or erase
'                                      (1 = replace, 0 = remove)
'---------------------------------------------------------------------------
SUB ChangeWall (x, y, direction, RepCh)

	SELECT CASE direction
	CASE CTOP
		MazeArray(x, y).top = RepCh
		MazeArray(x, y - 1).Bottom = RepCh
	CASE CBOTTOM
		MazeArray(x, y).Bottom = RepCh
		MazeArray(x, y + 1).top = RepCh
	CASE CLEFT
		MazeArray(x, y).Left = RepCh
		MazeArray(x - 1, y).Right = RepCh
	CASE CRIGHT
		MazeArray(x, y).Right = RepCh
		MazeArray(x + 1, y).Left = RepCh
	END SELECT

END SUB

'--------------------------------------------------------------------------
' CheckForClosedArea
'
'   Prevents the path from shutting itself into a closed area from which it
'   might take a while to back its way out of. While this routine does not
'   wholly prevent this, it eliminates many of the possibilities which take
'   time to check. The routine accomplishes this by checking to see if the
'   current move will cause the path to touch itself.  If the path is going
'   to touch, it retraces the path until it comes to the block which was
'   touched.  If any of the blocks along the way back has an untouched block
'   (four walls) on opposite sides of it, it can be assumed that the move has
'   a high chance of causing the path to enclose itself.
'
'           PARAMETERS:    move - Starting location to check path
'--------------------------------------------------------------------------
FUNCTION CheckForClosedArea (move AS MoveType)

	CheckForClosedArea = FALSE
	
	ConvertDirToXY move.direction, xmove, ymove             ' Get the location of where the current move goes.
	currx = move.x + xmove * move.Spaces
	curry = move.y + ymove * move.Spaces
	Touching = FALSE

	'Check to see if moving up or down
	IF move.direction = CTOP OR move.direction = CBOTTOM THEN
		
		IF currx + 1 <= MazeWidth THEN                      ' Check the right side to find if wall is touched.
			IF NumberOfWalls(currx + 1, curry) <> 4 THEN
				TouchedX = currx + 1
				TouchedY = curry
				Touching = TRUE
			END IF
		END IF
		
		IF currx - 1 >= 1 THEN                              ' Check the left side to find if wall is touched.
			IF NumberOfWalls(currx - 1, curry) <> 4 THEN
				TouchedX = currx - 1
				TouchedY = curry
				Touching = TRUE
			END IF
		END IF
		
		FOR xcheck = -1 TO 1                                ' Check ahead-left, straight-ahead and ahead-right to find if wall is touched.
			IF curry + ymove >= 1 AND curry + ymove <= MazeHeight AND currx + xcheck >= 1 AND currx + xcheck <= MazeWidth THEN
				IF NumberOfWalls(currx + xcheck, curry + ymove) <> 4 THEN
					TouchedX = currx + xcheck
					TouchedY = curry + ymove
					Touching = TRUE
				END IF
			END IF
		NEXT xcheck
	END IF
	
	
	IF move.direction = CLEFT OR move.direction = CRIGHT THEN ' Check to see if moving left or right
		
		IF curry + 1 <= MazeHeight THEN                      ' Check top to find if wall is touched.
			IF NumberOfWalls(currx, curry + 1) <> 4 THEN
				TouchedX = currx
				TouchedY = curry + 1
				Touching = TRUE
			END IF
		END IF
		
		IF curry - 1 >= 1 THEN                               ' Check bottom to find if wall is touched.
			IF NumberOfWalls(currx, curry - 1) <> 4 THEN
				TouchedX = currx
				TouchedY = curry - 1
				Touching = TRUE
			END IF
		END IF

		
		FOR ycheck = -1 TO 1                                 ' Check ahead-top, straight-ahead and ahead-bottom to find if wall is touched.
			IF currx + xmove >= 1 AND currx + xmove <= MazeWidth AND curry + ycheck >= 1 AND curry + ycheck <= MazeHeight THEN
				IF NumberOfWalls(currx + xmove, curry + ycheck) <> 4 THEN
				TouchedX = currx + xmove
				TouchedY = curry + ycheck
				Touching = TRUE
			END IF
			END IF
		NEXT ycheck
	END IF
	
	IF NOT Touching THEN EXIT FUNCTION                      ' If the wall hasn't been touched, there is no problem.

	' There was a touch, so check for an untouched box on opposite side for each block in the path.
	SavePointer = StackPointer     'Ensure the move stack is returned to its proper position.
	DIM CheckMove AS MoveType
	CheckMove = move
	UntouchedBlocks = FALSE
	FoundTouch = FALSE

	' Loop until either the block that was touched is found, or we find a block with untouched blocks on opposite sides.
	WHILE NOT UntouchedBlocks AND NOT FoundTouch
		ConvertDirToXY CheckMove.direction, xmove, ymove
		currx = CheckMove.x
		curry = CheckMove.y
		SpacesChecked = 1

		' Check through all the blocks in one move.
		DO WHILE NOT UntouchedBlocks AND NOT FoundTouch AND SpacesChecked <= CheckMove.Spaces
			currx = currx + xmove
			curry = curry + ymove
			
			IF currx = TouchedX AND curry = TouchedY THEN  ' Found the place where the wall is touched.
				FoundTouch = TRUE
				EXIT DO
			END IF
			' Get the number of walls for each block on all sides of the path block. Edges count as untouched blocks.
			IF currx + 1 <= MazeWidth THEN
				RightBox = NumberOfWalls(currx + 1, curry)
			ELSE
				RightBox = 4
			END IF
			IF currx - 1 >= 1 THEN
				LeftBox = NumberOfWalls(currx - 1, curry)
			ELSE
				LeftBox = 4
			END IF
			IF curry + 1 <= MazeHeight THEN
				BottomBox = NumberOfWalls(currx, curry + 1)
			ELSE
				BottomBox = 4
			END IF
			IF curry - 1 >= 1 THEN
				TopBox = NumberOfWalls(currx, curry - 1)
			ELSE
				TopBox = 4
			END IF
			
			IF RightBox = 4 AND LeftBox = 4 THEN    ' Check to see if opposite blocks are untouched.
				UntouchedBlocks = TRUE
				CheckForClosedArea = TRUE
			END IF
			IF TopBox = 4 AND BottomBox = 4 THEN
				UntouchedBlocks = TRUE
				CheckForClosedArea = TRUE
			END IF
			SpacesChecked = SpacesChecked + 1
		LOOP
		PopMove CheckMove
	WEND
	StackPointer = SavePointer                      ' Restore the stack to its original place.

END FUNCTION

'---------------------------------------------------------------------------
' ClosePath
'
'   Puts back all the walls opened by a given move.  This is used when a
'   move is popped off the stack because it has no more valid directions.
'
'               PARAMETERS:   move - The move to close path from
'--------------------------------------------------------------------------
SUB ClosePath (move AS MoveType)

	ConvertDirToXY move.direction, xmove, ymove
	x = move.x
	y = move.y
	FOR i = 1 TO move.Spaces
		ChangeWall x, y, move.direction, 1
		x = x + xmove
		y = y + ymove
		PrintBlock MazeArray(x, y), x, y, MazeColor
	NEXT i

END SUB

'---------------------------------------------------------------------------
' CompleteMaze
'
'   Finishes breaking out the walls of the maze after a valid path has been
'   found.  It does this by first picking random points along the valid path
'   and calling the BustOut routine from these points. This helps ensure that
'   there will be many alternative accessible paths.  After it chooses the
'   random points, it checks each block in the maze to make sure that it
'   does not have four walls.  If there are blocks with four walls, the
'   BustOut routine is called with this block as a starting point.
'
'                       PARAMETERS:     None
'---------------------------------------------------------------------------
SUB CompleteMaze

	DIM TempMove AS MoveType
	SavePointer = StackPointer          ' Preserve the stack pointer.
	
	DO WHILE SavePointer > 1            ' Loop from the end of the path back to the beginning.

		'Choose which block on the valid path to start the BustOut routine.
		Interval = TurnRate + INT(RND(1) * TurnRate) + 1
		SpacesPassed = 0
		WHILE SpacesPassed < Interval AND StackPointer > 1
		  PopMove TempMove
		  SpacesPassed = SpacesPassed + TempMove.Spaces
		WEND
		IF StackPointer <= 1 THEN EXIT DO
		BustOut TempMove.x, TempMove.y
	LOOP

	StackPointer = SavePointer          ' Restore the stack pointer.
	FOR ycheck = 1 TO MazeHeight        ' Check each block in the maze to ensure that it does not have four walls.
		FOR xcheck = 1 TO MazeWidth
			IF MazeArray(xcheck, ycheck).top + MazeArray(xcheck, ycheck).Bottom + MazeArray(xcheck, ycheck).Left + MazeArray(xcheck, ycheck).Right = 4 THEN
				BustOut xcheck, ycheck
			END IF
		NEXT xcheck
	NEXT ycheck

END SUB

'----------------------------------------------------------------------------
' ConvertDirToXY
'
'   Converts directions into the horizontal and vertical movements needed
'   to move in a specified direction.
'
'               PARAMETERS:   Direction - Movement direction
'                             xmove     - X coordinate of next position
'                             ymove     - Y coordinate of next position
'----------------------------------------------------------------------------
SUB ConvertDirToXY (direction, xmove, ymove)

	SELECT CASE direction
		CASE CTOP
			ymove = -1: xmove = 0
		CASE CBOTTOM
			ymove = 1: xmove = 0
		CASE CLEFT
			ymove = 0: xmove = -1
		CASE CRIGHT
			ymove = 0: xmove = 1
		CASE ELSE
			ymove = 0: xmove = 0
	END SELECT

END SUB

'---------------------------------------------------------------------------
' CreatePath
'
'   This is the main controlling routine which determines a valid path through
'   the maze.  It contains two main loops.  The outer loop is executed once
'   for each move.  The inner loop is executed once for each block in the move.
'   The basic flow of the loops is as follows.  First, a random move is
'   generated.  The move is then checked to make sure it doesn't run into a
'   wall or try to enclose itself.  If the move passes the tests, it is added
'   to the stack of moves.  If the end of the path is on an edge, and the path
'   has travelled the minimum distance, then the path is done.  If a move does
'   not pass a test, then the move is deleted and the previous move is told
'   that it cannot move in that direction.  If all of the directions have been
'   used up, then that move is removed from the stack, and the move previous
'   to that is told it can't move in that direction.  By doing this, it is
'   assured that the maze will eventually be solved.  However, It may take
'   the maze generator a while to back all the way out of situations where
'   it loops on itself. Routines were added to try to cut down on this
'   happening to some degree of success.  However, small mazes with long
'   minimum path lengths may still take a while to generate.  If an error is
'   generated during maze creation then FALSE is returned by CreatePath,
'   ELSE CreatePath is TRUE.
'
'                       PARAMETERS:   None
'----------------------------------------------------------------------------
FUNCTION CreatePath
	
	ON ERROR GOTO MazeErrorHandler              ' Initialize error trapping.

	touched = FALSE
	currx = EntryX
	curry = EntryY
	DIM CurrMove AS MoveType
	DIM newmove AS MoveType
	CurrMove.x = EntryX
	CurrMove.y = EntryY
	finished = FALSE
	TimeOfMaze! = TIMER                         ' Initialize error detection.
	MazeError = FALSE

	' Loop until an edge is found after travelling the minimum path length .
	WHILE NOT finished
		ValidMove = FALSE
		GenerateMove CurrMove     'Get a move to check.
		'Loop until a valid move is generated
		WHILE ValidMove = FALSE
			'Perform Error checking
			IF (TIMER - TimeOfMaze! > 8) OR (MazeError = TRUE) THEN
				CreatePath = FALSE
				EXIT FUNCTION
			END IF

			ConvertDirToXY CurrMove.direction, xmove, ymove
			ValidMove = TRUE
			SpacesChecked = 1
			currx = CurrMove.x
			curry = CurrMove.y

			' Check each block in the move.
			DO WHILE SpacesChecked <= CurrMove.Spaces AND ValidMove
				currx = currx + xmove
				curry = curry + ymove

				' Ensure no wall was run into.  If it was, change the spaces so move stops at the wall.
				IF currx < 1 OR currx > MazeWidth OR curry < 1 OR curry > MazeHeight THEN
					CurrMove.Spaces = SpacesChecked - 1
					EXIT DO
				END IF

				' Check to see if our own path has been run into.
				IF NumberOfWalls(currx, curry) <> 4 THEN

					' If moved one space, then it is not a valid direction, cancel it.
					IF SpacesChecked = 1 THEN
						ValidMove = FALSE
						CancelDirection CurrMove, CurrMove.direction

						' If there are no more directions to choose from, get rid of the last move.
						IF CurrMove.top + CurrMove.Bottom + CurrMove.Left + CurrMove.Right = 4 THEN
							PopMove CurrMove
							totalpathlen = totalpathlen - CurrMove.Spaces
							CancelDirection CurrMove, CurrMove.direction
							ClosePath CurrMove
						END IF
						GenerateMove CurrMove       ' Generate a new move to check.
					ELSE
						' Change the spaces before crossing out own path.
						CurrMove.Spaces = SpacesChecked - 1
						EXIT DO
					END IF
				ELSE
					SpacesChecked = SpacesChecked + 1
				END IF
			LOOP

			' Make sure that the path is not closing itself off.
			IF ValidMove AND CheckForClosedArea(CurrMove) THEN
				ValidMove = FALSE
				CancelDirection CurrMove, CurrMove.direction
				GenerateMove CurrMove
			END IF
		WEND

		IF StackPointer < PathLength * 10 THEN      ' Push the move now.
			StackPointer = StackPointer + 1
			stackVar(StackPointer) = CurrMove
		ELSE
			MazeError = TRUE
		END IF

		' Process the current move here.
		ConvertDirToXY CurrMove.direction, xmove, ymove
		x = CurrMove.x
		y = CurrMove.y
		FOR i = 1 TO CurrMove.Spaces
			ChangeWall x, y, CurrMove.direction, 0
			x = x + xmove
			y = y + ymove
			PrintBlock MazeArray(x, y), x, y, MazeColor  ' Show the path block.
		NEXT i

		currx = CurrMove.x + CurrMove.Spaces * xmove
		curry = CurrMove.y + CurrMove.Spaces * ymove
		newmove.x = currx
		newmove.y = curry
		totalpathlen = totalpathlen + CurrMove.Spaces
		CurrMove = newmove

		' Check to see if we are at a wall.
		IF currx = MazeWidth OR curry = MazeHeight OR currx = 1 OR curry = 1 THEN

			' If so, is it time to exit?
			IF totalpathlen >= PathLength THEN
				ExitX = currx
				ExitY = curry
				finished = TRUE
				IF ExitY = MazeHeight THEN
					MazeArray(ExitX, ExitY).Bottom = 0
				ELSEIF ExitY = 1 THEN
					MazeArray(ExitX, ExitY).top = 0
				ELSEIF ExitX = MazeWidth THEN
					MazeArray(ExitX, ExitY).Right = 0
				ELSEIF ExitX = 1 THEN
					MazeArray(ExitX, ExitY).Left = 0
				END IF
			ELSE
				' Ensure we turn away from the exit to prevent the path from
				' closing itself off.  If the path has touched the far wall already
				' then touching one of the side walls is no problem.
				IF currx = 1 OR currx = MazeWidth THEN
					IF EntryY = 1 AND NOT touched THEN CancelDirection CurrMove, CTOP
					IF EntryY = MazeHeight AND NOT touched THEN CancelDirection CurrMove, CBOTTOM
					IF EntryX = currx THEN
						IF curry > EntryY THEN
							CancelDirection CurrMove, CTOP
						ELSE
							CancelDirection CurrMove, CBOTTOM
						END IF
					END IF
					IF currx = 1 AND EntryX = MazeWidth THEN touched = 1
					IF currx = MazeWidth AND EntryX = 1 THEN touched = 1
				END IF
				IF curry = 1 OR curry = MazeHeight THEN
					IF EntryX = 1 AND NOT touched THEN CancelDirection CurrMove, CLEFT
					IF EntryX = MazeWidth AND NOT touched THEN CancelDirection CurrMove, CRIGHT
					IF EntryY = curry THEN
						IF currx > EntryX THEN
							CancelDirection CurrMove, CLEFT
						ELSE
							CancelDirection CurrMove, CRIGHT
						END IF
					END IF
					IF curry = 1 AND EntryY = MazeHeight THEN touched = TRUE
					IF curry = MazeHeight AND EntryY = 1 THEN touched = TRUE
				END IF
			END IF
		END IF
	WEND

	IF NOT MazeError THEN CreatePath = TRUE ELSE CreatePath = FALSE
	ON ERROR GOTO 0

END FUNCTION

'--------------------------------------------------------------------------
' DisplayChanges
'
'   Displays a list of changes that the player can easily make.
'
'          PARAMETERS:     None
'--------------------------------------------------------------------------
SUB DisplayChanges
	
	DisplayGameTitle                ' Display game title.
	
	COLOR 7                         ' Set colors so text prints white.
	Center "The following game characteristics can be easily changed from", 5
	Center "within the QuickBASIC Interpreter.  To change the values of  ", 6
	Center "these characteristics, locate the corresponding CONST or DATA", 7
	Center "statements in the source code and change their values, then  ", 8
	Center "restart the program (press Shift+F5).                        ", 9
   
	COLOR 15                        ' Set foreground color to bright white.
	Center "Songs played during this game           ", 11
	Center "Size of the maze game grid              ", 12
	Center "How quickly the monsters move           ", 13
	Center "Initial time allowed per maze           ", 14
	Center "Color of Player 1                       ", 15
	Center "Color of Player 2                       ", 16
	Center "Color of the Monsters                   ", 17
	Center "Maze dimensions at each difficulty level", 18
	Center "Amount of time path preview is shown    ", 19
   
	COLOR 7
	Center "The CONST statements and instructions on changing them are   ", 21
	Center "located at the beginning of the main program.                ", 22
	
	DO WHILE INKEY$ = "": LOOP      ' Wait for any key to be pressed.
	CLS                             ' Clear screen.

END SUB

'---------------------------------------------------------------------------
' DisplayGameTitle
'
'   Displays title of the game.
'
'       PARAMETERS:   None
'---------------------------------------------------------------------------
SUB DisplayGameTitle
	
	SCREEN 0                            ' Set screen mode 0.
	WIDTH SCREENWIDTH, 25               ' Set width to 80, height to 25.
	COLOR 4, 0                          ' Set colors for red on black.
	CLS                                 ' Clear the screen.
	LOCATE 1, 2
	PRINT CHR$(201); STRING$(76, 205); CHR$(187); ' Draw top border.
	FOR i% = 2 TO 24                              ' Draw left and right borders.
		LOCATE i%, 2
		PRINT CHR$(186); TAB(79); CHR$(186);
	NEXT i%
	LOCATE 25, 2
	PRINT CHR$(200); STRING$(76, 205); CHR$(188); ' Draw bottom border.

	'Print game title centered at top of screen
	COLOR 0, 4                          ' Set colors to black on red.
	Center "     Microsoft     ", 1     ' Center game title on lines
	Center "     Q M A Z E     ", 2     ' 1 and 2.
	Center "   Press any key to continue   ", 25
	COLOR 7, 0

END SUB

'--------------------------------------------------------------------------
' DisplayIntro
'
'   Explains the object of the game and show how to play.
'
'           PARAMETERS:   None
'--------------------------------------------------------------------------
SUB DisplayIntro
	
	DisplayGameTitle                    ' Display game title
	COLOR 7                             ' Set colors so text prints white.
	Center "Copyright (C) 1990 Microsoft Corporation.  All Rights Reserved.", 4
	Center "You are moving through a maze at breakneck speed, racing the clock or ", 6
	Center "a friend.  The object is to finish the maze before time runs out      ", 7
	Center "or before your opponent finishes.  Monsters can be added to increase  ", 8
	Center "difficulty.  The amount of time allowed to complete the maze increases", 9
	Center "for every monster in the maze.  The player who finishes first wins    ", 10
	Center "the points for the maze.                                              ", 11
	COLOR 4                             ' Change foreground color for line to red.
	Center STRING$(74, 196), 13         ' Put horizontal red line on screen.
	COLOR 7                             ' Change foreground color back to white.
	Center " Game Controls ", 13        ' Display game controls.
	Center "  General           Player 1 Controls          Player 2 Controls  ", 15
	Center "                         (Up)                       (Up)          ", 17
	Center "  P - Pause               " + CHR$(24) + "                          E            ", 18
	Center "  Q - Quit       (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)       (Left) S   F (Right)  ", 19
	Center "                        " + CHR$(25) + "                          D          ", 20
	Center "                        (Down)                     (Down)         ", 21

	PLAY STARTOFGAMESOUND              'Play introductory melody.
	DO                                 'Wait for any key to be pressed.
		kbd$ = UCASE$(INKEY$)
	LOOP WHILE kbd$ = ""
	IF kbd$ = "Q" THEN                 'Allow player to quit now
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
' DisplayWinner
'
'   Displays a screen between mazes.
'
'           PARAMETERS:  None
'----------------------------------------------------------------------------
SUB DisplayWinner

	CLS                                     ' Clear the screen.
	LINE (5, 5)-(315, 195), , B             ' Draw a border.
	Diff = NumOfMazes - MazesFinished       ' Determine number of mazes remaining.
	LOCATE 3, 16
	IF Diff = 0 THEN PRINT "GAME OVER" ELSE PRINT "Maze #"; MazesFinished

	LOCATE 6, 5
	IF Player(1).Done THEN                  ' Display number of seconds player 1 took to complete the maze.
		PRINT "Player 1 finished in"; MazeTime - Player(1).TimeLeft; "seconds"
	ELSE
		PRINT "Player 1 did not finish!"
	END IF
 
	Temp = Player(1).TimeLeft - Player(2).TimeLeft
	LOCATE 10, 5
	IF Temp = 0 THEN
		IF NumOfPlayers = 2 THEN            ' Message based on number of players.
			PRINT "Tie maze: nobody wins this round"
		ELSE
			PRINT "Player 1 loses this round"
		END IF
	ELSE
		IF SGN(Temp) = 1 THEN Win = 1 ELSE Win = 2  ' Determine who won.
		PRINT "Player"; Win; "wins maze by"; ABS(Temp); "seconds!"
		Player(Win).Score = Player(Win).Score + ABS(Temp) * 100
	END IF
	LOCATE 13, 9: PRINT USING "Player 1 score: ###,###"; Player(1).Score
 
	IF NumOfPlayers = 2 THEN                ' For two players.
		LOCATE 8, 5
		IF Player(2).Done THEN
			PRINT "Player 2 finished in"; MazeTime - Player(2).TimeLeft; "seconds"
		ELSE
			PRINT "Player 2 did not finish!"
		END IF
		LOCATE 15, 9: PRINT USING "Player 2 score: ###,###"; Player(2).Score
	END IF

	DO: LOOP UNTIL INKEY$ = ""              ' Clear keyboard buffer.
	IF Diff > 0 AND NOT GameOver THEN
		LOCATE 20, 8: PRINT "Number of mazes left:"; Diff
		PLAY ENDOFMAZESOUND                 ' Play maze completion melody.
		LOCATE 23, 6: PRINT "<Press Spacebar for next maze>"
		MazesFinished = MazesFinished + 1   ' Add one to the number of mazes completed.
		WHILE INKEY$ <> " ": WEND
	ELSE
		IF NumOfPlayers = 2 THEN
			LOCATE 18, 5
			Hold& = Player(1).Score - Player(2).Score
			IF Hold& = 0 THEN
				PRINT "EQUAL POINTS - This is a tie game!"                  ' Announce a tie.
			ELSE
				IF SGN(Hold&) = 1 THEN Win = 1 ELSE Win = 2
				PRINT "PLAYER"; Win; "WINS GAME BY"; ABS(Hold&); "POINTS!"  ' Announce winner.
			END IF
		END IF
		GameOver = TRUE
		PLAY ENDOFGAMESOUND                 ' Play end-of-game melody.
	END IF
	
END SUB

'--------------------------------------------------------------------------
' DrawMonster
'
'   Draws a monster given the color and the x,y coordinate of where it is
'   in the maze.
'
'           PARAMETERS:    X         - X coordinate of the monster
'                          Y         - Y coordinate of the monster
'                          WhatColor - Color of the monster
'--------------------------------------------------------------------------
SUB DrawMonster (x, y, WhatColor)

	CIRCLE (StartX + x * BLOCKSIZE + BLOCKSIZE / 2, StartY + y * BLOCKSIZE + BLOCKSIZE / 2), (BLOCKSIZE / 2 - 2), WhatColor

END SUB

'---------------------------------------------------------------------------
' DrawPlayer
'
'   Draws a player given the color and player.
'
'           PARAMETERS:  Player    - Which player to draw
'                        WhatColor - Player color
'---------------------------------------------------------------------------
SUB DrawPlayer (Player, WhatColor)

	IF Player = 1 THEN
		LINE (StartX + Player(1).x * BLOCKSIZE + BLOCKSIZE / 4 + 1, StartY + Player(1).y * BLOCKSIZE + BLOCKSIZE / 4 + 1)-(StartX + Player(1).x * BLOCKSIZE + BLOCKSIZE * .75 - 1, StartY + Player(1).y * BLOCKSIZE + BLOCKSIZE * .75 - 1), WhatColor, BF
	ELSE
		LINE (StartX + Player(2).x * BLOCKSIZE + BLOCKSIZE / 4, StartY + Player(2).y * BLOCKSIZE + BLOCKSIZE / 4)-(StartX + Player(2).x * BLOCKSIZE + BLOCKSIZE * .75, StartY + Player(2).y * BLOCKSIZE + BLOCKSIZE * .75), WhatColor, B
	END IF

END SUB

'----------------------------------------------------------------------------
' GenerateMaze
'
'   Initializes the maze variables and picks a starting point for the maze,
'   then calls two routines which complete the maze.
'
'           PARAMETERS:   None
'---------------------------------------------------------------------------
SUB GenerateMaze
	
	REDIM MazeArray(MazeWidth, MazeHeight) AS MazeType   'Make sure the MazeArray is big enough.
	StackPointer = 1
	FOR i = 1 TO MazeWidth          ' Initialize the maze so every wall is up.
		FOR j = 1 TO MazeHeight
			MazeArray(i, j).Left = 1: MazeArray(i, j).Right = 1
			MazeArray(i, j).top = 1: MazeArray(i, j).Bottom = 1
		NEXT j
	NEXT i
	CLS                             ' Clear and show the maze grid.
	PrintMaze

	Entrydir = RND(1)               ' Choose the entry point for the maze.
	Entryside = RND(1)
	IF Entrydir > .5 THEN
		EntryX = INT(RND(1) * MazeWidth) + 1
		IF Entryside > .5 THEN
			EntryY = MazeHeight
			MazeArray(EntryX, EntryY).Bottom = 0
		ELSE
			EntryY = 1
			MazeArray(EntryX, EntryY).top = 0
		END IF
	ELSE
		EntryY = INT(RND(1) * MazeHeight) + 1
		IF Entryside > .5 THEN
			EntryX = MazeWidth
			MazeArray(EntryX, EntryY).Right = 0
		ELSE
			EntryX = 1
			MazeArray(EntryX, EntryY).Left = 0
		END IF
	END IF

	' With more complex mazes that require more moves, the stack array may
	' need to be made larger.  Currently, no problems.
	REDIM stackVar(1 TO PathLength * 10) AS MoveType

	IF NOT CreatePath THEN
		GenerateMaze                ' Call GenerateMaze again
	ELSE
	first! = TIMER          ' Set up a small delay for user to see path.
		DO: LOOP UNTIL TIMER > first! + SHOWDELAY
		CLS                         ' Clear screen from players.
		IF ScreenMode = 7 THEN COLOR 14
		LOCATE 2, 11: PRINT "Generating maze..."
		CompleteMaze
		LOCATE 2, 11: PRINT SPACE$(20)
	END IF

END SUB

'----------------------------------------------------------------------------
' GenerateMove
'
'   Generates a random move for the valid maze path to make. Decides what
'   directions it may move in, then choose one of the directions.  Generate
'   a random number of spaces to move in the given direction based on the
'   turn rate.  A variable of type MoveType will have a value of 1 in a
'   field if a move in that direction is invalid.
'
'           PARAMETERS:  move - User-defined type that contains the current
'                               valid moves for a square
'-----------------------------------------------------------------------------
SUB GenerateMove (move AS MoveType)
	
	' Determine how many directions are valid, and choose one.
	IF move.y = 1 THEN
		move.top = 1
	ELSEIF MazeArray(move.x, move.y - 1).top + MazeArray(move.x, move.y - 1).Bottom + MazeArray(move.x, move.y - 1).Left + MazeArray(move.x, move.y - 1).Right <> 4 THEN
		move.top = 1
	END IF

	IF move.y = MazeHeight THEN
		move.Bottom = 1
	ELSEIF MazeArray(move.x, move.y + 1).top + MazeArray(move.x, move.y + 1).Bottom + MazeArray(move.x, move.y + 1).Left + MazeArray(move.x, move.y + 1).Right <> 4 THEN
		move.Bottom = 1
	END IF

	IF move.x = 1 THEN
		move.Left = 1
	ELSEIF MazeArray(move.x - 1, move.y).top + MazeArray(move.x - 1, move.y).Bottom + MazeArray(move.x - 1, move.y).Left + MazeArray(move.x - 1, move.y).Right <> 4 THEN
		move.Left = 1
	END IF

	IF move.x = MazeWidth THEN
		move.Right = 1
	ELSEIF MazeArray(move.x + 1, move.y).top + MazeArray(move.x + 1, move.y).Bottom + MazeArray(move.x + 1, move.y).Left + MazeArray(move.x + 1, move.y).Right <> 4 THEN
		move.Right = 1
	END IF


	AvailableMoves = 4 - (move.Left + move.Right + move.top + move.Bottom)
   
	IF AvailableMoves = 0 THEN
		move.direction = 0
		move.Spaces = 1
		EXIT SUB
	END IF
   
	NewDirection = INT(RND(1) * AvailableMoves) + 1
	
	' Determine what direction was randomly chosen.
	Counter = 1
	IF move.top = 0 THEN
		IF NewDirection = Counter THEN
			move.direction = CTOP
		END IF
		Counter = Counter + 1
	END IF

	IF move.Bottom = 0 THEN
		IF NewDirection = Counter THEN
			move.direction = CBOTTOM
		END IF
		Counter = Counter + 1
	END IF
   
	IF move.Left = 0 THEN
		IF NewDirection = Counter THEN
			move.direction = CLEFT
		END IF
		Counter = Counter + 1
	END IF
   
	IF move.Right = 0 THEN
		IF NewDirection = Counter THEN
			move.direction = CRIGHT
		END IF
		Counter = Counter + 1
	END IF

	move.Spaces = INT(RND(1) * TurnRate) + 1    ' Decide how far to go in the chosen direction

END SUB

'--------------------------------------------------------------------------
' GetGameOptions
'
'   Prompts for and saves various game parameters.
'
'           PARAMETERS:   None
'--------------------------------------------------------------------------
SUB GetGameOptions

	SCREEN 0                            ' Set screen mode 0.
	WIDTH SCREENWIDTH, 25               ' Set width to 80, height to 25.,
	COLOR 7, 0                          ' Set colors to white on black.
	CLS                                 ' Clear the screen.
	LOCATE 7, 20: PRINT "Default is"; DEFAULTPLAYERS
	COLOR 15
	DO                                  ' Get the number of Players.
		LOCATE 6, 23: PRINT SPACE$(50)
		LOCATE 6, 23
		INPUT "How many players? (1 or 2) ", InputHold$
	LOOP UNTIL InputHold$ = "1" OR InputHold$ = "2" OR LEN(InputHold$) = 0
	NumOfPlayers = VAL(InputHold$)
	IF NumOfPlayers = 0 THEN NumOfPlayers = DEFAULTPLAYERS

	COLOR 7
	LOCATE 10, 20: PRINT "Default is"; DEFAULTLEVEL
	COLOR 15                            ' Change foreground color to bright white.
	DO                                  ' Get the difficulty level.
		LOCATE 9, 23: PRINT SPACE$(50)
		LOCATE 9, 23
		INPUT "Difficulty level? (1 to 5) ", InputHold$
		Level = VAL(LEFT$(InputHold$, 1))
	LOOP UNTIL (Level > 0 AND Level <= MAXLEVEL AND LEN(InputHold$) < 2) OR (LEN(InputHold$) = 0)
	IF Level = 0 THEN Level = DEFAULTLEVEL

	COLOR 7                             ' Change foreground color back to white.
	LOCATE 13, 20: PRINT "Default is"; DEFAULTNUMMAZES
	COLOR 15
	DO                                  ' Get number of mazes to create.
		LOCATE 12, 23: PRINT SPACE$(50)
		LOCATE 12, 23
		INPUT "Play how many mazes? (1 to 10) ", InputHold$
		NumOfMazes = VAL(LEFT$(InputHold$, 2))
	LOOP UNTIL (NumOfMazes > 0 AND NumOfMazes <= MAXMAZES AND LEN(InputHold$) < 3) OR (LEN(InputHold$) = 0)
	IF NumOfMazes = 0 THEN NumOfMazes = DEFAULTNUMMAZES

	COLOR 7                             ' Change foreground color back to white.
	LOCATE 16, 20: PRINT "Default is"; DEFAULTMONSTERS
	COLOR 15
	DO                                  ' Get number of monsters; allow for 0 entry.
		LOCATE 15, 23: PRINT SPACE$(50)
		LOCATE 15, 23
		INPUT "How many monsters? (0 to 10) ", InputHold$
		AvailMonsters = VAL(LEFT$(InputHold$, 2))
		IF AvailMonsters = 0 AND InputHold$ <> "0" THEN AvailMonsters = -1
	LOOP UNTIL (AvailMonsters > -1 AND AvailMonsters <= MAXMONSTERS AND LEN(InputHold$) < 3) OR (LEN(InputHold$) = 0)
	IF LEN(InputHold$) = 0 THEN AvailMonsters = DEFAULTMONSTERS

	COLOR 7                             ' Change foreground color back to white.
	LOCATE 19, 20: PRINT "Default is ";
	IF DEFAULTSHOWMAZE THEN PRINT "YES" ELSE PRINT "NO"
	COLOR 15
	DO                                  ' Get visible or invisible maze choice
		LOCATE 18, 23: PRINT SPACE$(40)
		LOCATE 18, 23
		PRINT "Visible mazes? (Y or N) ";
		INPUT "", InputHold$
		InputHold$ = UCASE$(InputHold$)
	LOOP UNTIL (InputHold$ = "Y") OR (InputHold$ = "N") OR (LEN(InputHold$) = 0)
	IF InputHold$ = "N" THEN
		ShowMaze = FALSE
	ELSEIF InputHold$ = "Y" THEN
		ShowMaze = TRUE
	ELSE
		ShowMaze = DEFAULTSHOWMAZE      ' Use the default.
	END IF

END SUB

'---------------------------------------------------------------------------
' GetMonsterDirection
'
'   Decides which direction a monster should move given its current position
'   and the direction it had been moving.  Currently, the direction is chosen
'   as follows: Determine how many directions the monster has a choice of
'   moving. Based on this, determine a percentage which is the likelihood
'   of continuing in the same direction.  If the monster isn't to move in the
'   same direction, a new direction is chosen (it may be the same direction
'   that it was moving before).  This routine decides how smart the monsters
'   will be.
'
'           PARAMETERS:      X         - X coordinate of the monster
'                            Y         - Y coordinate of the monster
'                            direction - Current direction of the monster
'                                        movement
'---------------------------------------------------------------------------
FUNCTION GetMonsterDirection (x, y, direction)

	NewDirection = FALSE                ' Assume it's not changing direction.

	' Determine how many directions are available.
	AvailableDirections = 4 - NumberOfWalls(x, y)
	IF (x = ExitX AND y = ExitY) OR (x = EntryX AND y = EntryY) THEN
		AvailableDirections = AvailableDirections - 1
	END IF

	' Decides the percentage of whether the monster will continue in the same direction.
	ContinueDirection = 1 - (AvailableDirections - 1) * .25
	SELECT CASE direction
		CASE CTOP
			 IF MazeArray(x, y).top = 0 AND y > 1 AND RND(1) < ContinueDirection THEN GetMonsterDirection = direction
		CASE CBOTTOM
			IF MazeArray(x, y).Bottom = 0 AND y < MazeHeight AND RND(1) < ContinueDirection THEN GetMonsterDirection = direction
		CASE CLEFT
			IF MazeArray(x, y).Left = 0 AND x > 1 AND RND(1) < ContinueDirection THEN GetMonsterDirection = direction
		CASE CRIGHT
			IF MazeArray(x, y).Right = 0 AND x < MazeWidth AND RND(1) < ContinueDirection THEN GetMonsterDirection = direction
		CASE ELSE
			NewDirection = TRUE
	END SELECT

	IF NOT NewDirection THEN EXIT FUNCTION  ' If monster doesn't change direction.

	NewDirection = INT(RND(1) * AvailableDirections) + 1    ' Pick a new direction.
	Counter = 1
	IF MazeArray(x, y).top = 0 AND y > 1 THEN
		IF NewDirection = Counter THEN GetMonsterDirection = CTOP
		Counter = Counter + 1
	END IF
	IF MazeArray(x, y).Bottom = 0 AND y < MazeHeight THEN
		IF NewDirection = Counter THEN GetMonsterDirection = CBOTTOM
		Counter = Counter + 1
	END IF
	IF MazeArray(x, y).Left = 0 AND x > 1 THEN
		IF NewDirection = Counter THEN GetMonsterDirection = CLEFT
		Counter = Counter + 1
	END IF
	IF MazeArray(x, y).Right = 0 AND x < MazeWidth THEN
		IF NewDirection = Counter THEN GetMonsterDirection = CRIGHT
		Counter = Counter + 1
	END IF

END FUNCTION

'---------------------------------------------------------------------------
' InitVariables
'
'   Initializes player keys and game variables.
'
'           PARAMETERS:      None
'---------------------------------------------------------------------------
SUB InitVariables

	KEY 15, CHR$(0) + CHR$(25)              ' P key (Pause)
	KEY 16, CHR$(0) + CHR$(16)              ' Q key (Quit)
	KEY 21, CHR$(128) + CHR$(72)            ' Extended Up key for player 1.
	KEY 22, CHR$(128) + CHR$(75)            ' Extended Left key for player 1.
	KEY 23, CHR$(128) + CHR$(77)            ' Extended Right key for player 1.
	KEY 24, CHR$(128) + CHR$(80)            ' Extended Down key for player 1.

	'ON KEY (X) indicates what subroutine to jump to when KEY (X) is pressed.
	ON KEY(11) GOSUB MovePlayer1Up
	ON KEY(12) GOSUB MovePlayer1Left
	ON KEY(13) GOSUB MovePlayer1Right
	ON KEY(14) GOSUB MovePlayer1Down
	ON KEY(15) GOSUB PauseGame              ' Pause the game.
	ON KEY(16) GOSUB QuitGame               ' Quit the game.
	ON KEY(21) GOSUB MovePlayer1Up
	ON KEY(22) GOSUB MovePlayer1Left
	ON KEY(23) GOSUB MovePlayer1Right
	ON KEY(24) GOSUB MovePlayer1Down
	ON TIMER(1) GOSUB TimerUpdate           'Timer interrupts every second.

	IF NumOfPlayers = 2 THEN
		KEY 17, CHR$(0) + CHR$(UP2)         'Up for player 2.
		KEY 18, CHR$(0) + CHR$(DOWN2)       'Down for player 2.
		KEY 19, CHR$(0) + CHR$(LEFT2)       'Left for player 2.
		KEY 20, CHR$(0) + CHR$(RIGHT2)      'Right for player 2.
		ON KEY(17) GOSUB MovePlayer2Up      '17 is the UP for player 2.
		ON KEY(18) GOSUB MovePlayer2Down    '18 is the DOWN key for player 2.
		ON KEY(19) GOSUB MovePlayer2Left    '19 is the LEFT key for player 2.
		ON KEY(20) GOSUB MovePlayer2Right   '20 is the RIGHT key for player 2.
	END IF

	FOR i = 1 TO 2                          ' Initialize player variables.
		Player(i).Score = 0
		Player(i).TimeLeft = 0
		Player(i).Done = FALSE
	NEXT i
									  
	IF ScreenMode = 7 THEN                  ' Set up correct screen colors for mode 7.
		MazeColor = MAZECOLOR7
		Player(1).PColor = PLAYER1COLOR7
		Player(2).PColor = PLAYER2COLOR7
		MonsterColor = MONSTERCOLOR7
		GameBkGround = GAMEBKGROUND7
	ELSE
		MazeColor = MAZECOLOR1              ' Set up correct screen colors for mode 1.
		Player(1).PColor = PLAYER1COLOR1
		Player(2).PColor = PLAYER2COLOR1
		MonsterColor = MONSTERCOLOR1
		GameBkGround = GAMEBKGROUND1
	END IF
	GameOver = FALSE
	MazesFinished = 1
	MazeTime = DEFAULTTIME + AvailMonsters * 5 - (Level - 1) * 5    ' Number of seconds to finish all mazes.  Allow 5 more seconds for each monster.
	MonsterUpdateTime = DEFAULTMONSTERTIME
	RESTORE                                 ' Restore and reread DATA statements.
	FOR i = 1 TO Level
		READ MazeWidth, MazeHeight, PathLength, TurnRate
	NEXT
	StartX = (300 / BLOCKSIZE - MazeWidth) / 2 * BLOCKSIZE + 1
	StartY = (180 / BLOCKSIZE - MazeHeight) / 2 * BLOCKSIZE + 1
	
END SUB

'--------------------------------------------------------------------------
' Keys
'
'   Turns key event-processing on, off, or temporarily stops.
'
'           PARAMETERS:   onoff - If it's 1 = enable, 2 = disable, 3 = stop.
'--------------------------------------------------------------------------
SUB Keys (onoff)

	FOR i = 11 TO 24        ' Loop through all defined keys.
		SELECT CASE onoff
		CASE 1
			KEY(i) ON
		CASE 2
			KEY(i) OFF
		CASE 3
			KEY(i) STOP
		END SELECT
	NEXT i

END SUB

'----------------------------------------------------------------------------
' MonsterControl
'
'   Controls, checks and updates the monsters.  Checks
'   to see if any of the players have moved into any of the monsters.
'
'               PARAMETERS:   None
'----------------------------------------------------------------------------
SUB MonsterControl STATIC

	IF UBOUND(Monsters) <> AvailMonsters THEN   ' Initialize monster array.
		REDIM Monsters(1 TO AvailMonsters) AS MonsterType
		NumMonsters = 0
	END IF

	Keys (3)                    ' Temporarily stop arrow keys processing.
	FOR i = 1 TO NumMonsters                    ' Check for players hitting monsters.
		FOR j = 1 TO NumOfPlayers
			IF Monsters(i).x = Player(j).x AND Monsters(i).y = Player(j).y AND Monsters(i).Active THEN
				Player(j).Dead = TRUE
				PLAY PLAYERDEATHSOUND
				DrawPlayer j, GameBkGround
			END IF
		NEXT j
	NEXT i
	Keys (1)                                    ' Enable key event-processing.

	IF NumMonsters < AvailMonsters THEN         ' Add another monster to the maze if there isn't the right number.
		NumMonsters = NumMonsters + 1
		IF NumMonsters MOD 3 = 1 THEN           ' Add every third one at the exit of the maze.
			Monsters(NumMonsters).x = ExitX
			Monsters(NumMonsters).y = ExitY
		ELSE
			' Choose a point along the last 80 percent of the valid path to place the monster.
			RandomPlace = INT(RND(1) * (StackPointer - StackPointer * .2)) + StackPointer * .2
			Monsters(NumMonsters).x = stackVar(RandomPlace).x
			Monsters(NumMonsters).y = stackVar(RandomPlace).y
		END IF
		Monsters(NumMonsters).Active = TRUE
	END IF
	IF TIMER - LastUpdate! < MonsterUpdateTime THEN EXIT SUB
	LastUpdate! = TIMER

	FOR i = 1 TO NumMonsters                    ' Move the monsters.
		IF Monsters(i).Active THEN
			Dir = GetMonsterDirection(Monsters(i).x, Monsters(i).y, Monsters(i).direction)
			Monsters(i).direction = Dir
			DrawMonster Monsters(i).x, Monsters(i).y, GameBkGround  ' Erase current position.
			ConvertDirToXY Dir, xmove, ymove
			Monsters(i).x = Monsters(i).x + xmove
			Monsters(i).y = Monsters(i).y + ymove
			DrawMonster Monsters(i).x, Monsters(i).y, MonsterColor
		END IF
	NEXT i

	Keys (3)                    ' Temporarily stop arrow keys processing.
	FOR i = 1 TO NumMonsters                    ' Check for monsters hitting players.
		FOR j = 1 TO NumOfPlayers
			IF Monsters(i).x = Player(j).x AND Monsters(i).y = Player(j).y AND Monsters(i).Active THEN
				Player(j).Dead = TRUE           ' Kill the player
				PLAY PLAYERDEATHSOUND           ' Play death melody.
				DrawPlayer j, GameBkGround
			END IF
		NEXT j
	NEXT i
	Keys (1)                                    ' Enable key event-processing.

END SUB

'---------------------------------------------------------------------------
' NumberOfWalls
'
'   Returns the number of walls in a given block.
'
'           PARAMETERS:    X - X coordinate of the maze square to be checked
'                          Y - Y coordinate of the maze square to be checked
'---------------------------------------------------------------------------
FUNCTION NumberOfWalls (x, y)

	NumberOfWalls = MazeArray(x, y).top + MazeArray(x, y).Bottom + MazeArray(x, y).Left + MazeArray(x, y).Right

END FUNCTION

'--------------------------------------------------------------------------
' PlayGame
'
'   Loops until a given maze is exited or the game is ended.
'   The basic flow of the routine is to update all of the various things like
'   monsters, scores, and checks to see if somebody was killed.
'
'               PARAMETERS:    None
'--------------------------------------------------------------------------
SUB PlayGame

	PrintMaze                       ' Show the maze.

	IF ExitY = MazeHeight THEN      ' Print the exit path markers.
		LINE (StartX + ExitX * BLOCKSIZE, StartY + ExitY * BLOCKSIZE + BLOCKSIZE - 1)-(StartX + ExitX * BLOCKSIZE, StartY + ExitY * BLOCKSIZE + BLOCKSIZE + 2), 12, BF
		LINE (StartX + ExitX * BLOCKSIZE + BLOCKSIZE, StartY + ExitY * BLOCKSIZE + BLOCKSIZE - 1)-(StartX + ExitX * BLOCKSIZE + BLOCKSIZE, StartY + ExitY * BLOCKSIZE + BLOCKSIZE + 2), 12, BF
	ELSEIF ExitY = 1 THEN
		LINE (StartX + ExitX * BLOCKSIZE, StartY + ExitY * BLOCKSIZE - 2)-(StartX + ExitX * BLOCKSIZE, StartY + ExitY * BLOCKSIZE + 1), 12, BF
		LINE (StartX + ExitX * BLOCKSIZE + BLOCKSIZE, StartY + ExitY * BLOCKSIZE - 2)-(StartX + ExitX * BLOCKSIZE + BLOCKSIZE, StartY + ExitY * BLOCKSIZE + 1), 12, BF
	ELSEIF ExitX = MazeWidth THEN
		LINE (StartX + ExitX * BLOCKSIZE + BLOCKSIZE - 1, StartY + ExitY * BLOCKSIZE)-(StartX + ExitX * BLOCKSIZE + BLOCKSIZE + 2, StartY + ExitY * BLOCKSIZE), 12, BF
		LINE (StartX + ExitX * BLOCKSIZE + BLOCKSIZE - 1, StartY + ExitY * BLOCKSIZE + BLOCKSIZE)-(StartX + ExitX * BLOCKSIZE + BLOCKSIZE + 2, StartY + ExitY * BLOCKSIZE + BLOCKSIZE), 12, BF
	ELSEIF ExitX = 1 THEN
		LINE (StartX + ExitX * BLOCKSIZE - 2, StartY + ExitY * BLOCKSIZE)-(StartX + ExitX * BLOCKSIZE + 1, StartY + ExitY * BLOCKSIZE), 12, BF
		LINE (StartX + ExitX * BLOCKSIZE - 2, StartY + ExitY * BLOCKSIZE + BLOCKSIZE)-(StartX + ExitX * BLOCKSIZE + 1, StartY + ExitY * BLOCKSIZE + BLOCKSIZE), 12, BF
	END IF
	
	FOR i = 1 TO NumOfPlayers       ' Initialize needed values.
		Player(i).x = EntryX
		Player(i).y = EntryY
		Player(i).TimeLeft = 0
		Player(i).Done = FALSE
		Player(i).Dead = FALSE
		DrawPlayer i, Player(i).PColor  ' Draw player(s).
	NEXT i
	CountDown = MazeTime                ' Set time allowed to solve maze.
	MazeOver = FALSE
	LOCATE 1, 3: PRINT "Time left:"; CountDown; ' Display time left.
	LOCATE 1, 31: PRINT "Maze:"; MazesFinished; ' Display mazes finished.
	
	IF NumOfPlayers = 1 THEN                    ' Different for one or two players.
		LINE (191, 194)-(194, 197), Player(1).PColor, BF
		LOCATE 25, 15
	ELSE
		LOCATE 25, 3: PRINT "Player 2:";
		LINE (93, 193)-(98, 198), Player(2).PColor, B
		LINE (275, 193)-(278, 196), Player(1).PColor, BF
		LOCATE 25, 26
	END IF
	PRINT "Player 1:";
	SOUND 500, 3                        ' 500Hz tone for 3 clock ticks.
	Keys (1)                            ' Enable key event-processing.
	TIMER ON

	' This is the loop which runs until quit, both players are out, or time's up.
	DO
		DO: LOOP UNTIL INKEY$ = ""      ' Clear keyboard buffer.
		IF AvailMonsters > 0 THEN MonsterControl

		FOR i = 1 TO NumOfPlayers       ' Check to see if any of the players have been killed in the last update.
			IF Player(i).Dead THEN
				Player(i).Dead = FALSE
				Player(i).x = EntryX
				Player(i).y = EntryY
				DrawPlayer i, Player(i).PColor
			END IF
		NEXT i

		IF NumOfPlayers = 2 AND Player(1).Done AND Player(2).Done THEN
			MazeOver = TRUE
		ELSEIF NumOfPlayers = 1 AND Player(1).Done THEN
			MazeOver = TRUE
		END IF
	LOOP UNTIL MazeOver OR GameOver OR CountDown = 0
	TIMER OFF
	Keys (2)                            ' Disable key event-processing.
	
END SUB

'--------------------------------------------------------------------------
' PopMove
'
'   Removes the previous path move from the top of the stack and returns it.
'
'               PARAMETERS:    move - The previous move that will be returned
'                                     from the top of stackVar array
'--------------------------------------------------------------------------
SUB PopMove (move AS MoveType)

	IF StackPointer <> 0 THEN
		move = stackVar(StackPointer)
		StackPointer = StackPointer - 1
	 ELSE
		MazeError = TRUE
	 END IF

END SUB

'---------------------------------------------------------------------------
' PrintBlock
'
'   Prints the walls of a given block in the color specified and uses the
'   background color to print walls that have been removed.
'
'       PARAMETERS: block     - Block to draw
'                   X         - X coordinate of the block to be drawn
'                   Y         - Y coordinate of the block to be drawn
'                   whatColor - Color of the block to be drawn
'---------------------------------------------------------------------------
SUB PrintBlock (block AS MazeType, x, y, WhatColor)

	ActualX = StartX + x * BLOCKSIZE
	ActualY = StartY + y * BLOCKSIZE
	IF block.top = 1 THEN
		LINE (ActualX, ActualY)-(ActualX + BLOCKSIZE, ActualY), WhatColor
	ELSE
		LINE (ActualX + 1, ActualY)-(ActualX + BLOCKSIZE - 1, ActualY), GameBkGround
	END IF

	IF block.Bottom = 1 THEN
		LINE (ActualX, ActualY + BLOCKSIZE)-(ActualX + BLOCKSIZE, ActualY + BLOCKSIZE), WhatColor
	ELSE
		LINE (ActualX + 1, ActualY + BLOCKSIZE)-(ActualX + BLOCKSIZE - 1, ActualY + BLOCKSIZE), GameBkGround
	END IF

	IF block.Left = 1 THEN
		LINE (ActualX, ActualY)-(ActualX, ActualY + BLOCKSIZE), WhatColor
	ELSE
		LINE (ActualX, ActualY + 1)-(ActualX, ActualY + BLOCKSIZE - 1), GameBkGround
	END IF

	IF block.Right = 1 THEN
		LINE (ActualX + BLOCKSIZE, ActualY)-(ActualX + BLOCKSIZE, ActualY + BLOCKSIZE), WhatColor
	ELSE
		LINE (ActualX + BLOCKSIZE, ActualY + 1)-(ActualX + BLOCKSIZE, ActualY + BLOCKSIZE - 1), GameBkGround
	END IF

END SUB

'---------------------------------------------------------------------------
' PrintMaze
'
'   Prints out the entire maze or just the outside edge.
'
'               PARAMETERS:  None
'---------------------------------------------------------------------------
SUB PrintMaze

	IF ShowMaze THEN                ' Print the whole maze.
		FOR i = 1 TO MazeHeight
			FOR j = 1 TO MazeWidth
				PrintBlock MazeArray(j, i), j, i, MazeColor
			NEXT j
		NEXT i
	ELSE                            ' Print just the outside edge of the maze.
	   FOR i = 1 TO MazeWidth
			PrintBlock MazeArray(i, 1), i, 1, MazeColor
			PrintBlock MazeArray(i, MazeHeight), i, MazeHeight, MazeColor
	   NEXT i
	   FOR j = 1 TO MazeHeight
			PrintBlock MazeArray(1, j), 1, j, MazeColor
			PrintBlock MazeArray(MazeWidth, j), MazeWidth, j, MazeColor
	   NEXT j
	END IF

END SUB

'---------------------------------------------------------------------------
' ProcessPlayerInput
'
'   Processes the player input.
'
'               PARAMETERS:   i  -   Player number to be processed
'---------------------------------------------------------------------------
SUB ProcessPlayerInput (i)

	Keys (3)                    ' Temporarily stop arrow keys processing.
	IF PlayerMove(i) <> 0 AND NOT Player(i).Done AND NOT Player(i).Dead THEN   'Only if player input and not done

		ValMove = FALSE         ' Check for valid direction.
		SELECT CASE PlayerMove(i)
		CASE CTOP
			IF MazeArray(Player(i).x, Player(i).y).top = 0 AND Player(i).y > 1 THEN ValMove = TRUE
		CASE CBOTTOM
			IF MazeArray(Player(i).x, Player(i).y).Bottom = 0 AND Player(i).y < MazeHeight THEN ValMove = TRUE
		CASE CLEFT
			IF MazeArray(Player(i).x, Player(i).y).Left = 0 AND Player(i).x > 1 THEN ValMove = TRUE
		CASE CRIGHT
			IF MazeArray(Player(i).x, Player(i).y).Right = 0 AND Player(i).x < MazeWidth THEN ValMove = TRUE
		END SELECT

		IF ValMove THEN
			UpdatePosition PlayerMove(i), i
		ELSE
			SELECT CASE PlayerMove(i)   ' If invalid, check maze exit
			CASE CTOP
				IF MazeArray(Player(i).x, Player(i).y).top = 0 AND Player(i).y = 1 THEN ValMove = TRUE
			CASE CBOTTOM
				IF MazeArray(Player(i).x, Player(i).y).Bottom = 0 AND Player(i).y = MazeHeight THEN ValMove = TRUE
			CASE CLEFT
				IF MazeArray(Player(i).x, Player(i).y).Left = 0 AND Player(i).x = 1 THEN ValMove = TRUE
			CASE CRIGHT
				IF MazeArray(Player(i).x, Player(i).y).Right = 0 AND Player(i).x = MazeWidth THEN ValMove = TRUE
			END SELECT

			IF Player(i).x <> ExitX OR Player(i).y <> ExitY THEN ValMove = FALSE

			IF ValMove THEN
				Player(i).Done = TRUE
				Player(i).TimeLeft = CountDown
				UpdatePosition PlayerMove(i), i
				PLAY PLAYEROUTSOUND     ' Play the melody for when the player exits the maze.

			END IF
		END IF
		PlayerMove(i) = 0               ' Clear player move.
	END IF
	Keys (1)                            ' Enable key event-processing.

END SUB

'-------------------------------------------------------------------------
' UpdatePosition
'
'   Updates the player's coordinates and moves the player.  Also draws the
'   blocks of the maze around the player.  This allows the player
'   to see one space in any direction when travelling through an invisible
'   maze.
'
'           PARAMETERS:   Dir - Direction of travel
'                         Plr - Player number whose position is being shown
'-------------------------------------------------------------------------
SUB UpdatePosition (Dir, Plr)

	DrawPlayer Plr, GameBkGround        ' Draw player.
	ConvertDirToXY Dir, xmove, ymove
	Player(Plr).x = Player(Plr).x + xmove
	Player(Plr).y = Player(Plr).y + ymove

	IF NOT ShowMaze THEN
		IF Player(Plr).x >= 1 AND Player(Plr).x <= MazeWidth AND Player(Plr).y >= 1 AND Player(Plr).y <= MazeHeight THEN
			PrintBlock MazeArray(Player(Plr).x, Player(Plr).y), Player(Plr).x, Player(Plr).y, MazeColor
			IF Player(Plr).x - 1 > 0 THEN PrintBlock MazeArray(Player(Plr).x - 1, Player(Plr).y), (Player(Plr).x - 1), Player(Plr).y, MazeColor
			IF Player(Plr).x + 1 <= MazeWidth THEN PrintBlock MazeArray(Player(Plr).x + 1, Player(Plr).y), (Player(Plr).x + 1), Player(Plr).y, MazeColor
			IF Player(Plr).y - 1 > 0 THEN PrintBlock MazeArray(Player(Plr).x, Player(Plr).y - 1), Player(Plr).x, (Player(Plr).y - 1), MazeColor
			IF Player(Plr).y + 1 <= MazeHeight THEN PrintBlock MazeArray(Player(Plr).x, Player(Plr).y + 1), Player(Plr).x, (Player(Plr).y + 1), MazeColor
		END IF
	END IF
	DrawPlayer Plr, Player(Plr).PColor  ' Draw player.

END SUB

'-------------------------------------------------------------------------
' ValidBustDir
'
'   Returns the direction of a wall to remove.  Ensures that no
'   walls on the edge of the maze are broken out.
'
'           PARAMETERS:  X - X coordinate of the block being checked for
'                            removal
'                        Y - Y coordinate of the block being checked for
'                            removal
'-------------------------------------------------------------------------
FUNCTION ValidBustDir (x, y)

	DIM BreakableWalls AS MazeType
	BreakableWalls = MazeArray(x, y)
	IF x = 1 THEN BreakableWalls.Left = 0
	IF x = MazeWidth THEN BreakableWalls.Right = 0
	IF y = 1 THEN BreakableWalls.top = 0
	IF y = MazeHeight THEN BreakableWalls.Bottom = 0

	AvailableMoves = BreakableWalls.Left + BreakableWalls.Right + BreakableWalls.top + BreakableWalls.Bottom
	NewDirection = INT(RND(1) * AvailableMoves) + 1
	Counter = 1
	IF BreakableWalls.top = 1 THEN
		IF NewDirection = Counter THEN ValidBustDir = CTOP
		Counter = Counter + 1
	END IF
	IF BreakableWalls.Bottom = 1 THEN
		IF NewDirection = Counter THEN ValidBustDir = CBOTTOM
		Counter = Counter + 1
	END IF
	IF BreakableWalls.Left = 1 THEN
		IF NewDirection = Counter THEN ValidBustDir = CLEFT
		Counter = Counter + 1
	END IF
	IF BreakableWalls.Right = 1 THEN
		IF NewDirection = Counter THEN ValidBustDir = CRIGHT
		Counter = Counter + 1
	END IF

END FUNCTION

