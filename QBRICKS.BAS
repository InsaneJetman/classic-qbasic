'
'                                QBRICKS.BAS
'
'         Copyright (C) 1990 Microsoft Corporation. All Rights Reserved.
'
' Score points in QBricks by deflecting the ball into the brick walls.
' Hit the special bricks or clear all the bricks to advance to the next
' level.
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
' of this screen can be modified to change the following:
'    Block patterns
'    Length of paddles
'    Number of special bricks
'    Shape of the special bricks
'    End-of-level bonus multiplier
'    Paddle color
'    Ball color
'    Ball speed
'
' On the right side of each CONST statement, there is a comment that tells
' you what it does and how large or small you can set the value.  Above the
' DATA statements, there are comments that tell you the format of the
' information stored there.
'
' On your own, you can also add exciting sounds and visual effects or make any
' other changes that your imagination can dream up.  By reading the
' "Learn BASIC Now" book, you'll learn the techniques that will enable you
' to fully customize this game and to create games of your own.
'
'
' If the game won't run after you have changed it, you can exit without
' saving your changes by pressing Alt, F, X and choosing NO.
'
' If you do want to save your changes, press Alt, F, A and enter a filename
' for your version of the program.  Before you save your changes, however,
' you should make sure they work by running the program and verifying that
' your changes produce the desired results.  Also, always be sure to keep
' a backup of the original program.
'
DEFINT A-Z

CONST INITIALBALLSPEED = .07' Initial speed of the ball in seconds.  Range 0 (fastest) - .20 (slowest)
CONST BALLCOLOR = 12        ' Ball color.  Ranges from 1 to 15 but not BGCOLOR.
CONST BGCOLOR = 0           ' Background color of the game.  Range 0 to 7.
CONST PLAYERCOLOR1 = 2      ' Paddle color for player 1.  Range 1 to 15 but not BGCOLOR.
CONST PLAYERCOLOR2 = 3      ' Paddle color for player 2.  Range 1 to 15 but not BGCOLOR.
CONST PADDLELENGTH1 = 50    ' Starting paddle length for player 1.  Range 24 to 240.
CONST PADDLELENGTH2 = 50    ' Starting paddle length for player 2.  Range 24 to 120.
CONST NUMSPECIALBRICKS = 1  ' Number of special bricks in each level.  Range 0 to 50. Higher numbers make the game easier.
CONST SPECIALCHAR = 14      ' ASCII value of the special character.  14 is a musical note.  Try 3 for a heart or 2 for a smiley face.
CONST BONUSMULTIPLIER = 500 ' Minimum bonus amount for each round.  Range 0 to 1000 in increments of 100.
CONST PADDLEHORIZONTALMOVE = 24 ' Number of spaces paddle moves left or right each time the left or right key is pressed.  Range 1 to 30.
CONST INITNUMBALLS = 3      ' Initial number of balls.  Range 1 to 50.
CONST LEVELNEWBALL = 3      ' Level interval at which an extra ball is awarded.
CONST CUTLEVEL = 6          ' Level interval at which the paddle size is reduced.  Range 2 to 10.
CONST PADDLECUT = 33        ' The percent that the paddle will be reduced by.  Range 0 to 99. For example, at 33, a paddle that is 50 pixels wide will become 33 pixels wide.
CONST DEFAULTPLAYERS = 1    ' Default number of players.  Range 1 or 2.
' The following sound constants are used by the PLAY command to
' produce music during the game.  To change the sounds you hear, change
' these constants.  Refer to the online help for PLAY for the correct format.
' To completely remove sound from the game set the constants equal to null.
' For example:  STARTSOUND = ""
CONST STARTSOUND = "MBT180O2L8CDEDCDFECDCL4EL8C"    ' Music played when QBricks begins.
CONST PADDLEHITSOUND = "MBT120 L64 o3 g"  ' Music played when the ball hits the paddle.
CONST BLOCKHITSOUND = "MB T255 L8 o"      ' Music played when the ball hits a brick.
CONST NEXTLEVELSOUND = "MB T240 L2 N30 N34 N38 N45" ' Music played between levels.
CONST GAMEOVERSOUND = "T255 L16 O3 C O2 GEDC"       ' Music played when the game is over.

' The following are general constants and their values should not be changed.
CONST TRUE = -1                 ' QuickBASIC Interpreter uses -1 to mean TRUE.
CONST FALSE = 0                 ' 0 means FALSE.
CONST PADDLEVERTICALMOVE = 4    ' Distance paddle moves up or down each time the Up key or Down key is pressed.
CONST MAXLEVEL = 5              ' Level when the brick patterns start over.
CONST SCREENWIDTH = 40          ' Maximum width of the screen in characters.
CONST SCORECOLOR = 15           ' Color of the players' scores, level number and balls left displayed at the bottom of the screen.
CONST MAXBLOCKROW = 9           ' Lowest row that has bricks on it.
CONST STARTBRICKROW = 2         ' Highest row with bricks on it.
CONST BRICKSIZE = 2             ' Width of a brick in character-sized units.
CONST PIXELSIZE = 8             ' Number of pixels per brick.
CONST MAXROW = 184              ' The highest (in pixels) a paddle can move.
CONST MINROW = (MAXBLOCKROW + 2) * 8    ' The lowest (in pixels) a paddle can move.
CONST UP1 = 104                 ' ASCII code for UP key - Up for player 1.
CONST DOWN1 = 112               ' ASCII code for DOWN key - Down for player 1.
CONST LEFT1 = 107               ' ASCII code for LEFT key - Left for player 1.
CONST RIGHT1 = 109              ' ASCII code for RIGHT key - Right for player 1.
CONST UP2 = 101                 ' ASCII code for e - Up for player 2.
CONST LEFT2 = 115               ' ASCII code for s - Left for player 2.
CONST RIGHT2 = 102              ' ASCII code for f - Right for player 2.
CONST DOWN2 = 100               ' ASCII code for d - Down for player 2.
CONST PAUSE = 112               ' ASCII code for p - Pause.
CONST QUIT = 113                ' ASCII code for q - Quit.

' DECLARE statements tell the main program that subprograms and functions
'  exist and defines what data types they use.
DECLARE SUB BallHitPaddle (Player)                  ' Checks to see if the ball hit a paddle.  If it did, deflect the ball.
DECLARE SUB Center (text$, Row)                     ' Centers a line of text on a given row.
DECLARE SUB DisplayChanges ()                       ' Shows what changes can be made to this program.
DECLARE SUB DisplayGameTitle ()                     ' Displays the title of the game.
DECLARE SUB DisplayIntro ()                         ' Shows how to play the game.  Used at the start of the game or when the Help key is pressed.
DECLARE SUB DrawBall (BallX, BallY)                 ' Draws or erases the ball.
DECLARE SUB DrawBrick (BrickX, BrickY, BrickColor)  ' Draws or erases a brick.
DECLARE SUB DrawPaddle (PColor, PlayerNum)          ' Draws or erases a paddle.
DECLARE SUB EraseBall (X, Y)                        ' Erases the ball.
DECLARE SUB EraseBrick (X, Y)                       ' Erases a brick after the ball hits it.  The brick is physically erased by the DrawBrick subprogram.
DECLARE SUB GameOver ()                             ' Checks to see if the game is over.  If it is, ask player if he/she wants to play again.
DECLARE SUB GameParamSetup ()                       ' Determine the speed of the computer, set the graphics mode, etc.
DECLARE SUB GetGameOptions ()                       ' Asks for the number of players.
DECLARE SUB HorizontalScroll (display$, Row)        ' Generic subprogram to move a line of text across the screen.
DECLARE SUB MovePaddle (NewX, NewY, PlayerNum)      ' Moves the paddle(s).  Checks that paddles do not overlap and that paddle is completely on the screen.
DECLARE SUB NewBall ()                              ' Launches a new ball at the start of the game, between levels, or after a ball passes the paddles.
DECLARE SUB NextLevel ()                            ' Adds bonus points, draws new brick pattern, etc. after each level is complete.
DECLARE SUB RedrawPaddles ()                        ' Redraws the paddle(s).
DECLARE SUB SetDefaultPaddle ()                     ' Positions the paddle(s) to initial point.
DECLARE SUB UpdateScreen ()                         ' Redraws the score and levels.  Used after a brick is hit or bonus points awarded.

' Structure used for the paddles and the ball.
TYPE PositionType
	X AS INTEGER            ' Horizontal (X) position of the paddle, in pixels.
	Y AS INTEGER            ' Vertical (Y) position of the paddle, in pixels.
	OldX AS INTEGER         ' Last X position.  Used to erase the paddle or ball.
	OldY AS INTEGER         ' Last Y position.  Used to erase the paddle or ball.
	Size AS INTEGER         ' Size of the paddles.
	PColor AS INTEGER       ' Color of the ball or the paddle.
	XOffset AS INTEGER      ' Increment of horizontal ball movement.
	YOffset AS INTEGER      ' Increment of vertical ball movement.
	speed AS SINGLE         ' Interval, in seconds, between actual ball movements.
	Score AS LONG           ' Score for each player.
	NumBricksHit AS INTEGER ' Number of bricks each player has hit.
END TYPE

' DIM SHARED indicates that a variable is available to all subprograms.
' Without this statement, a variable used in one subprogram cannot be
' used by another subprogram or the main program.
DIM SHARED TempPADDLELENGTH AS INTEGER  ' Keeps the true length of the paddle.
DIM SHARED ScreenMode AS INTEGER        ' Graphics screen mode used.
DIM SHARED ScreenWide AS INTEGER        ' Width of the screen in characters
DIM SHARED GraphicsWidth AS INTEGER     ' Width of the screen in pixels.
DIM SHARED UsableWidth AS INTEGER       ' GraphicsWidth after assuming a small border around the screen.
DIM SHARED Ball AS PositionType         ' Variable for the ball.
DIM SHARED Bricks(25, 20) AS INTEGER    ' Array to represent all of the bricks on the screen.  The values determine brick color.  Blank spaces are filled with the background color (BgColor).
DIM SHARED NumBalls AS INTEGER          ' Number of balls left.
DIM SHARED Again AS INTEGER             ' Flag used to decide if the game should continue.
DIM SHARED NeedBall AS INTEGER          ' Flag used to determine if a new ball is needed?
DIM SHARED Level AS INTEGER             ' Current play level.
DIM SHARED LevelCount AS LONG           ' Number of bricks hit in the current level.
DIM SHARED MAXLEVELCount AS LONG        ' Maximum number of bricks in the current level.
DIM SHARED TimeToMoveBall AS SINGLE     ' Interval, in seconds, between ball movements.
DIM SHARED special AS STRING            ' Special character.
DIM SHARED JustHit AS INTEGER           ' Flag used to see if ball hit a paddle so the same paddle cannot hit the ball again until it hits the other paddle, a brick, or bounces off the top.
DIM SHARED NumberOfPlayers AS INTEGER   ' Number of players.
DIM SHARED LastHitBy AS INTEGER         ' Which player hit the ball last.
DIM SHARED LevelOver AS INTEGER         ' Flag that is TRUE when a level is completed.
DIM SHARED ActualBallSpeed AS SINGLE    ' Initial ball speed after determining the machine speed.
DIM SHARED Ballshape(20)  AS INTEGER    ' Array in which the ball image is stored.
DIM SHARED EraseBallOK AS INTEGER       ' Flag to decide if the ball's last position should be erased.
DIM SHARED LastX AS INTEGER             ' Flag that determines if the ball has just deflected horizontally off a brick.
DIM SHARED LastY AS INTEGER             ' Flag that determines if the ball has just deflected vertically off a brick.
DIM BadMode AS INTEGER                  ' Flag used to determine which graphics mode to use.
DIM KeyFlags AS INTEGER                 ' Holds the status of various keys, including Num Lock.

RANDOMIZE TIMER                 ' Seed the random number generator.

special = ""                    ' Build the string used to display the special bricks.
FOR X = 1 TO BRICKSIZE
	special = special + CHR$(SPECIALCHAR)   ' Add the SPECIALCHAR to the string.
NEXT X                          ' Repeat until the loop is done BRICKSIZE times.

' Determine which graphics mode to use.
ON ERROR GOTO ScreenError       ' Set up a place to jump to if an error occurs in the program.
BadMode = FALSE                 ' Assume that the graphics mode is okay.
ScreenMode = 7                  ' Set mode to 7 (an EGA mode).
SCREEN ScreenMode               ' Attempt SCREEN 7.
IF BadMode = TRUE THEN          ' If this attempt failed:
	ScreenMode = 1              ' try mode 1 (a CGA mode),
	BadMode = FALSE             ' assume that graphics mode is okay,
	SCREEN ScreenMode           ' attempt SCREEN 1.
END IF
ON ERROR GOTO 0                 ' Turn off error handling.

IF BadMode = TRUE THEN          ' If no graphics adapter...
	CLS
	LOCATE 11, 13: PRINT "CGA, EGA Color, or VGA graphics required to run QBRICKS.BAS"
ELSE
	DEF SEG = 0             ' Save the keyboard flags but force them all off for this game.
	KeyFlags = PEEK(1047)   ' Read the location that stores the keyboard flag.
	POKE 1047, &H0          ' Force them off.
	DEF SEG                 ' Restore the default segment.

	DisplayIntro            ' Display the introduction screen now.
	GetGameOptions          ' Ask how many players.

	DIM SHARED Paddle(NumberOfPlayers) AS PositionType ' Array used to represent the paddles.
	Level = 0               ' Start at the first level
	Again = TRUE            ' Set the flag used to continue the game.
	NextLevel               ' Set up the next level.
	
	DO
		'The code below moves the ball.
		IF TIMER >= TimeToMoveBall THEN         ' Time to move the ball again?
			TimeToMoveBall = TIMER + Ball.speed ' Decide when to move the ball again.
			LevelOver = FALSE                   ' Flag that is false unless a level has just been cleared.
			DeflectY = FALSE                    ' Flag that determines if the ball's vertical direction (Y) should be reversed.
			DeflectX = FALSE

			Ball.X = Ball.X + Ball.XOffset      ' Updates the ball's horizontal and vertical location.
			Ball.Y = Ball.Y + Ball.YOffset
 
			' Horizontally off the screen?
			IF Ball.X < 4 OR Ball.X > UsableWidth THEN
				DeflectX = TRUE
				LastX = FALSE
				LastY = FALSE
			END IF

			' At the top of the screen?
			IF Ball.Y < 4 THEN
				DeflectY = TRUE     ' Deflect the ball
				JustHit = FALSE     ' Allow a paddle to hit the ball
				LastX = FALSE
				LastY = FALSE
			ELSEIF Ball.Y > MAXROW THEN GameOver    'If not, did the ball just pass the lowest point that the paddles can go?
			END IF

			BallX = Ball.X \ 16     ' Determines where the ball is relative to the bricks (20 bricks fit on the 320 pixel screen so each brick is 16 pixels wide).
			BallY = Ball.Y \ 8      ' Similar to above but for the Y direction.  The screen is 200 pixels high.
  
			LevelOver = FALSE       ' Assume that the level is not over yet.
  
			IF Bricks(BallY, BallX) <> BGCOLOR THEN                 ' Hit a brick?
				' Yes.  Hit a brick.
				IF EraseBallOK THEN DrawBall Ball.OldX, Ball.OldY   ' Erase the ball.
				EraseBrick BallX, BallY                             ' Erase the brick.
				EraseBallOK = FALSE                                 ' Since the new ball location was never drawn, EraseBallOK must be FALSE to keep the game from trying to erase it.
				WhereX = Ball.X MOD 16                              ' Horizontal position within the brick.
			   
				' If the ball hits the left or right edge, try to bounce by
				'  changing the horizontal offset.  If that just happened,
				'  change the vertical offset instead.
				IF (NOT LastX AND (WhereX = 0 OR WhereX = 12)) OR LastY THEN
					DeflectX = TRUE ' Change X direction.
					LastX = TRUE    ' Mark last deflection as in the X direction.
				ELSE                ' If the ball hit the middle of a block...
					DeflectY = TRUE ' Change Y direction.
					LastY = TRUE    ' Mark last deflection as in the Y direction.
				END IF
			ELSE
				IF EraseBallOK THEN DrawBall Ball.OldX, Ball.OldY   ' Erase the old position of the ball unless another part of the program said not to erase it.
				DrawBall Ball.X, Ball.Y ' Draw ball in the new location.
				Ball.OldX = Ball.X      ' Update the old X and Y positions.
				Ball.OldY = Ball.Y
				EraseBallOK = TRUE      ' Assume that it is okay to delete the ball next time.
				LastX = FALSE           ' Reset LastX and LastY so they'll be clear next time.
				LastY = FALSE
			END IF
		   
			' The FOR...NEXT loop below tests to see if the new ball position
			' has hit a paddle.  If so, update ball and paddle.
			FOR Player = 1 TO NumberOfPlayers
				BallHitPaddle Player
			NEXT Player

			' Change the direction of the ball as appropriate.
			IF DeflectY AND NOT LevelOver THEN Ball.YOffset = -Ball.YOffset
			IF DeflectX THEN Ball.XOffset = -Ball.XOffset
		END IF
	
		k$ = INKEY$                 ' Get keypress.
 
		IF LEN(k$) > 0 THEN         ' Only execute the code below if a key was pressed.
			IF ASC(k$) = 0 THEN     ' This returns the ASC code of the left-most character in the string.  It is 0 if the key was an extended ASCII key - like the cursor keys.
				SELECT CASE ASC(LCASE$(RIGHT$(k$, 1)))  ' Use the right-most character to decide what key was pressed.
				CASE LEFT1
					MovePaddle -PADDLEHORIZONTALMOVE, 0, 1
				CASE RIGHT1
					MovePaddle PADDLEHORIZONTALMOVE, 0, 1
				CASE UP1
					MovePaddle 0, -PADDLEVERTICALMOVE, 1
				CASE DOWN1
					MovePaddle 0, PADDLEVERTICALMOVE, 1
				END SELECT
			ELSE                                        ' The first character was not ASCII 0 so the key was a normal letter or number.
				IF NumberOfPlayers = 2 THEN             ' Only execute if two people are playing.
					SELECT CASE ASC(LCASE$(RIGHT$(k$, 1)))  ' Use the ASCII value to evaluate which key was pressed.
					CASE LEFT2                          ' The letter s.
						MovePaddle -PADDLEHORIZONTALMOVE, 0, 2
					CASE RIGHT2                         ' The letter f.
						MovePaddle PADDLEHORIZONTALMOVE, 0, 2
					CASE UP2                            ' The letter e.
						MovePaddle 0, -PADDLEVERTICALMOVE, 2
					CASE DOWN2                          ' The letter d.
						MovePaddle 0, PADDLEVERTICALMOVE, 2
					END SELECT
				END IF
				SELECT CASE ASC(LCASE$(RIGHT$(k$, 1)))      'Regardless of the number of players, check for Quit and Pause.
				CASE PAUSE
					IF ScreenMode <> 1 THEN COLOR 12 + BACKGROUNDCOLOR  ' Change colors.

					SOUND 1100, .75                     ' Tone at 1100 hertz for 75 clock ticks.
					Center "* PAUSED *", MINROW \ 8 + 2 ' Display pause message.
					WHILE INKEY$ = "": WEND             ' Wait for a keypress.

					COLOR BGCOLOR                       ' Restore normal colors.
					Center SPACE$(10), MINROW \ 8 + 2

					' Ensures that the ball isn't duplicated if it is directly under the "* PAUSED *" text.
					IF EraseBallOK THEN
						EraseBall Ball.X, Ball.Y
						DrawBall Ball.X, Ball.Y
					END IF

					RedrawPaddles                       ' Draw the paddles again in case the PAUSED message overwrote them.
				CASE QUIT
					IF ScreenMode = 1 THEN              ' Set the correct color scheme.
						COLOR BGCOLOR
					ELSE
						COLOR 3 + BGCOLOR, BGCOLOR
					END IF

					SOUND 1700, 1           ' Tone at 1700 hertz for 1 clock tick.
					SOUND 1100, .75         ' Tone at 1100 hertz for .75 clock ticks.
					Center "Really quit? (Y/N) ", (MINROW \ 8 + 2)   ' Display prompt.
					DO
						k$ = UCASE$(INKEY$) ' Wait for desired key to be pressed.
					LOOP WHILE k$ = ""
					Center SPACE$(19), (MINROW \ 8 + 2)  ' Clear prompt off of the screen.

					IF k$ = "Y" THEN        ' Does player want to quit?
						Again = FALSE       ' Set Again (for ' PLAY AGAIN' ) so that the game will not restart.
						NumBalls = -1       ' Set number of balls to ending amount.
					END IF

					' Ensures that the ball isn't duplicated if it is directly under the "Really Quit? (Y/N)" text.
					IF EraseBallOK THEN
						EraseBall Ball.X, Ball.Y
						DrawBall Ball.X, Ball.Y
					END IF

					RedrawPaddles           ' Draw paddles again.
				END SELECT
			END IF
		END IF
	   
		IF NeedBall THEN            ' See if a new ball is needed.
			NeedBall = FALSE        ' Reset flag.
			NewBall                 ' Launch a new ball.
		END IF
	LOOP WHILE Again
	
	DisplayChanges                  ' Display suggested changes screen.
	
	DEF SEG = 0                     ' Restore the previous flag settings.
	POKE 1047, KeyFlags
	DEF SEG                         ' Restore the default segment.

END IF

END


' The following is the data for all 5 of the brick patterns used in the game.
' The data for each pattern must be 7 rows by 20 columns (delimited by commas).
' A "0" is used for a blank brick.  Any other number represents a color
' code for that brick (range 1 - 15).

' Data for Screen 1
DATA  4,5,3,3,3,3,0,0,0,0,0,0,0,0,3,3,3,3,5,4
DATA  0,4,5,3,3,3,2,2,0,0,0,0,2,2,3,3,3,5,4,0
DATA  0,0,4,5,3,3,3,2,3,0,0,3,2,3,3,3,5,4,0,0
DATA  0,0,4,5,3,3,3,3,3,3,3,3,3,3,3,3,5,4,0,0
DATA  0,0,0,0,4,5,5,3,3,3,3,3,5,5,5,4,0,0,0,0
DATA  0,0,0,0,0,0,4,5,5,3,3,5,5,4,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,4,4,4,4,4,4,0,0,0,0,0,0,0

' Data for Screen 2
DATA  1,1,1,1,1,0,0,0,1,1,1,1,0,0,0,1,1,1,1,1
DATA  9,9,9,9,9,0,0,0,9,9,9,9,0,0,0,9,9,9,9,9
DATA  5,5,5,5,5,0,0,0,5,5,5,5,0,0,0,5,5,5,5,5
DATA  13,13,13,13,13,0,0,0,13,13,13,13,0,0,0,13,13,13,13,13
DATA  3,3,3,3,3,0,0,0,3,3,3,3,0,0,0,3,3,3,3,3
DATA  11,11,11,11,11,0,0,0,11,11,11,11,0,0,0,11,11,11,11,11
DATA  11,11,11,11,11,0,0,0,11,11,11,11,0,0,0,11,11,11,11,11

' Data for Screen 3
DATA  1,1,8,8,8,1,1,0,0,5,5,0,0,1,1,8,8,8,1,1
DATA  5,1,1,8,1,1,5,0,0,5,5,0,0,5,1,1,8,1,1,5
DATA  0,5,1,1,1,5,0,0,5,5,5,5,0,0,5,1,1,1,5,0
DATA  0,5,5,1,5,5,0,0,5,1,1,5,0,0,5,5,1,5,5,0
DATA  0,0,5,1,5,0,0,5,5,1,1,5,5,0,0,5,1,5,0,0
DATA  0,0,0,5,0,0,0,5,1,8,8,1,5,0,0,0,5,0,0,0
DATA  0,0,0,5,0,0,1,1,8,8,8,8,1,1,0,0,5,0,0,0

' Data for Screen 4
DATA  5,2,14,9,0,0,0,0,9,14,14,9,0,0,0,0,9,14,2,5
DATA  5,5,2,14,14,0,0,14,14,2,2,14,14,0,0,14,14,2,5,5
DATA  0,5,5,2,2,9,9,2,2,9,9,2,2,9,9,2,2,5,5,0
DATA  0,0,5,5,5,5,5,9,9,5,5,9,9,5,5,5,5,5,0,0
DATA  0,5,5,2,2,9,9,2,2,9,9,2,2,9,9,2,2,5,5,0
DATA  5,5,2,14,14,0,0,14,14,2,2,14,14,0,0,14,14,2,5,5
DATA  5,2,14,9,0,0,0,0,9,14,14,9,0,0,0,0,9,14,2,5

' Data for Screen 5
DATA  0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0
DATA  1,0,1,9,1,0,1,9,1,0,0,1,9,1,0,1,9,1,0,1
DATA  9,1,9,5,9,1,9,5,9,1,1,9,5,9,1,9,5,9,1,9
DATA  5,9,5,13,5,9,5,13,5,9,9,5,13,5,9,5,13,5,9,5
DATA  13,5,13,4,13,5,13,4,13,5,5,13,4,13,5,13,4,13,5,13
DATA  4,13,4,0,4,13,4,0,4,13,13,4,0,4,13,4,0,4,13,4
DATA  0,4,0,0,0,4,0,0,0,4,4,0,0,0,4,0,0,0,4,0


ScreenError:                ' Screen test error-handling routine.
	BadMode = TRUE
	RESUME NEXT

'----------------------------------------------------------------------------
' BallHitPaddle
'
'    Deflects the ball if the ball hits the paddle.
'
'           PARAMETERS:   Player - Which player's paddle to check
'----------------------------------------------------------------------------
SUB BallHitPaddle (Player)
   
	' Checks if the paddle and the ball overlap.
	IF ABS(Paddle(Player).Y - Ball.Y) < 8 AND Ball.X >= Paddle(Player).X AND Ball.X <= Paddle(Player).X + Paddle(Player).Size THEN
		DrawPaddle Paddle(Player).PColor, Player
	   
		' Players can only hit the ball once before the ball must hit the top,
		' a brick, or another paddle.
	   
		IF EraseBallOK THEN DrawBall Ball.X, Ball.Y ' Erase the ball if appropriate.
		EraseBallOK = FALSE                         ' Make sure that main ball control section does not try to erase a ball that was already erased.
	   
		IF JustHit <> Player THEN
			Ball.YOffset = -Ball.YOffset    ' Deflect the ball.
			JustHit = Player                ' JustHit assures that the same player doesn't hit the ball more than once before the ball hits a brick, the top, or the other paddle.
			LastHitBy = Player              ' Used to assign scores properly.
			PLAY PADDLEHITSOUND
			DrawPaddle Paddle(Player).PColor, Player
			LastX = FALSE
			LastY = FALSE
		END IF
	END IF

END SUB

'----------------------------------------------------------------------------
' Center
'
'    Centers a string of text on a specified row.
'
'           PARAMETERS:   Row   - Row (line) to put the text on
'                         Text$ - Text to be centered
'----------------------------------------------------------------------------
SUB Center (text$, Row)

	LOCATE Row, (ScreenWide \ 2) - LEN(text$) \ 2 + 1  ' Calculate the position on the screen where the text should be centered
	PRINT text$;

END SUB

'----------------------------------------------------------------------------
' DisplayChanges
'
'    Displays list of changes that the player can easily make.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayChanges

	DisplayGameTitle        ' Print game title.
	
	COLOR 7                 ' White text.
	Center "The following game characteristics can be easily changed from", 5
	Center "within the QuickBASIC Interpreter.  To change the values of  ", 6
	Center "these characteristics, locate the corresponding CONST or DATA", 7
	Center "statements in the source code and change their values, then  ", 8
	Center "restart the program (press Shift+F5).                        ", 9

	COLOR 15
	Center "Block patterns               ", 11
	Center "Length of paddles            ", 12
	Center "Number of special bricks     ", 13
	Center "Shape of the special bricks  ", 14
	Center "End-of-level bonus multiplier", 15
	Center "Paddle color                 ", 16
	Center "Ball color                   ", 17
	Center "Ball speed                   ", 18

	COLOR 7             ' White letters.
	Center "The CONST statements and instructions on changing them are   ", 20
	Center "located at the beginning of the main program.                ", 21

	DO WHILE INKEY$ = "": LOOP  ' Wait for any keypress.
	CLS

END SUB

'----------------------------------------------------------------------------
' DisplayGameTitle
'
'    Displays the title of the game.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB DisplayGameTitle
	
	SCREEN 0                        ' Set Screen mode 0.
	WIDTH 80, 25                    ' Set width to 80, height to 25.
	COLOR 4, 0                      ' Set colors for red on black.
	CLS                             ' Clear the screen.
	ScreenWide = 80                 ' Set screen width variable to match current width.

	' Draw an outline around screen with extended ASCII characters.
	LOCATE 1, 2
	PRINT CHR$(201); STRING$(76, 205); CHR$(187);   ' Draw top border.
	FOR X = 2 TO 24                 ' Draw left and right borders.
		LOCATE X, 2
		PRINT CHR$(186); TAB(79); CHR$(186);
	NEXT X
	LOCATE 25, 2
	PRINT CHR$(200); STRING$(76, 205); CHR$(188);   ' Draw bottom border.

	' Print game title centered at top of screen.
	COLOR 0, 4                      ' Set colors to black (0) on red (4) letters.
	Center "     Microsoft     ", 1                 ' Center game title on lines
	Center "   Q B R I C K S   ", 2                 ' 1 and 2.
	Center "   Press any key to continue   ", 25    ' Center prompt on line 25.
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

	DisplayGameTitle                ' Display game title.

	COLOR 7
	Center "Copyright (C) 1990 Microsoft Corporation.  All Rights Reserved.", 4
	Center "Score points by deflecting the ball into the brick walls. In a      ", 6
	Center "two-player game, the player to hit the ball last gets the points.   ", 7
	Center "Hit the special bricks (" + CHR$(SPECIALCHAR) + ") or clear all the bricks to advance to    ", 8
	Center "the next level.  Ball speed increases every level and the paddles(s)", 9
	Center "shorten after a certain level.  Bonus balls are awarded for clearing", 10
	Center "several levels. The game ends when all balls have been played.      ", 11

	COLOR 4
	LOCATE 13, 4
	PRINT STRING$(74, 196)          ' Put horizontal red line on screen.
	COLOR 7                         ' Change foreground color back to white.
	Center " Game Controls ", 13    ' Display game controls.
	Center "General               Player 1               Player 2    ", 15
	Center "                        (Up)                   (Up)      ", 17
	Center "P - Pause                " + CHR$(24) + "                      E       ", 18
	Center "    Q - Quit        (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)   (Left) S   F (Right)  ", 19
	Center "                         " + CHR$(25) + "                      D       ", 20
	Center "                       (Down)                 (Down)     ", 21

	PLAY STARTSOUND                     ' Play melody for introduction.
	DO                                  ' Wait for any key to be pressed.
		kbd$ = UCASE$(INKEY$)
	LOOP WHILE kbd$ = ""
	IF kbd$ = "Q" THEN                  'Allow player to quit now
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
' DrawBall
'
'    Draws or erases the ball.  By default, PUT replaces the new graphic
'    image with whatever was already on the screen.  The first PUT statement
'    draws the object; the second PUT statement to the same location erases
'    the object without affecting any other objects.
'
'           PARAMETERS:   BallX     - X (horizontal) location of the ball, in pixels
'                         BallY     - Y (vertical) location of the ball, in pixels
'----------------------------------------------------------------------------
SUB DrawBall (BallX, BallY)

	PUT (BallX, BallY), Ballshape

END SUB

'----------------------------------------------------------------------------
' DrawBrick
'
'    Draws or erases a brick.
'
'           PARAMETERS:   BrickX     - X location of the brick, in logical units
'                         BrickY     - Y location of the ball, in screen rows
'                         BrickColor - Color to draw the brick
'----------------------------------------------------------------------------
SUB DrawBrick (BrickX, BrickY, BrickColor)
  
	' Calculate screen locations from the logical location of the brick.
	X = BrickX * PIXELSIZE * BRICKSIZE
	Y = BrickY * PIXELSIZE
	Size = BRICKSIZE * PIXELSIZE

	' Decide if erasing or drawing a brick.
	IF BrickColor = BGCOLOR THEN
		LINE (X, Y)-(X + Size, Y + PIXELSIZE - 1), BGCOLOR, BF
	ELSE                            ' Draw the brick...
		LINE (X + 1, Y + 1)-(X + Size - 1, Y + PIXELSIZE - 1), 15, B
		PAINT (X + 2, Y + 2), BrickColor, 15
		LINE (X + 1, Y + PIXELSIZE - 1)-(X + Size - 1, Y + PIXELSIZE - 1), 7
		LINE (X + Size - 1, Y + 1)-(X + Size - 1, Y + PIXELSIZE - 1), 7
	END IF

END SUB

'----------------------------------------------------------------------------
' DrawPaddle
'
'    Draws or erases a paddle.
'
'           PARAMETERS:   PColor - Paddle color.  Erases the paddle if PColor is set to
'                                  the background color (BgColor)
'                         Player - Which paddle to affect
'----------------------------------------------------------------------------
SUB DrawPaddle (PColor, Player)

	LINE (Paddle(Player).X, Paddle(Player).Y)-(Paddle(Player).X + Paddle(Player).Size, Paddle(Player).Y + 1), PColor, BF

END SUB

'----------------------------------------------------------------------------
' EraseBall
'
'    Erases the ball by drawing a square filled with the background color
'    over the ball.
'
'           PARAMETERS:   X - X (horizontal) location of the ball, in pixels
'                         Y - Y (vertical) location of the ball, in pixels
'----------------------------------------------------------------------------
SUB EraseBall (X, Y)

	LINE (X, Y)-(X + 2, Y + 2), BGCOLOR, BF

END SUB

'----------------------------------------------------------------------------
' EraseBrick
'
'    Logically erases a brick struck by the ball.  Calls DrawBrick
'    to physically erase the brick.
'
'           PARAMETERS:   X - Logical horizontal location (column) of the brick
'                         Y - Logical vertical location (row) of the brick
'----------------------------------------------------------------------------
SUB EraseBrick (X, Y)

	IF LevelOver = TRUE THEN EXIT SUB    ' Just to be sure a new level does not
										 ' immediately erase bricks.

	BrickHit = Bricks(Y, X)              ' Store the value of the brick that was hit in case it was a special brick.
	LevelCount = LevelCount + BrickHit   ' Add the brick color value to the LevelCount total.  This is necessary to know when to stop a round if no special bricks are used.
	Bricks(Y, X) = BGCOLOR               ' Logically erase the brick.
	DrawBrick X, Y, BGCOLOR              ' Physically erase the brick.
			   
	Octave$ = STR$(Y MOD 7)
	PLAY BLOCKHITSOUND + Octave$ + " c"

	' Score the hit.
	Paddle(LastHitBy).Score = Paddle(LastHitBy).Score + 10 * Y
	Paddle(LastHitBy).NumBricksHit = Paddle(LastHitBy).NumBricksHit + 1
	UpdateScreen
	JustHit = FALSE ' Set JustHit to FALSE to allow a paddle to hit the ball.

	' See if the brick was a special brick or all the bricks have been hit.
	IF BrickHit = 1000 OR LevelCount = MAXLEVELCount THEN
		NextLevel          ' Go to next level
		LevelOver = TRUE
	END IF

END SUB

'----------------------------------------------------------------------------
' GameOver
'
'    Checks to see if the game should be considered over.  If yes, end game.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB GameOver

	DrawBall Ball.X, Ball.Y  ' Ensure that the ball is fully erased.

	NumBalls = NumBalls - 1  ' Reduce the number of balls remaining by one.
	UpdateScreen             ' Update display to show correct number of balls remaining.

	IF NumBalls < 1 THEN     ' If player has no more balls left,
		PLAY GAMEOVERSOUND
		DrawBall Ball.X, Ball.Y         ' Erases the ball that was just drawn.
		DrawBall Ball.OldX, Ball.OldY   ' Erases the last real ball position.
	   
		' Set up information for a one player print out
		LOCATE 25, 1: PRINT SPACE$(SCREENWIDTH);
		DrawPaddle BGCOLOR, 1                       ' Erase paddle 1.
		Play1Bricks$ = "Bricks hit:" + RIGHT$((SPACE$(2) + STR$(Paddle(1).NumBricksHit)), 4)
		Play1Score$ = "Score:" + RIGHT$((SPACE$(9) + STR$(Paddle(1).Score)), 9)
		
		IF NumberOfPlayers = 1 THEN
			Center "Player 1 stats", MAXBLOCKROW + 6 ' Print the statistics.
			Center Play1Bricks$, MAXBLOCKROW + 8
			Center Play1Score$, MAXBLOCKROW + 10
			End$ = ""
		ELSE                                ' Generate strings for the 2 player statistics.
			Play2Bricks$ = Play1Bricks$ + "     Bricks hit:" + RIGHT$((SPACE$(2) + STR$(Paddle(2).NumBricksHit)), 4)
			Play2Score$ = Play1Score$ + "     Score:" + RIGHT$((SPACE$(9) + STR$(Paddle(2).Score)), 9)
			DrawPaddle BGCOLOR, 2           ' Erase paddle 2.
	
			WhoWon$ = "Tie Game.  Nobody"                 ' Assume tie game.
			IF Paddle(1).Score > Paddle(2).Score THEN     ' Player 1 won.
				WhoWon$ = "Player 1"
			ELSEIF Paddle(1).Score < Paddle(2).Score THEN 'Player 2 won.
				WhoWon$ = "Player 2"
			END IF
	   
			Center "Player 1 stats" + SPACE$(6) + "Player 2 stats ", MAXBLOCKROW + 6
			Center Play2Bricks$, MAXBLOCKROW + 8    ' Print the two-player stats.
			Center Play2Score$, MAXBLOCKROW + 10
			End$ = WhoWon$ + " is the winner!" ' Display winner.
		END IF
	
		Center End$, MAXBLOCKROW + 2         ' Show winner if two-player game, otherwise print a space.
		Center "Last level played: " + STR$(Level), MAXBLOCKROW + 4   ' Show the last level.
		Center "Play again? (Y/N)", 24       ' Center prompt for Play Again.

		DO
			k$ = UCASE$(INKEY$)             ' Accept a key from the player.
		LOOP WHILE k$ <> "Y" AND k$ <> "N"  ' Wait for either Y or N.

		Again = FALSE

		IF k$ = "Y" THEN                    ' Does user wish to play again?
			Again = TRUE                    ' Yes, restart game.
			Level = 0
			NextLevel
		END IF
		NeedBall = FALSE                    ' Not out of balls.
	ELSE
		NeedBall = TRUE                     ' Out of balls.
	END IF

END SUB

'----------------------------------------------------------------------------
' GameParamSetup
'
'     Initializes game values and player paddle values before game begins.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB GameParamSetup

	Paddle(1).PColor = PLAYERCOLOR1         ' Set up paddle colors for player 1.
	TempPADDLELENGTH = PADDLELENGTH1        ' Store the length of the paddle.

	IF NumberOfPlayers = 2 THEN             ' Do only if there are two players.
		Paddle(2).PColor = PLAYERCOLOR2     ' Set up paddle colors for player 2.
		TempPADDLELENGTH = PADDLELENGTH2    ' Store the length of the paddle.
	END IF

	ScreenWide = SCREENWIDTH                ' Make the ScreenWide variable equal to the true SCREENWIDTH.
	GraphicsWidth = ScreenWide * PIXELSIZE  ' Determine how many pixels wide the screen is.
	UsableWidth = GraphicsWidth - 7
	Ball.PColor = BALLCOLOR                 ' Set the color, speed, and number of balls.
	NumBalls = INITNUMBALLS
	
	' Determine machine performance in a generic manner...
	X! = TIMER
	FOR g! = 1 TO 500
	NEXT g!
	X! = TIMER - X!
	SELECT CASE X!
		CASE 0 TO .39            ' For 386-type machines.
			ActualBallSpeed = INITIALBALLSPEED
		CASE IS < .5          ' For PC/AT-type machines.
			ActualBallSpeed = INITIALBALLSPEED / 2
		CASE ELSE                ' For XT-type machines.
			ActualBallSpeed = 0
	END SELECT
	Ball.speed = ActualBallSpeed     ' Set the actual start-up speed.

	FOR Indx = 1 TO NumberOfPlayers  ' Set scores and paddle sizes to initial values.
		Paddle(Indx).Size = TempPADDLELENGTH
		Paddle(Indx).Score = 0
		Paddle(Indx).NumBricksHit = 0
	NEXT Indx

	SCREEN ScreenMode               ' Use the best graphics mode available.
	IF ScreenMode = 7 THEN
		COLOR BGCOLOR, BGCOLOR  ' Set appropriate colors.
	ELSE
		COLOR BGCOLOR
	END IF

	CLS                             ' Draw the ball and store it in an array for fast animation.
	LINE (50, 49)-(50, 51), Ball.PColor
	LINE (49, 50)-(51, 50), Ball.PColor
	GET (49, 49)-(51, 51), Ballshape
	
END SUB

'----------------------------------------------------------------------------
' GetGameOptions
'
'    Asks how many people will be playing.
'
'           PARAMETERS:   None
'----------------------------------------------------------------------------
SUB GetGameOptions

	COLOR 7         ' Set colors for screen to be cleared.
	CLS

	LOCATE 9, 32: PRINT "Default is"; DEFAULTPLAYERS
	COLOR 15
	DO
		LOCATE 8, 24: PRINT SPACE$(50)
		LOCATE 8, 24
		INPUT "How many players? (1 or 2) ", PaddleHold$
	LOOP UNTIL PaddleHold$ = "1" OR PaddleHold$ = "2" OR LEN(PaddleHold$) = 0
	NumberOfPlayers = VAL(PaddleHold$)
	IF NumberOfPlayers = 0 THEN NumberOfPlayers = DEFAULTPLAYERS

END SUB

'----------------------------------------------------------------------------
' HorizontalScroll
'
'     Displays a string moving across the screen at a given line.
'
'           PARAMETERS:    M$  - String to be displayed
'                          Row - Screen row where string is displayed
'----------------------------------------------------------------------------
SUB HorizontalScroll (M$, Row)

	M$ = SPACE$(ScreenWide + 2) + M$    ' Add ending spaces for display.
	FOR i = 1 TO LEN(M$) - 1            ' Loop through the message in m$.
		LOCATE Row, 1                   ' Position the message on passed row value.
		PRINT MID$(M$, LEN(M$) - i, ScreenWide - 1) ' Use the MID$() function to print a SCREENWIDTH-1 character piece of the entire message.  The piece is determined by the value of X.
		Delay! = TIMER + .05            ' Delay the printing of each letter by .1 second.
		DO WHILE TIMER < Delay!: k$ = INKEY$: LOOP  ' Clears keyboard buffer.
	NEXT i
	RedrawPaddles                       ' In case the text covered the paddle(s).

END SUB

'----------------------------------------------------------------------------
' MovePaddle
'
'     Checks to see if the paddle can be displayed at the new location.  If so, draw it.
'
'           PARAMETERS:    NewX      - X offset from current paddle position
'                          NewY      - Y offset from current paddle position
'                          PlayerNum - Which player's paddle to move
'----------------------------------------------------------------------------
SUB MovePaddle (NewX, NewY, PlayerNum)

	' Use temporary variables in case the paddle cannot move to the new location.
	TempX = Paddle(PlayerNum).X + NewX      ' Set temporary variables in case the paddle
	TempY = Paddle(PlayerNum).Y + NewY      '  cannot move to the new location
	
	OppOver = FALSE                         ' Assume that paddles do not overlap.
	OppUnder = FALSE
   
	IF NumberOfPlayers = 2 THEN
		OppNum = 3 - PlayerNum              ' Get number of opponent.
	   
		' Is opponent under this paddle?
		IF TempX >= Paddle(OppNum).X AND TempX <= (Paddle(OppNum).X + Paddle(OppNum).Size - 1) THEN
			OppUnder = TRUE
		END IF
	   
		' Or above this paddle?
		IF TempX <= Paddle(OppNum).X AND (TempX + Paddle((PlayerNum)).Size - 1) >= Paddle(OppNum).X THEN
			OppOver = TRUE
		END IF
	   
		' Cannot move vertically into the other paddle.
		IF NewX = 0 AND TempY = Paddle(OppNum).Y AND (OppOver OR OppUnder) THEN EXIT SUB
	   
		' Cannot move horizontally into the other paddle
		IF NewY = 0 AND TempY = Paddle(OppNum).Y AND OppUnder THEN TempX = Paddle(OppNum).X + Paddle(OppNum).Size
		IF NewY = 0 AND TempY = Paddle(OppNum).Y AND OppOver THEN TempX = Paddle(OppNum).X - Paddle(OppNum).Size
	END IF

	' Do not move paddle if new position is out of bounds.
	IF TempY > MAXROW OR TempY < MINROW THEN EXIT SUB
	IF TempX < 1 THEN TempX = 1
	IF TempX + Paddle(PlayerNum).Size >= GraphicsWidth THEN TempX = GraphicsWidth - Paddle(PlayerNum).Size
		
	' Erase old paddle location, update the paddle location, and draw the paddle at the new location.
	DrawPaddle BGCOLOR, PlayerNum
	Paddle(PlayerNum).OldX = Paddle(PlayerNum).X
	Paddle(PlayerNum).OldY = Paddle(PlayerNum).Y
	Paddle(PlayerNum).X = TempX
	Paddle(PlayerNum).Y = TempY
	BallHitPaddle PlayerNum ' Check to see if the paddle is hitting the ball now.
	DrawPaddle Paddle(PlayerNum).PColor, PlayerNum

END SUB

'----------------------------------------------------------------------------
' NewBall
'
'     Launches a new ball at the start of the game, to start a new
'     level, or when a player misses the ball.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB NewBall

	' Set the new location of the ball.
	Ball.X = INT(RND(1) * 20) * 4 + 120' Make the ball roughly centered.
	Ball.Y = MINROW - 8
	Ball.OldX = Ball.X
	Ball.OldY = Ball.Y

	Ball.XOffset = 4            ' Set the offsets of the ball.
	Ball.YOffset = 4
   
	' Determine left or right movement.
	IF RND(1) > .5 THEN Ball.XOffset = -Ball.XOffset

	SetDefaultPaddle

	DrawBall Ball.X, Ball.Y     ' Draw the ball.
	UpdateScreen                ' Update information displayed on the screen.
	JustHit = FALSE             ' Have not hit anything yet.

	FOR Indx = 1 TO 2           ' Generate two beeps.
		SOUND 300, .4
		Restart& = TIMER + .9   ' Calculate amount of time to wait before starting ball moving.
		DO
			ClearKeyBuffer$ = INKEY$    ' Clear the keyboard buffer.
		LOOP WHILE TIMER < Restart&
	NEXT Indx

	' Two quick beeps to warn the player(s) that the round is about to start.
	SOUND 300, .4
	SOUND 400, .2
   
	DO WHILE INKEY$ <> "": LOOP     ' Clear the keyboard buffer just in case.
   
	EraseBall Ball.X, Ball.Y        ' Erase the ball.
	EraseBallOK = FALSE             ' Be sure that the ball updating code does not try to erase a ball that wasn't drawn.

END SUB

'----------------------------------------------------------------------------
' NextLevel
'
'     Prepares to begin a new level by awarding bonus points, drawing new brick
'     walls, etc.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB NextLevel
   
	Level = Level + 1
	IF Level = 1 THEN   ' First round.
		GameParamSetup
	ELSE
		PLAY NEXTLEVELSOUND
		LevelEnd$ = " Level" + STR$(Level - 1) + " completed! " + STR$((Level - 1) * BONUSMULTIPLIER) + " Bonus Points! "
		HorizontalScroll LevelEnd$, 15              ' Display prompt saying level is complete.
		Ball.speed = INT(ActualBallSpeed * (.95 ^ Level) * 100 + .5) / 100  ' Increase ball speed.
		
		FOR Indx = 1 TO (BONUSMULTIPLIER * (Level - 1)) \ 100   ' Add bonus points.
			Paddle(LastHitBy).Score = Paddle(LastHitBy).Score + 100
			UpdateScreen
			SOUND (Indx * 25 + 15), 1 / Indx * 18.2         ' Play the sound while adding bonus.
			PauseLen# = TIMER + 1 / Indx
			WHILE TIMER < PauseLen#: WEND
		NEXT Indx
		EraseBallOK = TRUE
	END IF

	CLS

	FOR Y = 0 TO 25                         ' Clear bricks in the array.
		FOR X = 0 TO 20
			Bricks(Y, X) = BGCOLOR
		NEXT X
	NEXT Y
	
	IF Level MOD CUTLEVEL = 0 THEN          ' See if it is time to shorten the paddles.
		TempPADDLELENGTH = TempPADDLELENGTH * ((100 - PADDLECUT) / 100)   ' Decrease paddle size.
		IF TempPADDLELENGTH < 8 THEN TempPADDLELENGTH = 8   ' But no shorter than 8 pixels.
		
		FOR Indx = 1 TO NumberOfPlayers     ' Set both paddles to the same length.
			Paddle(Indx).Size = TempPADDLELENGTH
		NEXT Indx
	END IF
   
	' See if it is time for a bonus ball.
	IF Level MOD LEVELNEWBALL = 0 THEN NumBalls = NumBalls + 1
	IF Level MOD MAXLEVEL = 1 THEN RESTORE  ' Have all designs been shown ?

	MAXLEVELCount = 0                       ' Reset brick counting variables.
	LevelCount = 0
	
	FOR Y = STARTBRICKROW TO MAXBLOCKROW - 1      ' Draw new brick pattern.
		FOR X = 0 TO 19
			READ C                                ' Get data for new block.
			Bricks(Y, X) = (C MOD 32) + BGCOLOR
			IF Bricks(Y, X) <> BGCOLOR THEN
				MAXLEVELCount = MAXLEVELCount + (C MOD 32) ' Add the value to MAXLEVELCount so that the end of the level can be detected if there are no special bricks.
			   
				' Draw the brick using the correct number of colors for this screen mode.
				IF ScreenMode = 1 THEN
					DrawBrick X, Y, Bricks(Y, X) MOD 3 + 1
				ELSE
					DrawBrick X, Y, Bricks(Y, X) MOD 16
				END IF
			END IF
		NEXT X
	NEXT Y

	' Put the special bricks on the screen, replacing existing bricks.
	Indx = 1
	DO WHILE Indx <= NUMSPECIALBRICKS
		DO
			' Select random X and Y positions of the special brick.
			XRandom = INT(RND(1) * 20)
			YRandom = INT(RND(1) * 5) + STARTBRICKROW
		LOOP WHILE Bricks(YRandom, XRandom) = BGCOLOR OR Bricks(YRandom, XRandom) = 1000    ' Make sure the special brick goes into an existing bricks that has not already been used by another special brick.

		IF ScreenMode <> 1 THEN COLOR 14        ' Set color of special bricks.
		LOCATE YRandom + 1, XRandom * BRICKSIZE + 1 ' Move cursor to location of brick.
		PRINT special;                          ' Print the special brick.
		Bricks(YRandom, XRandom) = 1000         ' Put 1000 into the Bricks array where the special brick is so that EraseBrick can detect when a special brick is hit.
		Indx = Indx + 1
	LOOP
   
	NewBall                     ' Get a new ball.

END SUB

'----------------------------------------------------------------------------
' RedrawPaddles
'
'     Draws the paddles.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB RedrawPaddles

	FOR paddles = 1 TO NumberOfPlayers
		DrawPaddle Paddle(paddles).PColor, paddles
	NEXT paddles

END SUB

'----------------------------------------------------------------------------
' SetDefaultPaddle
'
'     Puts the paddle(s) into their respective starting places.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB SetDefaultPaddle

	FOR i = 1 TO NumberOfPlayers     ' Clear the current paddles.
		DrawPaddle BGCOLOR, i
	NEXT i

	IF NumberOfPlayers = 2 THEN      ' Set the default position of the paddle(s).
		FOR PaddleNumber = 1 TO NumberOfPlayers
			Paddle(PaddleNumber).X = (((GraphicsWidth - 80) \ PaddleNumber) - TempPADDLELENGTH)         ' Sets the horizontal position of the paddle(s).
			Paddle(PaddleNumber).Y = MAXROW
		NEXT PaddleNumber
	ELSE
		Paddle(1).X = (GraphicsWidth - TempPADDLELENGTH) / 2
		Paddle(1).Y = MAXROW
	END IF

	RedrawPaddles            ' Show them again.

END SUB

'----------------------------------------------------------------------------
' UpdateScreen
'
'    Puts new scores, levels, and ball counts on the screen.
'
'           PARAMETERS:    None
'----------------------------------------------------------------------------
SUB UpdateScreen

	IF ScreenMode <> 1 THEN COLOR SCORECOLOR         ' Set screen color for messages.
	LOCATE 25, 1

	' Display the data a little differently for one-player and two-player games.
	IF NumberOfPlayers = 1 THEN
		PRINT USING "Balls:###"; NumBalls;
		PRINT USING " Level:###"; Level;
		IF Paddle(1).Score > 9999000 THEN Paddle(1).Score = 0
		PRINT USING " Player 1:#,###,###"; Paddle(1).Score;
	ELSE
		IF Paddle(2).Score > 999000 THEN Paddle(2).Score = 0
		PRINT USING "Play2:###,###"; Paddle(2).Score;
		PRINT USING " B:###"; NumBalls;
		PRINT USING " L:###"; Level;
		IF Paddle(1).Score > 999000 THEN Paddle(1).Score = 0
		PRINT USING " Play1:###,###"; Paddle(1).Score;
	END IF

END SUB

