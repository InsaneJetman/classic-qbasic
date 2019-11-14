'
'                                QSPACE.BAS
'
'         Copyright (C) 1990 Microsoft Corporation. All Rights Reserved.
'
' Your mission in QSpace is to defend your orbiting starbases from enemy
' attack.  Protect them by firing your own interceptor missiles to destroy
' the incoming missiles.
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
'
'    Enemy missile speed at the start of the game
'    Songs played during this game
'    Color of the game pieces (EGA or VGA systems only)
'    Speed of the targeting crosshair
'    Number of missiles falling at the start of the game
'    Size of missile explosions
'    Duration of the explosions
'    GAME OVER messages
'
' On the right side of each CONST statement, there is a comment that tells
' you what it does and how big or small you can set the value.  Above the
' DATA statements, there are comments that tell you the format of the
' information stored there.
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

' These CONST statements define things in the game that you can control.
CONST GAMEBKGROUNDS7 = 0            ' Screen background color in screen mode 7. Can't be any of the other game colors.  Usually is black (0).
CONST INITIALDELAY = .1             ' Initial value of the Incoming missile delay.  Increase the number to make the Incoming missiles slower; decrease to make them faster.  After odd-numbered waves, the IncomingDelay gets 33% shorter.
CONST INITNUMSTARBASES = 3          ' Initial number of starbases.  This sets the starting value of the MaxStarbases variable.  Range is 1 to 4.
CONST INITNUMMISSILES = 3           ' Number of incoming missiles when the game starts.  This is just the start - the number of missiles increases as you complete more waves.  Range 1 to 6.
CONST TARGETSPEED = 10              ' How fast and far the target moves.  Range 4 to 30.
CONST EXPLOSIONDELAY = .1           ' Rate that explosions grow.  Range .05 to .25.
CONST EXPLRADIUS = 16               ' How big the explosion gets before it stops and is erased.  Range 5 to 75.
CONST PLANETS7 = 9                  ' Planet color in screen mode 7.
CONST BASECOLORS7 = 7               ' Starbase color in screen mode 7. Can't be the same as GAMEBKGROUNDS7.
CONST ENEMYCOLORS7 = 3              ' Enemy missile color in screen mode 7.  Can't be the same as GAMEBKGROUNDS7.
CONST INTERCEPTCOLORS7 = 2          ' Interceptor missile color in screen mode 7.  Can't be the same as GAMEBKGROUNDS7.
CONST EXPLCOLORS7 = 12              ' Explosion color in screen mode 7. Can't be the same as GAMEBKGROUNDS7.
CONST TARGETCOLORS7 = 2             ' Target crosshair color for screen mode 7.  Can't be the same as GAMEBKGROUNDS7.
CONST FASTESTMISSILE = .005         ' Lowest time delay between enemy missile movements.
CONST RATIOINTERCEPTTOINCOMING = 5  ' How fast your interceptor missiles move compared to enemy missiles.  At 5, interceptors move at least 5 times faster than enemy missiles.  Range 1 to 20.
' The following sound constants are used by the PLAY command to
' produce music during the game.  To change the sounds you hear, change
' these constants.  Refer to the online help for PLAY for the correct format.
' To completely remove sound from the game set the constants equal to null.
' For example:  GAMESTARTSONG = ""
CONST GAMESTARTSONG = "MBT150L4O2CD-CL8A-FAECD-L4A-F"           ' Played when program starts. No limits.
CONST WAVEOVERSONG = "MB O2 T240 L4 N40 N44 N48 N55 N48 L2 N53" ' Played at the end of each wave. No limits.
CONST GAMEOVERSONG = "MB O1 T240 L2 g- g- L1 a"                 ' Played when the game is over. No limits.

' The following CONST statements should not be changed like the ones above
' because the program relies on them being this value.
CONST TRUE = -1                 ' Microsoft QuickBASIC uses -1 to mean TRUE.
CONST FALSE = 0                 ' 0 means FALSE.
CONST XSCALE = 320              ' Width of the screen.
CONST YSCALE = 200              ' Height of the screen.
CONST MAXY = YSCALE - 11        ' Highest vertical position that a missile can be.
CONST MINY = 11                 ' Lowest position an incoming missile can be can be.
CONST MINX = 11                 ' Left-most position an incoming missile can be.
CONST MAXX = XSCALE - 11        ' Right-most position that an Incoming missile can be.
CONST LEFTLAUNCHER = 90         ' Key that controls left FireBase.
CONST RIGHTLAUNCHER = 88        ' Key that controls right FireBase.
CONST PAUSE = 25                ' Key that pauses the game.
CONST QUIT = 16                 ' Key that quits the game.
CONST FACTOR = 250              ' Used to determine the radius of the starbases.  Increase to make the starbases smaller; decrease to make the starbases larger.
CONST PI = 3.14                 ' Value of the mathematical constant PI.  Used in determining the position of the starbases orbiting the planet.
CONST PLANETRADIUS = XSCALE * .62 ' The radius of the planet that the starbases orbit.
CONST GAMEBKGROUNDS1 = 0        ' Screen background in SCREEN 1.
CONST PLANETS1 = 2              ' Planet color in SCREEN 1.
CONST BASECOLORS1 = 1           ' Starbase color in SCREEN 1.
CONST ENEMYCOLORS1 = 3          ' Enemy missile color in SCREEN 1.
CONST INTERCEPTCOLORS1 = 3      ' Interceptor missile color in SCREEN 1.
CONST EXPLCOLORS1 = 2           ' Explosion color in SCREEN 1.
CONST TARGETCOLORS1 = 9         ' Target crosshair color for SCREEN 1
CONST RESOLUTION = 100          ' Controls how accurately a line is drawn.

' SUB and FUNCTION declarations
DECLARE SUB Center (Text$, Row)
DECLARE SUB DestroyStarbase (Z)
DECLARE SUB DisplayIntro ()
DECLARE SUB DisplayGameTitle ()
DECLARE SUB DisplayChanges ()
DECLARE SUB EraseMissileTrail (MNum)
DECLARE SUB Explode (Chosen, X, Y, WMissiles)
DECLARE SUB GameOver ()
DECLARE SUB HorizontalScroll (M$, Row)
DECLARE SUB InitScreen ()
DECLARE SUB InitFirebases ()
DECLARE SUB Keys (TurnKeysOn)
DECLARE SUB KeyPause ()
DECLARE SUB LaunchMissile (Chosen, XStart, YStart, XFinish, YFinish)
DECLARE SUB NewMissile ()
DECLARE SUB NewStarbase ()
DECLARE SUB NewInterceptor (X, Y)
DECLARE SUB StopMissile (Chosen, WMissiles)
DECLARE SUB UpdateMissiles (Start, Finish, WMissiles, NumOfTimes, WColor)
DECLARE SUB UpdateExplosions ()
DECLARE SUB UpdateTarget ()
DECLARE SUB UpdateScore ()
DECLARE SUB WaveComplete ()

' Structure definitions.
TYPE Missile
	X AS INTEGER                ' Current X (horizontal) position.
	Y AS INTEGER                ' Current Y (vertical) position.
	XStart AS INTEGER           ' Horizontal missile start position.
	YStart AS INTEGER           ' Vertical missile start position.
	XOffset AS INTEGER          ' # of X pixels to move each time the UpdateMissile subprogram is called.
	YOffset AS INTEGER          ' # of Y pixels to move each time the UpdateMissile subprogram is called.
	Active AS INTEGER           ' 0 = not active, 1 = in flight, 2 = frozen (while it explodes)
	XFinish AS INTEGER          ' X of the missile's target.
	YFinish AS INTEGER          ' Y of the missile's target.
	MaxCount AS INTEGER         ' Number of moves in the missile's primary direction until the missile moves in the secondary direction.
	Count AS INTEGER            ' Number of moves in the missile's primary direction.
	YMajor AS INTEGER           ' TRUE if the missile moves more vertically then horizontally.  FALSE otherwise.
END TYPE

TYPE GenericPos                 ' General-purpose data type for moving objects.  Used many places in QSpace.
	 X AS INTEGER               ' X position.
	 Y AS INTEGER               ' Y position.
	 Active AS INTEGER          ' FALSE (0) = Not active (destroyed, etc.), TRUE (-1) = Active.
	 OldX AS INTEGER            ' Last X position.  Used to make it possible to restore a previous position if the new one would be off the screen.
	 OldY AS INTEGER            ' Last Y position.  Used to make it possible to restore a previous position if the new one would be off the screen.
END TYPE

TYPE xplode                     ' Data type for explosions.
	 X AS INTEGER               ' X position.
	 Y AS INTEGER               ' Y position.
	 Active AS INTEGER          ' Explosion status.  FALSE (0) = No explosion, Greater than 0 = Radius of explosion.
	 MissileNum AS INTEGER      ' Number of the missile that was destroyed to cause this explosion.  Needed to erase missile path after explosion is over.
	 MType AS INTEGER           ' Type of the missile that exploded.  1 = incoming enemy missile, 2 = interceptor missile.
END TYPE

CLEAR , , 5120                  ' Set up a large stack for input processing

' DIM SHARED indicates that the following variable is available to all
' subprograms.  Without this statement, a variable used in one subprogram
' cannot be used by another subprogram or the main program.
DIM SHARED NumMissiles AS INTEGER           ' Maximum number of incoming missiles.  Initially set to InitNumMissiles.
DIM SHARED MaxStarbases AS INTEGER          ' Maximum number of starbases.  Initially set to InitNumStarbases.
DIM SHARED Incoming(1 TO 10)  AS Missile    ' Array used to track of all missiles, both incoming and interceptors.  Incoming missiles are numbered from 1 to 6; interceptors from 7 to 10.
DIM SHARED Starbase(1 TO 4) AS GenericPos   ' Array used to keep track of the starbases.  Game begins with 3 starbases but up to 4 starbases can exist depending on the score.  New bases added by the WaveComplete subprogram.
DIM SHARED ContinueGame AS INTEGER          ' A flag variable to track the status of the game.  1 = Game in progress, -1 = Begin new game, 0 = End game.
DIM SHARED Target AS GenericPos             ' Target crosshair.  The .active element is not used.
DIM SHARED NumIntercepts AS INTEGER         ' Number of interceptors flying.  No more than 4 can exist at any one time.
DIM SHARED Score AS LONG                    ' Score.
DIM SHARED Wave AS LONG                     ' Number of the current attack wave.
DIM SHARED WaveCount AS LONG                ' Number of missiles already launched in the current attack wave.
DIM SHARED NextIncoming AS SINGLE           ' Interval, in seconds from current time, to move the incoming missiles again.
DIM SHARED NextExplosion AS SINGLE          ' Delay until next explosion begins.
DIM SHARED Explosion(1 TO 10) AS xplode     ' Array that keeps track of the explosions.  Since no more than 10 missiles can be flying at once, no more than 10 simultaneous explosions are possible.
DIM SHARED IncomingDelay AS SINGLE          ' Delay between incoming missile movements.
DIM SHARED MissilesFlying AS INTEGER        ' Number of incoming missiles currently flying.
DIM SHARED BasesLeft AS INTEGER             ' Number of starbases left.  Used for scoring and in determining when the game is over.
DIM SHARED TotalIncoming AS LONG            ' Total number of incoming missiles that have been destroyed.  Used for the statistics at the end of the game.
DIM SHARED TotalInterceptors AS LONG        ' Total number of interceptors launched.  Used for the statistics at the end of the game.
DIM SHARED NextNewBase AS LONG              ' Score when a bonus new base will be awarded.
DIM SHARED NumExplosions AS INTEGER         ' Number of explosions in progress.
DIM SHARED PlanetColor  AS INTEGER          ' Color of the planet.
DIM SHARED EnemyColor AS INTEGER            ' Color of the enemy missiles.
DIM SHARED InterceptColor AS INTEGER        ' Color of interceptor missiles.
DIM SHARED ExplColor AS INTEGER             ' Color of the explosions.
DIM SHARED BaseColor AS INTEGER             ' Primary color of the starbase.
DIM SHARED GameBkGround AS INTEGER          ' Color of the game background.
DIM SHARED TargetColor AS INTEGER           ' Color of the target crosshair.
DIM SHARED ScreenMode AS INTEGER            ' Number of the screen mode we are running in.
DIM SHARED ScreenWidth AS INTEGER           ' Width of the screen. Used in various screen output functions.
DIM KeyFlags AS INTEGER                     ' Internal state of the keyboard flags when game starts.  Hold the state so it can be restored when the games ends.
DIM BadMode AS INTEGER                      ' Store the status of a valid screen mode.

ON ERROR GOTO ScreenError           ' Set up a place to jump to if an error occurs in the program.
BadMode = FALSE
ScreenMode = 7
SCREEN ScreenMode                   ' Attempt to go into SCREEN 7 (EGA screen).
IF BadMode = TRUE THEN              ' If this attempt failed.
	ScreenMode = 1
	BadMode = FALSE
	SCREEN ScreenMode               ' Attempt to go into SCREEN 1 (CGA screen).
END IF
ON ERROR GOTO 0                     ' Turn off error handling for now.

IF BadMode THEN                     ' If no graphics adapter.
	CLS
	LOCATE 10, 13: PRINT "CGA, EGA Color, or VGA graphics required to run QSPACE.BAS"
ELSE
	RANDOMIZE TIMER         ' Ensure that a new random number sequence is generated.
	DisplayIntro            ' Display the name of the game, control keys, etc.

	DEF SEG = 0             ' Set the current segment to the low memory area.
	KeyFlags = PEEK(1047)   ' Read the location that stores the keyboard flag.
	POKE 1047, &H0          ' Force them off.
	DEF SEG                 ' Restore the default segment.

	DO                                  ' For multiple games.
		RESTORE                         ' BASIC command to allow DATA statements to be reused.  Necessary for multiple games.
		ScreenWidth = 40                ' Set screen width of the two screens supported - 1 and 7.
		IncomingDelay = INITIALDELAY    ' Set initial incoming missile IncomingDelay to the value of the InitialDelay constant.
		Wave = 1                        ' Set wave number to 1 (first wave).
		WaveCount = 0                   ' Set number of missiles in the first wave to 0.  After the first wave, WaveCount is reset by the WaveComplete subprogram.
		Score = 0                       ' Set score to 0 to begin the game.
		InitScreen                      ' Initialize the screen, including drawing the planet.
		NumMissiles = INITNUMMISSILES   ' Set maximum number of missiles flying simultaneously in each wave to the value of the InitNumMissiles constant.
		MissilesFlying = 0              ' Set the number of missiles currently flying to 0.  Like WaveCount, this is cleared after subsequent waves by the WaveComplete subprogram.
		NumIntercepts = 0               ' Set the number of interceptors currently flying.
		NumExplosions = 0               ' Set the number of explosions currently happening.
		ContinueGame = TRUE             ' ContinueGame = TRUE means that a game is in progress.
		NextIncoming = TIMER            ' Time when incoming missiles will again fire.  Setting NextIncoming equal to the timer insures that the incoming missiles will begin moving immediately.
		NextExplosion = TIMER           ' Time when explosions will be updated again.   Setting NextExplosion equal to the timer ensures that the explosions will begin immediately.
		TotalIncoming = 0               ' Set total number of destroyed incoming missiles.  Missiles are counted as destroyed if hit by interceptor missiles, hit by the explosion of another incoming missile, or stopped by hitting their targets.
		TotalInterceptors = 0           ' Set total number of interceptors launched.
		BasesLeft = 0                   ' Set the number of bases remaining.  Necessary because the NewBase subprogram used below adds 1 to the current number of bases remaining.
		MaxStarbases = INITNUMSTARBASES     ' Set maximum number of starbases equal to the value of the InitNumStarbases constant.
		NextNewBase = 15000                 ' Set initial point at which a bonus starbase is awarded. After that, new starbases are awarded based on a formula in the WaveComplete subprogram.

		ERASE Starbase, Incoming, Explosion ' Set all elements of the entire Starbase, Incoming, and Explosion arrays to 0.
										 
		FOR i = 1 TO MaxStarbases           ' Loop to create the number of starbases called for in the MaxStarbases variable.
			NewStarbase                     ' Create a new starbase
		NEXT i

		InitFirebases                       ' Draw the firebases.

		FOR i = 1 TO NumMissiles            ' Start the incoming missiles flying.
			NewMissile
		NEXT i
	   
		' The KEY n and ON KEY statements below enable QSpace to move the
		' target crosshair the moment a key is pressed.  After the
		' KEY (X) ON statement, anytime key (X) is pressed, QSpace stops
		' what it was doing and moves the crosshair.  After the crosshair
		' moves, QSpace goes back to where it left off.  This method allows
		' BASIC to process keys instantly and without explicitly checking
		' the keyboard.
		KEY 15, CHR$(0) + CHR$(PAUSE)       ' P key (Pause)
		KEY 16, CHR$(0) + CHR$(QUIT)        ' Q key (Quit)
		KEY 17, CHR$(128) + CHR$(72)        ' Extended Up key for player 1.
		KEY 18, CHR$(128) + CHR$(75)        ' Extended Left key for player 1.
		KEY 19, CHR$(128) + CHR$(77)        ' Extended Right key for player 1.
		KEY 20, CHR$(128) + CHR$(80)        ' Extended Down key for player 1.

		ON KEY(11) GOSUB MoveCrossHairUp    ' Up key.
		ON KEY(12) GOSUB MoveCrossHairLeft  ' Left key.
		ON KEY(13) GOSUB MoveCrossHairRight ' Right key.
		ON KEY(14) GOSUB MoveCrossHairDown  ' Down key.
		ON KEY(15) GOSUB PauseGame          ' Pause the game.
		ON KEY(16) GOSUB QuitGame           ' Quit the game.
		ON KEY(17) GOSUB MoveCrossHairUp    ' Process Up key.
		ON KEY(18) GOSUB MoveCrossHairLeft  ' Process Left key.
		ON KEY(19) GOSUB MoveCrossHairRight ' the Right key.
		ON KEY(20) GOSUB MoveCrossHairDown  ' the Down key.
		Keys TRUE                           ' Enable key event processing.

		DO WHILE ContinueGame = TRUE                            ' ContinueGame is set to TRUE at the start of each game.  When the game is over, ContinueGame is set to either FALSE (do not play again) or 1 (play again).
			IF TIMER >= NextIncoming THEN                       ' If enough time has elapsed since the enemy incoming missiles last moved,
				NextIncoming = TIMER + IncomingDelay            ' Calculate when to move the incoming missiles again.
				UpdateMissiles 1, NumMissiles, 1, 1, EnemyColor ' Move the incoming missiles one step.  The 1 means move incoming missiles, a 2 would mean move interceptors; EnemyColor is the color of the incoming missiles -- usually cyan (3).
			END IF
			
			IF NumExplosions > 0 THEN                       ' Update explosions if there are any.
				IF TIMER >= NextExplosion THEN              ' If enough time has elapsed since the explosions were last updated,
					NextExplosion = TIMER + EXPLOSIONDELAY  ' calculate when to update the explosions again.
					UpdateExplosions                        ' Increase the size of any explosions.
				END IF
			END IF

			IF NumIntercepts > 0 THEN       ' Update interceptors if any are in the air.
				UpdateMissiles 7, 10, 2, RATIOINTERCEPTTOINCOMING, InterceptColor
			END IF
		   
			K$ = INKEY$                                     ' Get a key press.
			IF LEN(K$) > 0 THEN                             ' LEN(K$) will be 0 if no key was pressed.
				SELECT CASE ASC(UCASE$(K$))                 ' Prepare to compare the ASCII value of the key press (done with the ASC function).  UCASE$ forces upper-case.
					CASE LEFTLAUNCHER                       ' Key for the left launcher pressed.
						NewInterceptor MINX + 1, MAXY - 1   ' Launch interceptor missile.
					CASE RIGHTLAUNCHER                      ' Key for the right launcher pressed.
						NewInterceptor MAXX - 1, MAXY - 1   ' Launch interceptor missile.
				END SELECT
			END IF
	   
		LOOP                                ' Do again until the game is over.

	LOOP WHILE ContinueGame <> FALSE        ' At GameOver, the ContinueGame variable is set to either 1 or FALSE (0) depending on whether the player wants to try again.  If 1, then the game restarts.

	DisplayChanges                          ' Display the suggested changes.

	DEF SEG = 0                             ' Restore the previous flag settings.
	POKE 1047, KeyFlags
	DEF SEG

END IF

END                                         ' End of the main program code.

MoveCrossHairUp:
	Target.Y = Target.Y - TARGETSPEED
	UpdateTarget
	RETURN

MoveCrossHairDown:
	Target.Y = Target.Y + TARGETSPEED
	UpdateTarget
	RETURN

MoveCrossHairLeft:
	Target.X = Target.X - TARGETSPEED
	UpdateTarget
	RETURN

MoveCrossHairRight:
	Target.X = Target.X + TARGETSPEED
	UpdateTarget
	RETURN

PauseGame:
	Keys FALSE                          ' Turn all keys off.
	SOUND 1100, .75                     ' Tone at 1100 hertz for 75 clock ticks.
	Center " * Paused * ", 12           ' Display message on the screen.
	DO: LOOP UNTIL INKEY$ <> ""         ' Wait until player presses any key.
	Center SPACE$(12), 12
	Keys TRUE                           ' Turn the keys back on.
	RETURN

QuitGame:
	Keys FALSE                          ' Turn all keys off.
	SOUND 1700, 1                       ' Tone at 1700 hertz for 1 clock tick.
	SOUND 1100, .75                     ' Tone at 1100 hertz for .75 clock tick.
	Center " Really quit? (Y/N) ", 12   ' Make sure player really wants to quit.
	DO                                  ' Wait until player presses a key.
		a$ = UCASE$(INKEY$)
	LOOP UNTIL a$ <> ""
	IF a$ = "Y" THEN ContinueGame = FALSE   ' If so, set the main loop variable to FALSE to end main program level loop.
	Center SPACE$(20), 12                   ' Clear the message line.
	Keys TRUE                               ' Turn keys back on.
	RETURN

' All of the data for GameOver messages.  These can also be changed but the
' format must be the same.  For example, the first line has a 5: that says how
' many lines of data will come afterwards, and the next lines are made of two
' parts: what the rank is (such as "Cadet") and the comments to go along with it.
' You can add a new line by following the format the others have and adding
' one to the number at the top.  The last line has already been created so you
' can just change the 5 to a 6 for to add that message.

DATA 5                                                : ' The number of messages.
DATA "Cadet","Not good.  Everything destroyed."       : ' Lowest possible rank.
DATA "Ensign", "You saved a few people."              : ' Better rank.
DATA "Lieutenant","Your parents will be proud."       : ' Better rank.
DATA "Commander","Medal of Honor!"                    : ' Better rank.
DATA "Admiral","If only we had more like you!"        : ' Top rank.
DATA "Top Gun","You can guard our starbases anytime!!": ' The ultimate.

ScreenError:                ' QSpace uses this error handler to determine the highest available video mode.
	BadMode% = TRUE
	RESUME NEXT

'----------------------------------------------------------------------------
' Center
'
'    Centers the given text string on the indicated row.
'
'                       PARAMETERS:     text$   - The text to center
'                                       row     - The screen row to print on
'----------------------------------------------------------------------------
SUB Center (Text$, Row)
  
	LOCATE Row, (ScreenWidth - LEN(Text$)) \ 2 + 1
	PRINT Text$;

END SUB

'----------------------------------------------------------------------------
'DestroyStarbase
'
'    Declares a given base number as destroyed and determine the
'    number of star bases remaining.  If that number is zero then
'    call the GameOver routine. This subprogram does not do the
'    visual explosion of the starbase.
'
'           PARAMETERS:     BNum - Number of the starbase to destroy.
'----------------------------------------------------------------------------
SUB DestroyStarbase (BNum)

	Starbase(BNum).Active = FALSE       ' Set the passed starbase number to 0.
	BasesLeft = 0                       ' Assume there are no starbases left.
  
	FOR i = 1 TO MaxStarbases                               ' Perform one more than the initial number of starbases.
		BasesLeft = BasesLeft - Starbase(i).Active          ' If not 0, increase by one.
		IF Starbase(i).Active = TRUE THEN MaxStarbases = i  ' Keep counting until you've counted the number of starbases left.
	NEXT i
  
	IF BasesLeft = 0 THEN               ' If there are no starbases left,
		GameOver                        ' call the GameOver SUB.
	END IF
		
END SUB

'----------------------------------------------------------------------------
' DisplayChanges
'
'    Displays list of changes that the player can easily make.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB DisplayChanges
  
	Keys FALSE                          ' Disable key event processing.
	DisplayGameTitle                    ' Display game title.

	COLOR 7
	Center "The following game characteristics can be easily changed from", 5
	Center "within the QuickBASIC Interpreter.  To change the values of  ", 6
	Center "these characteristics, locate the corresponding CONST or DATA", 7
	Center "statements in the source code and change their values, then  ", 8
	Center "restart the program (press Shift + F5).                      ", 9

	COLOR 15
	Center "Enemy missile speed at the start of the game           ", 11
	Center "Songs played during this game                          ", 12
	Center "Color of the game pieces (EGA or VGA systems only)     ", 13
	Center "Speed of the targeting crosshair                       ", 14
	Center "Number of missiles falling at the start of the game    ", 15
	Center "Size of each missile explosion                         ", 16
	Center "Duration of the explosions                             ", 17
	Center "GAME OVER messages                                     ", 18

	COLOR 7
	Center "The CONST statements and instructions on changing them are   ", 20
	Center "located at the beginning of the main program.                ", 21

	DO WHILE INKEY$ = "": LOOP      ' Wait for any keypress.
	CLS                             ' Clear screen.

END SUB

'----------------------------------------------------------------------------
' DisplayGameTitle
'
'    Displays title of the game.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB DisplayGameTitle

	SCREEN 0                        ' Set Screen mode 0.
	WIDTH 80, 25                    ' Set width to 80, height to 25.
	COLOR 4, 0                      ' Set colors for red on black.
	CLS                             ' Clear the screen.
	ScreenWidth = 80                ' Set screen width variable to match current width.

	' Draw outline around screen with extended ASCII characters.
	LOCATE 1, 2
	PRINT CHR$(201); STRING$(76, 205); CHR$(187);   ' Draw top border.
	FOR i% = 2 TO 24
		LOCATE i%, 2
		PRINT CHR$(186); TAB(79); CHR$(186);        ' Draw left and right borders.
	NEXT i%
	LOCATE 25, 2
	PRINT CHR$(200); STRING$(76, 205); CHR$(188);   ' Draw bottom border.

	' Print game title centered at top of screen.
	COLOR 0, 4                                      ' Set colors to black on red.
	Center "     Microsoft     ", 1                 ' Center game title on lines
	Center "    Q S P A C E    ", 2                 ' 1 and 2.
	Center "   Press any key to continue   ", 25    ' Center prompt on line 25.
	COLOR 7, 0                                      ' Set colors to white on black.

END SUB

'----------------------------------------------------------------------------
' DisplayIntro
'
'    Explains the object of the game and show how to play.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB DisplayIntro
   
	DisplayGameTitle                    ' Display game title.

	COLOR 7
	Center "Copyright (C) 1990 Microsoft Corporation.  All Rights Reserved.", 4
	Center "Your starbases orbiting the planet Saurus are under attack from enemy", 6
	Center "fire!  You can protect them by firing your own interceptor missiles  ", 7
	Center "to destroy incoming missiles.                                        ", 8
	Center "The enemy attacks the planet in waves.  Each wave has more missiles  ", 10
	Center "than the one before it.  New waves are also faster or send more      ", 11
	Center "missiles at a time.  Bonus points and starbases are awarded for high ", 12
	Center "scores and completed waves.                                          ", 13
	
	COLOR 4
	Center STRING$(74, 196), 15         ' Put horizontal red line on screen.
	COLOR 7
	Center " Game Controls ", 15        ' Display game controls.
	Center "General        Missile Launchers               Target site    ", 17
	Center "                                             (Up)", 19
	Center "P - Pause      Z - Fire left launcher                 " + CHR$(24) + "          ", 20
	Center "Q - Quit       X - Fire right launcher       (Left) " + CHR$(27) + "   " + CHR$(26) + " (Right)", 21
	Center "                                            " + CHR$(25), 22
	Center "                                             (Down)", 23

	PLAY GAMESTARTSONG                  ' Play intro melody.

	DO                                  ' Wait for keypress to continue
		kbd$ = UCASE$(INKEY$)
	LOOP WHILE kbd$ = ""
	IF kbd$ = "Q" THEN                  ' Allow player to quit now
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
' EraseMissileTrail
'
'    Erases the trail of both enemy and interceptor missiles once
'    they have exploded. This subprogram erases one of many
'    possible missile trails temporarily stored in the Incoming() array.
'
'           PARAMETERS:     MNum - Missile line number to erase.
'----------------------------------------------------------------------------
SUB EraseMissileTrail (MNum)

	MaxCount = Incoming(MNum).MaxCount  ' Set temporary variable to the number of moves in the primary direction made before a move in the secondary direction.
	Count = MaxCount                    ' Temp variable that keeps track of how many times the trail has been followed since the last move in the secondary direction.
	IF Incoming(MNum).YMajor THEN       ' For best speed, use different routines for missiles that move mainly vertically (YMajor = TRUE) and those that move mainly horizontally (YMajor = FALSE).
		X = Incoming(MNum).XStart       ' Initial X position.
		XOff = Incoming(MNum).XOffset   ' Temp variable for the X offset.
		FOR Y = Incoming(MNum).YStart TO Incoming(MNum).Y STEP SGN(Incoming(MNum).YOffset)  ' Loop through all Y positions.
			PSET (X, Y), GameBkGround   ' Erase the dot.
			Count = Count - RESOLUTION  ' Decrease COUNT.  RESOLUTION controls how accurate this algorithm is (the higher the more accurate).
			IF Count <= 0 THEN          ' Don't move in the X direction until Y has moved enough for COUNT to drop to 0 or below.
				X = X + XOff            ' Move in the X direction.
				Count = Count + MaxCount' Reset counter.
			END IF
		NEXT
	ELSE                                ' Missile moves more horizontally than vertically.
		Y = Incoming(MNum).YStart       ' Initial Y position.
		YOff = Incoming(MNum).YOffset   ' Temp variable for the Y offset.
		FOR X = Incoming(MNum).XStart TO Incoming(MNum).X STEP SGN(Incoming(MNum).XOffset)  ' Loop through all X positions.
			PSET (X, Y), GameBkGround   ' Erase the dot.
			Count = Count - RESOLUTION  ' Decrease COUNT.
			IF Count <= 0 THEN          ' Has trail moved enough in the X direction to move in the X direction?
				Y = Y + YOff            ' Yes.  Move in the Y direction.
				Count = Count + MaxCount' Reset counter.
			END IF
		NEXT
	END IF

END SUB

'----------------------------------------------------------------------------
' Explode
'
'    Generates the explosion sound and set up an Explosion array
'    element to use when drawing the visual explosion.
'
'       PARAMETERS:     MNum       - Missile number that caused the explosion.
'                       WMissiles  - Type of missile being exploded (enemy or interceptor).
'----------------------------------------------------------------------------
SUB Explode (MNum, X, Y, WMissiles)
	IF Incoming(MNum).Active <> TRUE THEN EXIT SUB  ' Makes sure that the same missile is not exploded twice.
	PLAY "MB"                   ' Play (M)usic in the (B)ackground.
	SOUND 50, 2                 ' Tone at 50 hertz for 2 clock ticks (clock tick = .054 seconds).
	SOUND 40, 8                 ' Tone at 40 hertz for 8 clock ticks.

	DO                                          ' DO loop to determine the highest number of currently active explosions.
		XNum = XNum + 1                         ' Increase the counter.
	LOOP UNTIL Explosion(XNum).Active = FALSE   ' When this loop is done XNum will contain the number of a valid array offset to use for the new explosion.

	Explosion(XNum).Active = 1              ' Set the active status to 1.
	Explosion(XNum).X = X                   ' Set X and Y values to the current incoming
	Explosion(XNum).Y = Y                   '  missile's X and Y values.
	Explosion(XNum).MissileNum = MNum       ' Set to the missile number that was passed in as an argument.
	Explosion(XNum).MType = WMissiles       ' Set to the missile type that was passed in as an argument.
	Incoming(MNum).Active = 2               ' Set the specific incoming missile's active status to 2.
	NumExplosions = NumExplosions + 1       ' Increase the number of global explosions to add this one.
	NextExplosion = TIMER                   ' Ensure explosion begins immediately.

END SUB

'----------------------------------------------------------------------------
' GameOver
'
'    Displays the full-screen explosion, read the GAME OVER
'    messages, and display the score and statistics.
'    Also asks the player if he/she wants to play again.
'
'                   PARAMETERS:     None
'----------------------------------------------------------------------------
SUB GameOver
  
	DIM MessageCount AS LONG            ' Create the variables used for score, etc.
	DIM MaxMessages AS LONG
	DIM MaxScore AS LONG
 
	Keys FALSE                              ' Turn off the control keys.
	PLAY GAMEOVERSONG                       ' Play the game end melody.
	SOUND 38, 36                            ' Tone at 38 hertz for 36 clock ticks.
	FOR i = 1 TO XSCALE * .666 STEP 2       ' Draw an expanding explosion screen.
		CIRCLE (XSCALE / 2, YSCALE / 2), i, ExplColor
	NEXT i

	IF ScreenMode = 7 THEN
		COLOR 15, ExplColor         ' Display the ending score and wave for SCREEN 7.
	ELSE
		COLOR 0                     ' Display for SCREEN 1.
	END IF
	LOCATE 1, 3: PRINT USING "Score: ###,###,###"; Score
	LOCATE 1, ScreenWidth - 10: PRINT USING "Wave: ###"; Wave
  
	Center "Game statistics:", 8    ' Print the player's game statistics.
	Center "Number of missiles destroyed:" + STR$(TotalIncoming), 10
	Center "Number of interceptors launched:" + STR$(TotalInterceptors), 11

	READ MaxMessages                ' Read all the message choices from the DATA statements.
  
	DO                                  ' DO loop to read the Rank$ and Message$ for display.  This loop will end when the MaxScore is greater than or equal to the player's Score.
		READ Rank$, Message$            ' READ two elements from the next DATA statement.
		MaxScore = MaxScore + 10000& + 20000& * MessageCount
		MessageCount = MessageCount + 1 ' Increase message count.
	LOOP WHILE MaxScore < Score AND MessageCount < MaxMessages
   
	Center Message$, 15                             ' Display Message$ in the center of line 15.
	Center "Rank:  " + Rank$, 16                    ' Display the matching rank on line 16.
	Center "Would you like to try again? (Y/N)", 20 ' Ask if player wants to play again.
  
	DO: LOOP UNTIL INKEY$ = ""                      ' Clears the keyboard input buffer.
   
	DO                          ' Wait for a 'y' or 'n' keypress.
		a$ = UCASE$(INKEY$)
	LOOP WHILE a$ <> "Y" AND a$ <> "N"

	IF a$ = "Y" THEN
		ContinueGame = 1        ' Player wants to start playing again.
	ELSE
		ContinueGame = FALSE    ' Player wants to end the game.
	END IF

END SUB

'----------------------------------------------------------------------------
' HorizontalScroll
'
'    Displays a string moving across the screen at a given line.
'    Assumes a 40 column display.
'
'                   PARAMETERS:     M$  - String to be displayed.
'                                   Row - Screen row where string is displayed.
'
'----------------------------------------------------------------------------
SUB HorizontalScroll (M$, Row)

	M$ = SPACE$(ScreenWidth + 2) + M$       ' Add ending spaces for display.
	FOR i = 1 TO LEN(M$) - 1                ' Loop through the message in M$.
		LOCATE Row, 1                       ' Position the message on passed Row value.
		PRINT MID$(M$, LEN(M$) - i, ScreenWidth - 1)   ' Uses the MID$() function to print a ScreenWidth-1 character piece of the entire message.  The piece is determined by the value of X.
		UpdateTarget                        ' Redraw the target crosshair in case the scrolling letters overwrite it.
		Delay! = TIMER + .05                ' Delay the printing of each letter by .1 second.
		DO WHILE TIMER < Delay!: LOOP
	NEXT i
 
END SUB

'----------------------------------------------------------------------------
' InitFirebases
'
'    Draws two firebases at the lower left and right corners of the
'    screen and fills them with color.
'
'                   PARAMETERS:     None
'----------------------------------------------------------------------------
SUB InitFirebases
   
	' Draw the left missile launcher.
	LINE (0, YSCALE - 6)-(10, YSCALE - 11), 14  ' Draw each side of the triangle.
	LINE (0, YSCALE - 6)-(5, YSCALE - 1), 14
	LINE (10, YSCALE - 11)-(5, YSCALE - 1), 14
	PAINT (5, YSCALE - 6), 4, 14                ' Fill the triangle with color 4.
   
	' Draw the right missile launcher.
	LINE (XSCALE - 1, YSCALE - 6)-(XSCALE - 11, YSCALE - 11), 14
	LINE (XSCALE - 11, YSCALE - 11)-(XSCALE - 6, YSCALE - 1), 14
	LINE (XSCALE - 6, YSCALE - 1)-(XSCALE - 1, YSCALE - 6), 14
	PAINT (XSCALE - 6, YSCALE - 6), 4, 14   ' Fill the triangle with color 4.

END SUB

'----------------------------------------------------------------------------
' InitScreen
'
'    Initializes the game. Clears the screen, draws the game pieces,
'    and displays score and wave numbers.
'
'                   PARAMETERS:     None
'----------------------------------------------------------------------------
SUB InitScreen

	SCREEN 0                    ' Clear the screen for each game.
	SCREEN ScreenMode           ' Change to the most appropriate screen mode.
	SELECT CASE ScreenMode
	CASE 7                      ' Set colors for color screen.
		PlanetColor = PLANETS7
		EnemyColor = ENEMYCOLORS7
		InterceptColor = INTERCEPTCOLORS7
		ExplColor = EXPLCOLORS7
		BaseColor = BASECOLORS7
		GameBkGround = GAMEBKGROUNDS7
		TargetColor = TARGETCOLORS7
	CASE ELSE
		PlanetColor = PLANETS1  ' Set colors for mono screen.
		EnemyColor = ENEMYCOLORS1
		InterceptColor = INTERCEPTCOLORS1
		ExplColor = EXPLCOLORS1
		BaseColor = BASECOLORS1
		GameBkGround = GAMEBKGROUNDS1
		TargetColor = TARGETCOLORS1
	END SELECT
   
	COLOR , GameBkGround            ' Change the background color.

	Target.X = XSCALE / 2           ' Setup first X position.
	Target.Y = YSCALE / 2 + 5       ' Setup first Y position.
	Target.OldX = Target.X          ' Setup old target position as the current one.
	Target.OldY = Target.Y
	UpdateTarget                    ' Draw the initial target crosshair.
  
	DO: LOOP UNTIL INKEY$ = ""      ' Clear keyboard input buffer.
										   
	UpdateScore                     ' Display the initial score and wave number.
									' Draw the planet edge here and fill the planet with PlanetColor.
	CIRCLE (XSCALE / 2, YSCALE + 135), PLANETRADIUS, PlanetColor
	PAINT (XSCALE / 2, YSCALE - 1), PlanetColor

END SUB

'----------------------------------------------------------------------------
' KeyPause
'
'    Suspends key event processing.  This is different than
'    a KEY (X) OFF command because KEY (X) STOP stores key
'    events and will fire them when KEY (X) ON is used.
'
'                   PARAMETERS:     None.
'----------------------------------------------------------------------------
SUB KeyPause
	FOR i = 11 TO 20           ' Loop through all defined keys.
		KEY(i) STOP
	NEXT i
END SUB

'----------------------------------------------------------------------------
' Keys
'
'    Turns key event processing on or off.
'
'      PARAMETERS:     TurnKeysOn - If it's TRUE then enable, otherwise
'                      disable
'----------------------------------------------------------------------------
SUB Keys (TurnKeysOn)

	FOR i = 11 TO 20           ' Loop through all defined keys.
		IF TurnKeysOn THEN
			KEY(i) ON
		ELSE
			KEY(i) OFF
		END IF
	NEXT i

END SUB

'----------------------------------------------------------------------------
' LaunchMissile
'
'    Launches an interceptor or an enemy missile.
'
'     PARAMETERS:     Chosen  - Missile number to launch.
'                     XStart  - X (horizontal) position of where the missile begins.
'                     YStart  - Y (vertical) position of where the missile begins.
'                     XFinish - X position of where the missile is aimed.
'                     YFinish - Y position of where the missile is aimed.
'----------------------------------------------------------------------------
SUB LaunchMissile (Chosen, XStart, YStart, XFinish, YFinish)

	Incoming(Chosen).Active = TRUE          ' Set the active status to TRUE.
	Incoming(Chosen).XStart = XStart        ' Set the initial X position.
	Incoming(Chosen).YStart = YStart        ' Set initial Y position.
	Incoming(Chosen).XFinish = XFinish      ' Set the missile's X
	Incoming(Chosen).YFinish = YFinish      '  and Y destination location.
	Incoming(Chosen).X = XStart             ' Set the missile's current X
	Incoming(Chosen).Y = YStart             '  and Y to the start.
   
	' The code below determines which direction, either X or Y, is the
	' missile's primary direction.  Every time UpdateMissiles is called, the
	' missile will move in the primary direction.  MaxCount determines how many
	' primary moves are made before a secondary move is made but MaXCount is
	' not the actual number of moves since it is multiplied by RESOLUTION to
	' allow fast integer math to be used instead of slower floating-point.
	' Every time UpdateMissiles is called, Count is decreased by RESOLUTION.
	' When Count is less than 0, MaxCount added to Count and the missile moves
	' in the secondary direction.
	XDistance = XFinish - XStart
	YDistance = YFinish - YStart
	Incoming(Chosen).XOffset = SGN(XDistance)   ' Forces X and Y offsets that
	Incoming(Chosen).YOffset = SGN(YDistance)   '  are always -1, 0, or 1.

	IF ABS(XDistance) >= ABS(YDistance) THEN        ' Missile moves more horizontally than vertically.
		Incoming(Chosen).MaxCount = INT(ABS(XDistance) / (ABS(YDistance) + 1) * RESOLUTION) ' Determines how many horizontal moves to make before moving vertically.  RESOLUTION is used to round the value so fast integer math can be used.
		Incoming(Chosen).YMajor = FALSE             ' Sets flag to tell UpdateMissiles that primary direction is not Y.
	ELSE                                            ' Missile moves more vertically than horizontally.
		Incoming(Chosen).MaxCount = INT(ABS(YDistance) / (ABS(XDistance) + 1) * RESOLUTION) ' Determines how many vertical moves to make before moving horizontally.
		Incoming(Chosen).YMajor = TRUE              ' Sets flag to tell UpdateMissiles that primary direction is Y.
	END IF
	Incoming(Chosen).Count = Incoming(Chosen).MaxCount  ' Sets the number of times the missile has moved in the primary direction.
END SUB

'----------------------------------------------------------------------------
' NewInterceptor
'
'    Determines if there is room for another interceptor, and if so,
'    sets up another Incoming element and draw the crosshairs for a
'    permanent target point.
'
'     PARAMETERS:     StartX - The X screen position to beginning of the missile trail.
'                     StartY - The Y screen position to beginning of the missile trail.
'----------------------------------------------------------------------------
SUB NewInterceptor (StartX AS INTEGER, StartY AS INTEGER)

	IF NumIntercepts < 4 THEN                       ' Allow only 4 interceptor explosions on the screen at once.
		NumIntercepts = NumIntercepts + 1           ' Increase total number of intercepts by one.
		TotalInterceptors = TotalInterceptors + 1   ' Increase the number of total interceptors.
	  
		Chosen = 7                                  ' Start at an offset of 7 because the Incoming array handles both enemy and player missiles.
		DO UNTIL Incoming(Chosen).Active = FALSE    ' DO loop to find the first unused Incoming element.
			Chosen = Chosen + 1                     ' Increase offset by one.
		LOOP
   
		KeyPause                ' Disable key event processing.
		TargetX = Target.X      ' Store the current crosshair x location
		TargetY = Target.Y      '  and y location, in case the crosshair moves while this subprogram is running.
		Keys TRUE               ' Enable key event processing.

		' Draw the stationary crosshairs on the screen so we can see where the missile is heading.
		LINE (TargetX - 5, TargetY - 5)-(TargetX + 5, TargetY + 5), TargetColor
		LINE (TargetX + 5, TargetY - 5)-(TargetX - 5, TargetY + 5), TargetColor
					
		LaunchMissile Chosen, StartX, StartY, TargetX, TargetY
	END IF

END SUB

'----------------------------------------------------------------------------
' NewMissile
'
'    Develops the boundaries and parameters for a new enemy missile
'    to be fired. When completed, another enemy missile will be
'    setup for drawing on the screen.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB NewMissile

	' If WaveCount is more than the maximum enemy missile wave or ContinueGame isn't correct.
	IF WaveCount = 10 + Wave * 2 OR ContinueGame <> TRUE THEN EXIT SUB
	  
	WaveCount = WaveCount + 1               ' Increase WaveCount by one.
	TotalIncoming = TotalIncoming + 1       ' Increase the total incoming count by one.
	MissilesFlying = MissilesFlying + 1     ' Increase the count of missiles flying.
   
	DO                                          ' DO loop to select which starbase is the target.
		Targ = INT(RND(1) * MaxStarbases) + 1   ' Randomly select until we select one that is currently active.
	LOOP UNTIL Starbase(Targ).Active = TRUE
  
	Chosen = 1                                  ' Select first available missile.
	DO WHILE Incoming(Chosen).Active <> FALSE   ' DO loop to determine the next available Incoming element.
		Chosen = Chosen + 1                     ' Increment offset by one.
	LOOP

	XStart = INT(RND(1) * XSCALE - 1) + 1   ' Randomly select where to start.
	YStart = 12
	XFinish = Starbase(Targ).X              ' Work variables to hold the selected starbase's X and Y position.
	YFinish = Starbase(Targ).Y

	LaunchMissile Chosen, XStart, YStart, XFinish, YFinish

END SUB

'----------------------------------------------------------------------------
' NewStarbase
'
'    Determines a new starbase position and draws it in orbit around
'    the planet.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB NewStarbase

	Chosen = 1                              ' Setup initial starbase offset.
	DO WHILE Starbase(Chosen).Active = TRUE ' DO until we find one that hasn't been initialized.
		Chosen = Chosen + 1                 ' Increase the offset by one.
	LOOP
	BasesLeft = BasesLeft + 1               ' Increase the number of active bases by one.

	DO                          ' DO loop to determine if the randomly chosen starbase is within range.
		Angle! = RND(1) * 2 * PI                        ' Randomly select position along planet edge.
		Y = SIN(Angle!) * PLANETRADIUS + YSCALE + 155   ' Set X and Y based on that angle.
		X = COS(Angle!) * PLANETRADIUS + XSCALE / 2
		TooClose = FALSE                                ' Assume that the new starbase is not too close to another one.

		FOR i = 1 TO MaxStarbases       ' Loop to make sure there isn't a conflict with an existing starbase.
										' If starbase is close then set TooClose to TRUE.
			IF ABS(Starbase(i).X - X) < 20 AND Starbase(i).Active = TRUE THEN TooClose = TRUE
		NEXT i
	LOOP WHILE Y > YSCALE - 11 OR TooClose = TRUE
  
	Starbase(Chosen).X = X              ' Setup the chosen starbases X and Y coordinates.
	Starbase(Chosen).Y = Y
	Starbase(Chosen).Active = TRUE      ' Set starbase active status to TRUE.
										' Draw the base in orbit around the planet.
	CIRCLE (Starbase(Chosen).X, Starbase(Chosen).Y), 7, BaseColor, , , .3
	PAINT (Starbase(Chosen).X, Starbase(Chosen).Y), BaseColor
	LINE (Starbase(Chosen).X - XSCALE / FACTOR, Starbase(Chosen).Y - XSCALE / FACTOR)-(Starbase(Chosen).X + XSCALE / FACTOR, Starbase(Chosen).Y + XSCALE / FACTOR), 4, BF
	PSET (Starbase(Chosen).X, Starbase(Chosen).Y - 3), 14
	PSET (Starbase(Chosen).X, Starbase(Chosen).Y + 3), 14

END SUB

'----------------------------------------------------------------------------
' StopMissile
'
'    Stops the MNum missile and adjusts all global values that this
'    operation affects.
'
'           PARAMETERS:     MNum      - Missile number to stop
'                           WMissiles - Which type of missile: 1 = Incoming, 2 = Interceptor
'----------------------------------------------------------------------------
SUB StopMissile (MNum, WMissiles)

	EraseMissileTrail MNum              ' Erase the given missile's trail.
	Incoming(MNum).Active = FALSE       ' Set incoming active status to FALSE.
  
	IF WMissiles = 1 THEN
		UpdateScore                         ' Update the current score.
		MissilesFlying = MissilesFlying - 1 ' Reduce the number of missiles currently flying.
											' If all of the enemy missiles for this wave have already flown, call WaveComplete subprogram.
		IF WaveCount = 10 + 2 * Wave AND MissilesFlying = 0 THEN WaveComplete
		NewMissile                          ' Start a new enemy missile flying.
	ELSE
		NumIntercepts = NumIntercepts - 1   ' Decrease the number of intercepted missiles.
		XFinish = Incoming(MNum).XFinish    ' Setup work variables for the finish point of the missile.
		YFinish = Incoming(MNum).YFinish
		' Overwrite the target X with background.
		LINE (XFinish - 5, YFinish - 5)-(XFinish + 5, YFinish + 5), GameBkGround
		LINE (XFinish + 5, YFinish - 5)-(XFinish - 5, YFinish + 5), GameBkGround
	END IF

END SUB

'----------------------------------------------------------------------------
' UpdateExplosions
'
'    Updates all currently active explosions in the Explosions array.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB UpdateExplosions

	FOR XNum = 1 TO 10              ' Loop for the number of possible concurrent explosions.
		W = Explosion(XNum).Active  ' Set work variable for active status of explosion element.
		IF W > 0 THEN               ' If this explosion is active.
			X = Explosion(XNum).X   ' Set work variables to explosion X and Y coordinates.
			Y = Explosion(XNum).Y

			IF W > EXPLRADIUS THEN          ' If explosion status (radius) is greater than the max radius.
				FOR T = 1 TO EXPLRADIUS     ' Draw expanding circles with the background color to erase everything!
					CIRCLE (X, Y), T, GameBkGround
				NEXT T
			   
				' Stop the missile that caused the explosion.
				StopMissile Explosion(XNum).MissileNum, Explosion(XNum).MType

				FOR i = 1 TO MaxStarbases   ' Loop through all starbases. If starbase is active and within the exploding missile's range, destroy starbase.
					IF Starbase(i).Active = TRUE AND ((X - Starbase(i).X) ^ 2 + (Y - Starbase(i).Y) ^ 2) ^ .5 - EXPLRADIUS < -2 THEN DestroyStarbase i
				NEXT i
			  
				UpdateTarget                    ' Redraw the target crosshair.
				Explosion(XNum).Active = FALSE  ' Set this explosion's active status to FALSE.
			ELSE
				Explosion(XNum).Active = W + 1  ' Increase the status (radius) of current explosion.
				CIRCLE (X, Y), W, ExplColor     ' Draw another circle to increase the explosion visually.
				UpdateTarget                    ' Redraw the target crosshair.
			END IF
		END IF
	NEXT XNum

END SUB

'----------------------------------------------------------------------------
' UpdateMissiles
'
'    Updates one of the two types of missiles by drawing the missile
'    one pixel more in its direction of travel.
'
'           PARAMETERS:     Start      - Where in the Incoming array to begin looking
'                           Finish     - Where to stop looking
'                           WMissiles  - Missile type to update (enemy or defense)
'                           NumOfTimes - Number of times to update the missiles
'                           ColorToUse - Color to use for the updated line
'
' Note:  Start and Finish are not technically necessary since they can be
'        resolved from WMissiles.  Passing Start and Finish is faster than
'        determining them each time UpdateMissiles is called, however.
'----------------------------------------------------------------------------
SUB UpdateMissiles (Start, Finish, WMissiles, NumOfTimes, ColorToUse)

	FOR Chosen = Start TO Finish                ' Loop through the possible missiles.
		IF Incoming(Chosen).Active = TRUE THEN  ' If this incoming missile is active...
			X = Incoming(Chosen).X              ' Use temporary local
			Y = Incoming(Chosen).Y              '  variables for best speed.
			YOffset = Incoming(Chosen).YOffset
			XOffset = Incoming(Chosen).XOffset
			Count = Incoming(Chosen).Count
			MaxCount = Incoming(Chosen).MaxCount
			XFinish = Incoming(Chosen).XFinish
			YFinish = Incoming(Chosen).YFinish
		   
			' For maximum speed, use different routines for missiles that
			' move mainly horizontally than for ones that move mainly
			' vertically.
			IF Incoming(Chosen).YMajor THEN ' If missile is mainly vertical
				FOR i = 1 TO NumOfTimes     ' Do NumOfTimes
					C = POINT(X, Y)         ' Read the color of the point.
					PSET (X, Y), ColorToUse ' Add a new point to the trail.
					Count = Count - RESOLUTION  ' Decrease the Count.
					Y = Y + YOffset         ' Move vertically.
				   
					IF Count <= 0 THEN      ' Time for the horizontal move?
						X = X + XOffset     ' Yes.  Move horizontally.
						Count = Count + MaxCount' Prepare Count for the next horizontal movement.
					END IF
				   
					' Explode the missile if it hits another explosion, a base,
					'  or reaches its target Y.
					IF (C = ExplColor) OR (C = BaseColor) OR Y = YFinish THEN Explode Chosen, X, Y, WMissiles  ' Explode the chosen missile given the current missile type
				NEXT i
			ELSE                            ' Mainly horizontal
				FOR i = 1 TO NumOfTimes     ' Do NumOfTimes
					C = POINT(X, Y)         ' Read the color of the point.
					PSET (X, Y), ColorToUse ' Add a new point to the trail.
					Count = Count - RESOLUTION  ' Decrease the Count.
					X = X + XOffset         ' Move horizontally.
				   
					IF Count <= 0 THEN      ' Time for the vertical move?
						Y = Y + YOffset     ' Yes.  Move vertically.
						Count = Count + MaxCount' Prepare Count for the next vertical movement.
					END IF
				   
					' Explode the missile if it hits another explosion, a base,
					'  or reaches its target X.
					IF (C = ExplColor) OR (C = BaseColor) OR X = XFinish THEN Explode Chosen, X, Y, WMissiles  '  Explode the chosen missile given the current missile type
				NEXT i
			END IF
		   
			' Copy the temporary local variables back to the SHARED variables.
			Incoming(Chosen).Count = Count
			Incoming(Chosen).X = X
			Incoming(Chosen).Y = Y
	   
		END IF
	NEXT Chosen

END SUB

'----------------------------------------------------------------------------
' UpdateScore
'
'    Calculates new score, then performs a formatted print of the
'    Score and Wave values.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB UpdateScore

	' Calculate the new score.
	Score = Score + 10 * MissilesFlying * BasesLeft * Wave

	' Locate and do a formatted print of the current score and wave numbers.
	LOCATE 1, 3: PRINT USING "Score: ###,###,###"; Score
	LOCATE 1, ScreenWidth - 10: PRINT USING "Wave: ###"; Wave

END SUB

'----------------------------------------------------------------------------
' UpdateTarget
'
'    Checks to see if the coordinates for the target are within the
'    boundaries and adjusts, if necessary.  Erases the old target
'    crosshair and draws the new target crosshair in its new
'    position.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB UpdateTarget

	' If target goes off the screen horizontally, restore old horizontal position.
	IF Target.X > XSCALE - 5 OR Target.X < 5 THEN Target.X = Target.OldX

	' Target cannot move above the SCORE line or below the top of the planet.
	IF Target.Y > YSCALE - 53 OR Target.Y < 15 THEN Target.Y = Target.OldY

	' If the target is in a different position than when it was last updated.
	IF Target.X <> Target.OldX OR Target.Y <> Target.OldY THEN   ' Erase the old target.
		LINE (Target.OldX, Target.OldY + 5)-(Target.OldX, Target.OldY - 5), 0
		LINE (Target.OldX - 5, Target.OldY)-(Target.OldX + 5, Target.OldY), 0
		Target.OldX = Target.X          ' Make the old X and Y values equal to the current ones.
		Target.OldY = Target.Y
	END IF

	' Draw new target crosshair in the new X and Y position.
	LINE (Target.X, Target.Y + 5)-(Target.X, Target.Y - 5), 14
	LINE (Target.X - 5, Target.Y)-(Target.X + 5, Target.Y), 14

END SUB

'----------------------------------------------------------------------------
' WaveComplete
'
'    Handles the screen output when a wave has been completed. Also
'    sets up information for the next wave.
'
'           PARAMETERS:     None
'----------------------------------------------------------------------------
SUB WaveComplete

	KEY(15) OFF                 ' Disable the Pause key.
	KEY(16) OFF                 ' Disable the Quit key.

	WaveCount = 0               ' Reset the WaveCount variable that holds home many missiles have been launched in the current wave.
	WaveInterceptCount = 0      ' Reset the counter for the number of interceptors launched in the wave.
	Score = Score + Wave * 500  ' Calculate bonus points.
	Wave = Wave + 1             ' Increment to the next wave.
	PLAY WAVEOVERSONG           ' Play the wave-end melody.

	' Move the Wave Over, etc. message across the screen.
	M$ = STR$(500 * (Wave - 1)) + " point bonus!" + SPACE$(20) + "Wave" + STR$(Wave - 1) + " Complete!"
	HorizontalScroll M$, 10

	FOR XNum = 1 TO 10                      ' Loop through the 10 possible explosions.
		IF Explosion(XNum).Active > 0 THEN  ' If exploding now, explosion(mnum%).active will be greater than 0 (the radius of the explosion).
			X! = Explosion(XNum).X          ' Get the X coordinate of the explosion.
			Y! = Explosion(XNum).Y          ' Get the Y coordinate of the explosion.
		   
			FOR T = 1 TO EXPLRADIUS     ' Draw expanding circles with the background color to erase everything.
				CIRCLE (X!, Y!), T, GameBkGround
			NEXT T
		   
			StopMissile Explosion(XNum).MissileNum, Explosion(XNum).MType
			 
			FOR i = 1 TO MaxStarbases   ' Loop through all starbases. If starbase is active and within the exploding missile's range, destroy starbase.
				IF Starbase(i).Active = TRUE AND ((X! - Starbase(i).X) ^ 2 + (Y! - Starbase(i).Y) ^ 2) ^ .5 - EXPLRADIUS < -2 THEN DestroyStarbase i
			NEXT i
		  
			UpdateTarget            ' Redraw the target crosshair.
			Explosion(XNum).Active = FALSE  ' Reset the active flag so explosion can be re-used.
	  
		END IF
	NEXT XNum

	FOR i = 1 TO 10                     ' Loop through all missiles (both interceptor and enemy).
		IF Incoming(i).Active <> 0 THEN ' If it's flying or frozen,
			EraseMissileTrail i         ' erase it.
			IF i > 6 THEN               ' If it is an interceptor missile:
				XFinish = Incoming(i).XFinish     ' Store X coordinate of the missile's final target.
				YFinish = Incoming(i).YFinish     ' Get Y coordinate.
				' Erase the target at this line.
				LINE (XFinish - 5, YFinish - 5)-(XFinish + 5, YFinish + 5), GamBkGround ' Erase the target X.
				LINE (XFinish + 5, YFinish - 5)-(XFinish - 5, YFinish + 5), GameBkGround
			END IF

		END IF
		Incoming(i).Active = FALSE      ' Reset the active flag so missile can be re-used.
	NEXT i

	' If score is high enough score, add another starbase if there's room.
	IF Score > NextNewBase AND BasesLeft < 4 THEN
		M$ = "Bonus Starbase!"
		HorizontalScroll M$, 10     ' Scroll the bonus message across the screen.
	 
		NextNewBase = NextNewBase + 10000& * Wave  ' Determine when next new starbase will possibly be awarded.
		NewStarbase                 ' Setup another starbase.
		FOR i = 1 TO 4              ' Loop to determine need to update the number of starbases.
			IF Starbase(i).Active = TRUE THEN MaxStarbases = i
		NEXT i
	END IF

	' Determine how to make the next wave more difficult.
	IF Wave / 2 = Wave \ 2 AND NumMissiles < 6 THEN
		NumMissiles = NumMissiles + 1           ' If an even number wave, increase the # of missiles unless the maximum (6) has already been reached.
	ELSE
		IncomingDelay = IncomingDelay * .66667  ' Otherwise, make the incoming missiles 33% faster unless already at maximum speed.
		IF IncomingDelay < FASTESTMISSILE THEN IncomingDelay = FASTESTMISSILE
	END IF

	UpdateScore                     ' Show new score and wave.

	FOR i = 1 TO NumMissiles - 1    ' Create the new missiles (one more will be added by the StopMissile subprogram when WaveComplete is finished).
		NewMissile
	NEXT i

	NumIntercepts = 0               ' Reset the number of interceptors.
	NumExplosions = 0               ' Reset the number of explosions.
	LINE (1, MINY)-(XSCALE, YSCALE - 51), 0, BF   ' Erase this area and cover with black.
   
	DO: LOOP UNTIL INKEY$ = ""      ' Clear keyboard input buffer.
	Keys TRUE                       ' Enable key event processing.
   
END SUB

