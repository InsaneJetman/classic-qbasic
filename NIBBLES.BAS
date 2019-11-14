' NIBBLES   co-written by Mike Warning

'define constants
CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST MAXSNAKELENGTH = 1000

CONST STARTOVER = 1             ' Parameters to 'Level' SUB
CONST SAMELEVEL = 2
CONST NEXTLEVEL = 3

'define types
TYPE snakeBody
    row AS INTEGER
    col AS INTEGER
END TYPE

'the type defines the player's snake
TYPE snaketype
    head      AS INTEGER
    length    AS INTEGER
    row       AS INTEGER
    col       AS INTEGER
    direction AS INTEGER
    lives     AS INTEGER
    score     AS INTEGER
    scolor    AS INTEGER
    alive     AS INTEGER
END TYPE

'this type is used to represent the playing screen in memory
'It is used to simulate graphics in text mode, and has some interesting,
'and slightly advanced methods to increasing the speed of operation.
'Instead of the normal 80x25 text graphics using chr$(219) "€", we will be
'using chr$(220)"‹" and chr$(223) "ﬂ" and chr$(219) "€" to mimic an 80x50
'pixel screen.
'Check out sub-programs SET and POINTISTHERE to see how this is implimented
'feel free to copy these (as well as arenaType and the DIM ARENA stmt and the
'initialization code in the DrawScreen subprogram) and use them in your own
'programs

TYPE arenaType
    realRow     AS INTEGER        'Maps the 80x50 point into the real 80x25
    acolor      AS INTEGER        'Stores the current color of the point
    sister      AS INTEGER        'Each char has 2 points in it.  .SISTER is
END TYPE                          '-1 if sister point is above, +1 if below

'for faster play
DEFINT A-Z

'sub declarations
DECLARE SUB SpacePause (text$)
DECLARE SUB PrintScore (numPlayers%, score1%, score2%, lives1%, lives2%)
DECLARE SUB Intro ()
DECLARE SUB GetInputs (numPlayers, speed, diff$)
DECLARE SUB DrawScreen ()
DECLARE SUB PlayNibbles (numPlayers, speed, diff$)
DECLARE SUB Set (row, col, acolor)
DECLARE SUB Center (row, text$)
DECLARE SUB DoIntro ()
DECLARE SUB Initialize ()
DECLARE SUB SparklePause ()
DECLARE FUNCTION StillWantsToPlay ()
DECLARE FUNCTION PointIsThere (row, col, backColor)
DECLARE SUB Level (WhatToDO, sammy() AS snaketype)
DECLARE SUB InitColors ()
DECLARE SUB EraseSnake (snake() AS ANY, snakeBod() AS ANY, snakeNum%)

'Define global arrays
DIM SHARED arena(1 TO 50, 1 TO 80) AS arenaType

    RANDOMIZE TIMER
    
    Intro
    GetInputs numPlayers, speed, diff$
    DrawScreen

    DO
      PlayNibbles numPlayers, speed, diff$
    LOOP WHILE StillWantsToPlay
END

'Takes in a row and a string, and centers the string on that row
SUB Center (row, text$)

    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;

END SUB

SUB DrawScreen

    'initialize screen
    VIEW PRINT
    COLOR 14, 1
    CLS

    'Print title & message
    Center 1, "Nibbles!"
    Center 11, "Initializing Playing Field..."
    
    'Initialize arena array
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).realRow = INT((row + 1) / 2)
            arena(row, col).sister = (row MOD 2) * 2 - 1
        NEXT col
    NEXT row
    
END SUB

SUB EraseSnake (snake() AS snaketype, snakeBod() AS snakeBody, snakeNum)

    FOR c = 0 TO 9
        FOR b = snake(snakeNum).length - c TO 0 STEP -10
            tail = (snake(snakeNum).head + MAXSNAKELENGTH - b) MOD MAXSNAKELENGTH
            Set snakeBod(tail, snakeNum).row, snakeBod(tail, snakeNum).col, 1
        NEXT b
    NEXT c
    
END SUB

SUB GetInputs (numPlayers, speed, diff$)

    'Get first question
    COLOR 15, 1
    DO
        CLS
        LOCATE 9, 5
        INPUT "How Many Players?  (1 or 2)"; numPlayers
    LOOP UNTIL numPlayers = 1 OR numPlayers = 2


    'Get Second Question
    LOCATE 12, 5: PRINT "Enter Speed level between 1 and 100. -->"
    COLOR 14
    PRINT
    PRINT "          1   = Novice"
    PRINT "          90  = Expert"
    PRINT "          100 = Twiddle Fingers"
    PRINT "          (Computer speed may affect your skill level)"

    COLOR 15
    DO
        LOCATE 12, 44
        PRINT SPACE$(40);
        LOCATE 12, 44
        INPUT speed
    LOOP UNTIL speed >= 1 AND speed <= 100
  
    speed = (100 - speed) * 2 + 1

    LOCATE 20, 5: PRINT "Enter difficulty level -->"
    COLOR 14
    PRINT
    PRINT "          A = Amateur"
    PRINT "          P = Pro    (Speed increases during play)"

    COLOR 15
    DO
        LOCATE 20, 30
        PRINT SPACE$(40);
        LOCATE 20, 30
        INPUT diff$
        diff$ = UCASE$(diff$)
    LOOP UNTIL diff$ = "A" OR diff$ = "P"

    startTime# = TIMER                          ' Calculate speed of system
    FOR i# = 1 TO 1000: NEXT i#                 ' and do some compensation
    stopTime# = TIMER
    speed = speed * .5 / (stopTime# - startTime#)

END SUB

SUB InitColors
    
    FOR row = 1 TO 50
        FOR col = 1 TO 80
            arena(row, col).acolor = 1
        NEXT col
    NEXT row
   
    CLS
    'Set (turn on) pixels for screen border
    FOR col = 1 TO 80
        Set 3, col, 12
        Set 50, col, 12
    NEXT col

    FOR row = 4 TO 49
        Set row, 1, 12
        Set row, 80, 12
    NEXT row

END SUB

SUB Intro

    COLOR 14, 1
    CLS
    COLOR 12
    Center 3, "N I B B L E S !"
    COLOR 14
    Center 5, "INSTRUCTIONS:  Nibbles! is a game for 1 or 2 players.  You navigate"
    Center 6, "your snakes around the game board trying to eat up numbers while avoiding"
    Center 7, "running into walls or other snakes.  The more numbers you eat up, the"
    Center 8, "longer your snake becomes."
    COLOR 15
    Center 11, "This game uses the following keys:"
    Center 13, "  Left Player:  Up,Left,Right,Dowm  = W,A,S,D   "
    Center 14, " Right Player:  Up,Left,Right,Dowm  = Arrow Keys"
    Center 16, "Press 'P' at any time to pause the game"
    Center 25, "Push Any Key To Continue"
    COLOR 9
    SparklePause

END SUB

SUB Level (WhatToDO, sammy() AS snaketype) STATIC
    
    SELECT CASE (WhatToDO)
    CASE STARTOVER
        curLevel = 1
    CASE NEXTLEVEL
        curLevel = curLevel + 1
    END SELECT

    sammy(1).head = 1                       'Initialize Snakes
    sammy(1).length = 2
    sammy(1).alive = TRUE
    sammy(2).head = 1
    sammy(2).length = 2
    sammy(2).alive = TRUE

    InitColors
   
    SELECT CASE curLevel
    CASE 1
        Set 25, 40, 12
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 4: sammy(2).direction = 3

    CASE 2
        FOR i = 20 TO 60
            Set 25, i, 12
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4

    CASE 3
        FOR i = 10 TO 40
            Set i, 20, 12
            Set i, 60, 12
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 4
        FOR i = 4 TO 30
            Set i, 20, 12
            Set 53 - i, 60, 12
        NEXT i
        FOR i = 2 TO 40
            Set 38, i, 12
            Set 15, 81 - i, 12
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 60: sammy(2).col = 20
        sammy(1).direction = 3: sammy(2).direction = 4
   
    CASE 5
        FOR i = 13 TO 39
            Set i, 21, 12
            Set i, 59, 12
        NEXT i
        FOR i = 23 TO 57
            Set 11, i, 12
            Set 41, i, 12
        NEXT i
        sammy(1).row = 25: sammy(2).row = 25
        sammy(1).col = 50: sammy(2).col = 30
        sammy(1).direction = 1: sammy(2).direction = 2

    CASE 6
        FOR i = 4 TO 49
            IF i > 30 OR i < 23 THEN
                Set i, 10, 12
                Set i, 20, 12
                Set i, 30, 12
                Set i, 40, 12
                Set i, 50, 12
                Set i, 60, 12
                Set i, 70, 12
            END IF
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 7
        FOR i = 4 TO 49 STEP 2
            Set i, 40, 12
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 8
        FOR i = 4 TO 40
            Set i, 10, 12
            Set 53 - i, 20, 12
            Set i, 30, 12
            Set 53 - i, 40, 12
            Set i, 50, 12
            Set 53 - i, 60, 12
            Set i, 70, 12
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    CASE 9
        FOR i = 6 TO 47
            Set i, i, 12
            Set i, i + 28, 12
        NEXT i
        sammy(1).row = 40: sammy(2).row = 15
        sammy(1).col = 75: sammy(2).col = 5
        sammy(1).direction = 1: sammy(2).direction = 2
   
    CASE ELSE
        FOR i = 4 TO 49 STEP 2
            Set i, 10, 12
            Set i + 1, 20, 12
            Set i, 30, 12
            Set i + 1, 40, 12
            Set i, 50, 12
            Set i + 1, 60, 12
            Set i, 70, 12
        NEXT i
        sammy(1).row = 7: sammy(2).row = 43
        sammy(1).col = 65: sammy(2).col = 15
        sammy(1).direction = 2: sammy(2).direction = 1

    END SELECT

END SUB

SUB PlayNibbles (numPlayers, speed, diff$)

    'Initialize Snakes
    DIM sammyBody(MAXSNAKELENGTH - 1, 1 TO 2) AS snakeBody
    DIM sammy(1 TO 2) AS snaketype
    sammy(1).lives = 5
    sammy(1).score = 0
    sammy(1).scolor = 14
    sammy(2).lives = 5
    sammy(2).score = 0
    sammy(2).scolor = 13
                 
    Level STARTOVER, sammy()
    startRow1 = sammy(1).row: startCol1 = sammy(1).col
    startRow2 = sammy(2).row: startCol2 = sammy(2).col

    curSpeed = speed

    'play Nibbles until finished

    SpacePause "     Push Space to Start!    "
    gameOver = FALSE
    DO
        IF numPlayers = 1 THEN
            sammy(2).row = 0
        END IF

        number = 1          'Current number that snakes are trying to run into
        nonum = TRUE        'nonum = TRUE if a number is not on the screen

        playerDied = FALSE
        PrintScore numPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
        PLAY "T160O1>L20CDEDCDL10ECC"

        DO
            'Print number if no number exists
            IF nonum = TRUE THEN
                DO
                    numberRow = INT(RND(1) * 47 + 3)
                    NumberCol = INT(RND(1) * 78 + 2)
                    sisterRow = numberRow + arena(numberRow, NumberCol).sister
                LOOP UNTIL NOT PointIsThere(numberRow, NumberCol, 1) AND NOT PointIsThere(sisterRow, NumberCol, 1)
                numberRow = arena(numberRow, NumberCol).realRow
                nonum = FALSE
                COLOR 14, 1
                LOCATE numberRow, NumberCol
                PRINT RIGHT$(STR$(number), 1);
                count = 0
            END IF

            'Delay game
            FOR a# = 1 TO curSpeed:  NEXT a#

            'Get keyboard input & Change direction accordingly
            kbd$ = INKEY$
            SELECT CASE kbd$
                CASE "w", "W": IF sammy(2).direction <> 2 THEN sammy(2).direction = 1
                CASE "s", "S": IF sammy(2).direction <> 1 THEN sammy(2).direction = 2
                CASE "a", "A": IF sammy(2).direction <> 4 THEN sammy(2).direction = 3
                CASE "d", "D": IF sammy(2).direction <> 3 THEN sammy(2).direction = 4
                CASE CHR$(0) + "H": IF sammy(1).direction <> 2 THEN sammy(1).direction = 1
                CASE CHR$(0) + "P": IF sammy(1).direction <> 1 THEN sammy(1).direction = 2
                CASE CHR$(0) + "K": IF sammy(1).direction <> 4 THEN sammy(1).direction = 3
                CASE CHR$(0) + "M": IF sammy(1).direction <> 3 THEN sammy(1).direction = 4
                CASE "p", "P": SpacePause " Game Paused ... Push Space  "
                CASE ELSE
            END SELECT

            FOR a = 1 TO numPlayers
                'Move Snake
                SELECT CASE sammy(a).direction
                    CASE 1: sammy(a).row = sammy(a).row - 1
                    CASE 2: sammy(a).row = sammy(a).row + 1
                    CASE 3: sammy(a).col = sammy(a).col - 1
                    CASE 4: sammy(a).col = sammy(a).col + 1
                END SELECT

                'If snake hits number, respond accordingly
                IF numberRow = INT((sammy(a).row + 1) / 2) AND NumberCol = sammy(a).col THEN
                    PLAY "MBO0L16>CCCE"
                    IF sammy(a).length < (MAXSNAKELENGTH - 30) THEN
                        sammy(a).length = sammy(a).length + number * 4
                    END IF
                    sammy(a).score = sammy(a).score + number
                    PrintScore numPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
                    number = number + 1
                    IF number = 10 THEN
                        EraseSnake sammy(), sammyBody(), 1
                        EraseSnake sammy(), sammyBody(), 2
                        LOCATE numberRow, NumberCol: PRINT " "
                        Level NEXTLEVEL, sammy()
                        PrintScore numPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
                        IF numPlayers = 1 THEN sammy(2).row = 0
                        number = 1
                        IF diff$ = "P" THEN speed = speed - 10: curSpeed = speed
                    END IF
                    nonum = TRUE
                    IF curSpeed < 1 THEN curSpeed = 1
                END IF
            NEXT a

            FOR a = 1 TO numPlayers
                'If player runs into any point, or the head of the other snake, it dies.
                IF PointIsThere(sammy(a).row, sammy(a).col, 1) OR (sammy(1).row = sammy(2).row AND sammy(1).col = sammy(2).col) THEN
                    PLAY "MBO0L32EFGEFDC"
                    COLOR 1, 1
                    LOCATE numberRow, NumberCol
                    PRINT " "
                   
                    playerDied = TRUE
                    sammy(a).alive = FALSE
                    sammy(a).lives = sammy(a).lives - 1

                'Otherwise, move the snake, and erase the tail
                ELSE
                    sammy(a).head = (sammy(a).head + 1) MOD MAXSNAKELENGTH
                    sammyBody(sammy(a).head, a).row = sammy(a).row
                    sammyBody(sammy(a).head, a).col = sammy(a).col
                    tail = (sammy(a).head + MAXSNAKELENGTH - sammy(a).length) MOD MAXSNAKELENGTH
                    Set sammyBody(tail, a).row, sammyBody(tail, a).col, 1
                    sammyBody(tail, a).row = 0
                    Set sammy(a).row, sammy(a).col, sammy(a).scolor
                END IF
            NEXT a

        LOOP UNTIL playerDied

        curSpeed = speed                ' reset speed to initial value
       
        FOR a = 1 TO numPlayers
            EraseSnake sammy(), sammyBody(), a

            'If dead, then erase snake in really cool way
            IF sammy(a).alive = FALSE THEN
                'Update score
                sammy(a).score = sammy(a).score - 10
                PrintScore numPlayers, sammy(1).score, sammy(2).score, sammy(1).lives, sammy(2).lives
                
                IF a = 1 THEN
                    SpacePause " Sammy Dies! Push Space! --->"
                ELSE
                    SpacePause " <---- Jake Dies! Push Space "
                END IF
            END IF
        NEXT a

        Level SAMELEVEL, sammy()
     
    'Play next round, until either of snake's lives have run out.
    LOOP UNTIL sammy(1).lives = 0 OR sammy(2).lives = 0

END SUB

FUNCTION PointIsThere (row, col, acolor)

    'Simply checks the global arena array, and sees if the boolean flag is
    'set

    IF row <> 0 THEN
        IF arena(row, col).acolor <> acolor THEN
            PointIsThere = TRUE
        ELSE
            PointIsThere = FALSE
        END IF
    END IF
END FUNCTION

SUB PrintScore (numPlayers, score1, score2, lives1, lives2)
    COLOR 15, 1

    IF numPlayers = 2 THEN
        LOCATE 1, 1
        PRINT USING "#,###,#00  Lives: #  <--JAKE"; score2; lives2
    END IF

    LOCATE 1, 49
    PRINT USING "SAMMY-->  Lives: #     #,###,#00"; lives1; score1
END SUB

SUB Set (row, col, acolor)

    IF row <> 0 THEN
        arena(row, col).acolor = acolor             'assign color to arena
        realRow = arena(row, col).realRow           'Get real row of pixel
        topFlag = arena(row, col).sister + 1 / 2    'Deduce whether pixel
                                                    'is on topﬂ, or bottom‹
        sisterRow = row + arena(row, col).sister    'Get arena row of sister
        sisterColor = arena(sisterRow, col).acolor  'Determine sister's color

        LOCATE realRow, col

        IF acolor = sisterColor THEN                'If both points ar same
            COLOR acolor                            'Print chr$(219) "€"
            PRINT CHR$(219);
        ELSE
            IF topFlag THEN                         'Since you cannot have
                IF acolor > 7 THEN                  'bright backgrounds
                    COLOR acolor, sisterColor       'determine best combo
                    PRINT CHR$(223);                'to use.
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(220);
                END IF
            ELSE
                IF acolor > 7 THEN
                    COLOR acolor, sisterColor
                    PRINT CHR$(220);
                ELSE
                    COLOR sisterColor, acolor
                    PRINT CHR$(223);
                END IF
            END IF
        END IF
    END IF
END SUB

SUB SpacePause (text$)
    COLOR 15, 4
    Center 11, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 12, "€ " + text$ + " €"
    Center 13, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
    WHILE INKEY$ <> "": WEND
    WHILE INKEY$ <> " ": WEND
    COLOR 15, 1

    FOR i = 21 TO 26            ' Restore the screen background
        FOR j = 24 TO 56
            Set i, j, arena(i, j).acolor
        NEXT j
    NEXT i

END SUB

SUB SparklePause

    a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "

    WHILE INKEY$ = ""
        FOR a = 1 TO 5
            LOCATE 1, 1                             'print horizontal sparkles
            PRINT MID$(a$, a, 80);
            LOCATE 20, 1
            PRINT MID$(a$, 6 - a, 80);

            FOR b = 2 TO 19                         'Print Vertical sparkles
                c = (a + b) MOD 5
                IF c = 1 THEN
                    LOCATE b, 80
                    PRINT "*";
                    LOCATE 21 - b, 1
                    PRINT "*";
                ELSE
                    LOCATE b, 80
                    PRINT " ";
                    LOCATE 21 - b, 1
                    PRINT " ";
                END IF
            NEXT b
        NEXT a
    WEND

END SUB

FUNCTION StillWantsToPlay

    COLOR 15, 4
    Center 10, "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
    Center 11, "€       G A M E   O V E R       €"
    Center 12, "€                               €"
    Center 13, "€      Play Again?   (Y/N)      €"
    Center 14, "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"

    WHILE INKEY$ <> "": WEND
    DO
        kbd$ = UCASE$(INKEY$)
    LOOP UNTIL kbd$ = "Y" OR kbd$ = "N"

    COLOR 15, 1
    Center 10, "                                 "
    Center 11, "                                 "
    Center 12, "                                 "
    Center 13, "                                 "
    Center 14, "                                 "

    IF kbd$ = "Y" THEN
        StillWantsToPlay = TRUE
    ELSE
        StillWantsToPlay = FALSE
    END IF

END FUNCTION

