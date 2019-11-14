DECLARE SUB AddBullet (bullet() AS ANY, numBullets%, direction%, player AS ANY)
DECLARE SUB Center (row%, text$)
DECLARE SUB DeleteBullets (bullet() AS ANY, numBullets%)
DECLARE SUB DoIntro ()
'for faster play
DEFINT A-Z

'declare sub programs
DECLARE SUB DrawScreen ()
DECLARE SUB HideBullet (bullet AS ANY)
DECLARE SUB HidePlayer (player AS ANY)
DECLARE SUB Hunter ()
DECLARE SUB Initialize ()
DECLARE SUB MoveBullet (bullet AS ANY)
DECLARE SUB MoveBullets (bullet() AS ANY, numBullets%)
DECLARE SUB MovePlayer (player AS ANY)
DECLARE SUB ShowBullet (bullet AS ANY)
DECLARE SUB ShowPlayer (player AS ANY)
DECLARE SUB SparklePause ()

'declare types
TYPE bulletType
    row       AS INTEGER
    col       AS INTEGER
    direction AS INTEGER
    bcolor    AS INTEGER
END TYPE

TYPE playerType
    row         AS INTEGER
    col         AS INTEGER
    direction   AS INTEGER
    score       AS INTEGER
    icon        AS STRING * 1
    iconColor   AS INTEGER
END TYPE

'declare constants
CONST TRUE = -1
CONST FALSE = NOT TRUE

'declare global arrays
DIM SHARED arena$(25)       'String representation of playing field.  Used to
                            'Make creating/drawing the field easier
DIM SHARED map(25, 80)      'Represents the screen.  There is 1 array location
                            'for every screen char.  the value of a element of
                            'map(row,col) will be one of the following:
                            '   0 : Nothing there
                            '   1 : Wall
                            '   2 : Player
                            '   3 : bullet
                            'The use of map() speeds up the game
                            'considerably, since it has a quick way of
                            'knowing where things are.


    Initialize
    DoIntro
    DrawScreen
    Hunter
    COLOR 15, 0
    CLS

'Add bullet to bullet array
SUB AddBullet (bullet() AS bulletType, numBullets, direction, player AS playerType)
    IF numBullets < 10 THEN
        numBullets = numBullets + 1
        bullet(numBullets).row = player.row
        bullet(numBullets).col = player.col
        bullet(numBullets).direction = direction
        bullet(numBullets).bcolor = player.iconColor
        MoveBullet bullet(numBullets)
    END IF
END SUB

' Center a text string at row ROW
SUB Center (row, text$)

    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;

END SUB

'Goes through bullet array, and removes any bullet with direction of 0,
SUB DeleteBullets (bullet() AS bulletType, numBullets)

    top = 0                     'num bullets in the new array
    curr = 1                    'current index into the array
    WHILE curr <= numBullets
        IF bullet(curr).direction = 0 THEN
            HideBullet bullet(curr)
        ELSE
            top = top + 1
            bullet(top).row = bullet(curr).row
            bullet(top).col = bullet(curr).col
            bullet(top).direction = bullet(curr).direction
            bullet(top).bcolor = bullet(curr).bcolor
        END IF
        curr = curr + 1
    WEND
    numBullets = top

END SUB

SUB DoIntro

    WIDTH , 25
    VIEW PRINT
    LOCATE , , 0

    COLOR 12, 1
    CLS
    Center 3, "Q u i c k B A S I C   M A Z E    H U N T E R"
    COLOR 14
    Center 5, "INSTRUCTIONS:  Maze Hunter is a two player game!  Your goal is to hunt down"
    Center 6, "and kill your enemy in the maze. "

    COLOR 15
    Center 8, "The following keys are used while playing this game"
    Center 10, "Left Player Move Up:    t      Right Player Move Up:    8"
    Center 11, "Left Player Move Left:  f      Right Player Move Left:  4"
    Center 12, "Left Player Move Down:  g      Right Player Move Down:  5"
    Center 13, "Left Player Move Right: h      Right Player Move Right: 6"

    Center 15, "Left Player Shoot Up:    w     Right Player Shoot Up:    p"
    Center 16, "Left Player Shoot Left:  a     Right Player Shoot Left:  l"
    Center 17, "Left Player Shoot Down:  s     Right Player Shoot Down:  ;"
    Center 18, "Left Player Shoot Right: d     Right Player Shoot Right: '"


    Center 22, "[-] Decrease Game Delay   [+] Increase Game Delay   [Esc] Stop play"
    

    Center 25, "Push Any Key To Continue"
    COLOR 11
    SparklePause

END SUB

SUB DrawScreen

    COLOR 14, 1
    CLS
    VIEW PRINT
    FOR a = 1 TO 25
        LOCATE a, 1
        PRINT arena$(a);
    NEXT a

END SUB

'Prints a space over the location of the bullet
SUB HideBullet (bullet AS bulletType)
    LOCATE bullet.row, bullet.col
    PRINT " ";
    map(bullet.row, bullet.col) = 0
END SUB

'Draws a space over the player icon, thus hiding it
SUB HidePlayer (player AS playerType)
    LOCATE player.row, player.col
    PRINT " ";
    map(player.row, player.col) = 0
END SUB

SUB Hunter

    'dimension bullet array and players
    DIM bullet(100) AS bulletType
    DIM player1 AS playerType
    DIM player2 AS playerType

    'initialize players
    player1.icon = CHR$(1)
    player1.row = 12
    player1.col = 2
    player1.direction = 0
    player1.score = 0
    player1.iconColor = 12

    player2.icon = CHR$(2)
    player2.row = 12
    player2.col = 79
    player2.direction = 0
    player2.score = 0
    player2.iconColor = 11

    numBullets = 0

    ShowPlayer player1
    ShowPlayer player2

    finished = FALSE
    tickMax = 25                'delay factor
    
    DO
        COLOR 15
        DO  'Do until escape key hit
            DO  'Action Game loop
                tick = (tick + 2) MOD tickMax - 1
                IF tick = 0 THEN
                    COLOR 15, 1
                    Center 1, STR$(player2.score) + " > score <" + STR$(player1.score) + " "
                    MovePlayer player1
                    MovePlayer player2
                    MoveBullets bullet(), numBullets
                    DeleteBullets bullet(), numBullets
                ELSE
                    kbd$ = INKEY$
                END IF
            LOOP WHILE kbd$ = ""

            'handle keystrokes
            SELECT CASE kbd$
                CASE "-": GOSUB HunterDecreaseGameDelay
                CASE "+": GOSUB HunterIncreaseGameDelay
                CASE "w", "W": AddBullet bullet(), numBullets, 1, player1
                CASE "a", "A": AddBullet bullet(), numBullets, 3, player1
                CASE "s", "S": AddBullet bullet(), numBullets, 2, player1
                CASE "d", "D": AddBullet bullet(), numBullets, 4, player1
                CASE "t", "T": IF player1.direction = 2 THEN player1.direction = 0 ELSE player1.direction = 1
                CASE "f", "F": IF player1.direction = 4 THEN player1.direction = 0 ELSE player1.direction = 3
                CASE "g", "G": IF player1.direction = 1 THEN player1.direction = 0 ELSE player1.direction = 2
                CASE "h", "H": IF player1.direction = 3 THEN player1.direction = 0 ELSE player1.direction = 4
                CASE "p", "P": AddBullet bullet(), numBullets, 1, player2
                CASE "l", "l": AddBullet bullet(), numBullets, 3, player2
                CASE ";": AddBullet bullet(), numBullets, 2, player2
                CASE "'": AddBullet bullet(), numBullets, 4, player2
                CASE "8": IF player2.direction = 2 THEN player2.direction = 0 ELSE player2.direction = 1
                CASE "5": IF player2.direction = 1 THEN player2.direction = 0 ELSE player2.direction = 2
                CASE "2": IF player2.direction = 1 THEN player2.direction = 0 ELSE player2.direction = 2
                CASE "4": IF player2.direction = 4 THEN player2.direction = 0 ELSE player2.direction = 3
                CASE "6": IF player2.direction = 3 THEN player2.direction = 0 ELSE player2.direction = 4
                CASE CHR$(27): finished = TRUE
                CASE ELSE
            END SELECT
        LOOP UNTIL finished
    LOOP UNTIL score1 = 5 OR score2 = 5 OR finished
    EXIT SUB

HunterDecreaseGameDelay:
    IF tickMax > 2 THEN
        tickMax = tickMax - 1
    END IF
    COLOR 15
    LOCATE 1, 60
    PRINT "  Delay ="; tickMax - 1; " "
    RETURN

HunterIncreaseGameDelay:
    COLOR 15
    tickMax = tickMax + 1
    LOCATE 1, 60
    PRINT "  Delay ="; tickMax - 1; " "
    RETURN

END SUB

SUB Initialize

    'insure random maze
    RANDOMIZE (TIMER)

    'setup boarder
    arena$(1) = "Ú" + STRING$(78, "Ä") + "¿"
    arena$(25) = "À" + STRING$(78, "Ä") + "Ù"
    FOR a = 2 TO 24
        arena$(a) = "³" + SPACE$(78) + "³"
    NEXT a

    'draw maze elements
    FOR a = 1 TO 15
        row = RND(1) * 20 + 3
        col = RND(1) * 70 + 5
        MID$(arena$(row), col, 1) = CHR$(197)
        MID$(arena$(row - 1), col, 1) = CHR$(179)
        MID$(arena$(row + 1), col, 1) = CHR$(179)
        MID$(arena$(row), col - 1, 1) = CHR$(196)
        MID$(arena$(row), col - 2, 1) = CHR$(196)
        MID$(arena$(row), col + 1, 1) = CHR$(196)
        MID$(arena$(row), col + 2, 1) = CHR$(196)
    NEXT a

    'Scan through arena$() and where evere there is a wall, put a
    '1 in the map array to indicate it's location.  Put a 0 wherever there
    'is a blank space
    FOR row = 1 TO 25
        FOR col = 1 TO 80
            IF MID$(arena$(row), col, 1) = " " THEN
                map(row, col) = 0
            ELSE
                map(row, col) = 1
            END IF
        NEXT col
    NEXT row

END SUB

SUB MoveBullet (bullet AS bulletType)

    'Move the bullet based on the direction.
    SELECT CASE bullet.direction
        CASE 0
        CASE 1  'up
            SELECT CASE map(bullet.row - 1, bullet.col)
                CASE 0, 2, 3
                    HideBullet bullet
                    bullet.row = bullet.row - 1
                    ShowBullet bullet
                CASE 1
                    bullet.direction = 0
            END SELECT
        CASE 2  'down
            SELECT CASE map(bullet.row + 1, bullet.col)
                CASE 0, 2, 3
                    HideBullet bullet
                    bullet.row = bullet.row + 1
                    ShowBullet bullet
                CASE 1
                    bullet.direction = 0
            END SELECT
        CASE 3  'left
            SELECT CASE map(bullet.row, bullet.col - 1)
                CASE 0, 2, 3
                    HideBullet bullet
                    bullet.col = bullet.col - 1
                    ShowBullet bullet
                CASE 1
                    bullet.direction = 0
            END SELECT
        CASE 4  'right
            SELECT CASE map(bullet.row, bullet.col + 1)
                CASE 0, 2, 3
                    HideBullet bullet
                    bullet.col = bullet.col + 1
                    ShowBullet bullet
                CASE 1
                    bullet.direction = 0
            END SELECT
    END SELECT

END SUB

'Move every bullet in the bullet array
SUB MoveBullets (bullet() AS bulletType, numBullets)

    a = 1
    WHILE a <= numBullets
        MoveBullet bullet(a)
        a = a + 1
    WEND

END SUB

'based on the direction of the player, move the player
SUB MovePlayer (player AS playerType)

    'if bullet hit the player, inc score
    IF map(player.row, player.col) = 3 THEN
        player.score = player.score + 1
        BEEP
    END IF

    SELECT CASE player.direction
        CASE 0  'no motion
            ShowPlayer player
        CASE 1  'up
            SELECT CASE map(player.row - 1, player.col)
                CASE 0  'nothing
                    HidePlayer player
                    player.row = player.row - 1
                    ShowPlayer player
                CASE 1, 2   'wall,enemy
                    player.direction = 0
                CASE 3      'bullet
                    player.score = player.score + 1
                    BEEP
            END SELECT
        CASE 2  'down
            SELECT CASE map(player.row + 1, player.col)
                CASE 0  'nothing
                    HidePlayer player
                    player.row = player.row + 1
                    ShowPlayer player
                CASE 1, 2   'wall,enemy
                    player.direction = 0
                CASE 3      'bullet
                    player.score = player.score + 1
                    BEEP
            END SELECT
        CASE 3  'left
            SELECT CASE map(player.row, player.col - 1)
                CASE 0  'nothing
                    HidePlayer player
                    player.col = player.col - 1
                    ShowPlayer player
                CASE 1, 2   'wall,enemy
                    player.direction = 0
                CASE 3  'bullet
                    player.score = player.score + 1
                    BEEP
            END SELECT
        CASE 4  'right
            SELECT CASE map(player.row, player.col + 1)
                CASE 0  'nothing
                    HidePlayer player
                    player.col = player.col + 1
                    ShowPlayer player
                CASE 1, 2   'wall,enemy
                    player.direction = 0
                CASE 3  'bullet
                    player.score = player.score + 1
                    BEEP
            END SELECT
    END SELECT

END SUB

'print the bullet char at bullet location
SUB ShowBullet (bullet AS bulletType)
    COLOR bullet.bcolor
    LOCATE bullet.row, bullet.col
    PRINT CHR$(4);
    map(bullet.row, bullet.col) = 3
    
END SUB

'show player icon in player color at player location
SUB ShowPlayer (player AS playerType)
    COLOR player.iconColor
    LOCATE player.row, player.col
    PRINT player.icon;
    map(player.row, player.col) = 2
END SUB

SUB SparklePause

    a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "

    WHILE INKEY$ = ""
        'print horizontal sparkles
        FOR a = 1 TO 5
            LOCATE 1, 1
            PRINT MID$(a$, a, 80);
            LOCATE 20, 1
            PRINT MID$(a$, 6 - a, 80);

            'print vertical sparkles
            FOR b = 2 TO 19
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

