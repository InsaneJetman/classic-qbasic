DEFINT A-Z
DECLARE SUB EndGame ()
DECLARE SUB Center (row%, text$)
DECLARE SUB Intro ()
DECLARE SUB SparklePause ()
DECLARE SUB GetInputs (player1$, player2$, numGames%, gravity#)
DECLARE SUB PlayGame (player1$, player2$, numGames%, gravity#)
DECLARE SUB CyclePalette ()
DECLARE SUB DoExplosion (x#, y#)
DECLARE SUB MakeBattleField (TerrainHeight%())
DECLARE SUB PlaceCastles (XPos%(), YPos%(), TerrainHeight%())
DECLARE SUB UpdateScores (Record%(), PlayerNum%, Results%)
DECLARE FUNCTION DoShot% (PlayerNum%, XPos%, YPos%)
DECLARE FUNCTION ExplodeCastle% (x#, y#)
DECLARE FUNCTION GetNum# (row, col)
DECLARE FUNCTION PlotShot% (StartX%, StartY%, Angle#, Velocity%)

CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST SHOTSELF = 1
CONST BACKGROUNDCOLOR = 0
CONST TERRAINCOLOR = 1
CONST EXPLOSIONCOLOR = 2
CONST OBJECTCOLOR = 3

DIM SHARED CastleX(1 TO 2)
DIM SHARED CastleY(1 TO 2)

DIM SHARED PI#
DIM SHARED CastlePic&(1 TO 40)
DIM SHARED shot&(1 TO 10)

DIM SHARED gravity#
DIM SHARED Wind

DIM SHARED ScreenHeight
DIM SHARED ScreenWidth
DIM SHARED mode
DIM SHARED MaxCol
DIM SHARED BaseCol

    Intro
    GetInputs name1$, name2$, numGames, gravity#

    GOSUB InitializeVariables

    PlayGame name1$, name2$, numGames, gravity#
    EndGame
END




CGAPic:
    DATA 589840,-12301,-1,-62915521,-62915521,64575

CGAShot:
    DATA 196614,3210288&

EGAPic:
    DATA 1048592,-806105101,0,-806105101,0,-1,0,-1,0,-1,0,-1,0,-62915521,0
    DATA -62915521,0,-62915521,0,-62915521,0,-62915521,0,-62915521,0,-62915521
    DATA  0,-62915521,0,-62915521,0,-62915521,0,0,0,-62915521,0,0,0

EGAShot:
    DATA 196611,57568,57568,57568



InitializeVariables:
    PI# = 4 * ATN(1#)

    ON ERROR GOTO ScreenModeError
    mode = 9
    SCREEN mode
    ON ERROR GOTO 0
    IF mode = 9 THEN
        ScreenWidth = 640
        ScreenHeight = 350

        RESTORE EGAPic
        FOR Counter = 1 TO 39
            READ CastlePic&(Counter)
        NEXT Counter

        FOR Counter = 1 TO 4
            READ shot&(Counter)
        NEXT Counter

        COLOR 3, 1
        PALETTE TERRAINCOLOR, 2                     'Set color for ground
        PALETTE EXPLOSIONCOLOR, 4                   'Explosion color
        PALETTE OBJECTCOLOR, 12
        BaseCol = 30
        MaxCol = 80
    ELSE
        ScreenWidth = 320
        ScreenHeight = 200
        RESTORE CGAPic
        FOR Counter = 1 TO 6
            READ CastlePic&(Counter)
        NEXT Counter
        FOR Counter = 1 TO 2
            READ shot&(Counter)
        NEXT Counter
        COLOR 3, 0
        BaseCol = 10
        MaxCol = 40
    END IF

RETURN



ScreenModeError:
    IF mode = 1 THEN
        PRINT "Sorry, You must have CGA, EGA, or VGA graphics to play Castles"
        END
    ELSE
        mode = 1
        RESUME
    END IF

SUB Center (row, text$)

    LOCATE row, 41 - LEN(text$) / 2
    PRINT text$;

END SUB

SUB CyclePalette
    IF mode = 9 THEN
        PALETTE EXPLOSIONCOLOR, 38
        PALETTE EXPLOSIONCOLOR, 44
    ELSE
        COLOR 12, EXPLOSIONCOLOR
        COLOR 14, EXPLOSIONCOLOR
        COLOR 3, EXPLOSIONCOLOR
    END IF
END SUB

SUB DoExplosion (x#, y#)
    PLAY "MBO0L32EFGEFDC"
    Radius = ScreenHeight / 70
    IF mode = 9 THEN Increment# = .5 ELSE Increment# = 1.2
    FOR Counter# = 0 TO Radius STEP Increment#
        CIRCLE (x#, y#), Counter#, EXPLOSIONCOLOR
        CALL CyclePalette
    NEXT Counter#
    FOR Counter# = Radius TO 0 STEP (-1 * Increment#)
        CIRCLE (x#, y#), Counter#, BACKGROUNDCOLOR
        FOR Delay = 1 TO 100
        NEXT Delay
    NEXT Counter#
END SUB

FUNCTION DoShot (PlayerNum, XPos, YPos)
    IF PlayerNum = 1 THEN
        locateCol = 1
    ELSE
        IF mode = 9 THEN
            locateCol = 66
        ELSE
            locateCol = 26
        END IF
    END IF
    YShotPos = YPos - 3
    LOCATE 2, locateCol
    PRINT "Angle:";
    Angle# = GetNum#(2, locateCol + 7)

    LOCATE 3, locateCol
    PRINT "Velocity:";
    Velocity = GetNum#(3, locateCol + 10)

    IF PlayerNum = 2 THEN Angle# = 180 - Angle#

    VIEW PRINT 1 TO 4
    CLS 2
    VIEW PRINT 1 TO 25

    PlayerHit = PlotShot(XPos, YShotPos, Angle#, Velocity)
    IF PlayerHit = PlayerNum THEN
        DoShot = SHOTSELF
    ELSEIF PlayerHit <> 0 THEN
        DoShot = TRUE
    ELSE
        DoShot = FALSE
    END IF
END FUNCTION

SUB EndGame

    SCREEN 0
    COLOR 15, 0
    CLS

END SUB

FUNCTION ExplodeCastle (x#, y#)
    SHARED CastleX(), CastleY()
    ScaleX# = ScreenWidth / 320
    ScaleY# = ScreenHeight / 200
    IF x# < ScreenWidth / 2 THEN PlayerHit = 1 ELSE PlayerHit = 2
    PLAY "MBO0L16EFGEFDC"
    FOR Blast = 1 TO 8 * ScaleX#
        CIRCLE (CastleX(PlayerHit) + 3.5 * ScaleX#, CastleY(PlayerHit) + 7 * ScaleY#), Blast, EXPLOSIONCOLOR, , , -1.57
        LINE (CastleX(PlayerHit) + 7 * ScaleX#, CastleY(PlayerHit) + 9 * ScaleY# - Blast)-(CastleX(PlayerHit), CastleY(PlayerHit) + 9 * ScaleY# - Blast), EXPLOSIONCOLOR
        IF Blast MOD (10 - mode) < 3 THEN CALL CyclePalette
    NEXT Blast
    FOR Cloud = 1 TO 16 * ScaleX#
        IF Cloud < (8 * ScaleX#) THEN CIRCLE (CastleX(PlayerHit) + 3.5 * ScaleX#, CastleY(PlayerHit) + 7 * ScaleY#), (8 * ScaleX# + 1) - Cloud, BACKGROUNDCOLOR, , , -1.57
        CIRCLE (CastleX(PlayerHit) + 3.5 * ScaleX#, CastleY(PlayerHit)), Cloud, EXPLOSIONCOLOR, , , -1.57
        IF Cloud MOD (10 - mode) < 3 THEN CALL CyclePalette
    NEXT Cloud
    FOR Cloud = 16 * ScaleX# TO 1 STEP -1
        CIRCLE (CastleX(PlayerHit) + 3.5 * ScaleX#, CastleY(PlayerHit)), Cloud, BACKGROUNDCOLOR, , , -1.57
        FOR Delay = 1 TO 200
        NEXT Delay
    NEXT Cloud
    ExplodeCastle = PlayerHit
END FUNCTION

SUB GetInputs (player1$, player2$, numGames, gravity#)

    SCREEN 0
    COLOR 14, 1
    CLS

    DO
        LOCATE 9, 30
        LINE INPUT "Name of Player 1 :"; player1$
    LOOP UNTIL player1$ <> ""

    DO
        LOCATE 10, 30
        LINE INPUT "Name of Player 2 :"; player2$
    LOOP UNTIL player2$ <> ""

    LOCATE 12, 26
    INPUT "Play to how many points"; numGames

    DO
      LOCATE 14, 22
      INPUT "Gravity in Meters/Sec (Earth = 9.8)"; gravity#
    LOOP UNTIL gravity# > 0

END SUB

FUNCTION GetNum# (row, col)

    result$ = ""
    finished = FALSE

    DO WHILE NOT finished

        LOCATE row, col
        PRINT result$; CHR$(95); " ";
        
        kbd$ = INKEY$
        SELECT CASE kbd$
            CASE "0" TO "9"
                result$ = result$ + kbd$
            CASE "."
                IF INSTR(result$, ".") = 0 THEN
                    result$ = result$ + kbd$
                END IF
            CASE CHR$(13)
                finished = TRUE
            CASE CHR$(8)
                IF LEN(result$) > 0 THEN
                    result$ = LEFT$(result$, LEN(result$) - 1)
                END IF
            CASE ELSE
                IF LEN(kbd$) > 0 THEN
                    BEEP
                END IF
        END SELECT
    LOOP

    LOCATE row, col
    PRINT result$; " ";

    GetNum# = VAL(result$)

END FUNCTION

SUB Intro

    SCREEN 0
    COLOR 12, 1
    CLS
    Center 8, "Q u i c k B A S I C   C A S T L E S"
    COLOR 14
    Center 10, "Your mission is to destroy your opponent's castle"
    Center 11, "by varying the angle and power of your catapult"
    Center 12, "taking into account wind speed, gravity and terrain."
    Center 24, "Push Any Key To Continue"
    COLOR 15
    PLAY "T160O1L8CDEDCDL4ECC"
    SparklePause

END SUB

SUB MakeBattleField (TerrainHeight())
    IF mode = 9 THEN Increment = 2 ELSE Increment = 1
    TerrainHeight(0) = ScreenHeight - (10 + INT((ScreenHeight / 3) * RND + 1))
    FOR Counter = 1 TO ScreenWidth
        Motion = INT(20 * RND + 1)
        IF Counter < (ScreenWidth / 2) THEN OnFirstHalfScreen = TRUE ELSE OnFirstHalfScreen = FALSE
        IF INT(4 * RND + 1) = 1 THEN ShouldCheckScreenPos = TRUE ELSE ShouldCheckScreenPos = FLASE
        SELECT CASE Motion
            CASE 1 TO 10
                IF (ShouldCheckScreenPos AND OnFirstHalfScreen) THEN
                    Trend = Trend - Increment
                ELSEIF (ShouldCheckScreenPos AND (NOT OnFirstHalfScreen)) THEN
                    Trend = Trend + Increment
                ELSEIF Motion < 6 THEN
                    Trend = Trend - Increment
                ELSE
                    Trend = Trend + Increment
                END IF
            CASE 11 TO 14
                IF (ShouldCheckScreenPos AND OnFirstHalfScreen) THEN
                    Trend = Trend - Increment * 2
                ELSEIF (ShouldCheckScreenPos AND (NOT OnFirstHalfScreen)) THEN
                    Trend = Trend + Increment * 2
                ELSEIF Motion < 13 THEN
                    Trend = Trend - Increment * 2
                ELSE
                    Trend = Trend + Increment * 2
                END IF
            CASE 15
                Trend = 0
            CASE 16
                Trend = 1
            CASE 17
                Trend = -1
            CASE ELSE
        END SELECT
        SELECT CASE Trend
            CASE IS < -10
                TerrainHeight(Counter) = TerrainHeight(Counter - 1) - 3
            CASE IS < 0
                TerrainHeight(Counter) = TerrainHeight(Counter - 1) - 1
            CASE IS > 10
                TerrainHeight(Counter) = TerrainHeight(Counter - 1) + 3
            CASE IS > 0
                TerrainHeight(Counter) = TerrainHeight(Counter - 1) + 1
            CASE ELSE
                TerrainHeight(Counter) = TerrainHeight(Counter - 1)
        END SELECT
        IF TerrainHeight(Counter) > (ScreenHeight - (8 + mode)) THEN
            TerrainHeight(Counter) = (ScreenHeight - (8 + mode))
            IF OnFirstHalfScreen THEN Trend = -9 ELSE Trend = -3
        ELSE
            IF TerrainHeight(Counter) < (ScreenHeight / 2.2) THEN
                TerrainHeight(Counter) = (ScreenHeight / 2.2)
                IF OnFirstHalfScreen THEN Trend = 9 ELSE Trend = 3
            END IF
        END IF
        LINE (Counter, ScreenHeight)-(Counter, TerrainHeight(Counter)), TERRAINCOLOR
    NEXT Counter
    Wind = INT(10 * RND + 1) - 5
    IF (INT(3 * RND + 1) = 1) THEN
        IF Wind > 0 THEN
            Wind = Wind + INT(10 * RND + 1)
        ELSE
            Wind = Wind - INT(10 * RND + 1)
        END IF
    END IF
    IF Wind <> 0 THEN
        WindLineLength = Wind * (ScreenWidth / 320)
        LINE (ScreenWidth / 2, ScreenHeight - 15)-(ScreenWidth / 2 + WindLineLength, ScreenHeight - 15), EXPLOSIONCOLOR
        IF Wind > 0 THEN ArrowDir = -2 ELSE ArrowDir = 2
        LINE (ScreenWidth / 2 + WindLineLength, ScreenHeight - 15)-(ScreenWidth / 2 + WindLineLength + ArrowDir, ScreenHeight - 15 - 2), EXPLOSIONCOLOR
        LINE (ScreenWidth / 2 + WindLineLength, ScreenHeight - 15)-(ScreenWidth / 2 + WindLineLength + ArrowDir, ScreenHeight - 15 + 2), EXPLOSIONCOLOR
    END IF
END SUB

SUB PlaceCastles (CastleX(), CastleY(), TerrainHeight())
    ScaleX# = ScreenWidth / 320
    ScaleY# = ScreenHeight / 200
    FOR Counter = 1 TO 2
        CastleX(Counter) = INT((ScreenWidth / 3.2) * RND + ((ScreenWidth / 1.6 - 3) * (Counter - 1))) + 2
        CastleY(Counter) = TerrainHeight(CastleX(Counter)) - (9 * ScaleY#)
        PUT (CastleX(Counter), CastleY(Counter)), CastlePic&, PSET
        FOR FixTerrain = CastleX(Counter) TO CastleX(Counter) + (7 * ScaleX#)
            LINE (FixTerrain, ScreenHeight)-(FixTerrain, CastleY(Counter) + (9 * ScaleY#)), TERRAINCOLOR
            LINE (FixTerrain, 0)-(FixTerrain, CastleY(Counter) - 1), BACKGROUNDCOLOR
        NEXT FixTerrain
    NEXT Counter
END SUB

SUB PlayGame (player1$, player2$, numGames, gravity#)

    
    DIM TerrainHeight(0 TO 640)
    DIM TotalWins(1 TO 2)

    RANDOMIZE (TIMER)

    IF mode = 9 THEN
        PALETTE OBJECTCOLOR, 63
    ELSE
        COLOR 3, 0
    END IF

    FOR Counter = 1 TO numGames
        CLS
        CALL MakeBattleField(TerrainHeight())
        CALL PlaceCastles(CastleX(), CastleY(), TerrainHeight())
        DirectHit = FALSE
        DO WHILE DirectHit = FALSE
            LOCATE 1, 1
            PRINT player1$
            LOCATE 1, (MaxCol - 1 - LEN(player2$))
            PRINT player2$
            LOCATE 1, BaseCol + 3
            PRINT TotalWins(1); ">Score<"; TotalWins(2)
            IF Counter MOD 2 THEN FirstPlayer = 1 ELSE FirstPlayer = 2
            SecondPlayer = ABS(FirstPlayer - 3)
            DirectHit = DoShot(FirstPlayer, CastleX(FirstPlayer), CastleY(FirstPlayer))
            IF DirectHit = FALSE THEN
                LOCATE 1, 1
                PRINT player1$
                LOCATE 1, (MaxCol - 1 - LEN(player2$))
                PRINT player2$
                DirectHit = DoShot(SecondPlayer, CastleX(SecondPlayer), CastleY(SecondPlayer))
                IF DirectHit <> FALSE THEN CALL UpdateScores(TotalWins(), SecondPlayer, DirectHit)
            ELSE
                CALL UpdateScores(TotalWins(), FirstPlayer, DirectHit)
            END IF
        LOOP
        SLEEP 1
    NEXT Counter

    SCREEN 0
    COLOR 14, 1
    CLS
    LOCATE 8, 35: PRINT "GAME OVER!"
    LOCATE 10, 30: PRINT "Score:"
    LOCATE 11, 34: PRINT player1$; TAB(30 + 20); TotalWins(1)
    LOCATE 12, 34: PRINT player2$; TAB(30 + 20); TotalWins(2)
    Center 24, "Push Any Key To Continue"
    COLOR 14
    SparklePause

END SUB

FUNCTION PlotShot (StartX, StartY, Angle#, Velocity)

    Angle# = Angle# / 180 * PI#                'Convert degree angle to radians
    Radius = mode MOD 7
    InitialXVelocity# = COS(Angle#) * Velocity
    InitialYVelocity# = SIN(Angle#) * Velocity
    Oldx# = StartX
    Oldy# = StartY
    PLAY "MBo0L32A-L64CL16BL64A+"
    DirectHit = FALSE
    Impact = FALSE
    OnScreen = TRUE
    PlayerHit = 0
    NEEDERASE = FALSE

    IF Velocity < 2 THEN                          'Shot too slow - hit self
        x# = StartX
        y# = StartY
        Impact = TRUE
        DirectHit = TRUE
    END IF

    DO WHILE (NOT Impact) AND OnScreen
        x# = StartX + (InitialXVelocity# * t#) + (.5 * (Wind / 5) * t# ^ 2)
        y# = StartY + ((-1 * (InitialYVelocity# * t#)) + (.5 * gravity# * t# ^ 2)) * (ScreenHeight / 350)
        IF (x# >= ScreenWidth - 3) OR (x# <= 3) OR (y# >= ScreenHeight - 3) THEN
          OnScreen = FALSE
        END IF
        IF NEEDERASE THEN
            PUT (Oldx#, Oldy#), shot&, XOR
        END IF
        FOR LookX = -1 TO 1
            FOR LookY = -1 TO 1
                IF POINT(x# + LookX, y# + LookY) = TERRAINCOLOR OR POINT(x# + LookX, y# + LookY) = OBJECTCOLOR THEN Impact = TRUE
            NEXT
        NEXT
        IF OnScreen AND NOT Impact AND y# > 0 THEN
            PUT (x#, y#), shot&, PSET
            NEEDERASE = TRUE
            Oldx# = x#
            Oldy# = y#
        ELSE
            NEEDERASE = FALSE
            IF NOT OnScreen OR y# < 0 THEN
                Oldx# = 0
                Oldy# = 0
            ELSE
                FOR LookX = -1 TO 1
                    FOR LookY = -1 TO 1
                        IF POINT(x# + LookX, y# + LookY) = OBJECTCOLOR THEN DirectHit = TRUE
                    NEXT
                NEXT
            END IF
        END IF
        t# = t# + .1
    LOOP
    IF Impact THEN CALL DoExplosion(x#, y#)
    IF DirectHit THEN PlayerHit = ExplodeCastle(x#, y#)
    PlotShot = PlayerHit

END FUNCTION

SUB SparklePause

    COLOR 15, 1

    a$ = "*    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    *    "

    WHILE INKEY$ = ""
        FOR a = 1 TO 5
            LOCATE 1, 1
            PRINT MID$(a$, a, 80);
            LOCATE 20, 1
            PRINT MID$(a$, 6 - a, 80);

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

SUB UpdateScores (Record(), PlayerNum, Results)
    IF Results = SHOTSELF THEN
        Record(ABS(PlayerNum - 3)) = Record(ABS(PlayerNum - 3)) + 1
    ELSE
        Record(PlayerNum) = Record(PlayerNum) + 1
    END IF
END SUB

