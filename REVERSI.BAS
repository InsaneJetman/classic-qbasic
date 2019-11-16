DEFINT A-Z
OPTION BASE 1

DECLARE FUNCTION CheckPath% (i, IBound, IStep, j, JBound, JStep, Opponent)
DECLARE FUNCTION ValidMove% (Opponent)
DECLARE SUB ComputerMove ()
DECLARE SUB DisplayHelp ()
DECLARE SUB DisplayMsg (a$)
DECLARE SUB DrawCursor (row, col)
DECLARE SUB DrawGamePiece (row, col, PieceColor)
DECLARE SUB GameOver ()
DECLARE SUB InitGame ()
DECLARE SUB TakeBlocks (row, col, player)
DECLARE SUB UpdateScore ()
DECLARE SUB UserMove ()
DECLARE SUB DrawGameBoard ()

CONST TRUE = -1
CONST FALSE = 0
CONST QUIT = 113
CONST UP = 72
CONST DOWN = 80
CONST LEFT = 75
CONST RIGHT = 77
CONST BBLOCK = 1
CONST EBLOCK = 8
CONST ENTER = 13
CONST ULEFT = 71
CONST URIGHT = 73
CONST DLEFT = 79
CONST DRIGHT = 81
CONST PASS = 112
CONST DIFF = 100
CONST START = 115
CONST HELP = 104
CONST FMOVE = 99
CONST SPACE = 32

TYPE GameGrid
    player AS INTEGER
    nTake  AS INTEGER
    cx     AS INTEGER
    cy     AS INTEGER
END TYPE

TYPE GameStatus
    curRow   AS INTEGER
    curCol   AS INTEGER
    stat     AS INTEGER
    rScore   AS INTEGER
    bScore   AS INTEGER
    mDisplay AS INTEGER
    dLevel   AS STRING * 6
    GColor   AS INTEGER
END TYPE

DIM SHARED GS AS GameStatus, smode AS INTEGER
DIM SHARED GG(8, 8) AS GameGrid, GBoard AS INTEGER
DIM SHARED COMP AS INTEGER, HUMAN AS INTEGER, BG AS INTEGER
DIM SHARED GP(8, 8, 8) AS INTEGER, GW(8, 8) AS INTEGER

ON ERROR GOTO BadMode

DO
  READ smode
  vmode = TRUE
  SCREEN smode
LOOP UNTIL vmode = TRUE

IF smode = 0 THEN
  CLS
  LOCATE 10, 15: PRINT "No graphics screen mode available; cannot run REVERSI.BAS"
ELSE
  GS.stat = START
  GS.dLevel = "Novice"
  WHILE GS.stat <> QUIT
    IF GS.stat = START THEN
      InitGame
      DrawGameBoard
    END IF
    IF GS.stat <> COMP THEN
      IF ValidMove(COMP) THEN
        UserMove
      ELSEIF ValidMove(HUMAN) THEN
        DO
          DisplayMsg "You have no valid moves.  Select pass."
          DO
            a$ = INKEY$
          LOOP UNTIL a$ <> ""
        LOOP UNTIL ASC(RIGHT$(a$, 1)) = PASS
        LINE (0, 420)-(640, 447), 3, BF
        GS.mDisplay = FALSE
        GS.stat = COMP
        ComputerMove
      ELSE
        GameOver
      END IF
    ELSE
      IF ValidMove(HUMAN) THEN
        ComputerMove
      ELSEIF ValidMove(COMP) THEN
        DisplayMsg "Computer has no valid moves.  Your Turn."
        GS.stat = HUMAN
        UserMove
      ELSE
        GameOver
      END IF
    END IF
  WEND
  DisplayMsg "Game Over"
END IF

DATA 9, 10, 2, 3, 0

BadMode:
  vmode = FALSE
RESUME NEXT

END

FUNCTION CheckPath (i, IBound, IStep, j, JBound, JStep, Opponent)

  done = FALSE
  WHILE (i <> IBound OR j <> JBound) AND NOT done
    IF GG(i, j).player = GBoard THEN
      count = 0
      done = TRUE
    ELSEIF GG(i, j).player = Opponent THEN
      count = count + 1
      i = i + IStep
      j = j + JStep
      IF (i < 1 OR i > 8) OR (j < 1 OR j > 8) THEN
        count = 0
        done = TRUE
      END IF
    ELSE
      done = TRUE
    END IF
  WEND
  CheckPath = count
    
END FUNCTION

SUB ComputerMove
  BestMove = -99
  FOR row = 1 TO 8
    FOR col = 1 TO 8
      IF GG(row, col).nTake > 0 THEN
        IF GS.dLevel = "Novice" THEN
          value = GG(row, col).nTake + GW(row, col)
        ELSE
          value = GG(row, col).nTake + GW(row, col)
          SELECT CASE row
            CASE 1
              IF col < 5 THEN value = value + ABS(10 * GG(1, 1).player = COMP)
              IF col > 4 THEN value = value + ABS(10 * GG(1, 8).player = COMP)
            CASE 2
              IF GG(1, col).player <> COMP THEN value = value + 5 * (GG(1, col).player = HUMAN)
              IF col > 1 AND GG(1, col - 1).player <> COMP THEN value = value + 5 * (GG(1, col - 1).player = HUMAN)
              IF col < 8 AND GG(1, col + 1).player <> COMP THEN value = value + 5 * (GG(1, col + 1).player = HUMAN)
            CASE 7
              IF GG(8, col).player <> COMP THEN value = value + 5 * (GG(8, col).player = HUMAN)
              IF col > 1 AND GG(8, col - 1).player <> COMP THEN value = value + 5 * (GG(8, col - 1).player = HUMAN)
              IF col < 8 AND GG(8, col + 1).player <> COMP THEN value = value + 5 * (GG(8, col + 1).player = HUMAN)
            CASE 8
              IF col < 5 THEN value = value + ABS(10 * GG(8, 1).player = COMP)
              IF col > 4 THEN value = value + ABS(10 * GG(8, 8).player = COMP)
          END SELECT
          SELECT CASE col
            CASE 1
              IF row < 5 THEN value = value + ABS(10 * GG(1, 1).player = COMP)
              IF row > 4 THEN value = value + ABS(10 * GG(8, 1).player = COMP)
            CASE 2
              IF GG(row, 1).player <> COMP THEN value = value + 5 * (GG(row, 1).player = HUMAN)
              IF row > 1 AND GG(row - 1, 1).player <> COMP THEN value = value + 5 * (GG(row - 1, 1).player = HUMAN)
              IF row < 8 AND GG(row + 1, 1).player <> COMP THEN value = value + 5 * (GG(row + 1, 1).player = HUMAN)
            CASE 7
              IF GG(row, 8).player <> COMP THEN value = value + 5 * (GG(row, 8).player = HUMAN)
              IF row > 1 AND GG(row - 1, 8).player <> COMP THEN value = value + 5 * (GG(row - 1, 8).player = HUMAN)
              IF row < 8 AND GG(row + 1, 8).player <> COMP THEN value = value + 5 * (GG(row + 1, 8).player = HUMAN)
            CASE 8
              IF row < 5 THEN value = value + ABS(10 * GG(1, 8).player = COMP)
              IF row > 4 THEN value = value + ABS(10 * GG(8, 8).player = COMP)
          END SELECT
        END IF
        IF value > BestMove THEN
          BestMove = value
          bestrow = row
          bestcol = col
        END IF
      END IF
    NEXT col
  NEXT row

  TakeBlocks bestrow, bestcol, COMP
  GS.stat = HUMAN

END SUB

SUB DisplayHelp

  DIM a$(1 TO 18)

  a$(1) = "The object of Reversi is to finish the game with more of your red"
  a$(2) = "circles on the board than the computer has of blue (Monochrome"
  a$(3) = "monitors will show red as white and blue as black)."
  a$(4) = ""
  a$(5) = "1) You and the computer play by the same rules."
  a$(6) = "2) To make a legal move, at least one of the computer's circles"
  a$(7) = "   must lie in a horizontal, vertical, or diagonal line between"
  a$(8) = "   one of your existing circles and the square where you want to"
  a$(9) = "   move.  Use the arrow keys to position the cursor on the square"
  a$(10) = "   and hit Enter or the Space Bar."
  a$(11) = "3) You can choose Pass from the game controls menu on your first"
  a$(12) = "   move to force the computer to play first."
  a$(13) = "4) After your first move, you cannot pass if you can make a legal"
  a$(14) = "   move."
  a$(15) = "5) If you cannot make a legal move, you must choose Pass"
  a$(16) = "6) When neither you nor the computer can make a legal move, the"
  a$(17) = "   game is over."
  a$(18) = "7) The one with the most circles wins."

  LINE (0, 0)-(640, 480), BG, BF
  LINE (39, 15)-(590, 450), 0, B
  IF GBoard = 85 THEN
    PAINT (200, 200), CHR$(85), 0
  ELSE
    PAINT (200, 200), GBoard, 0
  END IF
  LINE (590, 25)-(600, 460), 0, BF
  LINE (50, 450)-(600, 460), 0, BF

  LOCATE 2, 35: PRINT "REVERSI HELP"
  FOR i = 1 TO 18
    LOCATE 3 + i, 7
    PRINT a$(i)
  NEXT i
  LOCATE 23, 25: PRINT "- Press any key to continue -"
  SLEEP: a$ = INKEY$
  DrawGameBoard
  DrawCursor GS.curRow, GS.curCol

END SUB

SUB DisplayMsg (a$)

  slen = LEN(a$)
  LX = (640 - 8 * (slen + 8)) / 2
  LINE (LX - 1, 420)-(640 - LX, 447), 0, B
  IF GBoard = 85 THEN
    PAINT (LX + 10, 430), CHR$(85), 0
  ELSE
    PAINT (LX + 10, 430), GBoard, 0
  END IF
  LOCATE 23, (80 - slen) / 2
  PRINT a$;
  GS.mDisplay = TRUE

END SUB

SUB DrawCursor (row, col)
  IF GG(row, col).nTake > 0 THEN
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 15, HUMAN
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 14, HUMAN
  ELSE
    lc = 0
    IF GG(row, col).player = 0 THEN lc = 7
    LINE (GG(row, col).cx, GG(row, col).cy - 15)-(GG(row, col).cx, GG(row, col).cy + 15), lc
    LINE (GG(row, col).cx - 1, GG(row, col).cy - 15)-(GG(row, col).cx - 1, GG(row, col).cy + 15), lc
    LINE (GG(row, col).cx + 15, GG(row, col).cy)-(GG(row, col).cx - 15, GG(row, col).cy), lc
  END IF
END SUB

SUB DrawGameBoard

  LINE (0, 0)-(640, 480), BG, BF
  LINE (239, 15)-(400, 40), 0, B
  LINE (39, 260)-(231, 390), 0, B
  LINE (39, 70)-(231, 220), 0, B
  LINE (269, 70)-(591, 390), 0, B

  IF GBoard = 85 THEN                  'If b&w
    PAINT (300, 25), CHR$(85), 0
    PAINT (150, 350), CHR$(85), 0
    PAINT (150, 124), CHR$(85), 0
    PAINT (450, 225), CHR$(85), 0
  ELSE
    PAINT (300, 25), GBoard, 0
    PAINT (150, 350), GBoard, 0
    PAINT (150, 124), GBoard, 0
    PAINT (450, 225), GBoard, 0
  END IF
  LINE (400, 25)-(410, 50), 0, BF
  LINE (250, 40)-(410, 50), 0, BF
  LINE (231, 80)-(240, 230), 0, BF
  LINE (50, 220)-(240, 230), 0, BF
  LINE (590, 80)-(600, 400), 0, BF
  LINE (280, 390)-(600, 400), 0, BF
  LINE (231, 270)-(240, 400), 0, BF
  LINE (50, 390)-(240, 400), 0, BF

  FOR i = 0 TO 8
    LINE (270, 70 + i * 40)-(590, 70 + i * 40), 0
    LINE (270 + i * 40, 70)-(270 + i * 40, 390), 0
    LINE (269 + i * 40, 70)-(269 + i * 40, 390), 0
  NEXT i
  
  LOCATE 2, 35: PRINT "R E V E R S I"

  LOCATE 5, 11: PRINT "Game Controls"
  LOCATE 7, 7: PRINT "S = Start New Game"
  LOCATE 8, 7: PRINT "P = Pass Turn"
  LOCATE 9, 7: PRINT "D = Set Difficulty"
  LOCATE 10, 7: PRINT "H = Display Help"
  LOCATE 11, 7: PRINT "Q = Quit"
  LOCATE 15, 12: PRINT "Game Status"
  LOCATE 17, 7: PRINT "Your Score:      "; GS.rScore; ""
  LOCATE 18, 7: PRINT "Computer Score:  "; GS.bScore
  LOCATE 20, 7: PRINT "Difficulty:   "; GS.dLevel

  FOR row = 1 TO 8
    FOR col = 1 TO 8
      IF GG(row, col).player <> GBoard THEN
        DrawGamePiece row, col, GG(row, col).player
      END IF
    NEXT col
  NEXT row

END SUB

SUB DrawGamePiece (row, col, GpColor)

  IF GBoard = 85 THEN
    LINE (232 + col * 40, 33 + row * 40)-(267 + col * 40, 67 + row * 40), 7, BF
    IF GpColor <> GBoard THEN
      CIRCLE (GG(row, col).cx, GG(row, col).cy), 15, 0
      PAINT (GG(row, col).cx, GG(row, col).cy), GpColor, 0
    END IF
    PAINT (235 + col * 40, 35 + row * 40), CHR$(85), 0
  ELSE
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 15, GpColor
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 14, GpColor
    PAINT (GG(row, col).cx, GG(row, col).cy), GpColor, GpColor
  END IF

END SUB

SUB GameOver
  Scorediff = GS.rScore - GS.bScore
  IF Scorediff = 0 THEN
    DisplayMsg "Tie Game"
  ELSEIF Scorediff < 0 THEN
    DisplayMsg "You lost by"
    PRINT ABS(Scorediff)
  ELSE
    DisplayMsg "You won by"
    PRINT Scorediff
  END IF
  DO
    GS.stat = ASC(RIGHT$(INKEY$, 1))
  LOOP UNTIL GS.stat = QUIT OR GS.stat = START
  LINE (0, 420)-(640, 447), BG, BF
END SUB

SUB InitGame
  SELECT CASE smode
    CASE 9:
      HUMAN = 4
      COMP = 1
      BG = 3
      GBoard = 8
    CASE ELSE:
      HUMAN = 7
      COMP = 0
      BG = 7
      IF smode = 10 THEN
        GBoard = 1
      ELSE
        GBoard = 85
      END IF
  END SELECT

  WINDOW SCREEN (640, 480)-(0, 0)
  GS.curCol = 5
  GS.curRow = 3
  GS.stat = FMOVE
  GS.bScore = 2
  GS.rScore = 2
  GS.mDisplay = FALSE

  FOR row = 1 TO 8
    FOR col = 1 TO 8
      GG(row, col).player = GBoard
      GG(row, col).nTake = 0
      GG(row, col).cx = 270 + (col - .5) * 40
      GG(row, col).cy = 70 + (row - .5) * 40
      GW(row, col) = 2
    NEXT col
  NEXT row
  GW(1, 1) = 99
  GW(1, 8) = 99
  GW(8, 1) = 99
  GW(8, 8) = 99
  FOR i = 3 TO 6
    FOR j = 1 TO 8 STEP 7
      GW(i, j) = 5
      GW(j, i) = 5
    NEXT j
  NEXT i
  GG(4, 4).player = HUMAN
  GG(5, 4).player = COMP
  GG(4, 5).player = COMP
  GG(5, 5).player = HUMAN
END SUB

SUB TakeBlocks (row, col, player)

  GG(row, col).player = player
  DrawGamePiece row, col, player

  FOR i = 1 TO GP(row, col, 1)
    GG(row, col - i).player = player
    DrawGamePiece row, col - i, player
  NEXT i
  FOR i = 1 TO GP(row, col, 2)
    GG(row, col + i).player = player
    DrawGamePiece row, col + i, player
  NEXT i
  FOR i = 1 TO GP(row, col, 3)
    GG(row - i, col).player = player
    DrawGamePiece row - i, col, player
  NEXT i
  FOR i = 1 TO GP(row, col, 4)
    GG(row + i, col).player = player
    DrawGamePiece row + i, col, player
  NEXT i
  FOR i = 1 TO GP(row, col, 5)
    GG(row - i, col - i).player = player
    DrawGamePiece row - i, col - i, player
  NEXT i
  FOR i = 1 TO GP(row, col, 6)
    GG(row + i, col + i).player = player
    DrawGamePiece row + i, col + i, player
  NEXT i
  FOR i = 1 TO GP(row, col, 7)
    GG(row - i, col + i).player = player
    DrawGamePiece row - i, col + i, player
  NEXT i
  FOR i = 1 TO GP(row, col, 8)
    GG(row + i, col - i).player = player
    DrawGamePiece row + i, col - i, player
  NEXT i

  IF player = HUMAN THEN
    GS.rScore = GS.rScore + GG(row, col).nTake + 1
    GS.bScore = GS.bScore - GG(row, col).nTake
  ELSE
    GS.bScore = GS.bScore + GG(row, col).nTake + 1
    GS.rScore = GS.rScore - GG(row, col).nTake
  END IF

  LOCATE 17, 7: PRINT "Your Score:      "; GS.rScore
  LOCATE 18, 7: PRINT "Computer Score:  "; GS.bScore

END SUB

SUB UserMove

  DrawCursor GS.curRow, GS.curCol
  DO
    DO
      a$ = INKEY$
    LOOP UNTIL a$ <> ""
    move = ASC(RIGHT$(a$, 1))
    IF GS.mDisplay THEN
      LINE (0, 420)-(640, 447), BG, BF
      GS.mDisplay = FALSE
    END IF
    SELECT CASE move
      CASE 71 TO 81:
        DrawGamePiece GS.curRow, GS.curCol, GG(GS.curRow, GS.curCol).player
        IF move < 74 THEN
          IF GS.curRow = BBLOCK THEN
            GS.curRow = EBLOCK
          ELSE
            GS.curRow = GS.curRow - 1
          END IF
        ELSEIF move > 78 THEN
          IF GS.curRow = EBLOCK THEN
            GS.curRow = BBLOCK
          ELSE
            GS.curRow = GS.curRow + 1
          END IF
        END IF
        IF move = 71 OR move = 75 OR move = 79 THEN
          IF GS.curCol = BBLOCK THEN
            GS.curCol = EBLOCK
          ELSE
            GS.curCol = GS.curCol - 1
          END IF
        ELSEIF move = 73 OR move = 77 OR move = 81 THEN
          IF GS.curCol = EBLOCK THEN
            GS.curCol = BBLOCK
          ELSE
            GS.curCol = GS.curCol + 1
          END IF
        END IF
        DrawCursor GS.curRow, GS.curCol
      CASE START:
        GS.stat = START
      CASE PASS:
        IF GS.stat = FMOVE THEN
          DisplayMsg "You passed.  Computer will make first move."
          GS.stat = COMP
        ELSE
          DisplayMsg "You can only pass on your first turn."
        END IF
      CASE HELP:
        DisplayHelp
      CASE DIFF:
        IF GS.dLevel = "Novice" THEN
          GS.dLevel = "Expert"
        ELSE
          GS.dLevel = "Novice"
        END IF
        LOCATE 20, 7
        PRINT "Difficulty:   "; GS.dLevel;
      CASE ENTER, SPACE:
        IF GG(GS.curRow, GS.curCol).nTake > 0 THEN
          TakeBlocks GS.curRow, GS.curCol, HUMAN
          GS.stat = COMP
        ELSE
          DisplayMsg "Invalid move.  Move to a space where the cursor is a circle."
        END IF
      CASE QUIT:
        GS.stat = QUIT
    END SELECT
  LOOP UNTIL GS.stat <> HUMAN AND GS.stat <> FMOVE

END SUB

FUNCTION ValidMove (Opponent)

  ValidMove = FALSE
  ERASE GP
  FOR row = 1 TO 8
    FOR col = 1 TO 8
      GG(row, col).nTake = 0

      IF GG(row, col).player = GBoard THEN
        IF col > 2 THEN
          GP(row, col, 1) = CheckPath(row, row, 0, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 1)
        END IF
        IF col < 7 THEN
          GP(row, col, 2) = CheckPath(row, row, 0, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 2)
        END IF
        IF row > 2 THEN
          GP(row, col, 3) = CheckPath(row - 1, 0, -1, col, col, 0, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 3)
        END IF
        IF row < 7 THEN
          GP(row, col, 4) = CheckPath(row + 1, 9, 1, col, col, 0, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 4)
        END IF
        IF col > 2 AND row > 2 THEN
          GP(row, col, 5) = CheckPath(row - 1, 0, -1, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 5)
        END IF
        IF col < 7 AND row < 7 THEN
          GP(row, col, 6) = CheckPath(row + 1, 9, 1, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 6)
        END IF
        IF col < 7 AND row > 2 THEN
          GP(row, col, 7) = CheckPath(row - 1, 0, -1, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 7)
        END IF
        IF col > 2 AND row < 7 THEN
          GP(row, col, 8) = CheckPath(row + 1, 9, 1, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 8)
        END IF
        IF GG(row, col).nTake > 0 THEN ValidMove = TRUE
      END IF
    NEXT col
  NEXT row

END FUNCTION

