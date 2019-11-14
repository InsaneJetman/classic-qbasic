' ======================================================================
'                                TORUS
'   This program draws a Torus figure. The program accepts user input
'   to specify various TORUS parameters. It checks the current system
'   configuration and takes appropriate action to set the best possible
'   initial mode.
' ======================================================================

DEFINT A-Z
DECLARE SUB GetConfig ()
DECLARE SUB SetPalette ()
DECLARE SUB TorusDefine ()
DECLARE SUB TorusCalc (T() AS ANY)
DECLARE SUB TorusColor (T() AS ANY)
DECLARE SUB TorusSort (Low, High)
DECLARE SUB TorusDraw (T() AS ANY, Index())
DECLARE SUB TileDraw (T AS ANY)
DECLARE SUB TorusRotate (First)
DECLARE SUB Delay (Seconds!)
DECLARE SUB CountTiles (T1, T2)
DECLARE SUB Message (Text$)
DECLARE SUB SetConfig (mode)
DECLARE FUNCTION Inside (T AS ANY)
DECLARE FUNCTION DegToRad! (Degrees)
DECLARE FUNCTION Rotated (Lower, Upper, Current, Inc)

' General purpose constants
CONST PI = 3.14159
CONST TRUE = -1, FALSE = 0
CONST BACK = 0
CONST TROW = 24, TCOL = 60

' Rotation flags
CONST RNDM = -1
CONST START = 0
CONST CONTINUE = 1

' Constants for best available screen mode
CONST VGA = 12
CONST MCGA = 13
CONST EGA256 = 9
CONST EGA64 = 8
CONST MONO = 10
CONST HERC = 3
CONST CGA = 1

' User-defined type for tiles - an array of these make a torus
TYPE Tile
   x1    AS SINGLE
   x2    AS SINGLE
   x3    AS SINGLE
   x4    AS SINGLE
   y1    AS SINGLE
   y2    AS SINGLE
   y3    AS SINGLE
   y4    AS SINGLE
   z1    AS SINGLE
   xc    AS SINGLE
   yc    AS SINGLE
   TColor AS INTEGER
END TYPE

' User-defined type to hold information about the mode
TYPE Config
   Scrn     AS INTEGER
   Colors   AS INTEGER
   Atribs   AS INTEGER
   XPix     AS INTEGER
   YPix     AS INTEGER
   TCOL     AS INTEGER
   TROW     AS INTEGER
END TYPE

DIM VC AS Config

' User-defined type to hold information about current Torus
TYPE TORUS
   Panel    AS INTEGER
   Sect     AS INTEGER
   Thick    AS SINGLE
   XDegree  AS INTEGER
   YDegree  AS INTEGER
   Bord     AS STRING * 3
   Delay    AS SINGLE
END TYPE

DIM TOR AS TORUS, Max AS INTEGER

' A palette of colors to paint with
DIM Pal(0 TO 300) AS LONG

' Error variables to check screen type
DIM InitRows AS INTEGER, BestMode AS INTEGER, Available AS STRING

' The code of the module-level program begins here
  
   ' Initialize defaults
   TOR.Thick = 3: TOR.Bord = "YES"
   TOR.Panel = 8: TOR.Sect = 14
   TOR.XDegree = 60: TOR.YDegree = 165

   ' Get best configuration and set initial graphics mode to it
   GetConfig
   VC.Scrn = BestMode
             
   DO WHILE TRUE           ' Loop forever (exit is from within a SUB)
          
      ' Get Torus definition from user
      TorusDefine
     
      ' Dynamically dimension arrays
      DO
         Tmp = TOR.Panel
         Max = TOR.Panel * TOR.Sect
                   
         ' Array for indexes
         REDIM Index(0 TO Max - 1) AS INTEGER
         ' Turn on error trap for insufficient memory
         ON ERROR GOTO MemErr
         ' Array for tiles
         REDIM T(0 TO Max - 1) AS Tile
         ON ERROR GOTO 0
      LOOP UNTIL Tmp = TOR.Panel
     
      ' Initialize array of indexes
      FOR Til = 0 TO Max - 1
         Index(Til) = Til
      NEXT

      ' Calculate the points of each tile on the torus
      Message "Calculating"
      TorusCalc T()
                 
      ' Color each tile in the torus.
      TorusColor T()
                
      ' Sort the tiles by their "distance" from the screen
      Message "Sorting"
      TorusSort 0, Max - 1
         
      ' Set the screen mode
      SCREEN VC.Scrn
      
      ' Mix a palette of colors
      SetPalette
      
      ' Set logical window with variable thickness
      ' Center is 0, up and right are positive, down and left are negative
      WINDOW (-(TOR.Thick + 1), -(TOR.Thick + 1))-(TOR.Thick + 1, TOR.Thick + 1)
         
      ' Draw and paint the tiles, the farthest first and nearest last
      TorusDraw T(), Index()
     
      ' Rotate the torus by rotating the color palette
      DO WHILE INKEY$ = ""
         Delay (TOR.Delay)
         TorusRotate CONTINUE
      LOOP
      SCREEN 0
      WIDTH 80
   LOOP
  
   ' Restore original rows
   WIDTH 80, InitRows

END

' Error trap to make torus screen independent
VideoErr:
   SELECT CASE BestMode    ' Fall through until something works
      CASE VGA
         BestMode = MCGA
         Available = "12BD"
      CASE MCGA
         BestMode = EGA256
         Available = "12789"
      CASE EGA256
         BestMode = CGA
         Available = "12"
      CASE CGA
         BestMode = MONO
         Available = "A"
      CASE MONO
         BestMode = HERC
         Available = "3"
      CASE ELSE
         PRINT "Sorry. Graphics not available. Can't run Torus."
         END
   END SELECT
   RESUME

' Trap to detect 64K EGA
EGAErr:
   BestMode = EGA64
   Available = "12789"
   RESUME NEXT

' Trap to detect insufficient memory for large Torus
MemErr:
   LOCATE 22, 1
   PRINT "Out of memory"
   PRINT "Reducing panels from"; TOR.Panel; "to"; TOR.Panel - 1
   PRINT "Reducing sections from"; TOR.Sect; "to"; TOR.Sect - 1;
   DO WHILE INKEY$ = "": LOOP
   TOR.Panel = TOR.Panel - 1
   TOR.Sect = TOR.Sect - 1
   RESUME NEXT

' Trap to determine initial number of rows so they can be restored
RowErr:
   IF InitRows = 50 THEN
      InitRows = 43
      RESUME
   ELSE
      InitRows = 25
      RESUME NEXT
   END IF

' ============================ CountTiles ==============================
'   Displays number of the tiles currently being calculated or sorted.
' ======================================================================
'
SUB CountTiles (T1, T2) STATIC

   ' Erase previous
   LOCATE TROW - 1, TCOL: PRINT SPACE$(19);
   ' If positive, display - give negative values to erase
   IF T1 > 0 AND T2 > 0 THEN
      LOCATE TROW - 1, TCOL
      PRINT "Tile ";
      PRINT USING " ###"; T1;
      PRINT USING " ###"; T2;
   END IF

END SUB

' ============================ DegToRad ================================
'   Convert degrees to radians, since BASIC trigonometric functions
'   require radians.
' ======================================================================
'
FUNCTION DegToRad! (Degrees) STATIC

   DegToRad! = (Degrees * 2 * PI) / 360

END FUNCTION

' =============================== Delay ================================
'   Delay based on time so that wait will be the same on any processor.
'   Notice the check for negative numbers so that the delay won't
'   freeze at midnight when the delay could become negative.
' ======================================================================
'
SUB Delay (Seconds!) STATIC
 
   Begin! = TIMER
   DO UNTIL (TIMER - Begin! > Seconds!) OR (TIMER - Begin! < 0)
   LOOP

END SUB

' ============================ GetConfig ===============================
'   Get the starting number of lines and the video adapter.
' ======================================================================
'
SUB GetConfig STATIC
SHARED InitRows AS INTEGER, BestMode AS INTEGER, Available AS STRING

   ' Assume 50 line display and fall through error
   ' until we get the actual number
   InitRows = 50
   ON ERROR GOTO RowErr
   LOCATE InitRows, 1

   ' Assume best possible screen mode
   BestMode = VGA
   Available = "12789BCD"
   
   ON ERROR GOTO VideoErr
   ' Fall through error trap until a mode works
   SCREEN BestMode
   ' If EGA, then check pages to see whether more than 64K
   ON ERROR GOTO EGAErr
   IF BestMode = EGA256 THEN SCREEN 8, , 1
   
   ON ERROR GOTO 0
   
   ' Reset text mode
   SCREEN 0, , 0
   WIDTH 80, 25
   
END SUB

' ============================== Inside ================================
'   Finds a point, T.xc and T.yc, that is mathematically within a tile.
'   Then check to see if the point is actually inside. Because of the
'   jagged edges of tiles, the center point is often actually inside
'   very thin tiles. Such tiles will not be painted, This causes
'   imperfections that are often visible at the edge of the Torus.
'
'   Return FALSE if a center point is not found inside a tile.
' ======================================================================
'
FUNCTION Inside (T AS Tile) STATIC
SHARED VC AS Config
DIM Highest AS SINGLE, Lowest AS SINGLE

   Border = VC.Atribs - 1

   ' Find an inside point. Since some tiles are triangles, the
   ' diagonal center isn't good enough. Instead find the center
   ' by drawing a diagonal from the center of the outside to
   ' a bottom corner.
   T.xc = T.x2 + ((T.x3 + (T.x4 - T.x3) / 2 - T.x2) / 2)
   T.yc = T.y2 + ((T.y3 + (T.y4 - T.y3) / 2 - T.y2) / 2)

   ' If we're on a border, no need to fill
   IF POINT(T.xc, T.yc) = Border THEN
      Inside = FALSE
      EXIT FUNCTION
   END IF

   ' Find highest and lowest Y on the tile
   Highest = T.y1
   Lowest = T.y1
   IF T.y2 > Highest THEN Highest = T.y2
   IF T.y2 < Lowest THEN Lowest = T.y2
   IF T.y3 > Highest THEN Highest = T.y3
   IF T.y3 < Lowest THEN Lowest = T.y3
   IF T.y4 > Highest THEN Highest = T.y4
   IF T.y4 < Lowest THEN Lowest = T.y4

   ' Convert coordinates to pixels
   X = PMAP(T.xc, 0)
   YU = PMAP(T.yc, 1)
   YD = YU
   H = PMAP(Highest, 1)
   L = PMAP(Lowest, 1)
 
   ' Search for top and bottom tile borders until we either find them
   ' both, or check beyond the highest and lowest points.
 
   IsUp = FALSE
   IsDown = FALSE

   DO
      YU = YU - 1
      YD = YD + 1
   
      ' Search up
      IF NOT IsUp THEN
         IF POINT(T.xc, PMAP(YU, 3)) = Border THEN IsUp = TRUE
      END IF
  
      ' Search down
      IF NOT IsDown THEN
         IF POINT(T.xc, PMAP(YD, 3)) = Border THEN IsDown = TRUE
      END IF
                                         
      ' If top and bottom are found, we're inside
      IF IsUp AND IsDown THEN
         Inside = TRUE
         EXIT FUNCTION
      END IF

   LOOP UNTIL (YD > L) AND (YU < H)
   Inside = FALSE

END FUNCTION

' ============================= Message ================================
'   Displays a status message followed by blinking dots.
' ======================================================================
'
SUB Message (Text$) STATIC
SHARED VC AS Config

   LOCATE TROW, TCOL: PRINT SPACE$(19);
   LOCATE TROW, TCOL
   COLOR 7       ' White
   PRINT Text$;
   COLOR 23      ' Blink
   PRINT " . . .";
   COLOR 7       ' White

END SUB

' ============================ Rotated =================================
'   Returns the Current value adjusted by Inc and rotated if necessary
'   so that it falls within the range of Lower and Upper.
' ======================================================================
'
FUNCTION Rotated (Lower, Upper, Current, Inc)

   ' Calculate the next value
   Current = Current + Inc
  
   ' Handle special cases of rotating off top or bottom
   IF Current > Upper THEN Current = Lower
   IF Current < Lower THEN Current = Upper
   Rotated = Current

END FUNCTION

' ============================ SetConfig ===============================
'   Sets the correct values for each field of the VC variable. They
'   vary depending on Mode and on the current configuration.
' ======================================================================
'
SUB SetConfig (mode AS INTEGER) STATIC
SHARED VC AS Config, BestMode AS INTEGER

   SELECT CASE mode
      CASE 1   ' Four-color graphics for CGA, EGA, VGA, and MCGA
         IF BestMode = CGA OR BestMode = MCGA THEN
            VC.Colors = 0
         ELSE
            VC.Colors = 16
         END IF
         VC.Atribs = 4
         VC.XPix = 319
         VC.YPix = 199
         VC.TCOL = 40
         VC.TROW = 25
      CASE 2   ' Two-color medium-res graphics for CGA, EGA, VGA, and MCGA
         IF BestMode = CGA OR BestMode = MCGA THEN
            VC.Colors = 0
         ELSE
            VC.Colors = 16
         END IF
         VC.Atribs = 2
         VC.XPix = 639
         VC.YPix = 199
         VC.TCOL = 80
         VC.TROW = 25
      CASE 3   ' Two-color high-res graphics for Hercules
         VC.Colors = 0
         VC.Atribs = 2
         VC.XPix = 720
         VC.YPix = 348
         VC.TCOL = 80
         VC.TROW = 25
      CASE 7   ' 16-color medium-res graphics for EGA and VGA
         VC.Colors = 16
         VC.Atribs = 16
         VC.XPix = 319
         VC.YPix = 199
         VC.TCOL = 40
         VC.TROW = 25
      CASE 8   ' 16-color high-res graphics for EGA and VGA
         VC.Colors = 16
         VC.Atribs = 16
         VC.XPix = 639
         VC.YPix = 199
         VC.TCOL = 80
         VC.TROW = 25
      CASE 9   ' 16- or 4-color very high-res graphics for EGA and VGA
         VC.Colors = 64
         IF BestMode = EGA64 THEN VC.Atribs = 4 ELSE VC.Atribs = 16
         VC.XPix = 639
         VC.YPix = 349
         VC.TCOL = 80
         VC.TROW = 25
      CASE 10  ' Two-color high-res graphics for EGA or VGA monochrome
         VC.Colors = 0
         VC.Atribs = 2
         VC.XPix = 319
         VC.YPix = 199
         VC.TCOL = 80
         VC.TROW = 25
      CASE 11  ' Two-color very high-res graphics for VGA and MCGA
         ' Note that for VGA screens 11, 12, and 13, more colors are
         ' available, depending on how the colors are mixed.
         VC.Colors = 216
         VC.Atribs = 2
         VC.XPix = 639
         VC.YPix = 479
         VC.TCOL = 80
         VC.TROW = 30
      CASE 12  ' 16-color very high-res graphics for VGA
         VC.Colors = 216
         VC.Atribs = 16
         VC.XPix = 639
         VC.YPix = 479
         VC.TCOL = 80
         VC.TROW = 30
      CASE 13  ' 256-color medium-res graphics for VGA and MCGA
         VC.Colors = 216
         VC.Atribs = 256
         VC.XPix = 639
         VC.YPix = 479
         VC.TCOL = 40
         VC.TROW = 25
      CASE ELSE
         VC.Colors = 16
         VC.Atribs = 16
         VC.XPix = 0
         VC.YPix = 0
         VC.TCOL = 80
         VC.TROW = 25
         VC.Scrn = 0
         EXIT SUB
   END SELECT
   VC.Scrn = mode

END SUB

' ============================ SetPalette ==============================
'   Mixes palette colors in an array.
' ======================================================================
'
SUB SetPalette STATIC
SHARED VC AS Config, Pal() AS LONG

   ' Mix only if the adapter supports color attributes
   IF VC.Colors THEN
      SELECT CASE VC.Scrn
         CASE 1, 2, 7, 8
            ' Red, green, blue, and intense in four bits of a byte
            ' Bits: 0000irgb
            ' Change the order of FOR loops to change color mix
            Index = 0
            FOR Bs = 0 TO 1
               FOR Gs = 0 TO 1
                  FOR Rs = 0 TO 1
                     FOR Hs = 0 TO 1
                        Pal(Index) = Hs * 8 + Rs * 4 + Gs * 2 + Bs
                        Index = Index + 1
                     NEXT
                  NEXT
               NEXT
            NEXT
         CASE 9
            ' EGA red, green, and blue colors in 6 bits of a byte
            ' Capital letters repesent intense, lowercase normal
            ' Bits:  00rgbRGB
            ' Change the order of FOR loops to change color mix
            Index = 0
            FOR Bs = 0 TO 1
               FOR Gs = 0 TO 1
                  FOR Rs = 0 TO 1
                     FOR HRs = 0 TO 1
                        FOR HGs = 0 TO 1
                           FOR HBs = 0 TO 1
                              Pal(Index) = Rs * 32 + Gs * 16 + Bs * 8 + HRs * 4 + HGs * 2 + HBs
                              Index = Index + 1
                           NEXT
                        NEXT
                     NEXT
                  NEXT
               NEXT
            NEXT
         CASE 11, 12, 13
            ' VGA colors in 6 bits of 3 bytes of a long integer
            ' Bits:  000000000 00bbbbbb 00gggggg 00rrrrrr
            ' Change the order of FOR loops to change color mix
            ' Decrease the STEP and increase VC.Colors to get more colors
            Index = 0
            FOR Rs = 0 TO 63 STEP 11
               FOR Bs = 0 TO 63 STEP 11
                  FOR Gs = 0 TO 63 STEP 11
                     Pal(Index) = (65536 * Bs) + (256 * Gs) + Rs
                     Index = Index + 1
                  NEXT
               NEXT
            NEXT
         CASE ELSE
      END SELECT
      ' Assign colors
      IF VC.Atribs > 2 THEN TorusRotate RNDM
   END IF

END SUB

' ============================ TileDraw ================================
'   Draw and optionally paint a tile. Tiles are painted if there are
'   more than two atributes and if the inside of the tile can be found.
' ======================================================================
'
SUB TileDraw (T AS Tile) STATIC
SHARED VC AS Config, TOR AS TORUS

   'Set border
   Border = VC.Atribs - 1

   IF VC.Atribs = 2 THEN
      ' Draw and quit for two-color modes
      LINE (T.x1, T.y1)-(T.x2, T.y2), T.TColor
      LINE -(T.x3, T.y3), T.TColor
      LINE -(T.x4, T.y4), T.TColor
      LINE -(T.x1, T.y1), T.TColor
      EXIT SUB
   ELSE
      ' For other modes, draw in the border color
      ' (which must be different than any tile color)
      LINE (T.x1, T.y1)-(T.x2, T.y2), Border
      LINE -(T.x3, T.y3), Border
      LINE -(T.x4, T.y4), Border
      LINE -(T.x1, T.y1), Border
   END IF

   ' See if tile is large enough to be painted
   IF Inside(T) THEN
      'Black out the center to make sure it isn't paint color
      PRESET (T.xc, T.yc)
      ' Paint tile black so colors of underlying tiles can't interfere
      PAINT STEP(0, 0), BACK, Border
      ' Fill with the final tile color.
      PAINT STEP(0, 0), T.TColor, Border
   END IF
 
   ' A border drawn with the background color looks like a border.
   ' One drawn with the tile color doesn't look like a border.
   IF TOR.Bord = "YES" THEN
      Border = BACK
   ELSE
      Border = T.TColor
   END IF

   ' Redraw with the final border
   LINE (T.x1, T.y1)-(T.x2, T.y2), Border
   LINE -(T.x3, T.y3), Border
   LINE -(T.x4, T.y4), Border
   LINE -(T.x1, T.y1), Border

END SUB

DEFSNG A-Z
' =========================== TorusCalc ================================
'   Calculates the x and y coordinates for each tile.
' ======================================================================
'
SUB TorusCalc (T() AS Tile) STATIC
SHARED TOR AS TORUS, Max AS INTEGER
DIM XSect AS INTEGER, YPanel AS INTEGER
  
   ' Calculate sine and cosine of the angles of rotation
   XRot = DegToRad(TOR.XDegree)
   YRot = DegToRad(TOR.YDegree)
   CXRot = COS(XRot)
   SXRot = SIN(XRot)
   CYRot = COS(YRot)
   SYRot = SIN(YRot)

   ' Calculate the angle to increment between one tile and the next.
   XInc = 2 * PI / TOR.Sect
   YInc = 2 * PI / TOR.Panel
  
   ' First calculate the first point, which will be used as a reference
   ' for future points. This point must be calculated separately because
   ' it is both the beginning and the end of the center seam.
   FirstY = (TOR.Thick + 1) * CYRot
                                 
   ' Starting point is x1 of 0 section, 0 panel     last     0
   T(0).x1 = FirstY                             ' +------+------+
   ' Also x2 of tile on last section, 0 panel   ' |      |      | last
   T(TOR.Sect - 1).x2 = FirstY                  ' |    x3|x4    |
   ' Also x3 of last section, last panel        ' +------+------+
   T(Max - 1).x3 = FirstY                       ' |    x2|x1    |  0
   ' Also x4 of 0 section, last panel           ' |      |      |
   T(Max - TOR.Sect).x4 = FirstY                ' +------+------+
   ' A similar pattern is used for assigning all points of Torus
  
   ' Starting Y point is 0 (center)
   T(0).y1 = 0
   T(TOR.Sect - 1).y2 = 0
   T(Max - 1).y3 = 0
   T(Max - TOR.Sect).y4 = 0
                          
   ' Only one z coordinate is used in sort, so other three can be ignored
   T(0).z1 = -(TOR.Thick + 1) * SYRot
  
   ' Starting at first point, work around the center seam of the Torus.
   ' Assign points for each section. The seam must be calculated separately
   ' because it is both beginning and of each section.
   FOR XSect = 1 TO TOR.Sect - 1
       
      ' X, Y, and Z elements of equation
      sx = (TOR.Thick + 1) * COS(XSect * XInc)
      sy = (TOR.Thick + 1) * SIN(XSect * XInc) * CXRot
      sz = (TOR.Thick + 1) * SIN(XSect * XInc) * SXRot
      ssx = (sz * SYRot) + (sx * CYRot)
  
      T(XSect).x1 = ssx
      T(XSect - 1).x2 = ssx
      T(Max - TOR.Sect + XSect - 1).x3 = ssx
      T(Max - TOR.Sect + XSect).x4 = ssx
                                         
      T(XSect).y1 = sy
      T(XSect - 1).y2 = sy
      T(Max - TOR.Sect + XSect - 1).y3 = sy
      T(Max - TOR.Sect + XSect).y4 = sy
                                         
      T(XSect).z1 = (sz * CYRot) - (sx * SYRot)
   NEXT
  
   ' Now start at the first seam between panel and assign points for
   ' each section of each panel. The outer loop assigns the initial
   ' point for the panel. This point must be calculated separately
   ' since it is both the beginning and the end of the seam of panels.
   FOR YPanel = 1 TO TOR.Panel - 1
        
      ' X, Y, and Z elements of equation
      sx = TOR.Thick + COS(YPanel * YInc)
      sy = -SIN(YPanel * YInc) * SXRot
      sz = SIN(YPanel * YInc) * CXRot
      ssx = (sz * SYRot) + (sx * CYRot)
       
      ' Assign X points for each panel
      ' Current ring, current side
      T(TOR.Sect * YPanel).x1 = ssx
      ' Current ring minus 1, next side
      T(TOR.Sect * (YPanel + 1) - 1).x2 = ssx
      ' Current ring minus 1, previous side
      T(TOR.Sect * YPanel - 1).x3 = ssx
      ' Current ring, previous side
      T(TOR.Sect * (YPanel - 1)).x4 = ssx
                                          
      ' Assign Y points for each panel
      T(TOR.Sect * YPanel).y1 = sy
      T(TOR.Sect * (YPanel + 1) - 1).y2 = sy
      T(TOR.Sect * YPanel - 1).y3 = sy
      T(TOR.Sect * (YPanel - 1)).y4 = sy
                                        
      ' Z point for each panel
      T(TOR.Sect * YPanel).z1 = (sz * CYRot) - (sx * SYRot)
       
      ' The inner loop assigns points for each ring (except the first)
      ' on the current side.
      FOR XSect = 1 TO TOR.Sect - 1
                                                 
         ' Display section and panel
         CountTiles XSect, YPanel
                                                            
         ty = (TOR.Thick + COS(YPanel * YInc)) * SIN(XSect * XInc)
         tz = SIN(YPanel * YInc)
         sx = (TOR.Thick + COS(YPanel * YInc)) * COS(XSect * XInc)
         sy = ty * CXRot - tz * SXRot
         sz = ty * SXRot + tz * CXRot
         ssx = (sz * SYRot) + (sx * CYRot)
          
         T(TOR.Sect * YPanel + XSect).x1 = ssx
         T(TOR.Sect * YPanel + XSect - 1).x2 = ssx
         T(TOR.Sect * (YPanel - 1) + XSect - 1).x3 = ssx
         T(TOR.Sect * (YPanel - 1) + XSect).x4 = ssx
                                                          
         T(TOR.Sect * YPanel + XSect).y1 = sy
         T(TOR.Sect * YPanel + XSect - 1).y2 = sy
         T(TOR.Sect * (YPanel - 1) + XSect - 1).y3 = sy
         T(TOR.Sect * (YPanel - 1) + XSect).y4 = sy
                                                            
         T(TOR.Sect * YPanel + XSect).z1 = (sz * CYRot) - (sx * SYRot)
      NEXT
   NEXT
   ' Erase message
   CountTiles -1, -1

END SUB

DEFINT A-Z
' =========================== TorusColor ===============================
'   Assigns color atributes to each tile.
' ======================================================================
'
SUB TorusColor (T() AS Tile) STATIC
SHARED VC AS Config, Max AS INTEGER
        
   ' Skip first and last atributes
   LastAtr = VC.Atribs - 2
   Atr = 1

   ' Cycle through each attribute until all tiles are done
   FOR Til = 0 TO Max - 1
      IF (Atr >= LastAtr) THEN
         Atr = 1
      ELSE
         Atr = Atr + 1
      END IF
      T(Til).TColor = Atr
   NEXT

END SUB

' ============================ TorusDefine =============================
'   Define the attributes of a Torus based on information from the
'   user, the video configuration, and the current screen mode.
' ======================================================================
'
SUB TorusDefine STATIC
SHARED VC AS Config, TOR AS TORUS, Available AS STRING

' Constants for key codes and column positions
CONST ENTER = 13, ESCAPE = 27
CONST DOWNARROW = 80, UPARROW = 72, LEFTARROW = 75, RIGHTARROW = 77
CONST COL1 = 20, COL2 = 50, ROW = 9

   ' Display key instructions
   LOCATE 1, COL1
   PRINT "UP .............. Move to next field"
   LOCATE 2, COL1
   PRINT "DOWN ........ Move to previous field"
   LOCATE 3, COL1
   PRINT "LEFT ......... Rotate field value up"
   LOCATE 4, COL1
   PRINT "RIGHT ...... Rotate field value down"
   LOCATE 5, COL1
   PRINT "ENTER .... Start with current values"
   LOCATE 6, COL1
   PRINT "ESCAPE .................. Quit Torus"

   ' Block cursor
   LOCATE ROW, COL1, 1, 1, 12
   ' Display fields
   LOCATE ROW, COL1: PRINT "Thickness";
   LOCATE ROW, COL2: PRINT USING "[ # ]"; TOR.Thick;
 
   LOCATE ROW + 2, COL1: PRINT "Panels per Section";
   LOCATE ROW + 2, COL2: PRINT USING "[ ## ]"; TOR.Panel;
  
   LOCATE ROW + 4, COL1: PRINT "Sections per Torus";
   LOCATE ROW + 4, COL2: PRINT USING "[ ## ]"; TOR.Sect;
 
   LOCATE ROW + 6, COL1: PRINT "Tilt around Horizontal Axis";
   LOCATE ROW + 6, COL2: PRINT USING "[ ### ]"; TOR.XDegree;
  
   LOCATE ROW + 8, COL1: PRINT "Tilt around Vertical Axis";
   LOCATE ROW + 8, COL2: PRINT USING "[ ### ]"; TOR.YDegree;
  
   LOCATE ROW + 10, COL1: PRINT "Tile Border";
   LOCATE ROW + 10, COL2: PRINT USING "[ & ] "; TOR.Bord;
 
   LOCATE ROW + 12, COL1: PRINT "Screen Mode";
   LOCATE ROW + 12, COL2: PRINT USING "[ ## ]"; VC.Scrn

   ' Skip field 10 if there's only one value
   IF LEN(Available$) = 1 THEN Fields = 10 ELSE Fields = 12
 
   ' Update field values and position based on keystrokes
   DO
      ' Put cursor on field
      LOCATE ROW + Fld, COL2 + 2
      ' Get a key and strip null off if it's an extended code
      DO
         K$ = INKEY$
      LOOP WHILE K$ = ""
      Ky = ASC(RIGHT$(K$, 1))

      SELECT CASE Ky
         CASE ESCAPE
            ' End program
            CLS : END
         CASE UPARROW, DOWNARROW
            ' Adjust field location
            IF Ky = DOWNARROW THEN Inc = 2 ELSE Inc = -2
            Fld = Rotated(0, Fields, Fld, Inc)
         CASE RIGHTARROW, LEFTARROW
            ' Adjust field
            IF Ky = RIGHTARROW THEN Inc = 1 ELSE Inc = -1
            SELECT CASE Fld
               CASE 0
                  ' Thickness
                  TOR.Thick = Rotated(1, 9, INT(TOR.Thick), Inc)
                  PRINT USING "#"; TOR.Thick
               CASE 2
                  ' Panels
                  TOR.Panel = Rotated(6, 20, TOR.Panel, Inc)
                  PRINT USING "##"; TOR.Panel
               CASE 4
                  ' Sections
                  TOR.Sect = Rotated(6, 20, TOR.Sect, Inc)
                  PRINT USING "##"; TOR.Sect
               CASE 6
                  ' Horizontal tilt
                  TOR.XDegree = Rotated(0, 345, TOR.XDegree, (15 * Inc))
                  PRINT USING "###"; TOR.XDegree
               CASE 8
                  ' Vertical tilt
                  TOR.YDegree = Rotated(0, 345, TOR.YDegree, (15 * Inc))
                  PRINT USING "###"; TOR.YDegree
               CASE 10
                  ' Border
                  IF VC.Atribs > 2 THEN
                     IF TOR.Bord = "YES" THEN
                        TOR.Bord = "NO"
                     ELSE
                        TOR.Bord = "YES"
                     END IF
                  END IF
                  PRINT TOR.Bord
               CASE 12
                  ' Available screen modes
                  I = INSTR(Available$, HEX$(VC.Scrn))
                  I = Rotated(1, LEN(Available$), I, Inc)
                  VC.Scrn = VAL("&h" + MID$(Available$, I, 1))
                  PRINT USING "##"; VC.Scrn
               CASE ELSE
            END SELECT
         CASE ELSE
      END SELECT
   ' Set configuration data for graphics mode
   SetConfig VC.Scrn
   ' Draw Torus if ENTER
   LOOP UNTIL Ky = ENTER
 
   ' Remove cursor
   LOCATE 1, 1, 0
 
   ' Set different delays depending on mode
   SELECT CASE VC.Scrn
      CASE 1
         TOR.Delay = .3
      CASE 2, 3, 10, 11, 13
         TOR.Delay = 0
      CASE ELSE
         TOR.Delay = .05
   END SELECT
 
   ' Get new random seed for this torus
   RANDOMIZE TIMER

END SUB

' =========================== TorusDraw ================================
'   Draws each tile of the torus starting with the farthest and working
'   to the closest. Thus nearer tiles overwrite farther tiles to give
'   a three-dimensional effect. Notice that the index of the tile being
'   drawn is actually the index of an array of indexes. This is because
'   the array of tiles is not sorted, but the parallel array of indexes
'   is. See TorusSort for an explanation of how indexes are sorted.
' ======================================================================
'
SUB TorusDraw (T() AS Tile, Index() AS INTEGER)
SHARED Max AS INTEGER

   FOR Til = 0 TO Max - 1
      TileDraw T(Index(Til))
   NEXT

END SUB

' =========================== TorusRotate ==============================
'   Rotates the Torus. This can be done more successfully in some modes
'   than in others. There are three methods:
'
'     1. Rotate the palette colors assigned to each attribute
'     2. Draw, erase, and redraw the torus (two-color modes)
'     3. Rotate between two palettes (CGA and MCGA screen 1)
'
'   Note that for EGA and VGA screen 2, methods 1 and 2 are both used.
' ======================================================================
'
SUB TorusRotate (First) STATIC
SHARED VC AS Config, TOR AS TORUS, Pal() AS LONG, Max AS INTEGER
SHARED T() AS Tile, Index() AS INTEGER, BestMode AS INTEGER
DIM Temp AS LONG

   ' For EGA and higher rotate colors through palette
   IF VC.Colors THEN

      ' Argument determines whether to start at next color, first color,
      ' or random color
      SELECT CASE First
         CASE RNDM
            FirstClr = INT(RND * VC.Colors)
         CASE START
            FirstClr = 0
         CASE ELSE
            FirstClr = FirstClr - 1
      END SELECT
       
      ' Set last color to smaller of last possible color or last tile
      IF VC.Colors > Max - 1 THEN
         LastClr = Max - 1
      ELSE
         LastClr = VC.Colors - 1
      END IF
   
      ' If color is too low, rotate to end
      IF FirstClr < 0 OR FirstClr >= LastClr THEN FirstClr = LastClr

      ' Set last attribute
      IF VC.Atribs = 2 THEN
         ' Last for two-color modes
         LastAtr = VC.Atribs - 1
      ELSE
         ' Smaller of last color or next-to-last attribute
         IF LastClr < VC.Atribs - 2 THEN
            LastAtr = LastClr
         ELSE
            LastAtr = VC.Atribs - 2
         END IF
      END IF

      ' Cycle through attributes, assigning colors
      Work = FirstClr
      FOR Atr = LastAtr TO 1 STEP -1
         PALETTE Atr, Pal(Work)
         Work = Work - 1
         IF Work < 0 THEN Work = LastClr
      NEXT

   END IF

   ' For two-color screens, the best we can do is erase and redraw the torus
   IF VC.Atribs = 2 THEN
  
      ' Set all tiles to color
      FOR I = 0 TO Max - 1
         T(I).TColor = Toggle
      NEXT
      ' Draw Torus
      TorusDraw T(), Index()
      ' Toggle between color and background
      Toggle = (Toggle + 1) MOD 2

   END IF

   ' For CGA or MCGA screen 1, toggle palettes using the COLOR statement
   ' (these modes do not allow the PALETTE statement)
   IF VC.Scrn = 1 AND (BestMode = CGA OR BestMode = MCGA) THEN
      COLOR , Toggle
      Toggle = (Toggle + 1) MOD 2
      EXIT SUB
   END IF
       
END SUB

' ============================ TorusSort ===============================
'   Sorts the tiles of the Torus according to their Z axis (distance
'   from the "front" of the screen). When the tiles are drawn, the
'   farthest will be drawn first, and nearer tiles will overwrite them
'   to give a three-dimensional effect.
'
'   To make sorting as fast as possible, the Quick Sort algorithm is
'   used. Also, the array of tiles is not actually sorted. Instead a
'   parallel array of tile indexes is sorted. This complicates things,
'   but makes the sort much faster, since two-byte integers are swapped
'   instead of 46-byte Tile variables.
' ======================================================================
'
SUB TorusSort (Low, High)
SHARED T() AS Tile, Index() AS INTEGER
DIM Partition AS SINGLE

   IF Low < High THEN
      ' If only one, compare and swap if necessary
      ' The SUB procedure only stops recursing when it reaches this point
      IF High - Low = 1 THEN
         IF T(Index(Low)).z1 > T(Index(High)).z1 THEN
            CountTiles High, Low
            SWAP Index(Low), Index(High)
         END IF
      ELSE
      ' If more than one, separate into two random groups
         RandIndex = INT(RND * (High - Low + 1)) + Low
         CountTiles High, Low
         SWAP Index(High), Index(RandIndex%)
         Partition = T(Index(High)).z1
         ' Sort one group
         DO
            I = Low: J = High
            ' Find the largest
            DO WHILE (I < J) AND (T(Index(I)).z1 <= Partition)
               I = I + 1
            LOOP
            ' Find the smallest
            DO WHILE (J > I) AND (T(Index(J)).z1 >= Partition)
               J = J - 1
            LOOP
            ' Swap them if necessary
            IF I < J THEN
               CountTiles High, Low
               SWAP Index(I), Index(J)
            END IF
         LOOP WHILE I < J
       
         ' Now get the other group and recursively sort it
         CountTiles High, Low
         SWAP Index(I), Index(High)
         IF (I - Low) < (High - I) THEN
            TorusSort Low, I - 1
            TorusSort I + 1, High
         ELSE
            TorusSort I + 1, High
            TorusSort Low, I - 1
         END IF
      END IF
   END IF

END SUB

