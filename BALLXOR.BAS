DECLARE FUNCTION GetArraySize (WLeft, WRight, WTop, WBottom)

SCREEN 2

' Define a viewport and draw a border around it:
VIEW (20, 10)-(620, 190), , 1

CONST PI = 3.141592653589#

' Redefine the coordinates of the viewport with view
' coordinates:
WINDOW (-3.15, -.14)-(3.56, 1.01)

' Arrays in program are now dynamic:
' $DYNAMIC

' Calculate the view coordinates for the top and bottom of a
' rectangle large enough to hold the image that will be
' drawn with CIRCLE and PAINT:
WLeft = -.18
WRight = .18
WTop = .05
WBottom = -.05

' Call the GetArraySize function,
' passing it the rectangle's view coordinates:
ArraySize% = GetArraySize(WLeft, WRight, WTop, WBottom)

DIM Array(1 TO ArraySize%)  AS INTEGER

' Draw and paint the circle:
CIRCLE (0, 0), .18
PAINT (0, 0)

' Store the rectangle in Array:
GET (WLeft, WTop)-(WRight, WBottom), Array
CLS
' Draw a box and fill it with a pattern:
LINE (-3, .8)-(3.4, .2), , B
Pattern$ = CHR$(126) + CHR$(0) + CHR$(126) + CHR$(126)
PAINT (0, .5), Pattern$

LOCATE 21, 29
PRINT "Press any key to end."

' Initialize loop variables:
StepSize = .02
StartLoop = -PI
Decay = 1

DO
   EndLoop = -StartLoop
   FOR X = StartLoop TO EndLoop STEP StepSize
      Y = ABS(COS(X)) * Decay - .14

      ' The first PUT statement places the image
      ' on the screen:
      PUT (X, Y), Array, XOR

      ' Use an empty FOR...NEXT loop to delay
      ' the program and reduce image flicker:
      FOR I = 1 TO 5: NEXT I

      IF Y < -.13 THEN Decay = Decay * .9
      Esc$ = INKEY$
      IF Esc$ <> "" OR Decay < .01 THEN EXIT FOR

      ' The second PUT statement erases the image and
      ' restores the background:
      PUT (X, Y), Array, XOR
   NEXT X

   StepSize = -StepSize
   StartLoop = -StartLoop
LOOP UNTIL Esc$ <> "" OR Decay < .01

END

FUNCTION GetArraySize (WLeft, WRight, WTop, WBottom) STATIC

   ' Map the view coordinates passed to this function to
   ' their physical-coordinate equivalents:
   VLeft = PMAP(WLeft, 0)
   VRight = PMAP(WRight, 0)
   VTop = PMAP(WTop, 1)
   VBottom = PMAP(WBottom, 1)
' Calculate the height and width in pixels
   ' of the enclosing rectangle:
   RectHeight = ABS(VBottom - VTop) + 1
   RectWidth = ABS(VRight - VLeft) + 1

   ' Calculate size in bytes of array:
   ByteSize = 4 + RectHeight * INT((RectWidth + 7) / 8)

   ' Array is integer, so divide bytes by two:
   GetArraySize = ByteSize \ 2 + 1
END FUNCTION

