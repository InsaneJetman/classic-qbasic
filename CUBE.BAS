' Define the macro string used to draw the cube
' and paint its sides:
One$ =	"BR30 BU25 C1 R54 U45 L54 D45 BE20 P1,1	G20 C2 G20"
Two$ =	"R54 E20 L54 BD5 P2,2 U5 C4 G20 U45 E20 D45 BL5 P4,4"
Plot$ = One$ + Two$

APage% = 1	' Initialize values for the active and visual
VPage% = 0	' pages as well as the angle of rotation.
Angle% = 0

DO
   SCREEN 7, , APage%, VPage% ' Draw to the active page
   			      ' while showing the visual page.

   CLS 1		      ' Clear the active page.

   ' Rotate the	cube "Angle%" degrees:
   DRAW	"TA" + STR$(Angle%) + Plot$

   ' Angle% is some multiple of	15 degrees:
   Angle% = (Angle% + 15) MOD 360

   ' Drawing is complete, so make the cube visible in its
   ' new position by switching the active and visual pages:
   SWAP	APage%,	VPage%

LOOP WHILE INKEY$ = ""	      ' A keystroke ends the program.

END

