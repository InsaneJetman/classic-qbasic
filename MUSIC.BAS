' Turn on trapping of background music events:
PLAY ON

' Branch to the Refresh subroutine when there are fewer than
' two notes in the background music buffer:
ON PLAY(2) GOSUB Refresh

PRINT "Press any key to start, q to end."
Pause$ = INPUT$(1)

' Select the background music option for PLAY:
PLAY "MB"

' Start playing the music, so notes will be put in the
' background music buffer:
GOSUB Refresh

I = 0

DO

	' Print the numbers from 0 to 10,000 over and over until
	' the user presses the "q" key. While this is happening,
	' the music will repeat in the background:
	PRINT I
	I = (I + 1) MOD 10001
LOOP UNTIL INKEY$ = "q"

END

Refresh:

	' Plays the opening motive of
	' Beethoven's Fifth Symphony:
	Listen$ = "t180 o2 p2 p8 L8 GGG L2 E-"
	Fate$   = "p24 p8 L8 FFF L2 D"
	PLAY Listen$ + Fate$
	RETURN

