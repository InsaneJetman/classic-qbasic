DECLARE SUB Adapter ()
DEFINT A-Z

Adapter

SUB Adapter
ON LOCAL ERROR GOTO Handler
CONST False = 0, True = NOT False
' Use an array to keep track of our test results
DIM Mode(1 TO 13)

' Try screen modes and see which work.
FOR ModeNumber = 1 TO 13
	' Assume the test works unless you get an error.
	Mode(ModeNumber) = True
	SCREEN ModeNumber
NEXT ModeNumber

' Reset the screen after testing it.
SCREEN 0, 0
WIDTH 80

' Using test results figure out which adapter is out there.
' Tell operator which one he has.
' (See tables in SCREEN section of BASIC Language Reference
' to undertand why this logic works.)
PRINT "You have a ";
IF Mode(13) THEN
	IF Mode(7) THEN
		PRINT "VGA";
		UseableModes$ = "0-2, 7-13."
	ELSE
		PRINT "MCGA";
		UseableModes$ = "0-2, 11 and 13."
	END IF
ELSE
	IF Mode(7) THEN
		PRINT "EGA";
		UseableModes$ = "0-2, 7-10."
	ELSE
		IF Mode(3) THEN
			PRINT "Hercules";
			UseableModes$ = "3."
		END IF
		IF Mode(4) THEN
			PRINT "Olivetti";
			UseableModes$ = "4."
		END IF
		IF Mode(1) THEN
			PRINT "CGA";
			UseableModes$ = "0-2."
		ELSE
			PRINT "MDPA";
			UseableMode$ = "0."
		END IF
	END IF
END IF
PRINT " Graphics card that supports screen mode(s) "; UseableModes$
EXIT SUB

' Branch here when test fails and change text result.
Handler:
	Mode(ModeNumber) = False
	RESUME NEXT
END SUB

