' Declare external MASM procedures.
DECLARE SUB SetInt
DECLARE SUB RestInt

' Install new interrupt service routine.
CALL SetInt

' Set up the BASIC event handler.
ON UEVENT GOSUB SpecialTask
UEVENT ON

DO
' Normal program operation occurs here.
' Program ends when any key is pressed.
LOOP UNTIL INKEY$ <> ""

' Restore old interrupt service routine before quitting.
CALL RestInt

END

' Program branches here every 4.5 seconds.
SpecialTask:
' Code for performing the special task goes here, for example:
PRINT "Arrived here after 4.5 seconds."
RETURN

