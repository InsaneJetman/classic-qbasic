DECLARE FUNCTION CertifiedOperator% ()
CONST FALSE = 0, True = NOT FALSE

IF CertifiedOperator = FALSE THEN
	PRINT "Connection Refused."
	END
END IF

PRINT "Connected to Network."
'Main program continues here.
'  .
'  .
'  .
END

FUNCTION CertifiedOperator%
ON LOCAL ERROR GOTO Handler
'Count the number of times the operator tries to sign on.
Attempts% = 0

TryAgain:
'Assume the operator has valid credentials.
CertifiedOperator = True
'Keep track of bad entries.
Attempts% = Attempts% + 1
IF Attempts% > 3 THEN ERROR 255
'Check out the operator's credentials.
INPUT "Enter Account Number"; Account$
IF LEFT$(Account$, 4) <> "1234" THEN ERROR 200
INPUT "Enter Password"; Password$
IF Password$ <> "Swordfish" THEN ERROR 201
EXIT FUNCTION

Handler:
SELECT CASE ERR
    'Start over if account number doesn't have "1234" in it.
	CASE 200
		PRINT "Illegal account number. Please re-enter."
		RESUME TryAgain
    'Start over if the password is wrong.
	CASE 201
		PRINT "Wrong password. Please re-enter both items."
		RESUME TryAgain
    'Return false if operator makes too many mistakes.
	CASE 255
		CertifiedOperator% = FALSE
		EXIT FUNCTION
END SELECT

END FUNCTION
