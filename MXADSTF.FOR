C ******************** ADDSTRING  *********************
C This program is in file MXADSTF.FOR
C Declare interface to Stringassign subprogram. The pointer fields are
C declared INTEGER*4, so that different types of far pointers can be
C passed without conflict. The INTEGER*4 fields are essentially generic
C pointers. [VALUE] must be specified, or FORTRAN will pass pointers to
C pointers. INTEGER*2 also passed by [VALUE], to be consistent with
C declaration of Stringassign.
C
       INTERFACE TO SUBROUTINE STRASG [ALIAS:'STRINGASSIGN'] (S,SL,D,DL)
       INTEGER*4 S [VALUE]
       INTEGER*2 SL [VALUE]
       INTEGER*4 D [VALUE]
       INTEGER*2 DL [VALUE]
       END
C
C Declare heading of Addstring function in the same way as above: the
C pointer fields are INTEGER*4
C
       INTEGER*2 FUNCTION ADDSTR [ALIAS:'ADDSTRING'] (S1,S1LEN,S2,S2LEN)
       INTEGER*4 S1 [VALUE]
       INTEGER*2 S1LEN [VALUE]
       INTEGER*4 S2 [VALUE]
       INTEGER*2 S2LEN [VALUE]
C
C Local parameters TS1, TS2, and BIGSTR are temporary strings. STRDES is
C a four-byte object into which Stringassign will put BASIC string
C descriptor.
C
       CHARACTER*50 TS1, TS2
       CHARACTER*100 BIGSTR
       INTEGER*4 STRDES

	TS1 = " "
	TS2 = " "
	STRDES = 0

C
C Use the LOCFAR function to take the far address of data. LOCFAR returns
C a value of type INTEGER*4.
C
       CALL STRASG (S1, 0, LOCFAR(TS1), S1LEN)
       CALL STRASG (S2, 0, LOCFAR(TS2), S2LEN)
       BIGSTR = TS1(1:S1LEN) // TS2(1:S2LEN)
       CALL STRASG (LOCFAR(BIGSTR), S1LEN+S2LEN, LOCFAR(STRDES), 0)
       ADDSTR = LOC(STRDES)
       RETURN
       END
