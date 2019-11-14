C *********************** SHAKESPEARE ****************
C This program is in file MXSHKF.FOR
C Declare interface to BASIC routine ADDSTRING.
C All parameters must be passed NEAR, for compatibility with BASIC's
C conventions.
C


       INTERFACE TO SUBROUTINE ADDSTR[ALIAS:'ADDSTRING']
     * (S1,L1,S2,L2,S3,L3)
       INTEGER*2 S1 [NEAR]
       INTEGER*2 L1 [NEAR]
       INTEGER*2 S2 [NEAR]
       INTEGER*2 L2 [NEAR]
       INTEGER*2 S3 [NEAR]
       INTEGER*2 L3 [NEAR]
       END
C
C Declare subroutine SHAKESPEARE, which declares two strings, calls BASIC
C subroutine ADDSTRING, and prints the result.
C
       SUBROUTINE SHAKES [ALIAS:'SHAKESPEARE']
       CHARACTER*50 STR1, STR2
       CHARACTER*100 STR3
       INTEGER*2 STRLEN1, STRLEN2, STRLEN3
       INTEGER*2 TMP1, TMP2, TMP3
C
C The subroutine uses FORTRAN LEN_TRIM function, which returns the length
C of string, excluding trailing blanks. (All FORTRAN strings are initialized
C to blanks.)
C
       STR1 = 'To be or not to be;'
       STRLEN1 = LEN_TRIM(STR1)
       STR2 = ' that is the question.'
       STRLEN2 = LEN_TRIM(STR2)
       TMP1 = LOC(STR1)
       TMP2 = LOC(STR2)
       TMP3 = LOC(STR3)
       CALL ADDSTR (TMP1, STRLEN1, TMP2, STRLEN2, TMP3, STRLEN3)
       WRITE (*,*) STR3
       END

