{ ************************ SHAKESPEARE ******************
  This program is in file MXSHKP.PAS }

MODULE MPAS;
TYPE
    ADRCHAR = ADR OF CHAR;
VAR
    S1, S2, S3 : LSTRING (100);
    S1LEN, S2LEN, S3LEN : INTEGER;
    TMP1, TMP2, TMP3 : ADRCHAR;
{ Declare interface to procedure ADDSTRING, which concatenates first
  two strings passed and places the result in the third string
  passed. }
PROCEDURE ADDSTRING (VAR TMP1:ADRCHAR; VAR STR1LEN:INTEGER;
		     VAR TMP2:ADRCHAR; VAR STR2LEN:INTEGER;
		     VAR TMP3:ADRCHAR; VAR STR3LEN:INTEGER ); EXTERN;
{ Procedure Shakespeare declares two strings, calls Basic procedure
  AddString to concatenate them, then prints results. With LSTRING
  type, not that element 0 contains length byte. String data starts
  with element 1. }
PROCEDURE SHAKESPEARE;
    BEGIN
	S1:='To be or not to be;';
	S1LEN:=ORD(S1[0]);
	S2:=' that is the question.';
	S2LEN:=ORD(S2[0]);
	TMP1:=ADR(S1[1]);
	TMP2:=ADR(S2[1]);
	TMP3:=ADR(S3[1]);
	ADDSTRING (TMP1, S1LEN, TMP2, S2LEN, TMP3, S3LEN);
	S3[0]:=CHR(S3LEN);
	WRITELN(S3);
    END;
END.

