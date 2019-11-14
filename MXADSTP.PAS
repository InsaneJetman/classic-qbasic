{ **********************ADDSTRING ***********************
  This program is in file MXADSTP.PAS  }

{ Module MXADSTP--takes address and lengths of two BASIC
  strings, concatenates, and creates a BASIC string descriptor. }

MODULE MAXADSTP;
{ Declare type ADSCHAR for all pointer types. For ease of programming,
  all address variables in this module are considered pointers to
  characters, and all strings and string descriptors are considered
  arrays of characters. Also, declare the BASIC string descriptor
  type as a simple array of four characters. }

TYPE
    ADSCHAR = ADS OF CHAR;
    ADRCHAR = ADR OF CHAR;
    STRDESC = ARRAY[0..3] OF CHAR;
VAR
    MYDESC : STRDESC;
{ Interface to procedure BASIC routine StringAssign. If source
  string is a fixed-length string, S points to string data and SL
  gives length. If source string is a BASIC variable-length string,
  S points to a BASIC string descriptor and SL is 0. Similarly for
  destination string, D and DL. }
PROCEDURE STRINGASSIGN (S:ADSCHAR; SL:INTEGER;
			D:ADSCHAR; DL:INTEGER ); EXTERN;

FUNCTION ADDSTRING (S1:ADSCHAR; S1LEN:INTEGER;
		    S2:ADSCHAR; S2LEN:INTEGER) : ADRCHAR;

    VAR
	BIGSTR : ARRAY[0..99] OF CHAR;
{ Execute function by copying S1 to the array BIGSTR, appending S2
  to the end, and then copying combined data to the string descriptor. }

    BEGIN
	STRINGASSIGN (S1, 0, ADS BIGSTR[0], S1LEN);
	STRINGASSIGN (S2, 0, ADS BIGSTR[S1LEN], S2LEN);
	STRINGASSIGN (ADS BIGSTR[0], S1LEN+S2LEN, ADS MYDESC[0], 0);
	ADDSTRING := ADR MYDESC;
    END;  { End Addstring function,}
END.  {End module.}


