'Define arrays which will be passed to each new level
'       of recursion.
DECLARE SUB BigStrings (n%, s1$(), s2$(), s3$(), s4$())
DEFINT A-Z
DIM s1$(1 TO 2), s2$(1 TO 2), s3$(1 TO 2), s4$(1 TO 2)
' Compute the # of 64K blocks available in far memory.
n = FRE(-1) \ 65536
CLS
'Quit if not enough memory.
IF n < 1 THEN
             PRINT "Not enough memory for operation."
             END
END IF

' Start the recursion.
CALL BigStrings(n, s1$(), s2$(), s3$(), s4$())

SUB BigStrings (n, s1$(), s2$(), s3$(), s4$())
' Create a new array (up to 64K) for each level of recursion.
DIM a$(1 TO 2)
' Have n keep track of recursion level.
SELECT CASE n
' When at highest recusion level, process the strings.
        CASE 0
                PRINT s1$(1); s1$(2); s2$(1); s2$(2); s3$(1); s3$(2); s4$(1); s4$(2)
        CASE 1
                a$(1) = "Each "
                a$(2) = "word "
                s1$(1) = a$(1)
                s1$(2) = a$(2)
        CASE 2
                a$(1) = "pair "
                a$(2) = "comes "
                s2$(1) = a$(1)
                s2$(2) = a$(2)
        CASE 3
                a$(1) = "from "
                a$(2) = "separate "
                s3$(1) = a$(1)
                s3$(2) = a$(2)
        CASE 4
                a$(1) = "recursive "
                a$(2) = "procedures."
                s4$(1) = a$(1)
                s4$(2) = a$(2)
END SELECT

' Keep going until we're out of memory.
IF n > 0 THEN
                n = n - 1
' For each recursion, pass in previously created arrays.
                CALL BigStrings(n, s1$(), s2$(), s3$(), s4$())
END IF

END SUB


