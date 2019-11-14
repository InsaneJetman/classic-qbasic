DEFINT A-Z
'Define non-basic procedures
DECLARE FUNCTION addstring$(SEG s1$, BYVAL s1length, SEG s2$, BYVAL s2length)


'Create the data
a$ = "To be or not to be;"
b$ = " that is the question."

'Use non-BASIC function to add two BASIC far strings
c$ = addstring(a$, LEN(a$), b$, LEN(b$))

'print the result on the screen

PRINT c$
