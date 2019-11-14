DEFINT A-Z

'Start program in BASIC for proper initialization.
' Define external and internal procedures.
DECLARE SUB shakespeare ()
DECLARE SUB StringAssign (BYVAL srcsegment, BYVAL srcoffset, BYVAL srclen, BYVAL destsegment, BYVAL destoffset, BYVAL destlen)
DECLARE SUB addstring (instrg1off, instrg1len, instrg2off, instrg2len, outstrgoff, outstrglen)
DECLARE SUB StringRelease (s$)

'Go to main routine in second language
CALL shakespeare

'The non-BASIC program calls this SUB to add the two strings together
SUB addstring (instrg1off, instrg1len, instrg2off, instrg2len, outstrgoff, outstrglen)

'Create variable-length strings and transfer non-BASIC fixed strings to them.
'Use VARSEG() to compute the segement of the strings returned from the other
'language--this is the DGROUP segment, and all string descriptors are found
'in this segment (even though the far string itself is elsewhere).

CALL StringAssign(VARSEG(a$), instrg1off, instrg1len, VARSEG(a$), VARPTR(a$), 0)
CALL StringAssign(VARSEG(b$), instrg2off, instrg2len, VARSEG(b$), VARPTR(b$), 0)

' Process the strings--in this case, add them.
c$ = a$ + b$

' Calculate the new output length.
outstrglen = LEN(c$)

' Transfer string output to a non-BASIC fixed-length string.
CALL StringAssign(VARSEG(c$), VARPTR(c$), 0, VARSEG(c$), outstrgoff, outstrglen)

END SUB

