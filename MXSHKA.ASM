;*************************** SHAKESPEARE ******************************
; This program creates two strings and passes them to a BASIC procedure
; called addstring (in file MXADSTB.BAS).  This procedure concatenates
; the strings and passes the result to MASM which prints it.

	.model	medium,basic	    ;Use same memory model as BASIC.
	extrn	ADDSTRING:far
        .stack
        .data                       ;Create the data.
phrase1         db      "To be or not to be;"
phrase1len      dw      $-phrase1
phrase1off      dw      phrase1
phrase2         db      " that is the question."
phrase2len      dw      $-phrase2
phrase2off      dw      phrase2
sentence        db      100 dup(0)  ;Make room for return data
sentencelen     dw      0           ;and a length indicator.
sentenceoff     dw      sentence

        .code
SHAKESPEARE proc	 uses si

;First call BASIC to concatenate strings.
        lea     ax,phrase1off       ;Push far address of
        push    ax                  ;fixed-length string #1,
        lea     ax,phrase1len       ;and its length.
        push    ax
        lea     ax,phrase2off       ;Do the same for the
        push    ax                  ;address of string #2,
        lea     ax,phrase2len       ;and its length.
        push    ax
        lea     ax,sentenceoff      ;Push far address of
        push    ax                  ;the return string,
        lea     ax,sentencelen      ;and its length.
        push    ax
	call	ADDSTRING	    ;Call BASIC function to
				    ;concatenate the strings and
                                    ;put the result in the
                                    ;fixed-length return string.

;Call DOS to print string. The DOS string output routine (09H)
;requires that strings end with a "$" character.
        mov     bx,sentencelen      ;Go to end of the result string
        lea     si,sentence         ;and add a "$" (24h) character.
        mov     byte ptr [bx + si],24h

        lea     dx,sentence         ;Set up registers
        mov     ah,9                ;and call DOS to
        int     21h                 ;print result string.
        ret

SHAKESPEARE endp

        end
