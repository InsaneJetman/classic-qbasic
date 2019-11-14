;***************************** ADDSTRING ********************************
; This procedure accepts two far strings, concatenates them, and
; returns the result in the form of a far string.

        .model  medium,basic        ;Define memory model to match BASIC.
	extrn	STRINGASSIGN:far
	.stack
        .data?
        maxst = 50                  ;Maximum bytes reserved for strings
inbuffer1       db  maxst dup(0)    ;Room for first fixed-length string
inbuffer2       db  maxst dup(0)    ;and second one
outbuffer       db  2*maxst dup(0)  ;Work area for string processing
        .data
sh              dd  0               ;Output string descriptor
        .code
ADDSTRING	proc	uses si di ds, s1:far ptr, s1len, s2:far ptr, s2len

;First get BASIC to convert BASIC strings into standard form.
        les     ax,s1               ;Push far pointer to
        push    es                  ;input string descriptor.
        push    ax
        xor     ax,ax               ;Push a zero to indicate
        push    ax                  ;it is variable length.
        push    ds                  ;Push far pointer to
        lea     ax, inbuffer1       ;destination string.
        push    ax
        mov     ax,maxst            ;Push length of destination
        push    ax                  ;fixed-length string.
	call	STRINGASSIGN	    ;Call BASIC to assign variable-length
                                    ;string to fixed-length string.
        les     ax,s2               ;Push far pointer to second
        push    es                  ;input string descriptor.
        push    ax
        xor     ax,ax               ;Push a zero to indicate
        push    ax                  ;it is variable length.
        push    ds                  ;Push far pointer to
        lea     ax,inbuffer2        ;second destination string.
        push    ax
        mov     ax,maxst            ;Push length of destination
        push    ax                  ;fixed-length string.
	call	STRINGASSIGN	    ;Call BASIC to assign variable-length
                                    ;string to fixed-length string.
;Concatenate strings.
        lea     si,inbuffer1        ;Copy first string to buffer.
        lea     di,outbuffer
        mov     ax,ds
        mov     es,ax
        mov     cx,s1len
        rep     movsb
        lea     si,inbuffer2        ;Concatenate second string to
        mov     cx,s2len            ;end of first.
        rep     movsb

;Get BASIC to convert result back into a BASIC string.
        push    ds                  ;Push far pointer to fixed-length
        lea     ax,outbuffer        ;result string.
        push    ax
        mov     ax,s1len            ;Compute total length of
        mov     bx,s2len            ;fixed-length result string.
        add     ax,bx
        push    ax                  ;Push length.
        push    ds                  ;Push far pointer to sh (BASIC
        lea     ax,sh               ;will use this in StringAssign).
        push    ax
        xor     ax,ax               ;Push a zero for length
        push    ax                  ;indicating variable-length.
	call	STRINGASSIGN	;Call BASIC to assign the
                                    ;result to sh.
        lea     ax,sh               ;Return output string pointer
                                    ;in ax and go back to BASIC.
        ret

ADDSTRING	endp
                end
