;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
;  UIASM.ASM
;
;  Copyright (C) 1989-1990 Microsoft Corporation, All Rights Reserved
;
;  GetCopyBox : Gets screen box info and places into string variable
;  PutCopyBox : Puts screen box info from string variable onto screen
;  AttrBox    : Changes the color attributes of all characters within a box
;
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

;NOTE: For optimum speed, these routines write directly to screen memory
;      without waiting for re-trace.  If "snow" is a problem, these routines
;      will need modification.

.model medium

        extrn   STRINGADDRESS:far       ;BASIC RTL entry point for string info

.data

attr    db      ?                       ;destination attribute (AttrBox)
x0      db      ?                       ;x coord of upper-left
y0      db      ?                       ;y coord of upper-left
x1      db      ?                       ;x coord of lower-right
y1      db      ?                       ;y coord of lower-right
bwidth  db      ?                       ;box width
height  db      ?                       ;box height
strdoff dw      ?                       ;string pointer offset
strdseg dw      ?                       ;string pointer segment
scrseg  dw      ?                       ;screen segment
movword dw      ?                       ;word count to move/change

.code

;---------------------------------------place segment of screen memory
;---------------------------------------in SCRSEG
get_scrseg      proc

        push    ax                      ;save value of AX
        mov     ah,0Fh
        int     10h                     ;INT 10H fn. 0Fh - Get Video Mode
        mov     dgroup:scrseg,0B800h    ;assume COLOR screen for now
        cmp     al,07h                  ;is it MONOCHROME mode?
        jne     arnd1
        mov     dgroup:scrseg,0B000h    ;yes, set for mono screen seg
arnd1:  pop     ax                      ;restore AX
        ret                             ;and exit

get_scrseg      endp


;----------------------------------------Given X and Y in AH and AL, find
;----------------------------------------the offset into screen memory and
;----------------------------------------return in AX
get_memxy       proc

        push    dx                      ;save DX
        push    ax                      ;save coords
        mov     dl,160
        mul     dl                      ;multiply Y by 160
        pop     dx                      ;put coords in DX
        mov     dl,dh
        mov     dh,0
        add     dl,dl                   ;double X
        add     ax,dx                   ;and add to mult. result for final!
        pop     dx                      ;restore DX
        ret

get_memxy       endp


;-----------------------------------------------------------------------------
;----------------------------------------This is the routine that copies
;----------------------------------------screen info to the string variable
;-----------------------------------------------------------------------------
        public  GETCOPYBOX
getcopybox      proc    far

        push    bp
        mov     bp,sp
        push    ds
        push    es
        push    si
        push    di                      ;preserve registers

get_start:
        mov     bx,[bp + 14]            ;get y0
        mov     ax,[bx]
        mov     y0,al
        mov     bx,[bp + 12]            ;...x0
        mov     ax,[bx]
        mov     x0,al
        mov     bx,[bp + 10]            ;...y1
        mov     ax,[bx]
        mov     y1,al
        mov     bx,[bp + 8]             ;...x1
        mov     ax,[bx]
        mov     x1,al
        mov     bx,[bp + 6]             ;...and the destination str desc.

        push    bx
        call    STRINGADDRESS           ;for both near and far string support
        mov     strdoff, ax
        mov     strdseg, dx

        dec     x0                      ;subtract 1 from
        dec     y0                      ;all coordinates
        dec     x1                      ;to reflect BASIC's
        dec     y1                      ;screen base of 1 (not 0)

get_chkscr:
        call    get_scrseg              ;set up screen segment

get_setstr:
        mov     al,x1
        sub     al,x0                   ;find width of box
        mov     bwidth,al               ;and save
        add     al,1                    ;add one to width
        mov     ah,0                    ;to find # words to move
        mov     movword,ax              ;MovWord = (width+1)
        mov     al,y1
        sub     al,y0                   ;find height of box
        mov     height,al               ;and save
        mov     es,strdseg
        mov     di,strdoff              ;string is the destination
        mov     si,offset bwidth        ;point to width
        movsb                           ;put width in string
        mov     si,offset height
        movsb                           ;and the height, too

get_movstr:
        mov     al,y0
        mov     ah,x0                   ;put coords in AH and AL
        call    get_memxy               ;and find offset into screen mem
        mov     si,ax                   ;this will be the source

get_domove:
        mov     cx,movword
        push    ds
        mov     ds,scrseg
        rep     movsw                   ;move a row into the string
        pop     ds
        add     si,160
        sub     si,movword              ;Add 160-(movword*2) to si
        sub     si,movword              ;to point to next row
        cmp     height,0                ;was that the last row?
        je      get_done                ;yes, we're done
        dec     height                  ;decrement height
        jmp     get_domove              ;and do another row

get_done:
        pop     di
        pop     si
        pop     es
        pop     ds                      ;restore registers
        pop     bp
        ret     10                      ;there were 5 parameters

getcopybox      endp


;-----------------------------------------------------------------------------
;----------------------------------------This is the routine that copies the
;----------------------------------------information stored in the string to
;----------------------------------------the screen in the specified location
;-----------------------------------------------------------------------------
        public  PUTCOPYBOX
putcopybox      proc    far

        push    bp
        mov     bp,sp
        push    ds
        push    es
        push    si
        push    di                      ;preserve registers


put_start:
        mov     bx,[bp + 10]            ;get y0
        mov     ax,[bx]
        mov     y0,al
        mov     bx,[bp + 8]             ;...x0
        mov     ax,[bx]
        mov     x0,al
        mov     bx,[bp + 6]             ;...and the destination string

        push    bx
        call    STRINGADDRESS           ;for both near and far string support
        mov     strdoff, ax
        mov     strdseg, dx

        dec     x0                      ;subtract 1 from
        dec     y0                      ;all coordinates

put_chkscr:
        call    get_scrseg              ;set up scrseg

put_setstr:
        push    ds
        pop     es                      ;equate ES to DS

        mov     si,strdoff              ;point DS:SI to string mem
        push    ds
        mov     ds,strdseg
        mov     di,offset bwidth
        movsb                           ;get width
        mov     di,offset height
        movsb                           ;and height out of string
        pop     ds

        mov     al,bwidth
        add     al,1
        mov     ah,0
        mov     movword,ax              ;set movword to (bwidth+1)

        mov     ah,x0
        mov     al,y0                   ;get coords
        call    get_memxy               ;and find offset into screen mem
        mov     di,ax
        mov     es,scrseg               ;ES:DI -> screen mem (UL corner)

put_domove:
        mov     cx,movword
        push    ds
        mov     ds,strdseg
        rep     movsw                   ;move a row onto the screen
        pop     ds
        add     di,160
        sub     di,movword              ;add 160-(movword*2) to DI
        sub     di,movword              ;to point to next row on screen
        cmp     height,0                ;was that the last row?
        je      put_done                ;yes, we're finished
        dec     height
        jmp     put_domove              ;no, decrement and do again

put_done:
        pop     di
        pop     si
        pop     es
        pop     ds                      ;restore registers
        pop     bp
        ret     6                       ;pop off 3 parameters

putcopybox      endp

;-----------------------------------------------------------------------------
;----------------------------------------This is the routine that changes
;----------------------------------------the colors of the box's characters
;-----------------------------------------------------------------------------
        public  ATTRBOX
attrbox         proc    far

        push    bp
        mov     bp, sp
        push    ds
        push    es
        push    si
        push    di                      ;preserve registers

atr_start:
        mov     bx, [bp+14]             ;get y0
        mov     ax, [bx]
        mov     y0, al
        mov     bx, [bp+12]             ;...x0
        mov     ax, [bx]
        mov     x0, al
        mov     bx, [bp+10]             ;...y1
        mov     ax, [bx]
        mov     y1, al
        mov     bx, [bp+8]              ;...x1
        mov     ax, [bx]
        mov     x1, al
        mov     bx, [bp+6]              ;...and finally the new color value
        mov     ax, [bx]
        mov     attr, al

        dec     y0                      ;subtract 1 from
        dec     x0                      ;all coordinates
        dec     y1                      ;to reflect BASIC's
        dec     x1                      ;screen base of 1 (not 0)

atr_chkscr:
        call    get_scrseg              ;set up screen segment

atr_setup:
        mov     al, x1
        sub     al, x0                  ;find width of box
        inc     al
        xor     ah, ah
        mov     movword, ax             ;(width + 1 = movword)
        mov     al, y1
        sub     al, y0                  ;find height of box
        mov     height, al              ;and save

atr_chgclr:
        mov     al, y0
        mov     ah, x0                  ;put coords in AH and AL
        call    get_memxy               ;find offset into screen memory
        mov     di, ax                  ;(which is our destination)
        mov     es, scrseg
        mov     al, attr                ;get the color value to store

atr_doit:
        mov     cx, movword
atr_loop:
        inc     di                      ;skip the character value
        stosb                           ;write new color value
        loop    atr_loop                ;cx times
        add     di, 160                 ;add 160-(movword*2) to di
        sub     di, movword
        sub     di, movword
        cmp     height, 0               ;was that the last row?
        je      atr_done                ;yes, we be finished
        dec     height                  ;no, decrement height
        jmp     atr_doit

atr_done:
        pop     di
        pop     si
        pop     es
        pop     ds
        pop     bp                      ;restore registers
        ret     10                      ;pull off 5 paramters and return

attrbox         endp

        END
