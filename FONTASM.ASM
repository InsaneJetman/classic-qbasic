.MODEL	MEDIUM
;************************************************************
; FONTASM.ASM - assembly lang routines for Font Toolbox
;
;  Copyright (C) 1989-1990 Microsoft Corporation, All Rights Reserved
;
;   fl_SetBltDir    - Sets bltchar direction increments
;   fl_SetBltColor  - Sets color parameter for bltchar
;   fl_SetBltParams - Sets font related parameters for bltchar
;   fl_BltChar	    - Character output routine
;
;   fl_MovMem	    - Moves memory
;   fl_ansi	    - Maps IBM chars to Windows ANSI;
;
;************************************************************

; BASIC Procedures
EXTRN	    B$N1I2:far, B$PSTC:far

; BltChar data block
.DATA

; These are set by fl_SetBltParams
HdrLen	    dw	    0	    ;length of windows font file header
CharHeight  dw	    0	    ;character height
FirstChar   dw	    0	    ;first character defined in font
LastChar    dw	    0	    ;last character defined in font
DefaultChar dw	    0	    ;default character to use

; This is set by fl_SetBltColor
CharColor   dw	    0	    ;current character color

; These are set by fl_SetBltDir
XPixInc     dw	    1	    ;x inc for each pixel in character bitmap
YPixInc     dw	    0	    ;y inc for each pixel in character bitmap
XRowInc     dw	    0	    ;x inc for each row in character bitmap
YRowInc     dw	    1	    ;y inc for each row in character bitmap
XColInc     dw	    8	    ;x inc for each column (8 bits) in char bitmap
YColInc     dw	    0	    ;y inc for each column (8 bits) in char bitmap

.CODE
;********************************************************************
; fl_SetBltDir - Sets pixel, row, and column step values for bltchar
;
;   BASIC CALL:
;	fl.SetBltDir XPixInc%, YPixInc%, XRowInc%, YRowInc%
;
;   Comments:
;	When bltchar is blt-ing a bitmap to allow the different
;	directions to be output it uses preset counter increments
;	for moving a pixel, to the next row, and to the next column
;	of the bitmap. The pixel and row increments are input to this
;	routine. The column increments are calculates as 8 times the
;	pixel increment.
;
;********************************************************************

; Parameters
pXPixInc    equ     WORD PTR [bp+12]
pYPixInc    equ     WORD PTR [bp+10]
pXRowInc    equ     WORD PTR [bp+8]
pYRowInc    equ     WORD PTR [bp+6]

	    PUBLIC  FL_SETBLTDIR
fl_SetBltDir PROC

	    push    bp		;Entry
	    mov     bp,sp

	    mov     ax,pXRowInc ;Save input parameters
	    mov     XRowInc,ax
	    mov     ax,pYRowInc
	    mov     YRowInc,ax

	    mov     ax,pXPixInc
	    mov     XPixInc,ax
	    mov     cl,3
	    shl     ax,cl
	    mov     XColInc,ax	;Column increment = Pix Inc * 8

	    mov     ax,pYPixInc
	    mov     YPixInc,ax
	    mov     cl,3
	    shl     ax,cl
	    mov     YColInc,ax	;Column increment = Pix Inc * 8

	    pop     bp		;Exit
	    ret     8
fl_SetBltDir ENDP

;********************************************************************
; fl_SetBltColor - Sets the color of blt-ed characters
;
;   BASIC CALL:
;	fl.SetBltColor color
;
;********************************************************************

; Parameters
pColor	    EQU     WORD PTR [bp+6]

	    PUBLIC  FL_SETBLTCOLOR
fl_SetBltColor PROC

	    push    bp		    ;Entry
	    mov     bp,sp

	    mov     ax,pColor	    ;Save color in data block
	    mov     CharColor,ax

	    pop     bp		    ;Exit
	    ret     2

fl_SetBltColor	ENDP

;********************************************************************
; fl_SetBltParams - Sets font-related params for bltchar
;
;   BASIC CALL:
;	fl.SetBltParams HdrLen%, CharHgt%, FirstChar%, LastChar%, DefChar%
;
;********************************************************************

; Parameters
pHdrLen     equ     WORD PTR [bp+14]
pCharHgt    equ     WORD PTR [bp+12]
pFirstChar  equ     WORD PTR [bp+10]
pLastChar   equ     WORD PTR [bp+8]
pDefChar    equ     WORD PTR [bp+6]

	    PUBLIC  FL_SETBLTPARAMS
fl_SetBltParams PROC

	    push    bp		    ;Entry
	    mov     bp,sp

	    mov     ax,pHdrLen
	    mov     HdrLen,ax

	    mov     ax,pCharHgt
	    mov     CharHeight,ax

	    mov     ax,pFirstChar
	    mov     FirstChar,ax

	    mov     ax,pLastChar
	    mov     LastChar,ax

	    mov     ax,pDefChar
	    mov     DefaultChar,ax

	    pop     bp		    ;Exit
	    ret     10

fl_SetBltParams ENDP

;********************************************************************
; fl_BltChar - Outputs a character's bitmap to the screen
;
;   BASIC CALL:
;	fl.BltChar FontAddr(far), Char%, X%, Y%
;
;********************************************************************

; Parameters
pFASeg	    equ     WORD PTR [bp+14]
pFAOffset   equ     WORD PTR [bp+12]
pChar	    equ     WORD PTR [bp+10]
pX	    equ     WORD PTR [bp+8]
pY	    equ     WORD PTR [bp+6]

; Local Variables
.RowX	    equ     WORD PTR [bp-2]
.RowY	    equ     WORD PTR [bp-4]
.CharWid    equ     WORD PTR [bp-6]
.ColWid     equ     WORD PTR [bp-8]

	    PUBLIC  FL_BLTCHAR
fl_BltChar  PROC

	    push    bp		    ;Entry
	    mov     bp,sp
	    sub     sp,8	    ;Make room for local variables
	    push    di
	    push    si

	    ;Normalize font address (make offset as small as possible)
	    mov     ax,pFAOffset
	    mov     bx,pFASeg
	    push    ax
	    mov     cl,4
	    shr     ax,cl	    ;offset = offset div 16
	    add     bx,ax	    ;seg = seg + offset
	    pop     ax
	    and     ax,0Fh	    ;offset = original offset mod 16
	    mov     si,ax
	    mov     es,bx

	    ;Calculate character number
	    mov     bx,pChar
	    cmp     bx,LastChar
	    ja	    usedefchar	    ;Char is > last char, use default
	    sub     bx,FirstChar
	    jnc     getsize	    ;Char is > first char, is OK
usedefchar: mov     bx,DefaultChar

	    ;Get character width from character table in font
getsize:    shl     bx,1
	    shl     bx,1	    ;char = char * 4
	    add     bx,si	    ;offset into char table
	    mov     cx,es:[bx]	    ;cx = character width
	    mov     .CharWid,cx

	    ;Calculate character bitmap address
	    inc     bx		    ;move to next two bytes in char table
	    inc     bx
	    mov     cx,es:[bx]
	    add     si,cx	    ;add bitmap offset into font index
	    sub     si,HdrLen	    ;subtract length of header
	    dec     si		    ;decrement for use in output algorithm

	    ;Blt character
	    mov     cx,pX	    ;cx = x coord
	    mov     dx,pY	    ;dx = y coord

	    mov     bx,.CharWid

colloop:    mov     .RowX,cx	    ;save coordinates of this row
	    mov     .RowY,dx
	    push    bx		    ;save remaining bits in character
	    cmp     bx,8	    ;limit to 8 for this column
	    jle     colloop2
	    mov     bx,8

colloop2:   mov     .ColWid,bx	    ;save width of this column for other rows
	    mov     ax,CharHeight   ;counter for number of rows

rowloop:    push    ax
	    inc     si		    ;increment bitmap pointer
	    mov     al,es:[si]	    ;get byte from bitmap

pixloop:    shl     al,1	    ;check next bit (from left to right)
	    jnc     nextpixel	    ;skip this pixel

	    push    ax		    ;save registers
	    push    bx
	    push    cx
	    push    dx
	    push    es
	    push    si

	    mov     ax,CharColor    ;set up params for pset call
	    push    ax		    ;color
	    push    cx		    ;x-coordinate
	    push    dx		    ;y-coordinate
	    call    B$N1I2	    ;set graphics cursor location
	    call    B$PSTC	    ;call PSET

	    pop     si		    ;restore registers
	    pop     es
	    pop     dx
	    pop     cx
	    pop     bx
	    pop     ax

nextpixel:  jz	    nextrow	    ;skip remaining zero bits
	    add     cx,XPixInc	    ;increment x and y coordinates
	    add     dx,YPixInc
	    dec     bx		    ;check for end of byte
	    jnz     pixloop	    ;go for another pixel

nextrow:    mov     cx,.RowX	    ;retrieve the start coord of this row
	    mov     dx,.RowY
	    add     cx,XRowInc	    ;increment counters for next row
	    add     dx,YRowInc
	    mov     .RowX,cx	    ;save 'em back again
	    mov     .RowY,dx
	    mov     bx,.ColWid	    ;reset the column width
	    pop     ax		    ;check for the end of this column
	    dec     ax
	    jnz     rowloop	    ;repeat for another row

nextcol:    mov     cx,pX	    ;retrieve the start coord of this column
	    mov     dx,pY
	    add     cx,XColInc	    ;increment coordinates for next column
	    add     dx,YColInc
	    mov     pX,cx	    ;save coordinates to use after next column
	    mov     pY,dx
	    pop     bx		    ;check for end of the bitmap
	    sub     bx,8
	    ja	    colloop	    ;repeat for another column

	    ;Done
	    mov     ax,.CharWid     ;return value

	    pop     si		    ;Exit
	    pop     di
	    mov     sp,bp
	    pop     bp
	    ret     10
fl_BltChar  ENDP

;********************************************************************
; fl_MovMem - Moves memory bytes
;
;   BASIC CALL:
;	fl.MovMem source, dest, nbytes
;
;********************************************************************
	    PUBLIC  FL_MOVMEM
fl_MovMem   PROC
	    push    bp
	    mov     bp,sp
	    push    si
	    push    ds
	    push    di

	    les     di,[bp+12]
	    lds     si,[bp+8]
	    mov     cx,[bp+6]
	    rep	    movsb

	    pop     di
	    pop     ds
	    pop     si
	    pop     bp
	    ret     10
fl_MovMem   ENDP

;********************************************************************
; fl_ansi - Converts IBM char to Windows ANSI mapping
;
;   BASIC CALL:
;	ansi_byte = fl_ansi (ibm_char%)
;
;********************************************************************
.CODE
	    PUBLIC  FL_ANSI
fl_ansi     PROC
	    push    bp
	    mov     bp,sp

	    xor     ax,ax	    ; zero ax
	    mov     al,[bp+6]	    ; move input byte to ax
	    mov     bx,ax	    ; copy byte to bx
	    and     al,7FH	    ; mask off high bit
	    test    bl,80H	    ; test bx to see it high bit set
	    jz	    fl_a_2	    ; if so then byte < 128, no translation

	    mov     bx,OFFSET _OemToAnsiTable
	    xlat

fl_a_2:     pop     bp
	    ret     2
fl_ansi     ENDP


;***************************************************************************
;   USA OEM/ANSI translation tables.                                       *
;***************************************************************************
;

; This translation table is used by U.S.A. and some European countries.
; The original IBM extended character set is now addressed as Code Page 437.
; With DOS 3.3 or later, IBM introduced Code Page 850 as the preeminent
; multilingual character set.

; this translates Oem codes >= 128 to ANSI.
; there are 128 entries.

.DATA
_OemToAnsiTable  label   byte

        db   0C7H     ; 80h  C cedilla
        db   0FCh     ; 81h  u umlaut
        db   0E9h     ; 82h  e acute
        db   0E2h     ; 83h  a circumflex
        db   0E4h     ; 84h  a umlaut
        db   0E0h     ; 85h  a grave
        db   0E5h     ; 86h  a ring
        db   0E7h     ; 87h  c cedilla
        db   0EAh     ; 88h  e circumflex
        db   0EBh     ; 89h  e umlaut
        db   0E8h     ; 8Ah  e grave
        db   0EFh     ; 8Bh  i umlaut
        db   0EEh     ; 8Ch  i circumflex
        db   0ECh     ; 8Dh  i grave
        db   0C4h     ; 8Eh  A umlaut
        db   0C5h     ; 8Fh  A ring

        db   0C9h     ; 90h  E acute
        db   0E6h     ; 91h  ae
        db   0C6h     ; 92h  AE
        db   0F4h     ; 93h  o circumflex
        db   0F6h     ; 94h  o umlaut
        db   0F2h     ; 95h  o grave
        db   0FBh     ; 96h  u circumflex
        db   0F9h     ; 97h  u grave
        db   0FFh     ; 98h  y umlaut
        db   0D6h     ; 99h  O umlaut
        db   0DCh     ; 9Ah  U umlaut
        db   0A2h     ; 9Bh  cent
        db   0A3h     ; 9Ch  british pound
        db   0A5h     ; 9Dh  yen
        db   070h     ; 9Eh  Pesetas
        db   066h     ; 9Fh  florin (dutch)

        db   0E1h     ; A0h  a acute
        db   0EDh     ; A1h  i acute
        db   0F3h     ; A2h  o acute
        db   0FAh     ; A3h  u acute
        db   0F1h     ; A4h  n tilde
        db   0D1h     ; A5h  N tilde
        db   0AAh     ; A6h  a underlined superscript
        db   0BAh     ; A7h  o underlined superscript
        db   0BFh     ; A8h  inverted question mark
        db   05Fh     ; A9h  left top corner
        db   0ACh     ; AAh  right top corner
        db   0BDh     ; ABh  1/2
        db   0BCh     ; ACh  1/4
        db   0A1h     ; ADh  inverted point
        db   0ABh     ; AEh  <<
        db   0BBh     ; AFh  >>

        db   05Fh     ; B0h  here begins semigraphic characters
        db   05Fh     ; B1h
        db   05Fh     ; B2h
        db   0A6h     ; B3h  Vertical bar
        db   05Fh     ; B4h
        db   05Fh     ; B5h
        db   05Fh     ; B6h
        db   05Fh     ; B7h
        db   05Fh     ; B8h
        db   05Fh     ; B9h
        db   05Fh     ; BAh
        db   05Fh     ; BBh
        db   05Fh     ; BCh
        db   05Fh     ; BDh
        db   05Fh     ; BEh
        db   05Fh     ; BFh

        db   05Fh     ; C0h
        db   05Fh     ; C1h
        db   05Fh     ; C2h
        db   05Fh     ; C3h
        db   05Fh     ; C4h
        db   05Fh     ; C5h
        db   05Fh     ; C6h
        db   05Fh     ; C7h
        db   05Fh     ; C8h
        db   05Fh     ; C9h
        db   05Fh     ; CAh
        db   05Fh     ; CBh
        db   05Fh     ; CCh
        db   05Fh     ; CDh
        db   05Fh     ; CEh
        db   05Fh     ; CFh

        db   05Fh     ; D0h
        db   05Fh     ; D1h
        db   05Fh     ; D2h
        db   05Fh     ; D3h
        db   05Fh     ; D4h
        db   05Fh     ; D5h
        db   05Fh     ; D6h
        db   05Fh     ; D7h
        db   05Fh     ; D8h
        db   05Fh     ; D9h
        db   05Fh     ; DAh
        db   05Fh     ; DBh
        db   05Fh     ; DCh
        db   05Fh     ; DDh
        db   05Fh     ; DEh
        db   05Fh     ; DFh  end of semigraphic characters

        db   05Fh     ; E0h  alpha
        db   0DFh     ; E1h  german sharp S or greek beta
        db   05Fh     ; E2h  lambda
        db   0B6h     ; E3h  pi
        db   05Fh     ; E4h  sigma uc
        db   05Fh     ; E5h  sigma lc
        db   0B5h     ; E6h  mu
        db   05Fh     ; E7h  tau
        db   05Fh     ; E8h  phi uc
        db   05Fh     ; E9h  theta
        db   05Fh     ; EAh  omega
        db   05Fh     ; EBh  delta
        db   05Fh     ; ECh  infinite
        db   0D8h     ; EDh  math empty set or phi lc
        db   05Fh     ; EEh  math own sign
        db   05Fh     ; EFh  math include sign

        db   05Fh     ; F0h  math equivalence sign
        db   0B1h     ; F1h  + underlined
        db   05Fh     ; F2h  greater equal
        db   05Fh     ; F3h  less equal
        db   05Fh     ; F4h  math integral upper part
        db   05Fh     ; F5h  math integral lower part
        db   05Fh     ; F6h  math divide
        db   05Fh     ; F7h  math approximately (~)
        db   0B0h     ; F8h  degree
        db   0B7h     ; F9h  period accent (bold)
        db   0B7h     ; FAh  period accent
        db   05Fh     ; FBh  math root
        db   06Eh     ; FCh  n superscript
        db   0B2h     ; FDh  2 superscript
        db   05Fh     ; FEh
        db   05Fh     ; FFh  blank

        END
