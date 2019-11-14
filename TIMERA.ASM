;*************************  TIMERA.ASM  ******************************
; This program, along with TIMER.BAS, makes use of the BASIC SetUEvent
; routine to print a message on the screen every 4.5 seconds.
; This file has three procedures.  SetInt sets up the new DOS interrupt
; vector.  EventHandler increments a counter 18 times a second and
; notifies BASIC when 4.5 seconds have elapsed.  RestInt restores the
; old interrupt vector.

	    .model  medium, basic	    ;Stay compatible with BASIC.
	    extrn   SETUEVENT: far
            .code
SETINT	    proc    uses ds		    ;Get old interrupt vector
            mov     ax, 351CH               ;and save it.
			int	21h
            mov     word ptr cs:OldVector, bx
            mov     word ptr cs:OldVector + 2, es

            push    cs                      ;Set the new
            pop ds                          ;interrupt vector
            lea dx, EventHandler            ;to the address
            mov ax, 251CH                   ;of our service
            int 21H                         ;routine.
			ret	
SETINT	    endp

public  EVENTHANDLER                        ;Make this routine
                                            ;public for debugging--
EVENTHANDLER	proc			    ;it will check to see if
					    ;4.5 seconds have passed.
	    push    bx
            lea     bx, TimerTicks
            inc     byte ptr cs:[bx]        ;Have 4.5 seconds elapsed?
            cmp     byte ptr cs:[bx], 82
            jnz     Continue
            mov     byte ptr cs:[bx], 0     ;If true, reset counter,
            push    ax                      ;save registers, and
            push    cx                      ;have BASIC set the
            push    dx                      ;user event flag.
            push    es
	    call    SETUEVENT
            pop     es
            pop     dx                      ;Restore registers.
            pop     cx
            pop     ax
Continue:
            pop     bx
            jmp     cs:OldVector            ;Continue on with the
                                            ;old service routine.

TimerTicks  db      0                       ;Keep data in code segment
OldVector   dd      0                       ;where it can be found no
                                            ;matter where in memory the
EVENTHANDLER	endp			    ;interrupt occurs.

RESTINT     proc    uses ds		    ;Restore the old
            lds     dx, cs:OldVector        ;interrupt vector
            mov     ax, 251CH               ;so things will
            int     21h                     ;keep working when
            ret                             ;this BASIC program is
RESTINT     endp			    ;finished.
			end
