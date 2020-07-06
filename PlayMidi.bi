' PlayMidi.bi
'
' This library adds support for the original QBasic PLAY statement back to FreeBASIC
'
' PLAY - a device I/O statement that plays music
'
' Syntax: PLAY commandstring
'
'  * commandstring is a stringexpression that contains music commands:
'
' ----- Play Tones -----
'  Nn       plays note n (n = 0-84, 0 is a rest)
'  A-Gx     play note A, B, ..., G in current octave, suffixes:
'           +/# sharp, - flat, . dotted note, n use different length
'
' ----- Set Octaves -----
'  On       set the current octave (n = 0-6)
'  < or >   decrease or increase the current octave
'
' ----- Set Tone Duration and Tempo -----
'  Ln   set length of a note
'       L1 is whole note, L4 is quarter note, etc.
'       (n = 1-64, 4 default)
'  MS   staccato articulation, each note plays 3/4 of length
'  MN   normal articulation,   each note plays 7/8 of length
'  ML   legato articulation,   each note plays full length
'  Tn   set the tempo in beats (quarter note) per minute
'       (n = 32-320, 120 default)
'  Pn   pause for the specified note length (n = 1-64)
'       P1 is a whole-note pause, P2 is a half-note pause, etc.
'
' ----- Set Operation -----
'  MF   play music in the foreground
'  MB   play music in the background
'
' ----- Set MIDI Variables -----
'  In   set instrument (n = 0-127, 16 default)
'  Vn   set volume (n = 0-127, 127 default)

#inclib "PlayMidi"

Declare Sub Play(commandstring As String)
