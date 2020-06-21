' PlayMidi.bas
'
' This library adds support for the original QBasic PLAY statement back to FreeBASIC
'
' Original code by Mysoft: https://www.freebasic.net/forum/viewtopic.php?t=12995
'
' I've added a background thread so that "MF" works.
' This could use a lot of clean-up, or even a re-write.

#lang "fblite"

#include once "windows.bi"
#include once "win\mmsystem.bi"

'Namespace QBPlay

Type MidiMessage Field=1
  Number As Ubyte
  ParmA As Ubyte
  ParmB As Ubyte
  Reserved As Ubyte
End Type
#define MidiSendMessage(MSGVAR) midiOutShortMsg(MYPLAYDEVICE, *cptr(Integer Ptr,@MSGVAR))
#define MidiSetMessage(MSGVAR,NUMB,PAA,PBB) MSGVAR.Number=NUMB:MSGVAR.ParmA=PAA:MSGVAR.ParmB=PBB

Const PLAYRATE = 1

enum PlayModes
  pmLegato = 1
  pmNormal = 2
  pmStacato = 4
  pmPercentage = 7
  pmBackground = 16
End enum

Type NoteQueue
  before As NoteQueue Ptr
  after As NoteQueue Ptr
  note As Short
  duration As Double
  mode As Integer
  instrument As Integer
  counter As Integer Ptr
End Type

Declare Sub AddNote(note As Short, duration As Double, mode As Integer, counter As Integer Ptr, instrument As Integer = -1)
Declare Sub Play(TEXT As String)
Declare Sub MidiThread(ByVal userdata As Any Ptr)
Declare Sub WaitUntil(endTime As Double)

Dim Shared As HMIDIOUT MYPLAYDEVICE     '// MIDI device interface for sending MIDI output

Dim Shared As NoteQueue midiNoteQueue
Dim Shared As Any Ptr midiNoteQueueLock
Dim Shared As Any Ptr counterLock

'Scope
  Dim As MidiMessage MIDICONFIG
  MidiSetMessage(MIDICONFIG,&hC0,0,0)
  MidiSendMessage(MIDICONFIG)
  MidiSetMessage(MIDICONFIG,&hC1,0,0)
  MidiSendMessage(MIDICONFIG)
'End Scope

' init
'Scope
Dim As Integer FLAG
FLAG = midiOutOpen(@MYPLAYDEVICE, MIDI_MAPPER, 0, 0, null)
If (FLAG <> MMSYSERR_NOERROR) Then
    Print "Error opening MIDI Output."
End If

midiNoteQueue.before = @midiNoteQueue
midiNoteQueue.after = @midiNoteQueue
midiNoteQueue.note = -1
midiNoteQueue.duration = 0
midiNoteQueue.mode = pmNormal
midiNoteQueue.counter = 0
midiNoteQueueLock = MutexCreate()
counterLock = MutexCreate()

If ThreadCreate(@MidiThread) = 0 Then
    Print "Error starting MIDI thread."
End If

'End Scope

Sub WaitUntil(endTime As Double)
    Dim As Double closeTime = endTime - 0.100
    While timer < closeTime: Sleep 10: Wend
    While timer < endTime: Wend
End Sub

Sub MidiThread(ByVal userdata As Any Ptr)
Static As MidiMessage MIDICONFIG
Static As MidiMessage MYNOTE
Static As Double TMRNOTE,NOTSZ,PASZ
Static As Integer LEGATO,HADLEGA
Dim As Integer FLAG
Dim As NoteQueue Ptr queue
While True
    MutexLock midiNoteQueueLock
    queue = midiNoteQueue.before
    midiNoteQueue.before = queue->before
    queue->before->after = @midiNoteQueue
    MutexUnlock midiNoteQueueLock

    If queue = @midiNoteQueue Then
        Sleep 10
        TMRNOTE = 0
        Continue While
    End If

    'change instrument
    If queue->instrument >= 0 Then
        MidiSetMessage(MIDICONFIG,&hC0,queue->instrument,0)
        MidiSendMessage(MIDICONFIG)
        MidiSetMessage(MIDICONFIG,&hC1,queue->instrument,0)
        MidiSendMessage(MIDICONFIG)
    End If

    ' play note
    If queue->duration > 0 Then
      If queue->note < 1 Then
        NOTSZ=0
        PASZ=queue->duration
      Else
        LEGATO=0
        Select Case queue->mode And pmPercentage
        Case pmLegato:  NOTSZ = queue->duration: LEGATO = 1
        Case pmNormal:  NOTSZ = queue->duration * 0.875
        Case pmStacato: NOTSZ = queue->duration * 0.750
        Case Else:      NOTSZ = queue->duration * 0.875
        End Select
        PASZ = queue->duration - NOTSZ
      End If

      With MYNOTE
        .Number = &h90+LEGATO
        .ParmA = queue->note
        .ParmB = 127  'volume
        .Reserved = 0
      End With

      'if abs(timer-TMRNOTE) > 1/64 then
      if TMRNOTE = 0 then TMRNOTE = Timer
      If NOTSZ > 0 Then
        FLAG = MidiSendMessage(MYNOTE)
        If (FLAG <> MMSYSERR_NOERROR) Then
          Print "Error Playing note!":Sleep:End
        End If
        WaitUntil TMRNOTE + NOTSZ
        TMRNOTE += NOTSZ
      End If
      If HADLEGA>1 andalso LEGATO Then HADLEGA=1
      If HADLEGA=1 Then
        MYNOTE.Number = &h80+HADLEGA
        MidiSendMessage(MYNOTE)
      End If
      If HADLEGA Then HADLEGA -= 1

      MYNOTE.Number = &h80+LEGATO
      If LEGATO = 0 Then
        FLAG = MidiSendMessage(MYNOTE)
        If (FLAG <> MMSYSERR_NOERROR) Then
          Print "Error Playing note!":Sleep:End
        End If
      End If
      WaitUntil TMRNOTE + PASZ
      TMRNOTE += PASZ
      If LEGATO andalso HADLEGA=0 Then HADLEGA = 1
    End If

    If queue->counter <> 0 Then
        MutexLock counterLock
        *queue->counter -= 1
        MutexUnlock counterLock
    End If
    Delete queue
Wend
End Sub

Sub PlayFlush()
  Rem nada :P
End Sub

Sub AddNote(note As Short, duration As Double, mode As Integer, counter As Integer Ptr, instrument As Integer)
Dim As NoteQueue Ptr queue

queue = New NoteQueue
queue->note = note
queue->duration = duration
queue->mode = mode
queue->instrument = instrument
queue->instrument = instrument
queue->counter = 0

If (mode And pmBackground) = 0 And counter <> 0 Then
    queue->counter = counter
    MutexLock counterLock
    *counter += 1
    MutexUnlock counterLock
End If

MutexLock midiNoteQueueLock
queue->before = @midiNoteQueue
queue->after = midiNoteQueue.after
queue->after->before = queue
midiNoteQueue.after = queue
MutexUnlock midiNoteQueueLock
End Sub

' *******************************************************************
' *******************************************************************
' *******************************************************************

Sub Play(TEXT As String) 'Thread(ID as any ptr)

  Static As Short  MIDINOTES(142) = { 23    , _ ' B-1
  0,24      ,25,26      ,27,28       ,0,29      ,30,31      ,32,33  ,34,35       , _ 'C0 B0
  0,36      ,37,38      ,39,40       ,0,41      ,42,43      ,44,45  ,46,47       , _ 'C1 B1
  0,48      ,49,50      ,51,52       ,0,53      ,54,55      ,56,57  ,58,59       , _ 'C2 B2
  0,60      ,61,62      ,63,64       ,0,65      ,66,67      ,68,69  ,70,71       , _ 'C3 B3
  0,72      ,73,74      ,75,76       ,0,77      ,78,79      ,80,81  ,82,83       , _ 'C4 B4
  0,84      ,85,86      ,87,88       ,0,89      ,90,91      ,92,93  ,94,95       , _ 'C5 B5
  0,96      ,97,98      ,99,100      ,0,101    ,102,103    ,104,105,106,107      , _ 'C6 B6
  0,108    ,109,110    ,111,112      ,0,113    ,114,115    ,116,117,118,119      , _ 'C7 B7
  0,120    ,121,122    ,123,124      ,0,125    ,126,127    ,128,129,130,131      , _ 'C8 B8
  0,132    ,133,134    ,135,136      ,0,137    ,138,139    ,140,141,142,143      , _ 'C9 B9
  0,144   }

  #define CheckNote() If STPARM Then STPLAY=1:Goto _PlayNote_
  #macro ReadNumber(NUMB)
  NUMBSZ=0:NUMB=0:D=C+2
  While C<TXSZ andalso TEXT[C+1] >= 48 andalso TEXT[C+1] <= 57
    NUMBSZ += 1: C += 1
  Wend
  NUMB = valint(Mid$(TEXT,D,NUMBSZ))
  #endmacro

  #macro AddNewNote()
  If STSIZE=0 Then STSIZE=PL
  NLEN = (((PLAYRATE*60)/(PT Shr 2))*(1/STSIZE))*EXTRATOT
  If NOTE <> -1 Then
    NOTE += STCHG
    #ifdef MyDebug
    TMPSTR = Str$(STSIZE)
    If STSIZE < 10 Then TMPSTR = " "+TMPSTR
    Print "Note: " & NOTENAME(NOTE)+String$(EXTRA,"."), _
    "  Oct: " & PO & "  Lnt: " & TMPSTR & _
    "  Mode: " & MODENAME(PM And pmPercentage) & _
    "  " & INSTRUMENT(PI)
    #endif
    FREQ = MIDINOTES(2+((PO+1)*14)+NOTE)
    AddNote(FREQ,NLEN,PM,@COUNTER)
  Else
    AddNote(0,NLEN,PM,@COUNTER)
  End If
  STPARM=0:STSIZE=0:STCHG=0:STPLAY=0:NOTE=-1
  EXTRAATU=.5:EXTRATOT=1:EXTRA=0
  #endmacro

  #ifdef MyDebug
  Static As Zstring*3 NOTENAME(13) = { _
  "C","C#","D","D#","E","E#","F","F#","G","G#","A","A#","B","B#" }
  Static As Zstring*10 MODENAME(4) = {"","Legato  ","Normal  ","","Stacatto"}
  '"A Piano","B Chromatic Percussion","C Organ","D Guitar","E Bass", _
  '"F Strings","G Ensemble","H Brass","I Reed","J Pipe", _
  '"K Synth Lead","L Synth Pad","M","N","O","P Sound Effects" _
  Static As Zstring*30 INSTRUMENT(127) = { _
  "A0 Acoustic grand piano"      ,"A1 Bright acoustic piano"     , _
  "A2 Electric grand piano"      ,"A3 Honky-tonk piano"          , _
  "A4 Rhodes piano"              ,"A5 Chorused piano"            , _
  "A6 Harpsichord"               ,"A7 Clavinet"                  , _
  "B8 Celesta"                   ,"B9 Glockenspiel"              , _
  "B10 Music box"                ,"B11 Vibraphone"               , _
  "B12 Marimba"                  ,"B13 Xylophone"                , _
  "B14 Tubular bells"            ,"B15 Dulcimer"                 , _
  "C16 Hammond organ"            ,"C17 Percussive organ"         , _
  "C18 Rock organ"               ,"C19 Church organ"             , _
  "C20 Reed organ"               ,"C21 Accordion"                , _
  "C22 Harmonica"                ,"C23 Tango accordion"          , _
  "D24 Acoustic guitar (nylon)"  ,"D25 Acoustic guitar (steel)"  , _
  "D26 Electric guitar (jazz)"   ,"D27 Electric guitar (clean)"  , _
  "D28 Electric guitar (muted)"  ,"D29 Overdriven guitar"        , _
  "D30 Distortion guitar"        ,"D31 Guitar harmonics"         , _
  "E32 Acoustic bass"            ,"E33 Electric bass (finger)"   , _
  "E34 Electric bass (pick)"     ,"E35 Fretless bass"            , _
  "E36 Slap bass 1"              ,"E37 Slap bass 2"              , _
  "E38 Synth bass 1"             ,"E39 Synth bass 2"             , _
  "F40 Violin"                   ,"F41 Viola"                    , _
  "F42 Cello"                    ,"F43 Contrabass"               , _
  "F44 Tremolo strings"          ,"F45 Pizzicato strings"        , _
  "F46 Orchestral harp"          ,"F47 Timpani"                  , _
  "G48 String ensemble 1"        ,"G49 String ensemble 2"        , _
  "G50 Synth. strings 1"         ,"G51 Synth. strings 2"         , _
  "G52 Choir Aahs"               ,"G53 Voice Oohs"               , _
  "G54 Synth voice"              ,"G55 Orchestra hit"            , _
  "H56 Trumpet"                  ,"H57 Trombone"                 , _
  "H58 Tuba"                     ,"H59 Muted trumpet"            , _
  "H60 French horn"              ,"H61 Brass section"            , _
  "H62 Synth. brass 1"           ,"H63 Synth. brass 2"           , _
  "I64 Soprano sax"              ,"I65 Alto sax"                 , _
  "I66 Tenor sax"                ,"I67 Baritone sax"             , _
  "I68 Oboe"                     ,"I69 English horn"             , _
  "I70 Bassoon"                  ,"I71 Clarinet"                 , _
  "J72 Piccolo"                  ,"J73 Flute"                    , _
  "J74 Recorder"                 ,"J75 Pan flute"                , _
  "J76 Bottle blow"              ,"J77 Shakuhachi"               , _
  "J78 Whistle"                  ,"J79 Ocarina"                  , _
  "K80 Lead 1 (square)"          ,"K81 Lead 2 (sawtooth)"        , _
  "K82 Lead 3 (calliope lead)"   ,"K83 Lead 4 (chiff lead)"      , _
  "K84 Lead 5 (charang)"         ,"K85 Lead 6 (voice)"           , _
  "K86 Lead 7 (fifths)"          ,"K87 Lead 8 (brass + lead)"    , _
  "L88 Pad 1 (new age)"          ,"L89 Pad 2 (warm)"             , _
  "L90 Pad 3 (polysynth)"        ,"L91 Pad 4 (choir)"            , _
  "L92 Pad 5 (bowed)"            ,"L93 Pad 6 (metallic)"         , _
  "L94 Pad 7 (halo)"             ,"L95 Pad 8 (sweep)"            , _
  "M96"  ,"M97"  ,"M98"  ,"M99"  ,"M100" ,"M101" ,"M102" ,"M103" , _
  "N104" ,"N105" ,"N106" ,"N107" ,"N108" ,"N109" ,"N110" ,"N111" , _
  "O112" ,"O113" ,"O114" ,"O115" ,"O116" ,"O117" ,"O118" ,"O119" , _
  "P120 Guitar fret noise"       ,"P121 Breath noise"            , _
  "P122 Seashore"                ,"P123 Bird tweet"              , _
  "P124 Telephone ring"          ,"P125 Helicopter"              , _
  "P126 Applause"                ,"P127 Gunshot" }
  Dim As String TMPSTR
  #endif

  Static As Integer PT=120        'Playing quartes notes per minute
  Static As Integer PL=4          'Note length 1/2^(PL-1)
  Static As Integer PM=pmNormal   'play mode
  Static As Integer PO=3          'Oitave
  Static As Integer PI=1          'Play instrument
  Static As Integer NOTE=-1       'Note Playing
  Static As Integer STPARM        'Waiting Parameters
  Static As Integer STSIZE        'Already have size
  Static As Integer STCHG         'Already changed size
  Static As Single EXTRAATU=.5    'Extra size
  Static As Single EXTRATOT=1     'Extra total
  Dim As Integer STPLAY,EXTRA     'Go play!
  Dim As Integer TXSZ,NUMBSZ,C,D
  Dim As Double  NLEN,PLEN         'Calculated length
  Dim As Short   FREQ              'Note frequency
  Dim As Integer COUNTER=0

  TEXT = Ucase$(TEXT)
  TXSZ = Len(TEXT)-1

  For C = 0 To TXSZ

    _PlayNote_:
    If STPLAY Then
      AddNewNote()
    End If

    Select Case As Const TEXT[C]
    Case Asc("M")               'Modes
      CheckNote()
      C += 1: If C > TXSZ Then Exit For
      Select Case TEXT[C]
      Case Asc("B"),Asc("F")    ' -> Background/Foreground
        If TEXT[C]=Asc("B") Then
          PM Or= pmBackground
        Else
          PM And= (Not pmBackground)
        End If
      Case Asc("L")             ' -> Legato
        PM= (PM And (Not pmPercentage)) Or pmLegato
        'print "Mode Legato"
      Case Asc("N")             ' -> Normal
        PM= (PM And (Not pmPercentage)) Or pmNormal
        'print "Mode Normal"
      Case Asc("S")             ' -> Staccato
        PM= (PM And (Not pmPercentage)) Or pmStacato
        'print "Mode Stacato"
      End Select
    Case Asc("T")              'Tempo
      CheckNote()
      ReadNumber(PT)
      If NUMBSZ Then If PT < 32 Or PT > 255 Then PT = 120
      'print "Tempo " & PT
    Case Asc("L")              'Length
      CheckNote()
      ReadNumber(PL)
      If NUMBSZ Then If PL < 1 Or PL > 64 Then PL = 4
      'print "Length " & PL
    Case Asc("O")              'Octave
      CheckNote()
      ReadNumber(PO)
      If NUMBSZ Then If PO < 0 Or PO > 6 Then PO = 3
      'print "Octave " & PO
    Case Asc("I")              'Instrument
      CheckNote()
      ReadNumber(PI)
      AddNote(0, 0, PM, @COUNTER, PI)
    Case Asc(">")              'Increase Octave
      CheckNote()
      If PO < 6 Then PO += 1
      'print "Octave " & PO
    Case Asc("<")              'Decrease Octave
      CheckNote()
      If PO > 0 Then PO -= 1
      'print "Octave " & PO
    Case Asc("P")              'Pause
      CheckNote()
      ReadNumber(STSIZE)
      If STSIZE > 0 And STSIZE < 64 Then
        'print "Pause: " & STSIZE
        NOTE=-1: STPLAY = 1: Goto _PlayNote_
      Else
        STSIZE=0
      End If
    Case Asc("C") To Asc("G")  'Notes C-G
      CheckNote()
      STPARM = -1
      NOTE = (TEXT[C]-Asc("C"))*2
      'print "Note: " & NOTE
    Case Asc("A") To Asc("B")  'Notes A-B
      CheckNote()
      STPARM = -1
      NOTE = (TEXT[C]-Asc("A")+5)*2
      'print "Note: " & NOTE
    Case Asc("#"),Asc("+")     'Above note (sutenido)
      If STPARM andalso STCHG=0 Then
        STCHG=1
      End If
    Case Asc("-")              'Below note (bemol)
      If STPARM andalso STCHG=0 Then
        STCHG=-1
        'print "Bemol"
      End If
    Case Asc(".")              'extra 50%
      If STPARM Then
        EXTRATOT += EXTRAATU:EXTRAATU /= 2
        EXTRA += 1
        'print "Extra: " & fix(EXTRATOT*100)
      End If
    Case Asc("0") To Asc("9")  'notesize
      If STPARM And STSIZE=0 Then
        C -= 1
        ReadNumber(STSIZE)
        If STSIZE < 1 Or STSIZE > 64 Then STSIZE=0
      End If
    End Select

  Next C

  If STPARM Then
    AddNewNote()
  End If

  While COUNTER > 0: Sleep 10: Wend
End Sub

'End Namespace
