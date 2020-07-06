' PlayMidi.bas
'
' This library adds support for the original QBasic PLAY statement back to FreeBASIC
'
' PLAY - a device I/O statement that plays music
'
' Syntax
'   PLAY commandstring
'     * commandstring is a stringexpression that contains music commands:
' --------------------------Set Octaves and Play Tones--------------------------
'  On  Sets current octave (n = 0-6)    |  < or >  Up or down one octave
'  Nn  Plays note n (n = 0-84, 0 is a   |  A-G  Plays A, B, ..., G in current
'      rest)                            |      octave (+ = sharp, - = flat)
' --------------------------Set Tone Duration and Tempo-------------------------
'  Ln  Sets length of a note (L1 is     |  MS  Each note plays 3/4 of length
'      whole note, L4 is quarter note,  |  MN  Each note plays 7/8 of length
'      etc.)  n = 1-64                  |  ML  Each note plays full length
'  Tn  Sets number of quarter notes per |  Pn  Pause for the duration of
'      minute (n = 32-255, 120 default  |      n quarternotes (n = 1-64)
' --------------------------------Set Operation---------------------------------
'  MF  Plays music in foreground        |  MB  Plays music in background
' -------------------------------Execute Substrings-----------------------------
'        X + VARPTR$(string-expression)     Executes another command string

#lang "fb"

#include once "windows.bi"
#include once "win\mmsystem.bi"

Namespace QBPlay

Union MidiMessage
  Msg As ULong
  Type
    Number   As UByte
    ParmA    As UByte
    ParmB    As UByte
    Reserved As UByte
  End Type
End Union

Type QueueNode
  Declare Constructor ()
  before As QueueNode Ptr
  after As QueueNode Ptr
  note As Integer
  tempo As Integer
  duration As Integer
  articulation As Integer
  instrument As Integer
  volume As Integer
  counter As Integer Ptr
End Type

Type PlayStatus
  Declare Constructor ()
  octave As Integer
  tempo As Integer
  duration As Integer
  articulation As Integer
  instrument As Integer
  volume As Integer
End Type

Type MidiStatus
  Declare Constructor ()
  Declare Destructor ()
  hmo As HMIDIOUT
  instrument As Integer
End Type

Const MIDI_NOTE_BASE        = 36 - 1     ' 3 octaves - 1 for 0 vs 1 base
Const PLAY_SPEED            = 4.0 * 60.0 ' 1/4 notes & 60sec/min
Const ARTICULATION_MAX      = 600.0
Const ARTICULATION_LEGATO   = 600.0      ' full note length
Const ARTICULATION_NORMAL   = 525.0      ' 7/8 note length
Const ARTICULATION_STACCATO = 450.0      ' 3/4 note length
Const DEFAULT_OCTIVE        = 5 - 1      ' SPN octave 5
Const DEFAULT_TEMPO         = 120        ' bpm
Const DEFAULT_DURATION      = 4          ' 1/4 note
Const MAX_INSTRUMENT        = 127
Const DEFAULT_INSTRUMENT    = 0          ' piano
Const DEFAULT_VOLUME        = 127        ' 100%

Dim Shared As QueueNode noteQueue
Dim Shared As Any Ptr thread
Dim Shared As Any Ptr queueLock
Dim Shared As Any Ptr counterLock
Dim Shared As PlayStatus play
Dim Shared As MidiStatus midi

Sub WaitUntil(endTime As Double)
    Dim As Double closeTime = endTime - 0.100
    While Timer < closeTime: Sleep 10: Wend
    While Timer < endTime: Wend
End Sub

Sub MidiThread(ByVal userdata As Any Ptr)
    Dim As QueueNode Ptr node
    Dim As MidiMessage msg
    Dim As Double tStart, tEnd, tNext, tDelta
    tStart = Timer

    While True
        MutexLock queueLock
        node = noteQueue.after
        noteQueue.after = node->after
        node->after->before = @noteQueue
        If node = @noteQueue Then thread = 0
        MutexUnlock queueLock
        If node = @noteQueue Then Exit Sub

        tDelta = PLAY_SPEED / (node->tempo * node->duration)
        tNext = tStart + tDelta

        If node->note > 0 And node->duration > 0 And node->tempo > 0 And node->articulation > 0 Then
            If midi.instrument <> node->instrument Then
                msg.Number = &hC0
                msg.ParmA = node->instrument
                msg.ParmB = 0
                midiOutShortMsg(midi.hmo, msg.msg)
                midi.instrument = node->instrument
            End If

            msg.Number = &h90
            msg.ParmA = node->note + MIDI_NOTE_BASE
            msg.ParmB = node->volume
            midiOutShortMsg(midi.hmo, msg.msg)

            tEnd = tStart + tDelta * (node->articulation / ARTICULATION_MAX)
            WaitUntil tEnd

            msg.Number = &h80
            midiOutShortMsg(midi.hmo, msg.msg)
        End If

        WaitUntil tNext
        tStart = tNext

        If node->counter <> 0 Then
            MutexLock counterLock
            *node->counter -= 1
            MutexUnlock counterLock
        End If
    Wend
End Sub

Sub EnqueueNote(note As Integer, tempo As Integer, duration As Integer, articulation As Integer, instrument As Integer, volume As Integer = 127, counter As Integer Ptr = 0)
    Dim As QueueNode Ptr node = New QueueNode
    node->note         = note
    node->tempo        = tempo
    node->duration     = duration
    node->articulation = articulation
    node->instrument   = instrument
    node->volume       = volume
    node->counter      = counter

    If node->counter <> 0 Then
        MutexLock counterLock
        *node->counter += 1
        MutexUnlock counterLock
    End If

    MutexLock queueLock
    node->after = @noteQueue
    node->before = noteQueue.before
    node->before->after = node
    node->after->before = node
    If thread = 0 Then thread = ThreadCreate(@MidiThread)
    Dim As Boolean noThread = (thread = 0)
    MutexUnlock queueLock
    If noThread Then Error 1
End Sub

Constructor QueueNode ()
    before = @This
    after = @This
End Constructor

Constructor PlayStatus ()
    octave       = DEFAULT_OCTIVE
    tempo        = DEFAULT_TEMPO
    duration     = DEFAULT_DURATION
    articulation = ARTICULATION_NORMAL
    instrument   = DEFAULT_INSTRUMENT
    volume       = DEFAULT_VOLUME
End Constructor

Constructor MidiStatus ()
    If midiOutOpen(@hmo, MIDI_MAPPER, 0, 0, null) <> MMSYSERR_NOERROR Then Error 1
End Constructor

Destructor MidiStatus ()
    If midiOutClose(hmo) <> MMSYSERR_NOERROR Then Error 1
End Destructor

End Namespace

QBPlay.queueLock   = MutexCreate()
QBPlay.counterLock = MutexCreate()

Sub Play(commandstring As String)
    Dim As Integer counter
    QBPlay.play.octave -= 1
    QBPlay.EnqueueNote 12 * QBPlay.play.octave + 5, QBPlay.play.tempo, QBPlay.play.duration, QBPlay.play.articulation, QBPlay.play.instrument, QBPlay.play.volume, @counter
    QBPlay.EnqueueNote 12 * QBPlay.play.octave + 3, QBPlay.play.tempo, QBPlay.play.duration, QBPlay.play.articulation, QBPlay.play.instrument, QBPlay.play.volume, @counter
    QBPlay.EnqueueNote 12 * QBPlay.play.octave + 1, QBPlay.play.tempo, QBPlay.play.duration, QBPlay.play.articulation, QBPlay.play.instrument, QBPlay.play.volume, @counter
    While counter > 0: Sleep 10: Wend
End Sub

Print "Begin..."
Play ""
Print "End"
Sleep
