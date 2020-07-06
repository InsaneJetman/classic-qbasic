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

Const FIRST_OCTAVE         = 1          ' which SPN octave is octave 0 (MIDI starts at -2)
Const DEFAULT_OCTAVE       = 5 - FIRST_OCTAVE
Const MIDI_NOTE_BASE       = 12 * (2 + FIRST_OCTAVE) - 1
Const MAX_OCTAVE           = 7 - FIRST_OCTAVE
Const MAX_NOTE             = 12 * (MAX_OCTAVE + 1)
Const MAX_TEMPO            = 320        ' bpm (originally 255)
Const MIN_TEMPO            = 32         ' bpm
Const DEFAULT_TEMPO        = 120        ' bpm
Const SHORTEST_NOTE_LENGTH = 64         ' 64th note
Const DEFAULT_NOTE_LENGTH  = 4          ' quarter note
Const MAX_INSTRUMENT       = 127
Const DEFAULT_INSTRUMENT   = 0          ' piano
Const MAX_VOLUME           = 127
Const DEFAULT_VOLUME       = MAX_VOLUME
Const TIME_DIVISION        = 240        ' ticks per beat
Const WHOLE_NOTE           = 4 * TIME_DIVISION
Const TEMPO_1BPM           = 60000000   ' micro-seconds / minute
Const TEMPO_DEFAULT        = TEMPO_1BPM \ DEFAULT_TEMPO
Const BUFFER_SIZE          = 768
Const MIDIEVENT_SIZE       = 3 * sizeof(DWORD)

Declare Sub MidiCallback(byval hdrvr as HDRVR, byval uMsg as UINT, byval dwUser as DWORD_PTR, byval dw1 as DWORD_PTR, byval dw2 as DWORD_PTR)

Type MidiBuffer As _MidiBuffer

Type MidiPlayer
    Declare Constructor()
    Declare Destructor()
    Declare Sub Initialize()
    Declare Sub Shutdown()
    Declare Sub PlayBuffer(buffer As MidiBuffer Ptr)

    stream As HMIDISTRM
    streamLock As Any Ptr
    lastBuffer As MidiBuffer Ptr
    dataLock As Any Ptr
End Type

Union MidiMessage
    Msg As ULong
    Type Field = 1
        status As UByte
        data1  As UByte
        data2  As UByte
        Reserved As UByte
    End Type
End Union

Type _MidiBuffer
    Declare Constructor()
    Declare Destructor()
    Declare Sub AddRest(waitTime As UInteger)
    Declare Sub AddTempo(tempo As UInteger)
    Declare Sub AddEvent(status As UInteger, data1 As UInteger, data2 As UInteger, waitTime As UInteger = 0)
    Declare Sub AddEvent(waitTime As ULong, msg As MidiMessage)
    Declare Sub AddFinishLock()

    player As MidiPlayer Ptr
    buffer As Any Ptr
    bufferLength As Integer
    bufferRecorded As Integer
    finishLock As Any Ptr
End Type

Enum PlayArticulation
    ARTICULATION_STACCATO = 450,
    ARTICULATION_NORMAL   = 525,
    ARTICULATION_LEGATO   = 600,
    ARTICULATION_MAX      = 600
End Enum

Enum PlayMode
    MODE_FOREGROUND,
    MODE_BACKGROUND
End Enum

Type PlayStatus
    Declare Constructor()
    Declare Destructor()

    instrument As Integer
    volume As Integer
    octave As Integer
    noteLength As Integer
    tempo As Integer
    articulation As PlayArticulation
    mode As PlayMode
    statusLock As Any Ptr
End Type

Constructor MidiPlayer()
    streamLock = MutexCreate()
    dataLock = MutexCreate()
End Constructor

Destructor MidiPlayer()
    Shutdown
    MutexDestroy streamLock
    MutexDestroy dataLock
End Destructor

Sub MidiPlayer.Initialize()
    MutexLock streamLock
    If stream = 0 Then
        Dim As MMRESULT result
        result = midiStreamOpen(@stream, @result, 1, Cast(DWORD_PTR, @MidiCallback), 0, CALLBACK_FUNCTION)
        If result <> MMSYSERR_NOERROR Then Error 1

        Dim As MIDIPROPTIMEDIV timeDiv
        timeDiv.cbStruct = sizeof(MIDIPROPTIMEDIV)
        timeDiv.dwTimeDiv = TIME_DIVISION
        result = midiStreamProperty(stream, Cast(LPBYTE, @timeDiv), MIDIPROP_SET Or MIDIPROP_TIMEDIV)
        if result <> MMSYSERR_NOERROR Then Error 1

        ' Redundant if TEMPO_DEFAULT = 120 bpm
        Dim As MIDIPROPTEMPO   tempo
        tempo.cbStruct = sizeof(MIDIPROPTEMPO)
        tempo.dwTempo = TEMPO_DEFAULT
        result = midiStreamProperty(stream, Cast(LPBYTE, @tempo), MIDIPROP_SET Or MIDIPROP_TEMPO)
        if result <> MMSYSERR_NOERROR Then Error 1
    End If
    MutexUnlock streamLock
End Sub

Sub MidiPlayer.Shutdown()
    MutexLock streamLock
    If stream <> 0 Then
        Dim As MMRESULT result = midiStreamClose(stream)
        if result <> MMSYSERR_NOERROR Then Error 1
        stream = 0
    End If
    MutexUnlock streamLock
End Sub

Sub MidiPlayer.PlayBuffer(buffer As MidiBuffer Ptr)
    Dim As MIDIHDR Ptr header
    Dim As ULong result

    MutexLock dataLock
    lastBuffer = buffer
    MutexUnlock dataLock

    buffer->player = @This
    If buffer->finishLock <> 0 Then MutexLock buffer->finishLock

    header = new MIDIHDR
    header->lpData = buffer->buffer
    header->dwBufferLength = buffer->bufferLength
    header->dwBytesRecorded = buffer->bufferRecorded
    header->dwUser = Cast(DWORD_PTR, buffer)

    result = midiOutPrepareHeader(cast(HMIDIOUT, stream), header, sizeof(MIDIHDR))
    if result <> MMSYSERR_NOERROR Then Error 1

    result = midiStreamOut(stream, header, sizeof(MIDIHDR))
    if result <> MMSYSERR_NOERROR Then Error 1

    result = midiStreamRestart(stream)
    if result <> MMSYSERR_NOERROR Then Error 1
End Sub

Constructor _MidiBuffer()
    buffer = Allocate(BUFFER_SIZE)
    If buffer = 0 Then Error 1
    bufferLength = BUFFER_SIZE
End Constructor

Destructor _MidiBuffer()
    Deallocate buffer
    If finishLock <> 0 Then MutexDestroy finishLock
End Destructor

Sub _MidiBuffer.AddRest(waitTime As UInteger)
    If waitTime <= 0 Then Exit Sub
    Dim As MidiMessage msg
    msg.Reserved = MEVT_NOP
    AddEvent waitTime, msg
End Sub

Sub _MidiBuffer.AddTempo(tempo As UInteger)
    Dim As MidiMessage msg
    msg.msg = tempo
    msg.Reserved = MEVT_TEMPO
    AddEvent 0, msg
End Sub

Sub _MidiBuffer.AddEvent(status As UInteger, data1 As UInteger, data2 As UInteger, waitTime As UInteger)
    Dim As MidiMessage msg
    msg.status = status
    msg.data1 = data1
    msg.data2 = data2
    AddEvent waitTime, msg
End Sub

Sub _MidiBuffer.AddEvent(waitTime As ULong, msg As MidiMessage)
    If bufferRecorded + MIDIEVENT_SIZE > bufferLength Then
        Dim As Any Ptr newBuffer = Reallocate(buffer, bufferLength + BUFFER_SIZE)
        If newBuffer = 0 Then Error 1
        buffer = newBuffer
    End If

    Dim As MIDIEVENT Ptr newEvent = buffer + bufferRecorded
	newEvent->dwDeltaTime = waitTime
	newEvent->dwStreamID = 0
	newEvent->dwEvent = msg.msg
    bufferRecorded += MIDIEVENT_SIZE
End Sub

Sub _MidiBuffer.AddFinishLock()
    If finishLock = 0 Then finishLock = MutexCreate()
End Sub

Constructor PlayStatus()
    instrument   = DEFAULT_INSTRUMENT
    volume       = DEFAULT_VOLUME
    octave       = DEFAULT_OCTAVE
    noteLength   = DEFAULT_NOTE_LENGTH
    tempo        = DEFAULT_TEMPO
    articulation = ARTICULATION_NORMAL
    mode         = MODE_FOREGROUND
    statusLock   = MutexCreate()
End Constructor

Destructor PlayStatus()
    MutexDestroy statusLock
End Destructor

Sub MidiCallback(byval hdrvr as HDRVR, byval uMsg as UINT, byval dwUser as DWORD_PTR, byval dw1 as DWORD_PTR, byval dw2 as DWORD_PTR)
    Dim As MIDIHDR Ptr header = Cast(MIDIHDR Ptr, dw1)

    Select Case uMsg
    Case MOM_POSITIONCB:
    Case MOM_DONE:
        If header = 0 Then Error 1
        Dim As MidiBuffer Ptr buffer = Cast(MidiBuffer Ptr, header->dwUser)
        midiOutUnprepareHeader(cast(HMIDIOUT, buffer->player->stream), header, sizeof(MIDIHDR))
        Delete header

        MutexLock buffer->player->dataLock
        If buffer = buffer->player->lastBuffer Then
            buffer->player->lastBuffer = 0
            midiStreamStop buffer->player->stream
        End If
        MutexUnlock buffer->player->dataLock

        If buffer->finishLock <> 0 Then
            MutexUnlock buffer->finishLock
        Else
            Delete buffer
        End If
    Case MOM_OPEN, MOM_CLOSE:
    End Select
End Sub

Const ASCII_LT    = ASC("<")
Const ASCII_GT    = ASC(">")
Const ASCII_PLUS  = ASC("+")
Const ASCII_MINUS = ASC("-")
Const ASCII_SHARP = ASC("#")
Const ASCII_DOT   = ASC(".")

Type StringParser
    Declare Constructor(text As String)
    Declare Function GetChar() As Integer
    Declare Function PeekChar() As Integer
    Declare Function ReadNumber() As Integer
    Declare Function HasNumber() As Boolean

    text As String
    offset As Integer
    length As Integer
End Type

Constructor StringParser(text As String)
    This.text = text
    This.length = Len(text)
End Constructor

Function StringParser.GetChar() As Integer
    Dim As Integer result = PeekChar()
    If offset < length Then offset += 1
    Return result
End Function

Function StringParser.PeekChar() As Integer
    While offset < length
        Dim As Integer char = text[offset]

        Select Case text[offset]
        Case VK_SPACE:
        Case VK_TAB:
        Case Else
            Return text[offset]
        End Select
        offset += 1
    Wend
    Return -1
End Function

Function StringParser.ReadNumber() As Integer
    Dim As Integer result = -1
    While offset < length
        Dim As UInteger n = PeekChar() - VK_0
        If n > 10 Then Return result
        If result < 0 Then result = 0
        result = 10 * result + n
        offset += 1
    Wend
    Return result
End Function

Dim Shared player As MidiPlayer
Dim Shared status As PlayStatus

Sub Play(commandstring As String)
    player.Initialize
    Dim As MidiBuffer Ptr buffer = New MidiBuffer

    MutexLock status.statusLock
    Dim parser As StringParser = UCase(commandstring)
    While True
        Dim As Integer char = parser.GetChar()
        Select Case char
        Case VK_A To VK_G
            Dim As Integer note = 2 * (char - VK_C)
            If note < 0 Then note += 14
            If note < 5 Then note += 1
            char = parser.PeekChar()
            If char = ASCII_PLUS Or char = ASCII_SHARP Then
                note += 1
                parser.GetChar()
                char = parser.PeekChar()
            ElseIf char = ASCII_MINUS Then
                note -= 1
                parser.GetChar()
                char = parser.PeekChar()
            End If
            note += 12 * status.octave
            Dim As Integer fullNote = WHOLE_NOTE
            Dim As Integer fullDelta = fullNote
            While char = ASCII_DOT
                fullDelta \= 2
                fullNote += fullDelta
                parser.GetChar()
                char = parser.PeekChar()
            Wend
            Dim As Integer noteLength = parser.ReadNumber()
            If noteLength <= 0 Or noteLength > SHORTEST_NOTE_LENGTH Then noteLength = status.noteLength
            If note > 0 And note <= MAX_NOTE Then
                noteLength = fullNote \ noteLength
                Dim As Integer playLength = noteLength * status.articulation \ ARTICULATION_MAX
                Dim As Integer restLength = noteLength - playLength
                note += MIDI_NOTE_BASE
                buffer->AddEvent &h90, note, status.volume
                buffer->AddEvent &h80, note, status.volume, playLength
                buffer->AddRest restLength
            End If
        Case VK_I
            Dim As Integer instrument = parser.ReadNumber()
            If instrument <> status.instrument And instrument >= 0 And instrument <= MAX_INSTRUMENT Then
                buffer->AddEvent &hC0, instrument, 0
                status.instrument = instrument
            End If
        Case VK_L
            Dim As Integer noteLength = parser.ReadNumber()
            If noteLength > 0 And noteLength <= SHORTEST_NOTE_LENGTH Then status.noteLength = noteLength
        Case VK_M
            Select Case parser.GetChar()
            Case VK_B
                status.mode = MODE_BACKGROUND
            Case VK_F
                status.mode = MODE_FOREGROUND
            Case VK_S
                status.articulation = ARTICULATION_STACCATO
            Case VK_N
                status.articulation = ARTICULATION_NORMAL
            Case VK_L
                status.articulation = ARTICULATION_LEGATO
            End Select
        Case VK_N
            Dim As Integer note = parser.ReadNumber()
            If note = 0 Then
                Dim As Integer restLength = WHOLE_NOTE \ status.noteLength
                buffer->AddRest restLength
            ElseIf note >= 0 And note <= MAX_NOTE Then
                Dim As Integer noteLength = WHOLE_NOTE \ status.noteLength
                Dim As Integer playLength = noteLength * status.articulation \ ARTICULATION_MAX
                Dim As Integer restLength = noteLength - playLength
                note += MIDI_NOTE_BASE

                buffer->AddEvent &h90, note, status.volume
                buffer->AddEvent &h80, note, status.volume, playLength
                buffer->AddRest restLength
            End If
        Case VK_O
            Dim As Integer octave = parser.ReadNumber()
            If octave >= 0 And octave <= MAX_OCTAVE Then status.octave = octave
        Case VK_P
            Dim As Integer restLength = parser.ReadNumber()
            buffer->AddRest WHOLE_NOTE \ restLength
        Case VK_T
            Dim As Integer tempo = parser.ReadNumber()
            If tempo <> status.tempo And tempo >= MIN_TEMPO And tempo <= MAX_TEMPO Then
                buffer->AddTempo TEMPO_1BPM \ tempo
                status.tempo = tempo
            End If
        Case VK_V
            Dim As Integer volume = parser.ReadNumber()
            If volume >= 0 And volume <= MAX_VOLUME Then status.volume = volume
        Case ASCII_LT
            If status.octave > 0 Then status.octave -= 1
        Case ASCII_GT
            If status.octave < MAX_OCTAVE Then status.octave += 1
        Case -1
            Exit While
        End Select
    Wend

    Dim As PlayMode mode = status.mode
    If mode = MODE_FOREGROUND Then buffer->AddFinishLock
    player.PlayBuffer buffer
    MutexUnlock status.statusLock

    If mode = MODE_FOREGROUND Then
        MutexLock buffer->finishLock
        MutexUnlock buffer->finishLock
        Delete buffer
    End If
End Sub

End Namespace

Sub Play(commandstring As String)
    QBPlay.Play commandstring
End Sub

Play "<EDC"
Sleep
