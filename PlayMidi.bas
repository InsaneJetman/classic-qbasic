' PlayMidi.bas
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

#lang "fb"

#include once "windows.bi"
#include once "win\mmsystem.bi"

Namespace QBPlay

' Constants
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
Const DEFAULT_INSTRUMENT   = 16         ' Hammond organ
Const MAX_VOLUME           = 127
Const DEFAULT_VOLUME       = MAX_VOLUME
Const TIME_DIVISION        = 240        ' ticks per beat
Const WHOLE_NOTE           = 4 * TIME_DIVISION
Const TEMPO_1BPM           = 60000000   ' micro-seconds / minute
Const BUFFER_SIZE          = 768
Const MIDIEVENT_SIZE       = 3 * SizeOf(DWORD)
Const MIDI_SET_INSTRUMENT  = &hC0
Const MIDI_KEY_DOWN        = &h90
Const MIDI_KEY_UP          = &h80

' Type And Sub Declarations

Declare Sub MidiCallback(ByVal hdrvr As HDRVR, ByVal uMsg As UINT, ByVal dwUser As DWORD_PTR, ByVal dw1 As DWORD_PTR, ByVal dw2 As DWORD_PTR)

Type MidiBuffer As _MidiBuffer

' MidiPlayer handles the midi function calls
Type MidiPlayer
    Declare Constructor()
    Declare Destructor()
    Declare Sub Initialize(tempo As Integer = DEFAULT_TEMPO, instrument As Integer = DEFAULT_INSTRUMENT)
    Declare Sub Shutdown()
    Declare Sub PlayBuffer(buffer As MidiBuffer Ptr)

    stream As HMIDISTRM
    streamLock As Any Ptr
    lastBuffer As MidiBuffer Ptr
    dataLock As Any Ptr
End Type

' Structure for MIDI messages
Union MidiMessage
    Msg As ULong
    Type Field = 1
        status As UByte
        data1  As UByte
        data2  As UByte
        code   As UByte
    End Type
End Union

' MidiBuffer collects a set of MIDI events
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

' The articulations supported by PLAY
Enum PlayArticulation
    ARTICULATION_STACCATO = 450,
    ARTICULATION_NORMAL   = 525,
    ARTICULATION_LEGATO   = 600,
    ARTICULATION_MAX      = 600
End Enum

' The modes supported by PLAY
Enum PlayMode
    MODE_FOREGROUND,
    MODE_BACKGROUND
End Enum

' PlayStatus keeps track of the current status of PLAY variables
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

' Type Implementations

Constructor MidiPlayer()
    streamLock = MutexCreate()
    dataLock = MutexCreate()
End Constructor

Destructor MidiPlayer()
    Shutdown
    MutexDestroy streamLock
    MutexDestroy dataLock
End Destructor

' Initialize the MIDI stream
Sub MidiPlayer.Initialize(tempo As Integer, instrument As Integer)
    MutexLock streamLock
    If stream = 0 Then
        ' open stream
        Dim As MMRESULT result
        result = midiStreamOpen(@stream, @result, 1, Cast(DWORD_PTR, @MidiCallback), 0, CALLBACK_FUNCTION)
        If result <> MMSYSERR_NOERROR Then Error 1

        ' set time division (ticks per beat)
        Dim As MIDIPROPTIMEDIV timeDiv
        timeDiv.cbStruct = SizeOf(MIDIPROPTIMEDIV)
        timeDiv.dwTimeDiv = TIME_DIVISION
        result = midiStreamProperty(stream, Cast(LPBYTE, @timeDiv), MIDIPROP_SET Or MIDIPROP_TIMEDIV)
        If result <> MMSYSERR_NOERROR Then Error 1

        ' set tempo
        Dim As MIDIPROPTEMPO tempoProp
        tempoProp.cbStruct = SizeOf(MIDIPROPTEMPO)
        tempoProp.dwTempo = TEMPO_1BPM \ tempo
        result = midiStreamProperty(stream, Cast(LPBYTE, @tempoProp), MIDIPROP_SET Or MIDIPROP_TEMPO)
        If result <> MMSYSERR_NOERROR Then Error 1

        ' set instrument
        Dim As MidiMessage msg
        msg.status = MIDI_SET_INSTRUMENT
        msg.data1 = instrument
        result = midiOutShortMsg(Cast(HMIDIOUT, stream), msg.msg)
        If result <> MMSYSERR_NOERROR Then Error 1
    End If
    MutexUnlock streamLock
End Sub

' Close MIDI stream
Sub MidiPlayer.Shutdown()
    MutexLock streamLock
    If stream <> 0 Then
        Dim As MMRESULT result = midiStreamClose(stream)
        If result <> MMSYSERR_NOERROR Then Error 1
        stream = 0
    End If
    MutexUnlock streamLock
End Sub

' Play the MIDI events collected in a MidiBuffer
Sub MidiPlayer.PlayBuffer(buffer As MidiBuffer Ptr)
    Dim As MIDIHDR Ptr header
    Dim As ULong result

    ' record the last buffer added so that the stream can be stopped after it has finished playing
    MutexLock dataLock
    lastBuffer = buffer
    MutexUnlock dataLock

    ' modify buffer for playing
    buffer->player = @This

    ' enqueue and play the buffer
    header = New MIDIHDR
    header->lpData = buffer->buffer
    header->dwBufferLength = buffer->bufferLength
    header->dwBytesRecorded = buffer->bufferRecorded
    header->dwUser = Cast(DWORD_PTR, buffer)

    result = midiOutPrepareHeader(Cast(HMIDIOUT, stream), header, SizeOf(MIDIHDR))
    If result <> MMSYSERR_NOERROR Then Error 1

    result = midiStreamOut(stream, header, SizeOf(MIDIHDR))
    If result <> MMSYSERR_NOERROR Then Error 1

    result = midiStreamRestart(stream)
    If result <> MMSYSERR_NOERROR Then Error 1
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

' Add a rest (no-op) to the MIDI buffer
Sub _MidiBuffer.AddRest(waitTime As UInteger)
    If waitTime <= 0 Then Exit Sub
    Dim As MidiMessage msg
    msg.code = MEVT_NOP
    AddEvent waitTime, msg
End Sub

' Add a tempo change to the MIDI buffer
Sub _MidiBuffer.AddTempo(tempo As UInteger)
    Dim As MidiMessage msg
    msg.msg = tempo
    msg.code = MEVT_TEMPO
    AddEvent 0, msg
End Sub

' Construct and add a MIDI event to the MIDI buffer
Sub _MidiBuffer.AddEvent(status As UInteger, data1 As UInteger, data2 As UInteger, waitTime As UInteger)
    Dim As MidiMessage msg
    msg.status = status
    msg.data1 = data1
    msg.data2 = data2
    AddEvent waitTime, msg
End Sub

' Add a MIDI event to the MIDI buffer
Sub _MidiBuffer.AddEvent(waitTime As ULong, msg As MidiMessage)
    ' increase the buffer size if necessary
    If bufferRecorded + MIDIEVENT_SIZE > bufferLength Then
        Dim As Any Ptr newBuffer = Reallocate(buffer, bufferLength + BUFFER_SIZE)
        If newBuffer = 0 Then Error 1
        buffer = newBuffer
    End If

    ' add event to buffer
    Dim As MIDIEVENT Ptr newEvent = buffer + bufferRecorded
	newEvent->dwDeltaTime = waitTime
	newEvent->dwStreamID = 0
	newEvent->dwEvent = msg.msg
    bufferRecorded += MIDIEVENT_SIZE
End Sub

' Add a lock to the MidiBuffer to be unlocked when it has finished playing
Sub _MidiBuffer.AddFinishLock()
    If finishLock = 0 Then
        finishLock = MutexCreate()
        MutexLock finishLock
    End If
End Sub

' PlayStatus is initialized with default values
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

' MIDI Callback function handles clean-up of MIDI buffers after they are completed
Sub MidiCallback(ByVal hdrvr As HDRVR, ByVal uMsg As UINT, ByVal dwUser As DWORD_PTR, ByVal dw1 As DWORD_PTR, ByVal dw2 As DWORD_PTR)
    Dim As MIDIHDR Ptr header = Cast(MIDIHDR Ptr, dw1)

    Select Case uMsg
    Case MOM_POSITIONCB:
    Case MOM_DONE:
        ' clean up the MIDI header used to start playing the buffer
        If header = 0 Then Error 1
        Dim As MidiBuffer Ptr buffer = Cast(MidiBuffer Ptr, header->dwUser)
        If buffer = 0 Then Error 1
        midiOutUnprepareHeader(Cast(HMIDIOUT, buffer->player->stream), header, SizeOf(MIDIHDR))
        Delete header

        ' stop the stream if this was the last buffer in the queue
        MutexLock buffer->player->dataLock
        If buffer = buffer->player->lastBuffer Then
            buffer->player->lastBuffer = 0
            midiStreamStop buffer->player->stream
        End If
        MutexUnlock buffer->player->dataLock

        ' either unlock the buffer, or clean it up
        If buffer->finishLock <> 0 Then
            MutexUnlock buffer->finishLock
        Else
            Delete buffer
        End If
    Case MOM_OPEN, MOM_CLOSE:
    End Select
End Sub

' ASCII codes used by PLAY (VK_KEYs are used for characters and numbers)
Const ASCII_LT    = Asc("<")
Const ASCII_GT    = Asc(">")
Const ASCII_PLUS  = Asc("+")
Const ASCII_MINUS = Asc("-")
Const ASCII_SHARP = Asc("#")
Const ASCII_DOT   = Asc(".")

' A simple string parser for parsing the PLAY command string
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

' Get a character from the string
Function StringParser.GetChar() As Integer
    Dim As Integer result = PeekChar()
    If offset < length Then offset += 1
    Return result
End Function

' Peek at the next character in the string
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

' Read a number from the string
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

' MidiPlayer and PlayStatus objects used by PLAY
Dim Shared player As MidiPlayer
Dim Shared status As PlayStatus

' Implemention of the original QBasic PLAY function
Sub Play(commandstring As String)
    ' initialize the MidiPlayer (if necessary) and create a new MidiBuffer
    player.Initialize status.tempo, status.instrument
    Dim As MidiBuffer Ptr buffer = New MidiBuffer

    ' parse the command string
    MutexLock status.statusLock
    Dim parser As StringParser = UCase(commandstring)
    While True
        Dim As Integer char = parser.GetChar()
        Select Case char
        ' play a lettered note
        Case VK_A To VK_G
            ' computer note number
            Dim As Integer note = 2 * (char - VK_C)
            If note < 0 Then note += 14
            If note < 5 Then note += 1
            char = parser.PeekChar()
            note += 12 * status.octave
            ' handle sharp/flat modifier
            If char = ASCII_PLUS Or char = ASCII_SHARP Then
                note += 1
                parser.GetChar()
                char = parser.PeekChar()
            ElseIf char = ASCII_MINUS Then
                note -= 1
                parser.GetChar()
                char = parser.PeekChar()
            End If
            ' check for dots on the note
            Dim As Integer fullNote = WHOLE_NOTE
            Dim As Integer fullDelta = fullNote
            While char = ASCII_DOT
                fullDelta \= 2
                fullNote += fullDelta
                parser.GetChar()
                char = parser.PeekChar()
            Wend
            ' read note length if specified
            Dim As Integer noteLength = parser.ReadNumber()
            If noteLength <= 0 Or noteLength > SHORTEST_NOTE_LENGTH Then noteLength = status.noteLength
            ' add note to the buffer
            If note > 0 And note <= MAX_NOTE Then
                noteLength = fullNote \ noteLength
                Dim As Integer playLength = noteLength * status.articulation \ ARTICULATION_MAX
                Dim As Integer restLength = noteLength - playLength
                note += MIDI_NOTE_BASE
                buffer->AddEvent MIDI_KEY_DOWN, note, status.volume
                buffer->AddEvent MIDI_KEY_UP, note, status.volume, playLength
                buffer->AddRest restLength
            End If
        ' set the instrument
        Case VK_I
            Dim As Integer instrument = parser.ReadNumber()
            If instrument <> status.instrument And instrument >= 0 And instrument <= MAX_INSTRUMENT Then
                buffer->AddEvent MIDI_SET_INSTRUMENT, instrument, 0
                status.instrument = instrument
            End If
        ' set the note length
        Case VK_L
            Dim As Integer noteLength = parser.ReadNumber()
            If noteLength > 0 And noteLength <= SHORTEST_NOTE_LENGTH Then status.noteLength = noteLength
        ' mode change (playback or articulation)
        Case VK_M
            Select Case parser.GetChar()
            ' change playback mode
            Case VK_B
                status.mode = MODE_BACKGROUND
            Case VK_F
                status.mode = MODE_FOREGROUND
            ' change articulation
            Case VK_S
                status.articulation = ARTICULATION_STACCATO
            Case VK_N
                status.articulation = ARTICULATION_NORMAL
            Case VK_L
                status.articulation = ARTICULATION_LEGATO
            End Select
        ' play note
        Case VK_N
            Dim As Integer note = parser.ReadNumber()
            If note = 0 Then
                ' rest
                Dim As Integer restLength = WHOLE_NOTE \ status.noteLength
                buffer->AddRest restLength
            ElseIf note >= 0 And note <= MAX_NOTE Then
                ' note
                Dim As Integer noteLength = WHOLE_NOTE \ status.noteLength
                Dim As Integer playLength = noteLength * status.articulation \ ARTICULATION_MAX
                Dim As Integer restLength = noteLength - playLength
                note += MIDI_NOTE_BASE
                buffer->AddEvent MIDI_KEY_DOWN, note, status.volume
                buffer->AddEvent MIDI_KEY_UP, note, status.volume, playLength
                buffer->AddRest restLength
            End If
        ' change octave
        Case VK_O
            Dim As Integer octave = parser.ReadNumber()
            If octave >= 0 And octave <= MAX_OCTAVE Then status.octave = octave
        ' pause playback (rest)
        Case VK_P
            Dim As Integer restLength = parser.ReadNumber()
            buffer->AddRest WHOLE_NOTE \ restLength
        ' change tempo
        Case VK_T
            Dim As Integer tempo = parser.ReadNumber()
            If tempo <> status.tempo And tempo >= MIN_TEMPO And tempo <= MAX_TEMPO Then
                buffer->AddTempo TEMPO_1BPM \ tempo
                status.tempo = tempo
            End If
        ' change volume
        Case VK_V
            Dim As Integer volume = parser.ReadNumber()
            If volume >= 0 And volume <= MAX_VOLUME Then status.volume = volume
        ' decrease octave
        Case ASCII_LT
            If status.octave > 0 Then status.octave -= 1
        ' increase octave
        Case ASCII_GT
            If status.octave < MAX_OCTAVE Then status.octave += 1
        ' finished
        Case -1
            Exit While
        End Select
    Wend

    ' play the buffer
    Dim As PlayMode mode = status.mode
    If mode = MODE_FOREGROUND Then buffer->AddFinishLock
    player.PlayBuffer buffer
    MutexUnlock status.statusLock

    ' wait for the buffer to finish playing when in foreground playback mode
    If mode = MODE_FOREGROUND Then
        MutexLock buffer->finishLock
        MutexUnlock buffer->finishLock
        Delete buffer
    End If
End Sub

End Namespace

' Expose the PLAY function outside the QBPlay namespace
Sub Play(commandstring As String)
    QBPlay.Play commandstring
End Sub
