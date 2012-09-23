;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     TapeIO.asm                                         ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;             TapeIO Verarbeitung der Turingmaschine
;**=================================================================**

.186

INCLUDE ERRORIO.ASH
INCLUDE SCREENIO.ASH
INCLUDE FILEIO.ASH
INCLUDE UTILS.ASH
INCLUDE TURING.ASH

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING

;***********************************************************************
;********               Konstanten von TapeIO                   ********
;***********************************************************************

; Speicherverwaltung fr Tape
kMaxTapePara            EQU 2300               ;Paragraphs (a 16 Bytes)
kMaxTapeLength          EQU 16*kMaxTapePara    ;Bytes

; Farben fr TapeIO
kTapeBordCol            EQU kColWhite  + kColBlue SHL 4
kCursorColor            EQU kColRed    + kColBlue SHL 4
kEditCursorColor        EQU kColYellow + kColBlue SHL 4
kTapeColor              EQU kColGreen  + kColBlack SHL 4
kTapeEditColor          EQU kColBlack  + kColGreen SHL 4
kTapeColLoad            EQU kColBlack  + kColGray SHL 4

; Tape Anzeige
kTapeDisplay            EQU 68
kTapeDisplayPos         EQU 4 + 4 SHL 8
kTapeCursorNULL         EQU 6 + 3 SHL 8


;***********************************************************************
;********                   TapeIO-Struktur                     ********
;***********************************************************************
TAPETYPE        STRUCT
  Tape           DD ?            ; Pointer auf Band
  TapePos        DW ?            ; aktuelle Position auf Band
  DisplayPos     DW ?            ; Position auf Bildschirm [1..64]+10
  DisplayChar    DB ?            ; Positionspointerart
  DisplayCharCol DB ?            ; Farbe des Positionszeigers
TAPETYPE        ENDS


_DATA           SEGMENT PARA PUBLIC 'DATA'
;***********************************************************************
;********                 Daten von TapeIO                      ********
;***********************************************************************

; Hauptstuktur fr Tapeinformationen
TapeInfo                TAPETYPE <0, 0>

; Neue Titel fr Editmodus
szTTitelEdit    DB "Turing-Meais",161,"n V 1.00 - Tape-Edit-Mode",0
szTEditMenu     DB "&<Char&>-Change to char,  &<-move left,  &>-Move right,  &E&S&C-Exit editmode",0
szTBlank        DB "&I&N&S-Insert Char...,  &D&E&L-Delete Char from Tape",0

; Tapefenster
szTapeBorderO           DB "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Tape ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿",0
szTapeBorderM           DB "³                                                                      ³",0
szTapeBorderU           DB "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ",0

; Messagefenster von TapeIO
szTapeReadO             DB "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ TapeIO-Message ÍÍÍÍÍÍÍÍÍÍÍÍÍ»",0
szTapeReadM             DB "º       Reading tapedata from file...      º",0
szTapeReadU             DB "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼",0
szTapeWriteO            DB "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ TapeIO-Message ÍÍÍÍÍÍÍÍÍÍÍÍÍ»",0
szTapeWriteM            DB "º         Write tapedata to file...        º",0
szTapeWriteU            DB "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼",0

; Filetexte fr Tape
szTapeFileText1         DB "# Tape fr Turing Maschine", 0Dh, 0Ah, 0
szTapeFileText2         DB 0Dh, 0Ah, "# Tape-Text", 0Dh, 0Ah, 0

; Allgemeine Variablen
wDisplayPosBak          DW ?                    ; Zeigerposition Zwischenspeicher 
bDisplayCharBak         DB ?                    ; Pointer Zwischenspeicher
wTapePosBak             DW ?
wTFileHandle            DW ?                    ; Variable fr Filehandle
bTChar                  DB ?                    ; Variable zum Einlesen eines Zeichen eines Files
wTDummy                 DW ?                    ; Temp-Variable
szTTemp                 DB 300 DUP('$')         ; Variable zum einlesen einer Zeile eines Files
szTFilenameRead         DB "standard.tap",0     ; Tape-File zum einlesen
szTFilenameWrite        DB "save.tap",0         ; Tape-File zum speichern
wTFilenameOff           DW 0                    ; OFFSET fr Filename
wTScreen                DW 0                    ; Speichersegment fr Bildschirm

; Fehlermeldungen von TapeIO
szTErrorAllocMem        DB "Can't allocate more memory.",0
szTErrorOpenFile        DB "Couln't find tape-file.",0
szTErrorCreateFile      DB "Couln't create tape-file.",0
szTErrorStartPos        DB "Start-Position is not a number.",0
szTErrorReadFile        DB "Couln't read from the tape file.",0
szTErrorUndefChar       DB "Chars in tapefile are undefined.",0
szTErrorWriteFile       DB "Couln't write to file.",0
szTErrorCloseFile       DB "Couln't close tape file.",0
szTapeEndFound          DB "The Tape was at the end",0
szTErrorNoData          DB "Couln't found tapedata in the file.",0

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********               Prozeduren von TapeIO                   ********
;***********************************************************************

;***********************************************************************
; TapePrintAll
;   - Rahmen von Tape zeichnen
;   - aktuelle Tapedaten anzeigen
;   - Tapepointer-Cursor positionieren
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapePrintAll    PROC NEAR
                PUSH    AX
                PUSH    BX

                MOV     AL,kTapeBordCol
                CALL    SetColor

                MOV     BX,kTapeDisplayPos -2 SHL 8
                MOV     AX,OFFSET szTapeBorderO
                CALL    PrintString

                MOV     BX,kTapeDisplayPos -1 SHL 8
                MOV     AX,OFFSET szTapeBorderM
                CALL    PrintString

                MOV     BX,kTapeDisplayPos
                MOV     AX,OFFSET szTapeBorderM
                CALL    PrintString

                MOV     BX,kTapeDisplayPos +1 SHL 8
                MOV     AX,OFFSET szTapeBorderU
                CALL    PrintString

                ; Cursor in TapeFrame zeichnen
                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,TapeInfo.DisplayChar
                MOV     AH,TapeInfo.DisplayCharCol
                CALL    PrintChar

                ; Tape ausgeben
                CALL    TapePrint

                POP     BX
                POP     AX
                RET
TapePrintAll    ENDP


;***********************************************************************
; TapePrint
;   - aktuelle Tapedaten in Tapebereich schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapePrint       PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DI

                MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                MOV     DI,TapeInfo.TapePos             ; Aktuelle TapePos
                SUB     DI,TapeInfo.DisplayPos          ; Offset subtrahieren
@@Weiter1:      MOV     CX,kTapeDisplay
                MOV     AH,kTapeColor
                MOV     BX,kTapeDisplayPos +2
@@TapeLoop:     MOV     AL,BYTE PTR ES:[DI]
                CALL    PrintChar
                INC     DI
                INC     BX
                CMP     DI,kMaxTapeLength
                JA      @@Ende
                LOOP    @@TapeLoop

@@Ende:         POP     DI
                POP     CX
                POP     BX
                POP     AX
                RET
TapePrint       ENDP


;***********************************************************************
; TapeReadChar
;   - Zeichen von aktueller Tapepointer-Position lesen und zurckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: Zeichen der aktuellen Position
;-----------------------------------------------------------------------
TapeReadChar    PROC NEAR
                PUSH    DI
                PUSH    ES

                MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                MOV     DI,TapeInfo.TapePos

                MOV     AL,BYTE PTR ES:[DI]
                CMP     AL,0                    ; kein definierter Wert von Band ?
                JNZ     @@Ende
                MOV     AL,1                    ; einen definierten Wert zurckgeben

@@Ende:         POP     ES
                POP     DI
                RET
TapeReadChar    ENDP


;***********************************************************************
; TapeWriteChar
;   - Zeichen an aktueller Tapepointer-Position schreiben
;-----------------------------------------------------------------------
; Input:
;   - AL: das Zeichen
; Returnwert:
;-----------------------------------------------------------------------
TapeWriteChar   PROC NEAR
                PUSH    AX
                PUSH    DI

                MOV     DI,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,DI
                MOV     DI,TapeInfo.TapePos

                MOV     BYTE PTR ES:[DI],AL

                POP     DI
                POP     AX
                RET
TapeWriteChar   ENDP


;***********************************************************************
; TapeLeftShift
;   - Positionszeiger des Tapes um eins nach links (<) verschieben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeLeftShift   PROC NEAR
                PUSH    AX
                PUSH    BX

                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,' '
                MOV     AH,kColBlue SHL 4
                CALL    PrintChar

                DEC     TapeInfo.TapePos
                JNS     @@Weiter1

                ; Bandende erreicht
                MOV     AX,OFFSET szTapeEndFound
                CALL    NormalError
                MOV     wRunState,kStopped
                CALL    PrintMainMenu
                JMP     @@Ende

@@Weiter1:      DEC     TapeInfo.DisplayPos
                JNS     @@NoEOT
                MOV     TapeInfo.DisplayPos,kTapeDisplay /2

@@NoEOT:        MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,TapeInfo.DisplayChar
                MOV     AH,TapeInfo.DisplayCharCol
                CALL    PrintChar
                CALL    TapePrint

@@Ende:         POP     BX
                POP     AX
                RET
TapeLeftShift   ENDP


;***********************************************************************
; TapeRightShift
;   - Positionszeiger des Tapes um eins nach rechts (>) verschieben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeRightShift  PROC NEAR
                PUSH    AX
                PUSH    BX

                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,' '
                MOV     AH,kColBlue SHL 4
                CALL    PrintChar

                INC     TapeInfo.TapePos
                CMP     TapeInfo.TapePos,kMaxTapeLength
                JC      @@Weiter1

                ; Bandende erreicht
                MOV     AX,OFFSET szTapeEndFound
                CALL    NormalError
                MOV     wRunState,kStopped
                CALL    PrintMainMenu
                JMP     @@Ende

@@Weiter1:      INC     TapeInfo.DisplayPos
                CMP     TapeInfo.DisplayPos,kTapeDisplay
                JB      @@NoEOT               ; DisplayPos <= kTapeDisplay
                MOV     TapeInfo.DisplayPos,kTapeDisplay /2

@@NoEOT:        MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,TapeInfo.DisplayChar
                MOV     AH,TapeInfo.DisplayCharCol
                CALL    PrintChar
                CALL    TapePrint

@@Ende:         POP     BX
                POP     AX
                RET
TapeRightShift  ENDP


;***********************************************************************
; TapeEdit
;   - Edit-Modus aufrufen
;   - neues Men ausgeben
;   - eigene Eingabeschleife
;   - TapeEdit kann mit ESC verlassen werden
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeEdit        PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                ; Titel ausgeben
                MOV     AL,kMenuColor
                CALL    SetColor
                MOV     AX,OFFSET szTTitelEdit
                CALL    PrintTitle

                ; Editmen ausgeben
                MOV     AL,kMenuColor
                CALL    SetColor
                MOV     AX,OFFSET szTBlank
                MOV     BL,kMenuSelColor
                MOV     BH,23
                CALL    PrintMenu
                MOV     AX,OFFSET szTEditMenu
                MOV     BL,kMenuSelColor
                MOV     BH,24
                CALL    PrintMenu

                MOV     AL,kTapeEditColor
                CALL    SetColor

                MOV     AX,TapeInfo.DisplayPos
                MOV     wDisplayPosBak,AX
                MOV     AL,TapeInfo.DisplayChar
                MOV     bDisplayCharBak,AL
                MOV     TapeInfo.DisplayCharCol,kEditCursorColor
                MOV     TapeInfo.DisplayChar,''                

                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                MOV     AL,TapeInfo.DisplayChar
                MOV     AH,TapeInfo.DisplayCharCol
                CALL    PrintChar

                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar

@@EditLoop:     MOV     AX,0700h
                INT     21h               

                CMP     AL,0
                JNZ     @@WeiterN1
                MOV     AX,0700h
                INT     21h

                ; Cursor nach links schieben
                CMP     AL,04Bh
                JNZ     @@WeiterE1
                CALL    TapeLeftShift

                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar
                JMP     @@EditLoop

                ; Cursor nach rechts schieben
@@WeiterE1:     CMP     AL,04Dh
                JNZ     @@WeiterE2
                CALL    TapeRightShift

                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar
                JMP     @@EditLoop

                ; Insert Character
@@WeiterE2:     CMP     AL,052h
                JNZ     @@WeiterE3

                ; an Tapeanfang oder Tapeende schieben?
                MOV     AX,kMaxTapeLength /2
                CMP     TapeInfo.TapePos,AX
                JNC     @@GroesserI
                CLD
                XOR     DI,DI
                MOV     SI,1
                MOV     CX,TapeInfo.TapePos
                JMP     @@WeiterI1
@@GroesserI:    STD
                MOV     DI,kMaxTapeLength -1
                DEC     DI
                MOV     CX,DI
                MOV     SI,DI
                DEC     SI
                SUB     CX,TapeInfo.TapePos

                ; Segmente mit Tapeadresse laden
@@WeiterI1:     PUSH    DS
                MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                MOV     DS,AX
                REP     MOVSB
                POP     DS
                CLD
                MOV     AL,'@'
                CALL    TapeWriteChar
                CALL    TapePrint
                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar
                JMP     @@EditLoop

                ; Delete Character
@@WeiterE3:     CMP     AL,053h
                JNZ     @@WeiterE4

                ; an Tapeanfang oder Tapeende schieben?
                MOV     AX,kMaxTapeLength /2
                CMP     TapeInfo.TapePos,AX
                JNC     @@GroesserD
                STD
                MOV     DI,TapeInfo.TapePos
                MOV     SI,DI
                DEC     SI
                MOV     CX,SI
                JMP     @@WeiterD1
@@GroesserD:    CLD
                MOV     DI,TapeInfo.TapePos
                MOV     SI,DI
                INC     SI
                MOV     CX,kMaxTapeLength -1
                SUB     CX,SI

                ; Segmente mit Tapeadresse laden
@@WeiterD1:     PUSH    DS
                MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                MOV     DS,AX
                REP     MOVSB
                MOV     BYTE PTR DS:[SI],0              ; Leere Zelle auf 0 setzen
                POP     DS
                CLD
                CALL    TapePrint
                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar
                JMP     @@EditLoop

@@WeiterE4:     JMP     @@EditLoop


@@WeiterN1:     CMP     AL,27
                JNZ     @@WeiterN2
                JMP     @@Ende

@@WeiterN2:     CMP     AL,33
                JC      @@EditLoop
                CMP     AL,255
                JZ      @@EditLoop
                CALL    TapeWriteChar
                MOV     BX,kTapeCursorNULL
                ADD     BX,TapeInfo.DisplayPos
                ADD     BX,1 SHL 8                  ; auf n„chste Zeile               
                CALL    TapeReadChar
                MOV     AH,kTapeEditColor
                CALL    PrintChar
                JMP     @@EditLoop

@@Ende:         MOV     AX,wDisplayPosBak
                MOV     TapeInfo.DisplayPos,AX
                MOV     AL,bDisplayCharBak
                MOV     TapeInfo.DisplayChar,AL
                MOV     TapeInfo.DisplayCharCol,kCursorColor

                CALL    TapePrintAll

                POP     SI
                POP     ES
                POP     DI
                POP     CX
                POP     BX
                POP     AX
                RET
TapeEdit        ENDP


;***********************************************************************
; TapeWrite
;   - aktuelle Banddaten in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert: -
;-----------------------------------------------------------------------
TapeWrite       PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI
                PUSH    DS
                PUSH    SI

                CMP     AX,0FFFFh
                JZ      @@StandardFile
                MOV     wTFilenameOff,AX
                JMP     @@AllesKlar0
@@StandardFile: MOV     AX,OFFSET szTFilenameWrite
                MOV     wTFilenameOff,AX

                ; Messagebox ausgeben
@@AllesKlar0:   CALL    SaveScreen
                MOV     wTScreen,AX

                ; Farbe fr Messagebox setzen
                MOV     AL,kTapeColLoad
                CALL    SetColor

                ; Messagebox zeichnen
                MOV     AX,OFFSET szTapeWriteO
                MOV     BX,9 SHL 8 + 18
                CALL    PrintString
                MOV     AX,OFFSET szTapeWriteM
                MOV     BX,(10 SHL 8) + 18
                CALL    PrintString
                MOV     AX,OFFSET szTapeWriteU
                MOV     BX,11 SHL 8 + 18
                CALL    PrintString

                ; Tapefile kreieren
                MOV     DX,wTFilenameOff
                MOV     CX,0
                MOV     AH,3Ch
                INT     21h
                JNC     @@AllesKlar1
                MOV     AX,OFFSET szTErrorCreateFile
                CALL    NormalError
                JMP     @@Ende

@@AllesKlar1:   MOV     wTFileHandle,AX

                ; Allgemeine Beschreibung des Files
                MOV     AX,OFFSET szTapeFileText1
                MOV     BX,wTFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@AllesKlar2
                MOV     AX,OFFSET szTErrorWriteFile
                CALL    NormalError
                JMP     @@Close

                ; TapePos um den Offset im Speicher verkleinern
@@AllesKlar2:   MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                XOR     DI,DI                   ; vom Anfang des Tapes
                MOV     AL,0                    ; suchen nach 0
                MOV     CX,0FFFFh               ; max. Tapel„nge
                CLD                             ; DI incrementieren
                REPE    SCASB                   ; solange ES:[DI]==0
                MOV     wTDummy,DI               ; DI fr weitere Verarbeitung retten
                DEC     wTDummy
                MOV     AX,TapeInfo.TapePos
                SUB     AX,DI                   ; TapePos um DI verkleinern

                ; TapePosition in File schreiben
                MOV     BX,OFFSET szTTemp
                CALL    WORDToString            ; AX -> String (OFFSET in AX)
                MOV     BX,wTFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@AllesKlar3
                MOV     AX,OFFSET szTErrorWriteFile
                CALL    NormalError
                JMP     @@Close

@@AllesKlar3:   MOV     AX,OFFSET szTapeFileText2
                MOV     BX,wTFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@AllesKlar4
                MOV     AX,OFFSET szTErrorWriteFile
                CALL    NormalError
                JMP     @@Close

                ; Nun noch das Band speichern
@@AllesKlar4:   MOV     AX,WORD PTR [TapeInfo.Tape+2]
                MOV     ES,AX
                MOV     DI,wTDummy               ; Offset von erstem Zeichen != 0

@@WriteMore:    MOV     SI,OFFSET szTTemp
                MOV     CX,80

@@WriteLoop:    MOV     AL,BYTE PTR ES:[DI]
                INC     DI
                CMP     AL,0
                JZ      @@WriteEnd
                MOV     BYTE PTR [SI],AL
                INC     SI
                LOOP    @@WriteLoop

                INC     SI
                MOV     BYTE PTR [SI],0Dh
                INC     SI
                MOV     BYTE PTR [SI],0Ah
                INC     SI
                MOV     BYTE PTR [SI],0
                MOV     AX,OFFSET szTTemp
                MOV     BX,wTFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@WriteMore
                MOV     AX,OFFSET szTErrorWriteFile
                CALL    NormalError
                JMP     @@Close

@@WriteEnd:     MOV     BYTE PTR [SI],0Dh
                INC     SI
                MOV     BYTE PTR [SI],0Ah
                INC     SI
                MOV     BYTE PTR [SI],0
                MOV     AX,OFFSET szTTemp
                MOV     BX,wTFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@Close
                MOV     AX,OFFSET szTErrorWriteFile
                CALL    NormalError
                JMP     @@Close

@@Close:        MOV     AH,3Eh
                MOV     BX,wTFileHandle
                INT     21h
                JNC     @@Ende
                MOV     AX,OFFSET szTErrorCloseFile
                CALL    NormalError

                ; Alter Bildschirm wiederherstellen
@@Ende:         MOV     AX,wTScreen
                CALL    RestoreScreen

                ; Register wiederherstellen
                POP     SI
                POP     DS
                POP     DI
                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
TapeWrite       ENDP


;***********************************************************************
; TapeRead
;   - Banddaten von File lesen
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert:
;-----------------------------------------------------------------------
TapeRead        PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI

                CMP     AX,0FFFFh
                JZ      @@StandardFile
                MOV     wTFilenameOff,AX
                JMP     @@AllesKlar00
@@StandardFile: MOV     AX,OFFSET szTFilenameRead
                MOV     wTFilenameOff,AX

                ; Messagebox ausgeben
@@AllesKlar00:  CALL    SaveScreen
                MOV     wTScreen,AX

                MOV     AL,kTapeColLoad
                CALL    SetColor

                MOV     AX,OFFSET szTapeReadO
                MOV     BX,9 SHL 8 + 18
                CALL    PrintString
                MOV     AX,OFFSET szTapeReadM
                MOV     BX,(10 SHL 8) + 18
                CALL    PrintString
                MOV     AX,OFFSET szTapeReadU
                MOV     BX,11 SHL 8 + 18
                CALL    PrintString

                ; Speicherbereicht reservieren fr Tape
                MOV     AH,48h
                MOV     BX,kMaxTapePara
                INT     21h
                JNC     @@AllesKlar0
                PUSH    OFFSET szTErrorAllocMem
                JMP     FatalError

                ; Segmentadresse der Speicherstelle ablegen in TapeInfo.Tape
@@AllesKlar0:   MOV     WORD PTR [TapeInfo.Tape+2],AX
                MOV     WORD PTR [TapeInfo.Tape],0

                ; ganzer Speicherbereich auf '\0' setzen
                MOV     ES,AX
                XOR     DI,DI
                XOR     AX,AX
                MOV     CX,kMaxTapeLength
                CLD
                REP     STOSB

                ; Tapefile 'tape.txt' ”ffnen
                MOV     DX,wTFilenameOff
                MOV     AL,00
                MOV     AH,3Dh
                INT     21h
                JNC     @@AllesKlar1
                MOV     AX,OFFSET szTErrorOpenFile
                CALL    NormalError
                JMP     @@ErrorEnd

@@AllesKlar1:   MOV     wTFileHandle,AX

                ; Kommentare mit '#' berspringen
@@WaitForSPos:  MOV     AX,OFFSET szTTemp
                MOV     BX,wTFileHandle
                CALL    FileReadLine
                CMP     AX,1
                JNZ     @@AllesKlar11
                PUSH    OFFSET szTErrorNoData
                JMP     FatalError

@@AllesKlar11:  CMP     AX,0FFFFh
                JNZ     @@AllesKlar12
                PUSH    OFFSET szTErrorReadFile
                JMP     FatalError

@@AllesKlar12:  CMP     szTTemp,'#'
                JZ      @@WaitForSPos

                ; Startposition des Pointers aus File lesen
                MOV     AX,OFFSET szTTemp
                CALL    StringToWORD
                CMP     AX,0FFFFh
                JNZ     @@AllesKlar2
                PUSH    OFFSET szTErrorStartPos
                JMP     FatalError

@@AllesKlar2:   MOV     TapeInfo.TapePos,AX

                ; Bandmitte von File bestimmen
                MOV     AH,42h
                MOV     AL,1                    ; Current-Location
                MOV     BX,wTFileHandle
                XOR     CX,CX
                XOR     DX,DX
                INT     21h
                JNC     @@AllesKlar3
                PUSH    OFFSET szTErrorReadFile
                JMP     FatalError

@@AllesKlar3:   MOV     wTDummy,AX
                MOV     BX,wTFileHandle
                CALL    FileGetLength
                SUB     BX,wTDummy
                CMP     BX,kMaxTapeLength
                JB      @@AllesKlar4
                PUSH    OFFSET szTErrorReadFile
                JMP     FatalError

                ; Offset fr Tapedaten von File berechnen
@@AllesKlar4:   SHR     BX,1                    ; Filelaenge halbieren
                MOV     DI,kMaxTapeLength /2
                SUB     DI,BX
                MOV     ES,WORD PTR [TapeInfo.Tape+2]

                ; Daten aus Tapefile lesen und in Tapespeicher schreiben
@@ReadLoop:     MOV     AH,3Fh
                MOV     BX,wTFileHandle
                MOV     DX,OFFSET bTChar
                MOV     CX,1
                INT     21h
                JNC     @@AllesKlar5
                PUSH    OFFSET szTErrorReadFile
                JMP     FatalError

@@AllesKlar5:   OR      AX,AX                   ; EOF erreicht
                JZ      @@FileEOF

                ; Return oder LF in bTChar
                CMP     bTChar,0Dh
                JZ      @@ReadLoop
                CMP     bTChar,0Ah
                JZ      @@ReadLoop

                ; Ungltiges Zeichen in bTChar -> Ende  (Gltig: 32 < bTChar < 255)
                CMP     bTChar,33
                JNC     @@AllesKlar51
                PUSH    OFFSET szTErrorUndefChar
                JMP     FatalError

@@AllesKlar51:  CMP     bTChar,255
                JNE     @@AllesKlar52
                PUSH    OFFSET szTErrorUndefChar
                JMP     FatalError

                ; Kommentar in Tape.txt ?
@@AllesKlar52:  CMP     bTChar,'#'
                JZ      @@GartenhLoop

                ; Zeichen in Tape einfgen
                MOV     AL,bTChar
                CLD
                STOSB                           ; Zeichen in Memory Schreiben
                JMP     @@ReadLoop

                ; Kommentarzeile berspringen
@@GartenhLoop:  MOV     AH,3Fh
                MOV     BX,wTFileHandle
                MOV     DX,OFFSET bTChar
                MOV     CX,1
                INT     21h
                JNC     @@AllesKlar6
                PUSH    OFFSET szTErrorReadFile
                JMP     FatalError

@@AllesKlar6:   OR      AX,AX                   ; EOF erreicht
                JZ      @@FileEOF
                CMP     bTChar,13
                JC      @@ReadLoop
                CMP     bTChar,10
                JZ      @@ReadLoop
                JMP     @@GartenhLoop

                ; EOF des Tapefiles erreicht
@@FileEOF:      MOV     AH,3Eh
                MOV     BX,wTFileHandle
                INT     21h
                JNC     @@AllesKlar7
                PUSH    OFFSET szTErrorCloseFile
                JMP     FatalError

                ; Offset von TapeInfo.TapePos an Tape in Memory anpassen
@@AllesKlar7:   XOR     DI,DI                   ; von Tapebeginn suchen
                MOV     AL,0                    ; Nach 0 suchen
                MOV     CX,0FFFFh               ; Z„hler maximal einstellen
                CLD                             ; DI incrementieren
                REPE    SCASB                   ; solange ES:[DI]==0
                ADD     TapeInfo.TapePos,DI     ; Offset in DI zu TapePos addieren
                MOV     TapeInfo.DisplayPos,kTapeDisplay /2
                JMP     @@Ende

@@ErrorEnd:     MOV     TapeInfo.TapePos,kMaxTapeLength /2
                MOV     TapeInfo.DisplayPos,kTapeDisplay /2

                ; Alter Bildschirm wieder herstellen
@@Ende:         MOV     AX,wTScreen
                CALL    RestoreScreen

                MOV     TapeInfo.DisplayChar,''
                MOV     TapeInfo.DisplayCharCol,kCursorColor

                ; Register wiederherstellen
                POP     DI
                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
TapeRead        ENDP

_CODE           ENDS

END
