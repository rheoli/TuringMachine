;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     StateIO.asm                                        ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;              StateIO fÅr Behandlung des Turing-Stateliste
;**=================================================================**

.186

INCLUDE STATEIO.ASH
INCLUDE ERRORIO.ASH
INCLUDE SCREENIO.ASH
INCLUDE FILEIO.ASH
INCLUDE UTILS.ASH
INCLUDE TURING.ASH

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING

;***********************************************************************
;********              Konstanten von StateIO                   ********
;***********************************************************************

; State Farben               Textfarbe   + Hintergrundfarbe
kTapeColLoad            EQU kColBlack    + kColGray SHL 4
kStateBordCol           EQU kColWhite    + kColBlue SHL 4
kStateColor             EQU kColWhite    + kColBlue SHL 4
kAktiveState            EQU kColYellow   + kColGreen SHL 4
kStateColorSelect       EQU kColGreen    + kColBlack SHL 4
kStateColorEdit         EQU kColLightRed + kColBrown SHL 4
kCursorColor            EQU kColRed      + kColBlue SHL 4
kSelectChar             EQU kColLightRed + kColBlue SHL 4

; Zeichenposition des State-Programms
kStateDisplayPosX       EQU  9
kStateDisplayPosY       EQU  9
kThisStatePos           EQU 10
kThisStateTextPos       EQU 16
kThisStateLength        EQU 13
kInputPos               EQU 26
kInputCharPos           EQU 28
kInputLength            EQU  6
kNextStatePos           EQU 35
kNextStateTextPos       EQU 41
kNextStateLength        EQU 12
kOutputPos              EQU 50
kOutputCharPos          EQU 53
kOutputLength           EQU  6
kDirectionPos           EQU 59
kDirectionCharPos       EQU 63
kDirectionLength        EQU  9
kNextLine               EQU 1 SHL 8
kStateLines             EQU 13

; Versch
kExtraCode              EQU 1
kNoExtraCode            EQU 0


;***********************************************************************
;********              StateIO-Listen-Struktur                  ********
;***********************************************************************
STATETYPE        STRUCT
  ThisState     DW ?
  NextState     DW ?
  Input         DB ?
  Output        DB ?
  Direction     DB ?,?          ; word alignment
  LastEntry     DW ?            ; vorgÑnger Programmzeile
  NextEntry     DW ?            ; nÑchste Programmzeile
STATETYPE        ENDS


_DATA           SEGMENT PARA PUBLIC 'DATA'
;***********************************************************************
;********                Daten von StateIO                      ********
;***********************************************************************

; Hauptstuktur fÅr Stateinformationen
pStateFirst             DW 0            ; Erste StateListe (Pointer)
pStateAkt               DW 0            ; Aktuelle Programm-Zeile
pFirstDisplay           DW 0            ; Erste Statezeile die auf dem Bildschirm
                                        ; dargestellt wird
; Hauptfenster von StateIO
szStateTableO1          DB "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒø",0
szStateTableO2          DB "≥ current state ≥ input  ≥  next state  ≥ output ≥ direction ≥",0
szStateTableO3          DB "√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ¥",0
szStateTableM           DB "≥               ≥        ≥              ≥        ≥           ≥",0
szStateTableU           DB "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒŸ",0
szHALTState             DB "HALT",0

; Messagefenster von StateIO
szStateReadO            DB "…ÕÕÕÕÕÕÕÕÕÕÕÕÕ StateIO-Message ÕÕÕÕÕÕÕÕÕÕÕÕÕª",0
szStateReadM            DB "∫       Reading statedata from file...      ∫",0
szStateReadU            DB "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº",0
szStateWriteO           DB "…ÕÕÕÕÕÕÕÕÕÕÕÕÕ StateIO-Message ÕÕÕÕÕÕÕÕÕÕÕÕÕª",0
szStateWriteM           DB "∫         Write statedata to file...        ∫",0
szStateWriteU           DB "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº",0

; Filetexte fÅr Tape
szStateFileText1        DB "# State-Tabelle zur Turingmaschine", 0Dh, 0Ah, 0
szStateFileText2        DB "# [This State] [Input] [Next State] [Output] [Direction]", 0Dh, 0Ah, 0
szTSSpaces              DB "       ",0
szIpSpaces              DB "          ",0
szNSSpaces              DB "         ",0

; Neue Titel fÅr Editmodus
szSTitelEdit            DB "Turing-Meais",161,"n V 1.00 - State-Edit-Mode",0
szSEditMenu             DB "&<Red Char&>-Change data,  &\&/-Move down,  &/&\-Move up,  &E&S&C-Exit editmode",0
szSBlank                DB "&I&N&S-Insert a new State,  &D&E&L-Delete current State",0
szSNumberU              DB "&R&E&T-Change Number,  &E&S&C-Undo Change",0
szSNumberEO             DB "Input a Number from 0..65533,  &H-HaltState, &--ImposibleState",0
szSNumberO              DB "Input a Number from 0..65533",0
szSCharU                DB "&R&E&T-Change Character,  &E&S&C-Undo Change",0
szSCharO                DB "Input a Character",0
szSDirectionO           DB "&L-Go left,  &R-Go right",0

; Allgemeine Variablen
wNewAlloc               DW ?                    ; Zwischenspeicher
wThisState              DW ?                    ; Zwischenspeicher
bInput                  DB ?                    ; Zwischenspeicher
bChar                   DB ?                    ; Variable zum Einlesen eines Zeichen eines Files
wDummy                  DW ?                    ; Temp-Variable
szSTemp                 DB 300 DUP('$')         ; Variable zum einlesen einer Zeile eines FilesszPFilenameRead         DB "standard.sta",0     ; Tape-File zum einlesen
wSFileHandle            DW ?                    ; Variable fÅr Filehandle
szSFilenameRead         DB "standard.sta",0     ; Tape-File das gelanden wird
szSFilenameWrite        DB "save.sta",0         ; Tape-File zum speichern
wSFilenameOff           DW 0
wSScreen                DW 0                    ; Speichersegment fÅr Bildschirm
bCursorPos              DB ?                    ; Cursorposition auf Bildschirm
wNewWert                DW ?
wOldWert                DW ?
bNewChar                DB ?
bOldChar                DB ?
bXPos                   DB ?
bExtraCode              DB ?
bFirstChange            DB ?

; Backupvariablen fÅr Editmodus
pStateAktBak            DW ?
bCursorPosBak           DB ?

; Fehlermeldungen von TapeIO
szSErrorAllocMem        DB "Can't allocate more memory.",0
szSErrorOpenFile        DB "Couln't find statefile.",0
szSErrorCreateFile      DB "Couln't create statefile.",0
szSErrorStartPos        DB "Start-Position is not a number.",0
szSErrorReadFile        DB "Couln't read from the program file.",0
szSErrorWriteFile       DB "Couln't write to file.",0
szSErrorCloseFile       DB "Couln't close program file.",0
szSErrorStateNF         DB "Couln't found NextState.",0
szSErrorThisState       DB "Parse Error: ThisState not a number.",0
szSErrorNoMem           DB "Not enough memory.",0
szSErrorStateDLine      DB "Parse Error: Two lines are the same.",0
szSErrorBadLine         DB "Parse Error: Bad Line.",0
szSErrorUndefChar       DB "Parse Error: Line has a undefined character.",0
szSErrorUndefDir        DB "Parse Error: undefined direction-character.",0

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********              Prozeduren von StateIO                   ********
;***********************************************************************

;***********************************************************************
; StateGetFirst
;   - pStateFirst zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: pStateFirst
;-----------------------------------------------------------------------
StateGetFirst   PROC NEAR
                MOV     AX,pStateFirst
                RET
StateGetFirst   ENDP


;***********************************************************************
; StatePrintAll
;   - Rahmen von Statezeilen zeichnen
;   - aktuelle States ausgeben
;   - Cursor an aktuelle Statezeile schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrintAll   PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX

                MOV     AL,kStateBordCol
                CALL    SetColor

                MOV     BL,kStateDisplayPosX -1
                MOV     BH,kStateDisplayPosY -3
                MOV     AX,OFFSET szStateTableO1
                CALL    PrintString

                MOV     BL,kStateDisplayPosX -1
                MOV     BH,kStateDisplayPosY -2
                MOV     AX,OFFSET szStateTableO2
                CALL    PrintString

                MOV     BL,kStateDisplayPosX -1
                MOV     BH,kStateDisplayPosY -1
                MOV     AX,OFFSET szStateTableO3
                CALL    PrintString

                MOV     CX,kStateLines
                MOV     BL,kStateDisplayPosX -1
                MOV     BH,kStateDisplayPosY
                MOV     AX,OFFSET szStateTableM
@@TableLoop:    CALL    PrintString
                ADD     BX,kNextLine
                LOOP    @@TableLoop

                MOV     AX,OFFSET szStateTableU
                CALL    PrintString

                CALL    StatePrint

                MOV     BL,kStateDisplayPosX
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                MOV     AL,''
                MOV     AH,kCursorColor
                CALL    PrintChar
                MOV     AL,kAktiveState
                CALL    StateChngAttrib

                POP     CX
                POP     BX
                POP     AX
                RET
StatePrintAll   ENDP


;***********************************************************************
; StatePrint
;   - State-Tabelle in Rahmen schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrint      PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    ES

                CMP     pStateFirst,0
                JNZ     @@DatenDa
                JMP     @@Ende

@@DatenDa:      MOV     AX,pStateAkt
                MOV     ES,AX

                MOV     CX,kStateLines/2
                MOV     bCursorPos,0
@@PointerLoop:  MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JZ      @@EndeLoop
                MOV     ES,AX
                INC     bCursorPos
                LOOP    @@PointerLoop

@@EndeLoop:     MOV     AL,kStateColor
                CALL    SetColor
                
                MOV     AX,ES
                MOV     pFirstDisplay,AX

                MOV     DH,kStateDisplayPosY
                MOV     CX,kStateLines

@@PrintLoop:    PUSH    CX

                ; ThisState ausgeben
                MOV     AX,WORD PTR ES:STATETYPE.ThisState
                MOV     BX,OFFSET szSTemp
                CALL    WORDToString
                MOV     BL,kThisStateTextPos
                MOV     BH,DH
                MOV     CL,kTACenter
                CALL    PrintAlignString

                ; Input ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Input
                MOV     AH,kStateColor
                MOV     BL,kInputCharPos
                MOV     BH,DH
                CALL    PrintChar

                ; NextState ausgeben
                MOV     AX,WORD PTR ES:STATETYPE.NextState
                CMP     AX,kImpossibleState
                JNZ     @@Weiter1
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],'-'
                MOV     BYTE PTR [BX+1],0
                MOV     AX,BX
                JMP     @@Weiter3

@@Weiter1:      CMP     AX,kHALTState
                JNZ     @@Weiter2
                MOV     AX,OFFSET szHALTState
                JMP     @@Weiter3

@@Weiter2:      MOV     BX,OFFSET szSTemp
                CALL    WORDToString

@@Weiter3:      MOV     BL,kNextStateTextPos
                MOV     BH,DH
                MOV     CL,kTACenter
                CALL    PrintAlignString

                ; Output ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Output
                MOV     AH,kStateColor
                MOV     BL,kOutputCharPos
                MOV     BH,DH
                CALL    PrintChar

                ; Direction ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Direction
                MOV     AH,kStateColor
                MOV     BL,kDirectionCharPos
                MOV     BH,DH
                CALL    PrintChar

                ADD     DH,1

                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JNZ     @@AllesKlar1
                POP     CX
                JMP     @@Ende

@@AllesKlar1:   MOV     ES,AX

                POP     CX
                LOOP    @@WeiterLoop
                JMP     @@Ende

                ; 127 Bytegrenze Åbergehen
@@WeiterLoop:   JMP     @@PrintLoop

@@Ende:         POP     ES
                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
StatePrint      ENDP


;***********************************************************************
; StateGetThis
;   - aktueller ThisState zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: ThisState zurÅckgeben
;-----------------------------------------------------------------------
StateGetThis    PROC NEAR
                PUSH    ES

                MOV     ES,pStateAkt
                MOV     AX,ES:STATETYPE.ThisState

                POP     ES
                RET
StateGetThis    ENDP


;***********************************************************************
; StateGetNext
;   - aktueller NextState zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: NextState zurÅckgeben
;-----------------------------------------------------------------------
StateGetNext    PROC NEAR
                PUSH    ES

                MOV     ES,pStateAkt
                MOV     AX,ES:STATETYPE.NextState

                POP     ES
                RET
StateGetNext    ENDP


;***********************************************************************
; StateGetDirect
;   - aktuelles Direction zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: aktuelles Direction-Zeichen
;-----------------------------------------------------------------------
StateGetDirect  PROC NEAR
                PUSH    ES

                MOV     ES,pStateAkt
                MOV     AL,ES:STATETYPE.Direction

                POP     ES
                RET
StateGetDirect  ENDP

;***********************************************************************
; StateGetOutput
;   - aktuelles Output-Zeichen zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: aktuelles Output-Zeichen
;-----------------------------------------------------------------------
StateGetOutput  PROC NEAR
                PUSH    ES

                MOV     ES,pStateAkt
                MOV     AL,ES:STATETYPE.Output

                POP     ES
                RET
StateGetOutput  ENDP


;***********************************************************************
; StateSetThis
;   - State-Zeile anhand von State und Zeichen suchen und als aktuellen
;     eintragen
;   - Tabelle neu zeichnen
;-----------------------------------------------------------------------
; Input:
;   - AX: State
;   - BL: Zeichen
; Returnwert:
;   - AX: kStopped fÅr Programm stoppen, kRunning alles klar
;-----------------------------------------------------------------------
StateSetThis    PROC NEAR
                PUSH    BX
                PUSH    DI
                PUSH    ES

                MOV     DI,pStateFirst
                MOV     ES,DI

@@StateSearch:  CMP     ES:STATETYPE.ThisState,AX
                JZ      @@StateFound

@@NextLine:     MOV     DI,WORD PTR ES:STATETYPE.NextEntry
                CMP     DI,0            ; keine weitere Programmzeile vorhanden
                JNZ     @@Weiter
                MOV     AX,OFFSET szSErrorStateNF
                CALL    NormalError
                MOV     wRunState,kStopped
                CALL    PrintMainMenu
                MOV     AX,kStopped
                JMP     @@Ende

@@Weiter:       MOV     ES,DI
                JMP     @@StateSearch

@@StateFound:   CMP     ES:STATETYPE.Input,BL
                JZ      @@CharFound
                JMP     @@NextLine

@@CharFound:    MOV     AX,ES
                MOV     pStateAkt,AX         ; Neue Adresse speichern
                CALL    StatePrintAll
                MOV     DI,ES:STATETYPE.NextState
                CMP     DI,kHALTState
                JNZ     @@Running
                MOV     AX,kHalted
                JMP     @@Ende

@@Running:      MOV     AX,kRunning

@@Ende:         POP     ES
                POP     DI
                POP     BX
                RET
StateSetThis    ENDP


;***********************************************************************
; StateAddLine
;   - neue Zeile zur Stateliste hinzufÅgen
;-----------------------------------------------------------------------
; Input:
;   - AX: ThisState-Wert
;   - BL: Input-Wert
; Returnwert:
;   - AX: Speichersegment fÅr neue Werte
;-----------------------------------------------------------------------
StateAddLine    PROC NEAR
                PUSH    BX
                PUSH    DX
                PUSH    ES

                ; Werte sichern
                MOV     wThisState,AX
                MOV     bInput,BL

                ; Neuen Speicher reservieren
                MOV     AH,48h
                MOV     BX,1
                INT     21h
                JNC     @@AllesKlar1
                PUSH    OFFSET szSErrorNoMem
                JMP     FatalError

                ; Die bereits aus dem File gelesenen Daten eintragen
@@AllesKlar1:   MOV     wNewAlloc,AX
                MOV     ES,AX
                MOV     DX,wThisState
                MOV     WORD PTR ES:STATETYPE.ThisState,DX
                MOV     DL,bInput
                MOV     BYTE PTR ES:STATETYPE.Input,DL

                MOV     AX,pStateFirst
                CMP     AX,0
                JZ      @@EintrFirst
                MOV     ES,AX

                ; Nun das neue Element in die Liste eintragen
@@EintrLoop:    MOV     DX,WORD PTR ES:STATETYPE.ThisState
                CMP     DX,wThisState
                JB      @@NotFound
                CMP     DX,wThisState
                JZ      @@InputSort

                ; Normales EinfÅgen zwischen zwei Elemente
@@NormEintr:    MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JZ      @@InFirstEintr
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     BX,ES
                MOV     ES,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,BX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX
                MOV     ES,BX
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     ES,AX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                JMP     @@Ende

                ; An erster Stelle einfÅgen
@@InFirstEintr: MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     AX,ES
                MOV     ES,DX
                XOR     DX,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX
                MOV     DX,wNewAlloc
                MOV     pStateFirst,DX
                JMP     @@Ende

                ; Dies ist das erste Element in der Liste
@@EintrFirst:   MOV     DX,wNewAlloc
                MOV     pStateFirst,DX
                MOV     ES,DX
                XOR     DX,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                JMP     @@Ende

                ; bereits ein Element mit ThisState vorhanden,
                ; nun nach dem Input sortieren
@@InputSort:    MOV     DL,BYTE PTR ES:STATETYPE.Input
                CMP     DL,bInput
                JB      @@NotFound
                CMP     DL,bInput
                JNZ     @@NormEintr

                ; Das Element kommt bereits in der Liste vor
                MOV     AX,OFFSET szSErrorStateDLine
                CALL    NormalError
                JMP     @@NormEintr

                ; Ein Element in der Liste weitergehen
@@NotFound:     MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JZ      @@HintenEintr
                MOV     ES,AX
                JMP     @@EintrLoop

                ; zuhinterst in der Liste eintragen
@@HintenEintr:  MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     AX,ES
                MOV     ES,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,AX
                XOR     AX,AX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX

@@Ende:         MOV     AX,wNewAlloc
                POP     ES
                POP     DX
                POP     BX
                RET
StateAddLine    ENDP


;***********************************************************************
; StateInsertLine
;   - ein aus der State-Liste entferntes Element wird wieder ein-
;     gefÅgt
;-----------------------------------------------------------------------
; Input:
;   - AX: Speichersegment fÅr State
; Returnwert:
;   - AL: AL==0 Alles OK, AL==1 State doppelt
;-----------------------------------------------------------------------
StateInsertLine PROC NEAR
                PUSH    BX
                PUSH    DX
                PUSH    ES

                ; Die bereits aus dem File gelesenen Daten eintragen
@@AllesKlar1:   MOV     wNewAlloc,AX
                MOV     ES,AX
                MOV     DX,WORD PTR ES:STATETYPE.ThisState
                MOV     wThisState,DX
                MOV     DL,BYTE PTR ES:STATETYPE.Input
                MOV     bInput,DL

                MOV     AX,pStateFirst
                CMP     AX,0
                JZ      @@EintrFirst
                MOV     ES,AX

                ; Nun das neue Element in die Liste eintragen
@@EintrLoop:    MOV     DX,WORD PTR ES:STATETYPE.ThisState
                CMP     DX,wThisState
                JB      @@NotFound
                CMP     DX,wThisState
                JZ      @@InputSort

                ; Normales EinfÅgen zwischen zwei Elemente
@@NormEintr:    MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JZ      @@InFirstEintr
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     BX,ES
                MOV     ES,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,BX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX
                MOV     ES,BX
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     ES,AX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                JMP     @@Ende

                ; An erster Stelle einfÅgen
@@InFirstEintr: MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     AX,ES
                MOV     ES,DX
                XOR     DX,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX
                MOV     DX,wNewAlloc
                MOV     pStateFirst,DX
                JMP     @@Ende

                ; Dies ist das erste Element in der Liste
@@EintrFirst:   MOV     DX,wNewAlloc
                MOV     pStateFirst,DX
                MOV     ES,DX
                XOR     DX,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,DX
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                JMP     @@Ende

                ; bereits ein Element mit ThisState vorhanden,
                ; nun nach dem Input sortieren
@@InputSort:    MOV     DL,BYTE PTR ES:STATETYPE.Input
                CMP     DL,bInput
                JB      @@NotFound
                CMP     DL,bInput
                JNZ     @@NormEintr

                ; Das Element kommt bereits in der Liste vor
                MOV     AX,OFFSET szSErrorStateDLine
                CALL    NormalError
                JMP     @@NormEintr

                ; Ein Element in der Liste weitergehen
@@NotFound:     MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JZ      @@HintenEintr
                MOV     ES,AX
                JMP     @@EintrLoop

                ; zuhinterst in der Liste eintragen
@@HintenEintr:  MOV     DX,wNewAlloc
                MOV     WORD PTR ES:STATETYPE.NextEntry,DX
                MOV     AX,ES
                MOV     ES,DX
                MOV     WORD PTR ES:STATETYPE.LastEntry,AX
                XOR     AX,AX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX

@@Ende:         MOV     AL,0

@@Exit:         POP     ES
                POP     DX
                POP     BX
                RET
StateInsertLine ENDP


;***********************************************************************
; StateDelLine
;   - das Element mit Segmentadresse AX wird aus der State-Liste ent-
;     fernt
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse des zu entfernenden Elementes
; Returnwert: -
;-----------------------------------------------------------------------
StateDelLine    PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    DS
                PUSH    ES

                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                MOV     BX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JZ      @@FirstChange
                MOV     ES,AX
                MOV     WORD PTR ES:STATETYPE.NextEntry,BX
                JMP     @@NextChange

@@FirstChange:  MOV     pStateFirst,BX

@@NextChange:   CMP     BX,0
                JZ      @@LastChange
                MOV     ES,BX
                MOV     WORD PTR ES:STATETYPE.LastEntry,AX
                JMP     @@Ende

@@LastChange:   CMP     AX,0
                JZ      @@Ende
                MOV     ES,AX
                XOR     AX,AX
                MOV     WORD PTR ES:STATETYPE.NextEntry,AX

@@Ende:         POP     ES
                POP     DS
                POP     BX
                POP     AX
                RET
StateDelLine    ENDP


;***********************************************************************
; StatePrintEdit
;   - State-Liste in Tabelle schreiben (fÅr Edit-Modus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrintEdit  PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    ES

                CMP     pStateFirst,0
                JNZ     @@DatenDa
                JMP     @@Ende

@@DatenDa:      MOV     AX,pStateAkt
                MOV     ES,AX

                MOV     CX,kStateLines/2
                MOV     bCursorPos,0
@@PointerLoop:  MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JZ      @@EndeLoop
                MOV     ES,AX
                INC     bCursorPos
                LOOP    @@PointerLoop

@@EndeLoop:     MOV     AL,kStateBordCol
                CALL    SetColor

                MOV     CX,kStateLines
                MOV     BL,kStateDisplayPosX -1
                MOV     BH,kStateDisplayPosY
                MOV     AX,OFFSET szStateTableM
@@TableLoop:    CALL    PrintString
                ADD     BX,(1 SHL 8)
                LOOP    @@TableLoop

                MOV     AL,kStateColor
                CALL    SetColor

                MOV     CX,kStateLines
                MOV     DH,kStateDisplayPosY

@@PrintLoop:    PUSH    CX

                ; ThisState ausgeben
                MOV     AX,WORD PTR ES:STATETYPE.ThisState
                MOV     BX,OFFSET szSTemp
                CALL    WORDToString
                MOV     BL,kThisStateTextPos
                MOV     BH,DH
                MOV     CL,kTACenter
                CALL    PrintAlignString

                ; Input ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Input
                MOV     AH,kStateColor
                MOV     BL,kInputCharPos
                MOV     BH,DH
                CALL    PrintChar

                ; NextState ausgeben
                MOV     AX,WORD PTR ES:STATETYPE.NextState
                CMP     AX,kImpossibleState
                JNZ     @@Weiter1
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],'-'
                MOV     BYTE PTR [BX+1],0
                MOV     AX,BX
                JMP     @@Weiter3

@@Weiter1:      CMP     AX,kHALTState
                JNZ     @@Weiter2
                MOV     AX,OFFSET szHALTState
                JMP     @@Weiter3

@@Weiter2:      MOV     BX,OFFSET szSTemp
                CALL    WORDToString

@@Weiter3:      MOV     BL,kNextStateTextPos
                MOV     BH,DH
                MOV     CL,kTACenter
                CALL    PrintAlignString

                ; Output ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Output
                MOV     AH,kStateColor
                MOV     BL,kOutputCharPos
                MOV     BH,DH
                CALL    PrintChar

                ; Direction ausgeben
                MOV     AL,BYTE PTR ES:STATETYPE.Direction
                MOV     AH,kStateColor
                MOV     BL,kDirectionCharPos
                MOV     BH,DH
                CALL    PrintChar

                ADD     DH,1            ; NÑchste Zeile

                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JNZ     @@AllesKlar1
                POP     CX
                JMP     @@Ende

@@AllesKlar1:   MOV     ES,AX

                POP     CX
                LOOP    @@WeiterLoop
                JMP     @@Ende

                ; 127 Bytegrenze Åbergehen
@@WeiterLoop:   JMP     @@PrintLoop

@@Ende:         POP     ES
                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
StatePrintEdit  ENDP
                

;***********************************************************************
; StateChngAttrib
;   - Attribute einer State-Zeile Ñndern (fÅr Edit-Modus)
;-----------------------------------------------------------------------
; Input:
;   - AL: Farbe
; Returnwert: -
;-----------------------------------------------------------------------
StateChngAttrib PROC NEAR
                PUSH    AX
                PUSH    BX

                MOV     AH,58
                MOV     BL,kThisStatePos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    ChangeAttrib

                POP     BX
                POP     AX
                RET
StateChngAttrib ENDP


;***********************************************************************
; StateCursorDown
;   - eine Statezeile nach unten (im Editmodus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateCursorDown PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    ES

                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JNZ     @@AllesKlar1
                JMP     @@Ende

                ; Programmzeilen-Selektierung entfernen
@@AllesKlar1:   MOV     pStateAkt,AX

                MOV     AL,kStateColor
                CALL    StateChngAttrib

                INC     bCursorPos
                MOV     AL,bCursorPos
                CMP     AL,kStateLines
                JNC     @@NachOben

                ; Programmzeile selektiert Darstellen
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                JMP     @@Ende
                
@@NachOben:     MOV     bCursorPos,0
                MOV     AX,pStateAkt
                MOV     pFirstDisplay,AX

@@NewPrint:     CALL    StatePrintEdit

                ; Programmzeile selektiert Darstellen
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

@@Ende:         POP     ES
                POP     BX
                POP     AX
                RET
StateCursorDown ENDP


;***********************************************************************
; StateCursorUp
;   - eine Statezeile nach oben (im Editmodus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateCursorUp   PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    ES

                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JNZ     @@AllesKlar1
                JMP     @@Ende

                ; Programmzeilen-Selektierung entfernen
@@AllesKlar1:   MOV     pStateAkt,AX

                MOV     AL,kStateColor
                CALL    StateChngAttrib

                DEC     bCursorPos
                JS      @@NachMitte

                ; Programmzeile selektiert Darstellen
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                JMP     @@Ende
                
@@NachMitte:    MOV     bCursorPos,0
                MOV     AX,pStateAkt
                MOV     ES,AX

@@NewPrint:     CALL    StatePrintEdit

                ; Programmzeile selektiert Darstellen
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

@@Ende:         POP     ES
                POP     BX
                POP     AX
                RET
StateCursorUp   ENDP


;***********************************************************************
; StateEditNumber
;   - Editieren einer State-Nummer auf aktueller Zeile in Spalte AL
;   - Zugelassen sind die Zahlen 0-65533
;   - die beiden Zahlen 65534 und 65535 sind fÅr ImpossibleState und
;     HALTState reserviert
;-----------------------------------------------------------------------
; Input:
;   - AL: x-Position der Cursors
;   - AH: Extracodes zulassen (AH=1 JA, AH=0 Nein)
;   - BX: Aktueller Wert
; Returnwert:
;   - AX: neuer Wert
;   - BL: Aenderung (BL=0 Nein, BL=1 Ja)
;-----------------------------------------------------------------------
StateEditNumber PROC NEAR
                PUSH    CX
                PUSH    DX
                
                MOV     bXPos,AL
                MOV     bExtraCode,AH
                MOV     wOldWert,BX
                MOV     wNewWert,BX
                MOV     bFirstChange,1

                MOV     AL,kStateColorEdit
                CALL    SetColor

@@ESLoop:       MOV     AX,0700h
                INT     21h

                CMP     AL,0
                JNZ     @@WeiterN1
                MOV     AX,0700h
                INT     21h
                JMP     @@ESLoop

                ; ESC-Taste gedrÅckt
@@WeiterN1:     CMP     AL,27
                JNZ     @@WeiterN2
                MOV     AX,OFFSET szTSSpaces
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                MOV     CX,kTACenter
                CALL    PrintAlignString

                MOV     AX,wOldWert
                CMP     AX,kImpossibleState
                JZ      @@ISPrint
                CMP     AX,kHALTState
                JZ      @@HSPrint
                MOV     BX,OFFSET szSTemp
                CALL    WORDToString
                JMP     @@PrintOld
@@ISPrint:      MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],'-'
                MOV     BYTE PTR [BX+1],0
                MOV     AX,BX
                JMP     @@PrintOld
@@HSPrint:      MOV     AX,OFFSET szHALTState
@@PrintOld:     MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintAlignString
                MOV     AX,wOldWert
                MOV     BL,0
                JMP     @@Ende

                ; Impossible State einstellen
@@WeiterN2:     CMP     AL,'-'
                JNZ     @@WeiterN3
                CMP     bExtraCode,0
                JZ      @@EndeN2
                MOV     AX,OFFSET szTSSpaces
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                MOV     CX,kTACenter
                CALL    PrintAlignString

                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],'-'
                MOV     BYTE PTR [BX+1],0
                MOV     AX,BX
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintAlignString
                MOV     wNewWert,kImpossibleState
@@EndeN2:       JMP     @@ESLoop

                ; HALT-State einsetzen
@@WeiterN3:     CMP     AL,'H'
                JZ      @@HALTState
                CMP     AL,'h'
                JNZ     @@WeiterN4
@@HALTState:    CMP     bExtraCode,0
                JZ      @@EndeN3
                MOV     AX,OFFSET szTSSpaces
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                MOV     CX,kTACenter
                CALL    PrintAlignString

                MOV     AX,OFFSET szHALTState
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintAlignString
                MOV     wNewWert,kHALTState
@@EndeN3:       JMP     @@ESLoop

                ; Deletetaste
@@WeiterN4:     CMP     AL,08h
                JNZ     @@WeiterN5
                MOV     AX,wNewWert
                CMP     AX,kHALTState
                JZ      @@ExtraDel
                CMP     AX,kImpossibleState
                JZ      @@ExtraDel
                XOR     DX,DX
                MOV     BX,10
                DIV     BX
                MOV     wNewWert,AX
                JMP     @@PrintWert
@@ExtraDel:     MOV     wNewWert,0
                JMP     @@PrintWert

                ; Return-Taste
@@WeiterN5:     CMP     AL,13
                JNZ     @@WeiterN6
                MOV     AX,wNewWert
                MOV     BL,1
                JMP     @@Ende

@@WeiterN6:     CMP     AL,'0'
                JC      @@EndeN6
                CMP     AL,'9'
                JA      @@EndeN6
                SUB     AL,'0'
                MOV     BL,AL
                MOV     AX,wNewWert
                CMP     AX,kImpossibleState
                JZ      @@DelAll
                CMP     AX,kHALTState
                JZ      @@DelAll
                CMP     AX,wOldWert
                JNZ     @@NoDel
                CMP     bFirstChange,1
                JNZ     @@NoDel
                MOV     bFirstChange,0
@@DelAll:       XOR     AX,AX
                MOV     wNewWert,AX
@@NoDel:        CMP     AX,6554
                JNC     @@EndeN6
                MOV     CX,10
                MUL     CX
                XOR     BH,BH
                ADD     AX,BX
                JS      @@EndeN6
                CMP     AX,kHALTState
                JNC     @@EndeN6
                MOV     wNewWert,AX

@@PrintWert:    MOV     AX,OFFSET szTSSpaces
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                MOV     CX,kTACenter
                CALL    PrintAlignString

                MOV     AX,wNewWert
                MOV     BX,OFFSET szSTemp
                CALL    WORDToString
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintAlignString                
@@EndeN6:       JMP     @@ESLoop

@@Ende:         POP     DX
                POP     CX
                RET
StateEditNumber ENDP


;***********************************************************************
; StateEditChar
;   - Editieren eines zugelassenen Zeichen, der aktuellen Zeile in der
;     Spalte AL
;-----------------------------------------------------------------------
; Input:
;   - AL: x-Position der Cursors
;   - AH: Aktuelles Zeichen
;   - BL: (BL==0 alles Zeichen zugelassen, BL==1 nur 'R' und 'L' zuge-
;         lassen)
; Returnwert:
;   - AL: neues Zeichen
;   - AH: Aenderung (AH=0 Nein, AH=1 Ja)
;-----------------------------------------------------------------------
StateEditChar   PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DX
                
                MOV     bXPos,AL
                MOV     bOldChar,AH
                MOV     bNewChar,AH
                MOV     bExtraCode,BL
                MOV     bFirstChange,1

                MOV     AL,kStateColorEdit
                CALL    SetColor

@@ESLoop:       MOV     AX,0700h
                INT     21h

                CMP     AL,0
                JNZ     @@WeiterN1
                MOV     AX,0700h
                INT     21h
                JMP     @@ESLoop

                ; ESC-Taste gedrÅckt
@@WeiterN1:     CMP     AL,27
                JNZ     @@WeiterN2
                MOV     AL,bOldChar
                MOV     AH,kStateColorEdit
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintChar
                MOV     AL,bOldChar
                MOV     AH,0
                JMP     @@Ende

                ; Return-Taste
@@WeiterN2:     CMP     AL,13
                JNZ     @@WeiterN3
                MOV     AL,bNewChar
                MOV     AH,1
                JMP     @@Ende

@@WeiterN3:     CMP     bExtraCode,1
                JZ      @@WeiterN4
                CMP     AL,33
                JC      @@ESLoop
                CMP     AL,255
                JZ      @@ESLoop
@@NewChar:      MOV     bNewChar,AL
                MOV     AH,kStateColorEdit
                MOV     BL,bXPos
                MOV     BH,bCursorPos
                ADD     BH,kStateDisplayPosY
                CALL    PrintChar
                JMP     @@ESLoop

@@WeiterN4:     CMP     AL,'a'
                JC      @@NoWandel
                SUB     AL,'a'-'A'
@@NoWandel:     CMP     AL,'L'
                JZ      @@CodeOK
                CMP     AL,'R'
                JZ      @@CodeOK
                CMP     AL,'-'
                JZ      @@CodeOK
                JMP     @@ESLoop
@@CodeOK:       JMP     @@NewChar                

@@Ende:         POP     DX
                POP     CX
                POP     BX
                RET
StateEditChar   ENDP


;***********************************************************************
; StateEdit
;   - Edit-Modus fÅr State-Liste
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateEdit       PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    ES

                ; Titel ausgeben
                MOV     AL,kMenuColor
                CALL    SetColor
                MOV     AX,OFFSET szSTitelEdit
                CALL    PrintTitle

                ; EditmenÅ ausgeben
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu

                ; Runmodusdaten retten
                MOV     AL,bCursorPos
                MOV     bCursorPosBak,AL
                MOV     AX,pStateAkt
                MOV     pStateAktBak,AX

                ; Anfangsbuchstaben der Programmtitel Ñndern
                MOV     AL,kSelectChar
                MOV     AH,1                    ; 1 Zeichen Ñndern
                MOV     BL,kThisStatePos
                MOV     BH,kStateDisplayPosY -2
                CALL    ChangeAttrib
                MOV     BL,kInputPos
                CALL    ChangeAttrib
                MOV     BL,kNextStatePos +1
                CALL    ChangeAttrib
                MOV     BL,kOutputPos
                CALL    ChangeAttrib
                MOV     BL,kDirectionPos
                CALL    ChangeAttrib

                ; Programmzeile selektiert Darstellen
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                
@@EditLoop:     MOV     AX,0700h
                INT     21h               

                CMP     AL,0
                JNZ     @@WeiterN1
                MOV     AX,0700h
                INT     21h

                ; Cursor down
                CMP     AL,050h
                JNZ     @@WeiterE1
                CALL    StateCursorDown
                JMP     @@EditLoop

                ; Cursor up
@@WeiterE1:     CMP     AL,048h
                JNZ     @@WeiterE2
                CALL    StateCursorUp
                JMP     @@EditLoop

                ; Insert
@@WeiterE2:     CMP     AL,052h
                JNZ     @@WeiterE3
                MOV     AX,pStateAkt
                CMP     AX,0
                JZ      @@NullPoint
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.ThisState
                JMP     @@WeiterE21
@@NullPoint:    MOV     AX,1
@@WeiterE21:    MOV     BL,1
                CALL    StateAddLine
                MOV     ES,AX
                MOV     WORD PTR ES:STATETYPE.NextState,kImpossibleState
                MOV     BYTE PTR ES:STATETYPE.Output,'-'
                MOV     BYTE PTR ES:STATETYPE.Direction,'-'
                MOV     pStateAkt,AX
                CALL    StatePrintEdit
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                JMP     @@EditLoop

                ; Delete
@@WeiterE3:     CMP     AL,053h
                JNZ     @@WeiterE4
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JNZ     @@WeiterX1
                MOV     AX,WORD PTR ES:STATETYPE.LastEntry
                CMP     AX,0
                JZ      @@EndeE3
@@WeiterX1:     MOV     pStateAkt,AX
                MOV     AX,ES
                CALL    StateDelLine
                CALL    StatePrintEdit
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
@@EndeE3:       JMP     @@EditLoop

                ; Home-Taste
@@WeiterE4:     CMP     AL,47h
                JNZ     @@WeiterE5
                MOV     AX,pStateFirst
                MOV     pStateAkt,AX
                CALL    StatePrintEdit
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                JMP     @@EditLoop

                ; End-Taste
@@WeiterE5:     CMP     AL,4Fh
                JNZ     @@WeiterE6
                MOV     AX,pStateFirst
                MOV     ES,AX
@@E5LastLoop:   CMP     AX,0
                JZ      @@E5LastEnd
                MOV     ES,AX
                MOV     AX,ES:STATETYPE.NextEntry
                JMP     @@E5LastLoop
@@E5LastEnd:    MOV     AX,ES
                MOV     pStateAkt,AX
                CALL    StatePrintEdit
                MOV     AL,kStateColorSelect
                CALL    StateChngAttrib
                JMP     @@EditLoop

@@WeiterE6:     JMP     @@EditLoop

                ; ESC-Taste
@@WeiterN1:     CMP     AL,27
                JNZ     @@WeiterN11
                JMP     @@Ende

                ; Ab hier dÅrfen nur noch vergleiche kommen,
                ; die in Zusammenhang mit den StateeintrÑge sind

                ; Daten in Liste vorhanden
@@WeiterN11:    CMP     pStateAkt,0
                JNZ     @@WeiterN2
                JMP     @@EditLoop

                ; This State Ñndern
@@WeiterN2:     CMP     AL,'c'
                JNZ     @@WeiterN3

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSNumberO
                MOV     CX,OFFSET szSNumberU
                CALL    ChangeMenu

                MOV     AL,kStateColorEdit
                MOV     AH,kThisStateLength
                MOV     BL,kThisStatePos
                MOV     BH,kStateDisplayPosY
                ADD     BH,bCursorPos
                CALL    ChangeAttrib
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AL,kThisStateTextPos
                MOV     AH,kNoExtraCode
                MOV     BX,WORD PTR ES:STATETYPE.ThisState
                CALL    StateEditNumber
                CMP     BL,1                    ; Wert geÑndert
                JNZ     @@NoChange2
                MOV     WORD PTR ES:STATETYPE.ThisState,AX
                MOV     AX,pStateAkt
                CALL    StateDelLine
                MOV     AX,pStateAkt
                CALL    StateInsertLine
                CALL    StatePrintEdit
@@NoChange2:    MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu
@@EndeN2:       JMP     @@EditLoop

                ; Input Ñndern
@@WeiterN3:     CMP     AL,'i'
                JNZ     @@WeiterN4

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSCharO
                MOV     CX,OFFSET szSCharU
                CALL    ChangeMenu

                MOV     AL,kStateColorEdit
                MOV     AH,kInputLength
                MOV     BL,kInputPos
                MOV     BH,kStateDisplayPosY
                ADD     BH,bCursorPos
                CALL    ChangeAttrib
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AL,kInputCharPos
                MOV     AH,BYTE PTR ES:STATETYPE.Input
                MOV     BL,0
                CALL    StateEditChar
                CMP     AH,1                    ; Wert geÑndert
                JNZ     @@NoChange3
                MOV     BYTE PTR ES:STATETYPE.Input,AL
                MOV     AX,pStateAkt
                CALL    StateDelLine
                MOV     AX,pStateAkt
                CALL    StateInsertLine
                CALL    StatePrintEdit
@@NoChange3:    MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu
                JMP     @@EditLoop

                ; Next State Ñndern
@@WeiterN4:     CMP     AL,'n'
                JNZ     @@WeiterN5

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSNumberEO
                MOV     CX,OFFSET szSNumberU
                CALL    ChangeMenu

                MOV     AL,kStateColorEdit
                MOV     AH,kNextStateLength
                MOV     BL,kNextStatePos
                MOV     BH,kStateDisplayPosY
                ADD     BH,bCursorPos
                CALL    ChangeAttrib
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AL,kNextStateTextPos
                MOV     AH,kExtraCode
                MOV     BX,WORD PTR ES:STATETYPE.NextState
                CALL    StateEditNumber
                CMP     BL,1                    ; Wert geÑndert
                JNZ     @@NoChange4
                MOV     WORD PTR ES:STATETYPE.NextState,AX
@@NoChange4:    MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu

                JMP     @@EditLoop

                ; Output Ñndern
@@WeiterN5:     CMP     AL,'o'
                JNZ     @@WeiterN6

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSCharO
                MOV     CX,OFFSET szSCharU
                CALL    ChangeMenu

                MOV     AL,kStateColorEdit
                MOV     AH,kOutputLength
                MOV     BL,kOutputPos
                MOV     BH,kStateDisplayPosY
                ADD     BH,bCursorPos
                CALL    ChangeAttrib
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AL,kOutputCharPos
                MOV     AH,BYTE PTR ES:STATETYPE.Output
                MOV     BL,0
                CALL    StateEditChar
                CMP     AH,1                    ; Wert geÑndert
                JNZ     @@NoChange5
                MOV     BYTE PTR ES:STATETYPE.Output,AL
@@NoChange5:    MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu

                JMP     @@EditLoop

                ; Direction Ñndern
@@WeiterN6:     CMP     AL,'d'
                JNZ     @@WeiterN7

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSDirectionO
                MOV     CX,OFFSET szSCharU
                CALL    ChangeMenu

                MOV     AL,kStateColorEdit
                MOV     AH,kDirectionLength
                MOV     BL,kDirectionPos
                MOV     BH,kStateDisplayPosY
                ADD     BH,bCursorPos
                CALL    ChangeAttrib
                MOV     AX,pStateAkt
                MOV     ES,AX
                MOV     AL,kDirectionCharPos
                MOV     AH,BYTE PTR ES:STATETYPE.Direction
                MOV     BL,1
                CALL    StateEditChar
                CMP     AH,1                    ; Wert geÑndert
                JNZ     @@NoChange6
                MOV     BYTE PTR ES:STATETYPE.Direction,AL
@@NoChange6:    MOV     AL,kStateColorSelect
                CALL    StateChngAttrib

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szSBlank
                MOV     CX,OFFSET szSEditMenu
                CALL    ChangeMenu

                JMP     @@EditLoop

@@WeiterN7:     JMP     @@EditLoop

@@Ende:         MOV     AL,bCursorPosBak
                MOV     bCursorPos,AL
                MOV     AX,pStateAktBak
                CMP     AX,0
                JZ      @@WeiterEnde
                MOV     pStateAkt,AX

@@WeiterEnde:   CALL    StatePrintAll

                ; Altes MenÅ wiederherstellen
                CALL    PrintMainMenu

                POP     ES
                POP     CX
                POP     BX
                POP     AX
                RET
StateEdit       ENDP


;***********************************************************************
; StateRead
;   - State-Liste von File lesen
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fÅr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert:
;   - AX: pStateFirst kopie
;-----------------------------------------------------------------------
StateRead       PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                MOV     bFirstChange,0

                CMP     AX,0FFFFh
                JZ      @@StandardFile
                MOV     wSFilenameOff,AX
                JMP     @@AllesKlar0
@@StandardFile: MOV     AX,OFFSET szSFilenameRead
                MOV     wSFilenameOff,AX

                ; Messagebox ausgeben
@@AllesKlar0:   CALL    SaveScreen
                MOV     wSScreen,AX

                MOV     AL,kStateColor
                CALL    SetColor

                MOV     AX,OFFSET szStateReadO
                MOV     BX,9 SHL 8 + 18
                CALL    PrintString
                MOV     AX,OFFSET szStateReadM
                MOV     BX,(10 SHL 8) + 18
                CALL    PrintString
                MOV     AX,OFFSET szStateReadU
                MOV     BX,11 SHL 8 + 18
                CALL    PrintString

                XOR     SI,SI
                MOV     pStateFirst,0

                ; Programmfile îffnen
                MOV     DX,wSFilenameOff
                MOV     AL,00
                MOV     AH,3Dh
                INT     21h
                JNC     @@AllesKlar1
                MOV     AX,OFFSET szSErrorOpenFile
                CALL    NormalError
                JMP     @@ErrorEnd

                ; FileHandle speichern
@@AllesKlar1:   MOV     wSFileHandle,AX

                ; Hauptloop fÅr Programmzeileneinlesen
@@MainLoop:     MOV     AX,OFFSET szSTemp
                MOV     BX,wSFileHandle
                CALL    FileReadLine
                CMP     AX,1
                JNZ     @@AllesKlar11
                JMP     @@EndOfFile

@@AllesKlar11:  CMP     AX,0FFFFh
                JNZ     @@AllesKlar12
                PUSH    OFFSET szSErrorReadFile
                JMP     FatalError

                ; Kommentare Åberspringen
@@AllesKlar12:  CMP     szSTemp,'#'
                JZ      @@MainLoop

                ; Parsen der eingegebenen Daten
                MOV     DI,OFFSET szSTemp

                ; Spaces Åberspringen
@@SpaceLoop1:   CMP     BYTE PTR [DI],' '
                JNZ     @@EndSLoop1
                INC     DI
                JMP     @@SpaceLoop1

@@EndSLoop1:    MOV     AX,DI
@@ZahlLoop1:    CMP     BYTE PTR [DI],' '
                JZ      @@NextSFound1
                CMP     BYTE PTR [DI],0Dh
                JNC     @@AllesKlar2
                JMP     @@MainLoop              ; Leere Zeile erwischt

@@AllesKlar2:   INC     DI
                JMP     @@ZahlLoop1

                ; ThisState nun zwischen AX und DI
@@NextSFound1:  MOV     BYTE PTR [DI],0
                CALL    StringToWORD
                CMP     AX,0FFFFh
                JNZ     @@AllesKlar3
                PUSH    OFFSET szSErrorThisState
                JMP     FatalError

@@AllesKlar3:   MOV     wThisState,AX
                INC     DI

                MOV     bFirstChange,1

                ; Spaces Åberspringen
@@SpaceLoop2:   CMP     BYTE PTR [DI],' '
                JNZ     @@EndSLoop2
                INC     DI
                JMP     @@SpaceLoop2

@@EndSLoop2:    CMP     BYTE PTR [DI],0
                JNZ     @@AllesKlar4
                PUSH    OFFSET szSErrorBadLine
                JMP     FatalError

@@AllesKlar4:   MOV     BL,BYTE PTR [DI]
                CMP     BL,33
                JNC     @@AllesKlar41
                PUSH    OFFSET szSErrorUndefChar
                JMP     FatalError

@@AllesKlar41:  CMP     BL,255
                JNZ     @@AllesKlar42
                PUSH    OFFSET szSErrorUndefChar
                JMP     FatalError

@@AllesKlar42:  MOV     AX,wThisState
                CALL    StateAddLine
                MOV     ES,AX

                INC     DI

                ; Spaces Åberspringen
@@SpaceLoop3:   CMP     BYTE PTR [DI],' '
                JNZ     @@EndSLoop3
                INC     DI
                JMP     @@SpaceLoop3

@@EndSLoop3:    MOV     AX,DI
@@ZahlLoop3:    CMP     BYTE PTR [DI],' '
                JZ      @@NextSFound3
                CMP     BYTE PTR [DI],0
                JNZ     @@AllesKlar5
                PUSH    OFFSET szSErrorBadLine
                JMP     FatalError

@@AllesKlar5:   INC     DI
                JMP     @@ZahlLoop3

                ; NextState nun zwischen AX und DI
@@NextSFound3:  MOV     BYTE PTR [DI],0
                MOV     BX,AX
                ; Testen auf Impossible State
                CMP     BYTE PTR [BX],'-'
                JNZ     @@Weiter1
                CMP     BYTE PTR [BX+1],0
                JNZ     @@Weiter1
                MOV     AX,kImpossibleState
                JMP     @@AllesKlar6

@@Weiter1:      CMP     BYTE PTR [BX],'H'
                JNZ     @@Weiter2
                INC     BX
                CMP     BYTE PTR [BX],'A'
                JNZ     @@Weiter2
                INC     BX
                CMP     BYTE PTR [BX],'L'
                JNZ     @@Weiter2
                INC     BX
                CMP     BYTE PTR [BX],'T'
                JNZ     @@Weiter2
                MOV     AX,kHALTState
                JMP     @@AllesKlar6

@@Weiter2:      CALL    StringToWORD
                CMP     AX,0FFFFh
                JNZ     @@AllesKlar6
                PUSH    OFFSET szSErrorThisState
                JMP     FatalError

@@AllesKlar6:   MOV     WORD PTR ES:[SI].STATETYPE.NextState,AX
                INC     DI

                ; Spaces Åberspringen
@@SpaceLoop4:   CMP     BYTE PTR [DI],' '
                JNZ     @@EndSLoop4
                INC     DI
                JMP     @@SpaceLoop4

@@EndSLoop4:    CMP     BYTE PTR [DI],0
                JNZ     @@AllesKlar7
                PUSH    OFFSET szSErrorBadLine
                JMP     FatalError

@@AllesKlar7:   MOV     BL,BYTE PTR [DI]
                CMP     BL,33
                JNC     @@AllesKlar71
                PUSH    OFFSET szSErrorUndefChar
                JMP     FatalError

@@AllesKlar71:  CMP     BL,255
                JNZ     @@AllesKlar72
                PUSH    OFFSET szSErrorUndefChar
                JMP     FatalError

@@AllesKlar72:  MOV     BYTE PTR ES:[SI].STATETYPE.Output,BL
                INC     DI

                ; Spaces Åberspringen
@@SpaceLoop5:   CMP     BYTE PTR [DI],' '
                JNZ     @@EndSLoop5
                INC     DI
                JMP     @@SpaceLoop5

@@EndSLoop5:    CMP     BYTE PTR [DI],0
                JNZ     @@AllesKlar8
                PUSH    OFFSET szSErrorBadLine
                JMP     FatalError

@@AllesKlar8:   MOV     BL,BYTE PTR [DI]
                CMP     BL,'-'
                JNZ     @@Weiter3
                MOV     CX,WORD PTR ES:[SI].STATETYPE.NextState
                CMP     CX,kImpossibleState
                JZ      @@AllesKlar82
                CMP     CX,kHALTState
                JZ      @@AllesKlar82

@@Weiter3:      CMP     BL,'a'
                JC      @@AllesKlar81
                SUB     BL,'a'-'A'              ; In Grossbuchstabe wandeln
@@AllesKlar81:  CMP     BL,'L'
                JZ      @@AllesKlar82
                CMP     BL,'R'
                JZ      @@AllesKlar82
                PUSH    OFFSET szSErrorUndefDir
                JMP     FatalError

@@AllesKlar82:  MOV     BYTE PTR ES:[SI].STATETYPE.Direction,BL
                JMP     @@MainLoop

                ; File wieder schliessen
@@EndOfFile:    MOV     AH,3Eh
                MOV     BX,wSFileHandle
                INT     21h
                JNC     @@AllesKlar9
                PUSH    OFFSET szSErrorCloseFile
                JMP     FatalError

                CMP     bFirstChange,0
                JNZ     @@AllesKlar9
@@ErrorEnd:     MOV     pStateFirst,0

                ; Alter Bildschirm wieder herstellen
@@AllesKlar9:   MOV     AX,wSScreen
                CALL    RestoreScreen

                MOV     AX,pStateFirst
                MOV     pStateAkt,AX

                ; Register wiederherstellen
                POP     SI
                POP     ES
                POP     DI
                POP     DX
                POP     CX
                POP     BX
                RET
StateRead       ENDP


;***********************************************************************
; StateWrite
;   - aktuelle State-Liste in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fÅr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert: -
;-----------------------------------------------------------------------
StateWrite      PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI
                PUSH    DS
                PUSH    SI

                CMP     AX,0FFFFh
                JZ      @@StandardFile
                MOV     wSFilenameOff,AX
                JMP     @@AllesKlar0
@@StandardFile: MOV     AX,OFFSET szSFilenameWrite
                MOV     wSFilenameOff,AX

                ; Messagebox ausgeben
@@AllesKlar0:   CALL    SaveScreen
                MOV     wSScreen,AX

                ; Farbe fÅr Messagebox setzen
                MOV     AL,kStateColor
                CALL    SetColor

                ; Messagebox zeichnen
                MOV     AX,OFFSET szStateWriteO
                MOV     BX,9 SHL 8 + 18
                CALL    PrintString
                MOV     AX,OFFSET szStateWriteM
                MOV     BX,(10 SHL 8) + 18
                CALL    PrintString
                MOV     AX,OFFSET szStateWriteU
                MOV     BX,11 SHL 8 + 18
                CALL    PrintString

                ; Programmfile kreieren
                MOV     DX,wSFilenameOff
                MOV     CX,0
                MOV     AH,3Ch
                INT     21h
                JNC     @@AllesKlar1
                MOV     AX,OFFSET szSErrorCreateFile
                CALL    NormalError
                JMP     @@Ende

@@AllesKlar1:   MOV     wSFileHandle,AX

                ; Allgemeine Beschreibung des Files
                MOV     AX,OFFSET szStateFileText1
                MOV     BX,wSFileHandle
                CALL    FileWriteLine
                MOV     AX,OFFSET szStateFileText2
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,pStateFirst
                CMP     AX,0
                JZ      @@Close
                MOV     ES,AX

@@StateLoop:    MOV     AX,OFFSET szTSSpaces
                MOV     BX,wSFileHandle
                CALL    FileWriteLine
                CMP     AX,0                    ; Fehler aufgetreten
                JZ      @@AllesKlar2
                MOV     AX,OFFSET szSErrorWriteFile
                CALL    NormalError
                JMP     @@Close

@@AllesKlar2:   MOV     AX,WORD PTR ES:STATETYPE.ThisState
                MOV     BX,OFFSET szSTemp
                CALL    WORDToString
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,OFFSET szIpSpaces
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AL,BYTE PTR ES:STATETYPE.Input
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],AL
                INC     BX
                MOV     BYTE PTR [BX],0
                MOV     AX,OFFSET szSTemp
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,OFFSET szNSSpaces
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,WORD PTR ES:STATETYPE.NextState
                CMP     AX,kImpossibleState
                JNZ     @@Weiter1
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],'-'
                MOV     BYTE PTR [BX+1],0
                MOV     AX,OFFSET szSTemp
                JMP     @@PrintNS

@@Weiter1:      CMP     AX,kHALTState
                JNZ     @@Weiter2
                MOV     AX,OFFSET szHALTState
                JMP     @@PrintNS

@@Weiter2:      MOV     BX,OFFSET szSTemp
                CALL    WORDToString
@@PrintNS:      MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,OFFSET szIpSpaces
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AL,BYTE PTR ES:STATETYPE.Output
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],AL
                MOV     BYTE PTR [BX+1],0
                MOV     AX,OFFSET szSTemp
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,OFFSET szIpSpaces
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AL,BYTE PTR ES:STATETYPE.Direction
                MOV     BX,OFFSET szSTemp
                MOV     BYTE PTR [BX],AL
                INC     BX
                MOV     BYTE PTR [BX],0Dh
                INC     BX
                MOV     BYTE PTR [BX],0Ah
                INC     BX
                MOV     BYTE PTR [BX],0
                MOV     AX,OFFSET szSTemp
                MOV     BX,wSFileHandle
                CALL    FileWriteLine

                MOV     AX,WORD PTR ES:STATETYPE.NextEntry
                CMP     AX,0
                JZ      @@Close
                MOV     ES,AX
                JMP     @@StateLoop

@@Close:        MOV     AH,3Eh
                MOV     BX,wSFileHandle
                INT     21h
                JNC     @@Ende
                MOV     AX,OFFSET szSErrorCloseFile
                CALL    NormalError

                ; Alter Bildschirm wiederherstellen
@@Ende:         MOV     AX,wSScreen
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
StateWrite      ENDP

_CODE           ENDS

END

;**============================= Ende ==============================**

