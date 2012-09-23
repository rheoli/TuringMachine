;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     Turing.asm                                         ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;                   Hauptmodul der Turingmaschine
;**=================================================================**

.186

INCLUDE SCREENIO.ASH
INCLUDE UTILS.ASH
INCLUDE TAPEIO.ASH
INCLUDE STATEIO.ASH
INCLUDE ERRORIO.ASH

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING, SS:_STACK

;***********************************************************************
;********               Konstanten von Turing                   ********
;***********************************************************************

; Farbendefinitionen
kBackColor      EQU kColWhite + kColBlue SHL 4
kMenuColor      EQU kColBlack + kColGray SHL 4
kMenuSelColor   EQU kColRed   + kColGray SHL 4

;RunState Konstanten
kRunning                EQU 0
kStopped                EQU 1
kHalted                 EQU 2

; Total Zeilen auf Bildschirm
; KeysLine        EQU 24


_DATA           SEGMENT PARA PUBLIC 'DATA'
;***********************************************************************
;********                 Daten von Turing                      ********
;***********************************************************************

; Variablen die auch ausserhalb dieses Moduls verwendet werden drfen
PUBLIC          wRunState

; Titeltexte
szMTitelRun     DB "Turing-Meais",161,"n V 1.00",0
szMTitelStop    DB "Turing-Meais",161,"n V 1.00 - Stopped",0
szMTitelHalt    DB "Turing-Meais",161,"n V 1.00 - Halted",0

; Menutexte
szMRunMenu      DB "&S&p&a&c&e-Program Step,  &R-Restart, &.-Run Program,  &,-Trace Program",0
szMStopMenu     DB "&R-Restart,  &C-Continue",0
szMHaltMenu     DB "&R-Restart",0
szMBlank        DB " ",0
szMMainMenu     DB "&T-Edit Tape,  &P-Edit States,  &S-Save,  &L-Load,  &E&S&C-Exit Program",0
szMSaveMenu     DB "&T-Save Tape,  &P-Save States,  &E&S&C-Back to programmode",0
szMLoadMenu     DB "&T-Load Tape,  &P-Load States,  &E&S&C-Back to programmode",0
szMFileMenuO    DB "Inputfilename",0
szMFileMenuU    DB "&R&E&T-Save File,  &E&S&C-Exit without saving",0

; Titel fr Load/Save Messageboxen
szLoadState     DB " Load State Data ",0
szLoadTape      DB " Load Tape Data ",0
szSaveState     DB " Save State Data ",0
szSaveTape      DB " Save Tape Date ",0

; Filehandling
szMFilename     DB 20 DUP (' ')
szMSAbk         DB ".STA",0
szMTAbk         DB ".TAP",0

; Allgemeine Variablen von Turing
wCursorAtStart  DW 0            ; Bildschirmcursorposition gespeichert
wRunState       DW kRunning     ; RunState der Turingmaschine
wOldScreen      DW 0            ; Speichersegment fr Bildschirm

; Fehlermeldungstext
szMErrorPrgr    DB "Impossible State found.",0

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********                Prozeduren von Turing                  ********
;***********************************************************************

;***********************************************************************
; GeneralExit
;   - Beenden des Programms mit Bildschirm- und Cursorrestore
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
GeneralExit     PROC NEAR
                CALL    ResetScreen
                MOV     AX,wOldScreen
                CALL    RestoreScreen

                ; Cursor wieder an den alten Standort bringen
                MOV     AH,02h
                MOV     BH,00h
                MOV     DX,wCursorAtStart
                INT     10h                  

                MOV     AH,4Ch
                XOR     AL,AL
                INT     21h
GeneralExit     ENDP


;***********************************************************************
; PrintMainMenu
;   - Hauptmenu in Abh„ngigkeit von wRunState ausgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
PrintMainMenu   PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX

                MOV     AL,kMenuColor
                CALL    SetColor

                CMP     wRunState,kStopped
                JNZ     @@StateHaltT
                MOV     AX,OFFSET szMTitelStop
                JMP     @@Titel
@@StateHaltT:   CMP     wRunState,kHalted
                JNZ     @@StateRunT
                MOV     AX,OFFSET szMTitelHalt
                JMP     @@Titel
@@StateRunT:    MOV     AX,OFFSET szMTitelRun
@@Titel:        CALL    PrintTitle

                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     CX,OFFSET szMMainMenu
                CMP     wRunState,kStopped
                JNZ     @@StateHaltM
                MOV     BX,OFFSET szMStopMenu
                JMP     @@Menu
@@StateHaltM:   CMP     wRunState,kHalted
                JNZ     @@StateRunM
                MOV     BX,OFFSET szMHaltMenu
                JMP     @@Menu
@@StateRunM:    MOV     BX,OFFSET szMRunMenu
@@Menu:         CALL    ChangeMenu

                POP     CX
                POP     BX
                POP     AX
                RET
PrintMainMenu   ENDP


;***********************************************************************
; ProgramStep
;   - einen Programmschritt der Turingmaschine ausfhren
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: 1 weitere Steps m”glich, 0 Programmende erreicht
;-----------------------------------------------------------------------
ProgramStep     PROC NEAR

                ; N„chsten State ermitteln
                CALL    StateGetNext

                ; Impossible State gefunden
                CMP     AX,kImpossibleState
                JNZ     @@PrgrWeiter2
                MOV     AX,OFFSET szMErrorPrgr
                CALL    NormalError
                MOV     AX,kStopped
                JMP     @@Ende

@@PrgrWeiter2:  CALL    StateGetOutput
                CALL    TapeWriteChar

                CALL    StateGetDirect
                CMP     AL,'R'
                JNZ     @@LeftShift
                CALL    TapeRightShift
                JMP     @@NextCom

@@LeftShift:    CALL    TapeLeftShift
@@NextCom:      CALL    TapeReadChar
                MOV     BL,AL
                CALL    StateGetNext
                CMP     AX,kHALTState
                JNZ     @@RunWeiter
                MOV     AX,kHalted
                JMP     @@Ende

@@RunWeiter:    CALL    StateSetThis
                ; -> StateSetThis gibt sein AX-Rckgabewert direkt weiter

@@Ende:
                RET
ProgramStep     ENDP


;***********************************************************************
; SaveProc
;   - Auswahlmen fr zum speichern des Tapes oder des Programms
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
SaveProc        PROC    NEAR
                PUSH    AX
                PUSH    BX

                ; Savemen ausgeben
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMBlank
                MOV     CX,OFFSET szMSaveMenu
                CALL    ChangeMenu

@@SaveLoop:     MOV     AX,0700h
                INT     21h

                CMP     AL,0
                JNZ     @@Weiter0
                MOV     AX,0700h
                INT     21h
                JMP     @@SaveLoop

@@Weiter0:      CMP     AL,'a'
                JC      @@Weiter1
                SUB     AL,'a'-'A'

@@Weiter1:      CMP     AL,27
                JNZ     @@Weiter2
                JMP     @@Ende

@@Weiter2:      CMP     AL,'T'
                JNZ     @@Weiter3
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMFileMenuO
                MOV     CX,OFFSET szMFileMenuU
                CALL    ChangeMenu
                MOV     AX,OFFSET szMFilename
                MOV     BX,OFFSET szSaveTape
                MOV     CX,OFFSET szMTAbk
                CALL    GetFilename
                CMP     AX,1
                JZ      @@Ende
                MOV     AX,OFFSET szMFilename
                CALL    TapeWrite
                JMP     @@Ende
                
@@Weiter3:      CMP     AL,'P'
                JNZ     @@SaveLoop
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMFileMenuO
                MOV     CX,OFFSET szMFileMenuU
                CALL    ChangeMenu
                MOV     AX,OFFSET szMFilename
                MOV     BX,OFFSET szSaveTape
                MOV     CX,OFFSET szMSAbk
                CALL    GetFilename
                CMP     AX,1
                JZ      @@Ende
                MOV     AX,OFFSET szMFilename
                CALL    StateWrite

                ; Altes Men wiederherstellen
@@Ende:         CALL    PrintMainMenu

                POP     BX
                POP     AX
                RET
SaveProc        ENDP

;***********************************************************************
; LoadProc
;   - Auswahlmen zum Laden eines des Tapes oder des Programms
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
LoadProc        PROC    NEAR
                PUSH    AX
                PUSH    BX

                ; Loadmen ausgeben
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMBlank
                MOV     CX,OFFSET szMLoadMenu
                CALL    ChangeMenu

@@LoadLoop:     MOV     AX,0700h
                INT     21h

                CMP     AL,0
                JNZ     @@Weiter0
                MOV     AX,0700h
                INT     21h
                JMP     @@LoadLoop

@@Weiter0:      CMP     AL,'a'
                JC      @@Weiter1
                SUB     AL,'a'-'A'

@@Weiter1:      CMP     AL,27
                JNZ     @@Weiter2
                JMP     @@Ende

@@Weiter2:      CMP     AL,'T'
                JNZ     @@Weiter3
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMFileMenuO
                MOV     CX,OFFSET szMFileMenuU
                CALL    ChangeMenu
                MOV     AX,OFFSET szMFilename
                MOV     BX,OFFSET szLoadTape
                MOV     CX,OFFSET szMTAbk
                CALL    GetFilename
                CMP     AX,1
                JZ      @@Ende
                MOV     AX,OFFSET szMFilename
                CALL    TapeRead
                CALL    TapePrintAll
                MOV     wRunState,kRunning
                CALL    TapeReadChar
                MOV     BL,AL
                MOV     AX,1
                CALL    StateSetThis
                CMP     AX,0
                JNZ     @@Ende
                MOV     wRunState,kStopped
                JMP     @@Ende
                
@@Weiter3:      CMP     AL,'P'
                JNZ     @@LoadLoop
                MOV     AL,kMenuColor
                MOV     AH,kMenuSelColor
                MOV     BX,OFFSET szMFileMenuO
                MOV     CX,OFFSET szMFileMenuU
                CALL    ChangeMenu
                MOV     AX,OFFSET szMFilename
                MOV     BX,OFFSET szLoadState
                MOV     CX,OFFSET szMSAbk
                CALL    GetFilename
                CMP     AX,1
                JZ      @@Ende
                MOV     AX,OFFSET szMFilename
                CALL    StateRead
                CMP     AX,0
                JZ      @@NoStates
                CALL    StatePrintAll
                MOV     wRunState,kRunning
                CALL    TapeReadChar
                MOV     BL,AL
                MOV     AX,1
                CALL    StateSetThis
                CMP     AX,0
                JNZ     @@Ende
@@NoStates:     MOV     wRunState,kStopped
                CALL    StatePrintAll

                ; Altes Men wiederherstellen
@@Ende:         CALL    PrintMainMenu

                POP     BX
                POP     AX
                RET
LoadProc        ENDP


;***********************************************************************
; Main
;   - Hauptprogramm der Turing-Maschine
;-----------------------------------------------------------------------
Main            PROC NEAR

                CALL    SetFree                 ; von Programm nicht belegter
                                                ; Speicher freigeben
                MOV     AX,_DATA
                MOV     DS,AX

                MOV     AH,03h                  ; Get current cursor position
                MOV     BH,00h
                INT     10h
                MOV     wCursorAtStart,DX

                MOV     wRunState,kRunning
                
                CALL    SaveScreen
                MOV     wOldScreen,AX

                CALL    InitScreen

                ; Cursor ins Nirvana schicken
                MOV     AH,02h
                MOV     BH,00h
                MOV     DL,90
                MOV     DH,90
                INT     10h                  

                ; Tapedaten aus File lesen
                MOV     AX,0FFFFh
                CALL    TapeRead

                ; Programmdaten lesen
                MOV     AX,0FFFFh
                MOV     wRunState,kRunning
                CALL    StateRead
                CMP     AX,0
                JNZ     @@Weiter000
                MOV     wRunState,kStopped

                ; Hintergrundfarbe setzen
@@Weiter000:    MOV     AL,kBackColor
                CALL    SetColor
                CALL    ClrScr

                MOV     AL,kBackColor
                CALL    SetColor

                ; Tape ausgeben
                CALL    TapePrintAll

                ; Programm-Listing ausgeben
                CALL    StatePrintAll

                ; Grundeinstellung der Turingmaschine
                CMP     wRunState,kRunning
                JNZ     @@TuringMenu
                CALL    TapeReadChar
                MOV     BL,AL
                MOV     AX,1
                CALL    StateSetThis
                CMP     AX,kRunning
                JZ      @@TuringMenu
                MOV     wRunState,AX

@@TuringMenu:   CALL    PrintMainMenu

@@TuringLoop:   MOV     AX,700h
                INT     21h

                CMP     AL,0
                JNZ     @@NormalChar
                MOV     AX,700h
                INT     21h
                JMP     @@TuringLoop

@@NormalChar:   CMP     AL,'a'
                JC      @@Weiter0
                SUB     AL,'a'-'A'

                ; Space gedrckt -> Programmstep
@@Weiter0:      CMP     AL,' '
                JNZ     @@Weiter00
                CMP     wRunState,kRunning
                JNZ     @@TuringLoop
                CALL    ProgramStep
                MOV     wRunState,AX
                CMP     AX,kRunning
                JZ      @@TuringLoop

                ; Programm wurde mit HALT gestoppt
                CALL    PrintMainMenu
                JMP     @@TuringLoop

@@Weiter00:     CMP     AL,'L'
                JNZ     @@Weiter1
                CALL    LoadProc
                JMP     @@TuringLoop

@@Weiter1:      CMP     AL,'S'
                JNZ     @@Weiter2
                CALL    SaveProc
                JMP     @@TuringLoop

                ; Tapeedit
@@Weiter2:      CMP     AL,'T'
                JNZ     @@Weiter3
                CALL    TapeEdit
                CALL    TapeReadChar
                MOV     BL,AL
                CALL    StateGetThis
                CALL    StateSetThis
                CMP     AX,kRunning
                JZ      @@Run2
                MOV     wRunState,AX
                JMP     @@Ende2
@@Run2:         MOV     wRunState,kRunning
@@Ende2:        CALL    PrintMainMenu
                JMP     @@TuringLoop

                ; Programmedit
@@Weiter3:      CMP     AL,'P'
                JNZ     @@Weiter4
                CALL    StateEdit
                CALL    TapeReadChar
                MOV     BL,AL
                CALL    StateGetThis
                CALL    StateSetThis
                CMP     AX,kRunning
                JZ      @@Run3
                MOV     wRunState,AX
                JMP     @@Ende3
@@Run3:         MOV     wRunState,kRunning
@@Ende3:        CALL    PrintMainMenu
                JMP     @@TuringLoop

                ; Programmende
@@Weiter4:      CMP     AL,27
                JNZ     @@Weiter5
                JMP     @@Ende

                ; Restarten des Turingprogramms
@@Weiter5:      CMP     AL,'R'
                JNZ     @@Weiter6
                MOV     AX,0FFFFh
                CALL    TapeRead
                CALL    TapePrintAll
                CALL    TapeReadChar
                MOV     BL,AL
                MOV     AX,1
                CALL    StateSetThis
                CMP     AX,kRunning
                JZ      @@Run
                MOV     wRunState,AX
                JMP     @@Ende5
@@Run:          MOV     wRunState,kRunning
@@Ende5:        CALL    PrintMainMenu
                JMP     @@TuringLoop

@@Weiter6:      CMP     AL,'C'
                JNZ     @@Weiter7
                CMP     wRunState,kStopped
                JNZ     @@Ende6
                CALL    StateGetFirst
                CMP     AX,0
                JZ      @@Ende6
                MOV     wRunState,kRunning
                CALL    TapeReadChar
                MOV     BL,AL
                CALL    StateGetNext
                CALL    StateSetThis
                CMP     AX,kRunning
                JZ      @@Run6
                MOV     wRunState,AX
                JMP     @@Ende61
@@Run6:         MOV     wRunState,kRunning
@@Ende61:       CALL    PrintMainMenu
@@Ende6:        JMP     @@TuringLoop

@@Weiter7:      CMP     AL,'.'
                JNZ     @@Weiter8
                CMP     wRunState,kRunning
                JNZ     @@TuringLoop
@@RunState:     CALL    ProgramStep
                MOV     wRunState,AX
                CMP     AX,kRunning
                JNZ     @@Ende7
                JMP     @@RunState
@@Ende7:        CALL    PrintMainMenu
                JMP     @@TuringLoop

@@Weiter8:      CMP     AL,','
                JNZ     @@WeiterEnde
                CMP     wRunState,kRunning
                JNZ     @@TuringLoop
@@RunState2:    CALL    ProgramStep
                MOV     wRunState,AX
                CMP     AX,kRunning
                JNZ     @@Ende8                
                MOV     CX,60
@@RunLoop2:     PUSH    CX
                MOV     CX,0FFFFh
@@RunLoop:      LOOP    @@RunLoop
                POP     CX
                LOOP    @@RunLoop2
                JMP     @@RunState2
@@Ende8:        CALL    PrintMainMenu
                JMP     @@TuringLoop

@@WeiterEnde:   JMP     @@TuringLoop                

@@Ende:         JMP     GeneralExit

Main            ENDP

_CODE           ENDS


;***********************************************************************
;********           Stacksegment der Turing-Maschine            ********
;***********************************************************************
_STACK          SEGMENT PARA STACK USE16
                DB 1000h DUP (?)
_STACK          ENDS

END Main
