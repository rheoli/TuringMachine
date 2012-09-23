;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     ErrorIO.asm                                        ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;                     Fehlermeldungen ausgeben
;**=================================================================**

.186

INCLUDE SCREENIO.ASH
INCLUDE TURING.ASH

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING

_DATA           SEGMENT PARA PUBLIC 'DATA'
;***********************************************************************
;********               Variablen von ErrorIO                   ********
;***********************************************************************

; Allgemeine Variablen
wErrScreen      DW 0                    ; Variable fr Speicherallocation

; Guru-Meditation Text
szGuru1         DB "ษอออออออออออออออออออออออออออออออ Guru - Meditation ออออออออออออออออออออออออออออป",0
szGuru2         DB "บ                                                                              บ",0
szGuru3         DB "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ",0

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********               Prozeduren von ErrorIO                  ********
;***********************************************************************

;***********************************************************************
; FatalError
;   - Fatal-Fehlermeldung Ausgeben
;   - nach der Meldung wird das Programm sofort abgebrochen mit
;     GeneralExit aus Modul Turing.asm
;-----------------------------------------------------------------------
; Input:
;   - Auf Stack OFFSET zu Fehlermessage
; Returnwert: -
;-----------------------------------------------------------------------
FatalError      PROC NEAR

                CALL    SaveScreen
                MOV     wErrScreen,AX

                MOV     AX,wErrScreen
                CALL    GuruScreen

                MOV     AL,132
                MOV     AH,0
                CALL    SetColor
                MOV     AX,OFFSET szGuru1
                MOV     BL,0
                MOV     BH,0
                CALL    PrintString

                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,1
                CALL    PrintString
                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,2
                CALL    PrintString
                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,3
                CALL    PrintString

                MOV     AX,OFFSET szGuru3
                MOV     BL,0
                MOV     BH,4
                CALL    PrintString

                POP     AX
                MOV     BL,40
                MOV     BH,2
                MOV     CL,kTACenter
                CALL    PrintAlignString

                MOV     AH,07h
                INT     21h

                MOV     AX,wErrScreen
                CALL    RestoreScreen

                JMP     GeneralExit
FatalError      ENDP


;***************************************************************************
; NormalError
;   - Fehlermeldung Ausgeben
;   - nach der Fehlermeldung wird wieder in die aufrufende Prozedur
;     zurckgesprungen
;-----------------------------------------------------------------------
; Input:
;   - AX: Fehlermeldung
; Returnwert: -
;-----------------------------------------------------------------------
NormalError     PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    AX

                CALL    SaveScreen
                MOV     wErrScreen,AX

                MOV     AX,wErrScreen
                CALL    GuruScreen

                MOV     AL,132
                MOV     AH,0
                CALL    SetColor
                MOV     AX,OFFSET szGuru1
                MOV     BL,0
                MOV     BH,0
                CALL    PrintString

                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,1
                CALL    PrintString
                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,2
                CALL    PrintString
                MOV     AX,OFFSET szGuru2
                MOV     BL,0
                MOV     BH,3
                CALL    PrintString

                MOV     AX,OFFSET szGuru3
                MOV     BL,0
                MOV     BH,4
                CALL    PrintString

                POP     AX
                MOV     BL,40
                MOV     BH,2
                MOV     CL,kTACenter
                CALL    PrintAlignString

                MOV     AH,07h
                INT     21h

                MOV     AX,wErrScreen
                CALL    RestoreScreen

                POP     CX                                
                POP     BX
                POP     AX
                RET
NormalError     ENDP

_CODE           ENDS

END
