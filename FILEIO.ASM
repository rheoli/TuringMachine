;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     FileIO.asm                                         ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;                  Allgemeine FileIO Verarbeitung
;**=================================================================**

.186

ASSUME CS:_CODE, ES:NOTHING

_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********               Prozeduren von FileIO                   ********
;***********************************************************************

;***********************************************************************
; FileGetLength
;   - Filel„nge ab aktuellem Filepointer bestimmen
;-----------------------------------------------------------------------
; Input:
;   - BX: FileHandle
; Returnwert:
;   - BX: Filelaenge oder BX=0FFFFh wenn Fehler aufgetreten
;-----------------------------------------------------------------------
FileGetLength   PROC NEAR
                PUSH    AX
                PUSH    CX
                PUSH    DX

                MOV     AH,42h          ; Move File Pointer
                MOV     AL,01h          ; Seek from current Position
                XOR     CX,CX           ; no Pointer move
                XOR     DX,DX
                INT     21h
                JNC     @@CurrentOK

                MOV     BX,0FFFFh
                JMP     @@Ende

@@CurrentOK:    PUSH    AX              ; Save Current Position
                MOV     AH,42h          
                MOV     AL,02h          ; Seek from EndOfFile
                XOR     CX,CX           ; Search Total File Length
                XOR     DX,DX
                INT     21h
                JNC     @@EOFOK

                POP     BX
                MOV     BX,0FFFFh
                JMP     @@Ende

@@EOFOK:        POP     DX              ; Current Position in DX
                SUB     AX,DX           ; Total Tapetextl„nge

                PUSH    AX              ; Tapetextl„nge retten

                ; Filezeiger wieder auf Current stellen
                MOV     AH,42h
                XOR     AL,AL           ; Seek from begin
                XOR     CX,CX           ; DX ist bereits mit Offset initialisiert
                INT     21h
                JNC     @@Ende1

                POP     BX              ; gerettete Tapetextl„nge l”schen
                MOV     BX,0FFFFh
                JMP     @@Ende

@@Ende1:        POP     BX                

@@Ende:         POP     DX
                POP     CX
                POP     AX
                RET
FileGetLength   ENDP


;***********************************************************************
; FileReadLine
;   - Textzeile aus File lesen
;   - Zeilenenden mssen ein 0x0D, 0x0A enthalten, genau in dieser
;     Reihenfolge geschrieben sein.
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fr Zeichenkette aus File
;   - BX: FileHandle
; Returnwert:
;   - AX: 0x0000 fr alles OK, 0x0001 = EOF und 0xFFFF = Fehler
;-----------------------------------------------------------------------
FileReadLine    PROC NEAR
                PUSH    DI
                PUSH    DX
                PUSH    CX

                MOV     DI,AX
@@ReadLoop:
                MOV     CX,1
                MOV     DX,DI
                MOV     AH,3Fh
                INT     21h
                JC      @@ReadError
                OR      AX,AX
                JZ      @@EndOfFile
                CMP     BYTE PTR [DI],0Dh
                JZ      @@EndOfLine
                CMP     BYTE PTR [DI],0Ah
                JNZ     @@Incr
                MOV     BYTE PTR [DI],0
                JMP     @@ReadLoop
@@Incr:         INC     DI
                JMP     @@ReadLoop

@@EndOfLine:    MOV     BYTE PTR [DI],0
                XOR     AX,AX
                JMP     @@Ende
@@EndOfFile:    MOV     BYTE PTR [DI+1],0
                MOV     AX,1
@@Weiter:       JMP     @@Ende

@@ReadError:    MOV     AX,0FFFFh

@@Ende:         POP     CX
                POP     DX
                POP     DI
                RET
FileReadLine    ENDP

;***********************************************************************
; FileWriteLine
;   - Textzeile in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET des Textes in ASCIIZ
;   - BX: FileHandle
; Returnwert:
;   - AX: 0x0000 fr alles OK, 0xFFFF fr Fehler
;-----------------------------------------------------------------------
FileWriteLine   PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI
                PUSH    ES

                MOV     DX,AX                   ; OFFSET fr Text retten

                ; Textl„nge suchen
                MOV     DI,AX                   ; SCASB: ES:[DI]
                MOV     AX,DS
                MOV     ES,AX
                XOR     AL,AL                   ; Vergleichswert
                MOV     CX,0FFFFh               ; auf Maximum setzen
                CLD                             ; DI inkrementieren
                REPNE   SCASB

                SUB     DI,DX                   ; Textl„nge berechnen -> DI

                MOV     AH,40h
                MOV     CX,DI
                DEC     CX
                INT     21h
                JNC     @@OK
                MOV     AX,0FFFFh
                JMP     @@Ende

@@OK:           MOV     AX,0

@@Ende:         POP     ES
                POP     DI
                POP     DX
                POP     CX
                POP     BX
                RET
FileWriteLine   ENDP

_CODE           ENDS

END
