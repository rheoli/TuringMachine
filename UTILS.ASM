;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     Utils.asm                                          ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;               Allgemeine Prozeduren fÅr Turingmaschine
;**=================================================================**

.186

INCLUDE SCREENIO.ASH

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING


;***********************************************************************
;********               Konstanten von Utils                    ********
;***********************************************************************

kGetColor               EQU kColBlack    + kColGray SHL 4
kEditColor              EQU kColGreen    + kColBlack SHL 4


_DATA           SEGMENT PARA PUBLIC 'DATA'
;***********************************************************************
;********               Variablen von Utils                     ********
;***********************************************************************

szGetO          DB "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª",0
szGetM1         DB "∫  Please input a filename:  ∫",0
szGetM2         DB "∫                            ∫",0
szGetU          DB "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº",0

wUScreen        DW 0
wString         DW 0
wAbk            DW 0

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE'
;***********************************************************************
;********               Prozeduren von Utils                    ********
;***********************************************************************

;***********************************************************************ı
; StringToWORD
;   - Zeichenkette in WORD-Zahl wandeln
;-----------------------------------------------------------------------
; Input:
;   - AX: Zeichenkette ASCIIZ
; Returnwert:
;   - AX: 0xFFFF wenn Fehler aufgetreten, Gewandelte Zahl sonst
;-----------------------------------------------------------------------
StringToWORD    PROC NEAR
                PUSH    DI
                PUSH    BX
                PUSH    CX

                MOV     DI,AX
                XOR     AX,AX
                MOV     BX,1

@@ConvertLoop:  CMP     BYTE PTR [DI],0
                JZ      @@Ende

                CMP     BYTE PTR [DI],' '
                JNZ     @@Weiter1
                OR      BX,BX
                JZ      @@ErrorBereich
                INC     DI
                JMP     @@ConvertLoop

@@Weiter1:      SUB     BYTE PTR [DI],'0'
                JNC     @@ImBereich1
                JMP     @@ErrorBereich

@@ImBereich1:   CMP     BYTE PTR [DI],10
                JB      @@ImBereich2
                JMP     @@ErrorBereich

@@ImBereich2:   MOV     CL,10
                MUL     CL
                ADD     AL,BYTE PTR [DI]
                ADC     AH,0
                INC     DI
                XOR     BX,BX
                JMP     @@ConvertLoop

@@ErrorBereich: MOV     AX,0FFFFh

@@Ende:         POP     CX
                POP     BX
                POP     DI
                RET
StringToWORD    ENDP


;***********************************************************************
; SetFree
;   - den nicht belegten Speicherplatz freigeben 
;   - Info:  Da das Stack-Segment immer das letzte Segment in einer
;            EXE-Datei ist, deutet ES:0000 auf den Anfang und SS:SP
;            auf das Ende des Programms im Speicher. Dadurch kann die
;            LÑnge des Programms berechnet werden
;-----------------------------------------------------------------------
; Input:
;   - ES: Adresse des PSP
; Returnwert: -
; Register:
;   - AX, BX, CL und FLAGS werden verÑndert
;-----------------------------------------------------------------------
SetFree         PROC NEAR
                MOV     BX,SS           ;zunÑchst die beiden Segmentadressen
                MOV     AX,ES           ;voneinander abziehen. Das ergibt die
                SUB     BX,AX           ;Anzahl der Paragraphen vom PSP bis
                                        ;zum Anfang des Stack
                MOV     AX,SP           ;da sich der Stackpointer am Ende des
                MOV     CL,4            ;Stacksegments befindet, gibt sein
                SHR     AX,CL           ;Inhalt die LÑnge des Stacks an
                ADD     BX,AX           ;zur bisherigen LÑnge hinzuaddieren
                INC     BX              ;vorsichtshalber ein Paragraph mehr

                MOV     AH,4Ah          ;neue Grî·e an das DOS Åbergeben
                INT     21h

                RET                     ;zurÅck zum Aufrufer
SetFree         ENDP

;***********************************************************************
; WORDToString
;   - Eine Zahl (WORD) in einen String wandeln
;-----------------------------------------------------------------------
; Input:
;   - AX: zu wandende Zahl
;   - BX: OFFSET zum String
; Returnwert:
;   - AX: OFFSET zur Zahl in ASCIIZ
;-----------------------------------------------------------------------
WORDToString    PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DX

                ADD     BX,20
                MOV     BYTE PTR [BX],0
                DEC     BX
                MOV     CX,10                   ; Wert in AX

@@DIVLoop:      XOR     DX,DX
                DIV     CX                      ; DX:AX DIV CX
                ADD     DL,'0'
                MOV     BYTE PTR [BX],DL
                CMP     AX,0                    ; ganze Zahl geschrieben in String
                JZ      @@DIVEnde
                DEC     BX
                JMP     @@DIVLoop

@@DIVEnde:      MOV     AX,BX

                POP     DX
                POP     CX
                POP     BX
                RET
WORDToString    ENDP


;***********************************************************************
; GetFilename
;   - Filename eingeben Åber Dialogbox
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET auf String
;   - BX: OFFSET auf TitelString
;   - CX: OFFSET von Fileendung
; Returnwert:
;   - AX: 0 wenn alles OK, 1 wenn ESC gedrÅckt
;-----------------------------------------------------------------------
GetFilename     PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                MOV     DI,AX
                MOV     wString,AX
                MOV     SI,BX
                MOV     wAbk,CX

                ; Messagebox ausgeben
                CALL    SaveScreen
                MOV     wUScreen,AX

                MOV     AL,kGetColor
                CALL    SetColor

                MOV     AX,OFFSET szGetO
                MOV     BL,39
                MOV     BH,10
                MOV     CL,kTACenter
                CALL    PrintAlignString
                MOV     AX,SI
                CALL    PrintAlignString
                MOV     AX,OFFSET szGetM1
                ADD     BH,1
                CALL    PrintAlignString
                MOV     AX,OFFSET szGetM2
                ADD     BH,1
                CALL    PrintAlignString
                MOV     AX,OFFSET szGetU
                ADD     BH,1
                CALL    PrintAlignString

                MOV     AL,kEditColor
                MOV     AH,8
                MOV     BL,39-4
                MOV     BH,12
                CALL    ChangeAttrib

                CALL    SetColor

                MOV     BYTE PTR [DI],0

@@GetLoop:      MOV     AX,0700h
                INT     21h

                CMP     AL,0
                JNZ     @@WeiterN1
                MOV     AX,0700h
                INT     21h
                JMP     @@GetLoop

                ; Return
@@WeiterN1:     CMP     AL,13
                JNZ     @@WeiterN2
                PUSH    0
                JMP     @@Ende

                ; ESC
@@WeiterN2:     CMP     AL,27
                JNZ     @@WeiterN3
                PUSH    1
                JMP     @@Ende

                ; Delete
@@WeiterN3:     CMP     AL,08h
                JNZ     @@WeiterN4
                CMP     DI,wString
                JZ      @@EndeN3
                DEC     DI
                MOV     BYTE PTR [DI],' '
                MOV     AX,wString
                MOV     BL,39-4
                MOV     BH,12
                CALL    PrintString
                MOV     BYTE PTR [DI],0
@@EndeN3:       JMP     @@GetLoop

@@WeiterN4:     CMP     AL,'-'
                JZ      @@Buchst
                CMP     AL,'_'
                JZ      @@Buchst
                CMP     AL,'a'
                JC      @@NoKlein
                SUB     AL,'a'-'A'
@@NoKlein:      CMP     AL,'A'
                JC      @@WeiterN5
                CMP     AL,'Z'+1
                JNC     @@WeiterN5
@@Buchst:       MOV     CX,DI
                SUB     CX,wString
                CMP     CX,8
                JZ      @@EndeN4
                MOV     BYTE PTR [DI],AL
                MOV     BYTE PTR [DI+1],0
                INC     DI
                MOV     AX,wString
                MOV     BL,39-4
                MOV     BH,12
                CALL    PrintString
@@EndeN4:       JMP     @@GetLoop

@@WeiterN5:     CMP     AL,'0'
                JC      @@WeiterN6
                CMP     AL,'9'+1
                JNC     @@WeiterN6
                JMP     @@Buchst

@@WeiterN6:     JMP     @@GetLoop

                ; Alter Bildschirm wieder herstellen
@@Ende:         MOV     AX,wUScreen
                CALL    RestoreScreen

                MOV     SI,wAbk
                MOV     AX,DS
                MOV     ES,AX
                MOV     CX,5
                REP     MOVSB

                POP     AX

                POP     SI
                POP     ES
                POP     DI
                POP     CX
                POP     BX
                RET
GetFilename     ENDP

_CODE           ENDS

END
