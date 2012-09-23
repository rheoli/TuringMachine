;=====================================================================
;===========              Turing-Machine             =================
;=====================================================================
;==   Version:   1.00                                               ==
;==   Datum:     03.Juli 1996                                       ==
;==   Modul:     ScreenIO.asm                                       ==
;==                                                                 ==
;==   Copyright (c) 1996 by St.Toggweiler, A.Rietsch                ==
;=====================================================================

;**=================================================================**
;             Allgemeine Bildschirmausgabeprozeduren
;**=================================================================**

.186

ASSUME CS:_CODE, DS:_DATA, ES:NOTHING, SS:NOTHING

INCLUDE SCREENIO.ASH

;***********************************************************************
;********             Bildschirmsegment (Hardware)              ********
;***********************************************************************
_SCREEN         SEGMENT AT 0B800h
_SCREEN         ENDS

_DATA           SEGMENT PARA PUBLIC 'DATA' USE16
;***********************************************************************
;********                 Daten von ScreenIO                    ********
;***********************************************************************
BlankLine       DB "                                                                                ",0
szNoMem         DB "Not enough memory. HELP! HELP!", 10, 13, '$'

; Globale Variablen fÅr Screen
bAttribut       DB     7
bTemp           DB     ?
bTemp2          DB     ?
bLineHorz       DB   205
bLineKreuzL     DB   204
bLineKreuzR     DB   185

_DATA           ENDS


_CODE           SEGMENT PARA PUBLIC 'CODE' USE16
;***********************************************************************
;********              Prozeduren von ScreenIO                  ********
;***********************************************************************

;***********************************************************************
; CalcPoint
;   - Offsetadresse fÅr direkter Bildschirmzugriff berechnen
;-----------------------------------------------------------------------
; Input:
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert:
;   - DI: Offsetadresse auf Bildschirm
;-----------------------------------------------------------------------
CalcPoint       PROC NEAR
                PUSH    AX
                PUSH    BX

                XOR     AH,AH
                MOV     AL,kScrWidth *2         ; Screen Breite im Speicher
                MUL     BH                      ; y-Pos im Speicher
                XOR     BH,BH
                SHL     BL,1                    ; x-Pos im Speicher (Zeichen,Attrib) 1 WORD/Zeichen                
                ADD     AX,BX                   ; zu y-Pos addieren
                MOV     DI,AX                   ; in DI fÅr Offsetadresse

                POP     BX
                POP     AX
                RET
CalcPoint       ENDP


;***********************************************************************
; SetColor
;   - Text- und Hintergrundfarbe festlegen fÅr die Zeichen
;-----------------------------------------------------------------------
; Input:
;   - AL: Neue Text- und Hintergrundfarbe
; Returnwert: -
;-----------------------------------------------------------------------
SetColor        PROC NEAR
                MOV     bAttribut,AL            ; Attribut in allg. Variable speichern
                RET
SetColor        ENDP


;***********************************************************************
; InitScreen
;   - VideoMode initialisieren
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
InitScreen      PROC NEAR
                PUSHA
                ; Videomodus 80x25 setzen
                MOV     AX,3
                INT     10h

                POPA
                RET
InitScreen      ENDP


;***********************************************************************
; ResetScreen
;   - Normaler DOS-Screen wiederherstellen
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
ResetScreen     PROC NEAR
                PUSH    AX

                ; Videomodus 80x25 setzen
                MOV     AX,3
                INT     10h

                POP     AX
                RET
ResetScreen     ENDP


;***********************************************************************
; ChangeAttrib
;   - Farbe und Hintergrund von Zeichen Ñndern
;-----------------------------------------------------------------------
; Input:
;   - AL: Attribut
;   - AH: Anzahl Zeichen
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert: -
;-----------------------------------------------------------------------
ChangeAttrib    PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DI
                PUSH    ES

                MOV     CL,AH
                XOR     CH,CH

                MOV     DI,_SCREEN
                MOV     ES,DI
                CALL    CalcPoint

                INC     DI

@@AttribLoop:   MOV     BYTE PTR ES:[DI],AL
                ADD     DI,2
                LOOP    @@AttribLoop

                POP     ES
                POP     DI
                POP     CX
                POP     BX
                POP     AX
                RET
ChangeAttrib    ENDP


;***********************************************************************
; PrintChar
;   - Darstellen eines Zeichens auf dem Bildschirm
;   - Farbenattribut wird mit dem Zeichen mitgegeben
;-----------------------------------------------------------------------
; Input:
;   - AL: Zeichen
;   - AH: Attribut
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert: -
;-----------------------------------------------------------------------
PrintChar       PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    DX
                PUSH    DI
                PUSH    ES

                ; BildschirmÅberlauf kontrollieren
                CMP     BL,kScrWidth -1
                JA      @@Ende
                CMP     BH,kScrHeight -1
                JA      @@Ende

                ; Screen Segment laden
                MOV     DX,_SCREEN
                MOV     ES,DX
                ; Pixeloffset berechnen
                CALL    CalcPoint
                MOV     ES:[DI],AL              ; Zeichen
                MOV     ES:[DI+1],AH            ; Attribut

@@Ende:         POP     ES
                POP     DI
                POP     DX
                POP     BX
                POP     AX
                RET
PrintChar       ENDP


;***********************************************************************
; PrintString
;   - Schreiben eines Strings auf dem Bildschirm
;   - ASCII-String mit NULL-Zeichen-Ende
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert: -
;-----------------------------------------------------------------------
PrintString     PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                ; BildschirmÅberlauf kontrollieren
                CMP     BL,kScrWidth -1
                JA      @@Ende
                CMP     BH,kScrHeight -1
                JA      @@Ende

                CLD                             ; ZÑhler incrementieren

                MOV     SI,_SCREEN
                MOV     ES,SI
                CALL    CalcPoint               ; Screen-Offset berechnen
                MOV     SI,AX
                MOV     AH,bAttribut            ; Attribut fÅr alle Zeichen gleich
                LODSB                           ; DS:[SI] -> AL / SI++
                CMP     AL,0                    ; Stringende ?
                JZ      @@Ende
@@Write:        STOSW                           ; AX -> ES:[DI] / DI++
                LODSB
                CMP     AL,0                    ; Stringende ?
                JNZ     @@Write

@@Ende:         POP     SI
                POP     ES
                POP     DI
                POP     BX
                POP     AX
                RET
PrintString     ENDP


;***********************************************************************
; PrintAlignString
;   - Schreiben eines ausgerichteten Strings auf dem Bildschirm
;   - ASCII-String mit NULL-Zeichen-Ende
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
;   - BL: x-Koord
;   - BH: y-Koord
;   - CL: Text-Align (TA_LEFT, TA_RIGHT, TA_CENTER)
; Returnwert: -
;-----------------------------------------------------------------------
PrintAlignString PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                ; BildschirmÅberlauf kontrollieren
                CMP     BL,kScrWidth -1
                JA      @@Ende
                CMP     BH,kScrHeight -1
                JA      @@Ende

                MOV     DX,BX                   ; Koordinaten in DX

                MOV     SI,_SCREEN
                MOV     ES,SI
                MOV     SI,AX                   ; Stringoffset nach SI

                CLD                             ; ZÑhler incrementieren

                LODSB                           ; DS:[SI] -> AX
                CMP     AL,0
                JZ      @@ZaehlerEnde

                XOR     BX,BX

                ; StringlÑnge ermitteln
@@Zaehler:      INC     BX
                LODSB
                CMP     AL,0
                JNZ     @@Zaehler

@@ZaehlerEnde:
                SUB     SI,BX                   ; SI wieder auf den Grundzustand bringen
                DEC     SI
                MOV     AX,SI

                CMP     CL,kTALeft              ; LinksbÅndig?
                JNE     @@NextAlign1
@@AlignLeft:    JMP     @@EndAlign              ; keine Aenderungen

@@NextAlign1:   CMP     CL,kTACenter            ; Zentriert ?
                JNE     @@AlignRight

@@AlignCenter:  SHR     BL,1                    ; StringlÑnge halbieren
                SUB     DL,BL                   ; Koordinaten anpassen
                JC      @@Ende                  ; x-Pos < 0 ?
                JMP     @@EndAlign

@@AlignRight:   SUB     DL,BL                   ; x-Pos um StringlÑnge vermindern
                JC      @@Ende

@@EndAlign:     MOV     BX,DX
                CALL    PrintString

@@Ende:         POP     SI
                POP     ES
                POP     DI
                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
PrintAlignString ENDP


;***********************************************************************
; PrintTitle
;   - Schreiben eines Programmtitels
;   - der Titel wird immer auf die erste Bildschirmzeile geschrieben
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
; Returnwert: -
;-----------------------------------------------------------------------
PrintTitle      PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX

                MOV     CX,AX                   ; Offset des Textes retten

                MOV     AX,OFFSET BlankLine
                XOR     BX,BX
                CALL    PrintString

                MOV     AX,CX
                MOV     BL,40
                XOR     BH,BH
                MOV     CL,kTACenter
                CALL    PrintAlignString

                POP     CX
                POP     BX
                POP     AX
                RET
PrintTitle      ENDP


;***********************************************************************
; PrintMenu
;   - Schreiben eines MenÅs auf der von BH angegebenen Zeile
;   - den Zeichen mit vorhergehenden '&' werden mit dem Attribut von
;     BL geschrieben
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
;   - BL: Farbe fÅr Zeichen nach '&'
;   - BH: Zeile
; Returnwert: -
;-----------------------------------------------------------------------
PrintMenu       PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DI
                PUSH    ES
                PUSH    SI

                MOV     DI,AX                   ; Offset des Textes retten
                MOV     bTemp,BL
                MOV     bTemp2,BH

                MOV     AX,OFFSET BlankLine
                XOR     BL,BL
                CALL    PrintString

                CLD                             ; ZÑhler incrementieren

                MOV     SI,DI
                XOR     CL,CL                   ; '&'-ZÑhler auf Null setzen
                XOR     BH,BH                   ; BuchstabenzÑhler auf Null setzen

@@ZaehlLoop:    LODSB                           ; DS:[SI] -> AL;SI++
                CMP     AL,0
                JZ      @@EOSFound
                CMP     AL,'&'
                JZ      @@ZaehlLoop
                INC     BH
                JMP     @@ZaehlLoop

@@EOSFound:     MOV     SI,DI
                MOV     BL,40
                SHR     BH,1
                SUB     BL,BH
                MOV     BH,bTemp2
                CALL    CalcPoint               ; Offset in DI
                MOV     AX,_SCREEN
                MOV     ES,AX
                MOV     AH,bAttribut

@@MenuLoop:     LODSB                           ; DS:[SI] -> AL;SI++
                CMP     AL,0
                JZ      @@MenuEnde
                CMP     AL,'&'
                JNZ     @@PChar
                MOV     AH,bTemp                ; '&' Farbe verwenden
                LODSB
                CMP     AL,0
                JZ      @@MenuEnde
                STOSW
                MOV     AH,bAttribut
                JMP     @@MenuLoop
@@PChar:        STOSW
                JMP     @@MenuLoop

@@MenuEnde:     POP     SI
                POP     ES
                POP     DI
                POP     CX
                POP     BX
                POP     AX
                RET
PrintMenu       ENDP


;***********************************************************************
; ChangeMenu
;   - MenÅ Ñndern
;-----------------------------------------------------------------------
; Input:
;   - AL: MenÅfarbe
;   - AH: Selektierte Buchstabenfarbe
;   - BX: OFFSET von oberem MenÅ
;   - CX: OFFSET von unterem MenÅ
; Returnwert: -
;-----------------------------------------------------------------------
ChangeMenu      PROC NEAR
                PUSH    AX
                PUSH    BX
                PUSH    CX
                PUSH    DX

                CALL    SetColor
                MOV     DL,AH
                MOV     AX,BX
                MOV     BL,DL
                MOV     BH,23
                CALL    PrintMenu
                MOV     AX,CX
                MOV     BH,24
                CALL    PrintMenu

                POP     DX
                POP     CX
                POP     BX
                POP     AX
                RET
ChangeMenu      ENDP


;***********************************************************************
; ClrScr
;   - Lîschen des Bildschirms
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
ClrScr          PROC NEAR
                ; Register sichern
                PUSH    AX
                PUSH    CX
                PUSH    DI
                PUSH    ES
                ;
                MOV     AX,_SCREEN
                MOV     ES,AX
                XOR     DI,DI
                MOV     CX,kScrWidth*kScrHeight
                MOV     AL,32                   ; SPACE
                MOV     AH,bAttribut
                CLD                             ; ZÑhler incrementieren
                REP     STOSW
                ;
                POP     ES
                POP     DI
                POP     CX
                POP     AX
                RET
ClrScr          ENDP


;***********************************************************************
; SaveScreen
;   - aktueller Bildschirminhalt speichern
;   - Åber DOS-Alloc wird Speicher angefordert, fÅr den Bildschirm-
;     bereich
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: Speichersegment fÅr Bildschirminhalt
;-----------------------------------------------------------------------
SaveScreen      PROC NEAR
                PUSH    BX
                PUSH    CX
                PUSH    DI
                PUSH    DS
                PUSH    ES
                PUSH    SI

                CLD                             ; ZÑhler incrementieren

                MOV     BX,kScrWidth*kScrHeight *2/16           ; fÅr 80*25 Display
                MOV     AH,48h
                INT     21h
                JNC     @@AllesKlar
                ; Fehler beim Speicher allocieren
                CALL    ClrScr
                MOV     DX,OFFSET szNoMem
                MOV     AH,09h
                INT     21h
                MOV     AH,4Ch
                MOV     AL,1
                INT     21h                     ; Direkt ins DOS zurÅck

@@AllesKlar:    MOV     ES,AX                   ; Destination ES:[DI
                XOR     DI,DI
                MOV     AX,_SCREEN              ; Source DS:[SI]
                MOV     DS,AX
                XOR     SI,SI
                MOV     CX,kScrWidth*kScrHeight *2      ; Ganzer Bildschirm speichern
                REP     MOVSB

                MOV     AX,ES

                POP     SI
                POP     ES
                POP     DS
                POP     DI
                POP     CX
                POP     BX
                RET
SaveScreen      ENDP


;***********************************************************************
; GuruScreen
;   - der in AX gespeicherte Bildschirmbereich wird um 5 Zeilen nach
;     unten verschoben dargestellt
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse fÅr Speicherblock mit Bildschirminhalt
; Returnwert: -
;-----------------------------------------------------------------------
GuruScreen      PROC NEAR
                PUSH    CX
                PUSH    DI
                PUSH    DS
                PUSH    ES
                PUSH    SI

                CLD                             ; ZÑhler incrementieren

                MOV     DS,AX                   ; Source DS:[SI]
                XOR     SI,SI
                MOV     AX,_SCREEN              ; Destination ES:[DI]
                MOV     ES,AX
                MOV     DI,kScrWidth *2*5       ; die ersten 5 Zeilen Åberspringen
                MOV     CX,(kScrWidth*2*kScrHeight)-(kScrWidth*5*2)
                                                ; nur von 5.-25. Zeile von Source nach Dest kopieren
                REP MOVSB

                POP     SI
                POP     ES
                POP     DS
                POP     DI
                POP     CX
                RET
GuruScreen      ENDP


;***********************************************************************
; RestoreScreen
;   - gespeicherter Bildschirmbereich wieder auf Bildschirm schreiben
;   - angeforderter Speicher wieder freigeben
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse fÅr Speicherblock mit Bildschirminhalt
; Returnwert: -
;-----------------------------------------------------------------------
RestoreScreen   PROC NEAR
                PUSH    CX
                PUSH    DS
                PUSH    DI
                PUSH    ES
                PUSH    SI

                CLD                             ; ZÑhler incrementieren

                MOV     DS,AX                   ; Source DS:[SI]
                XOR     SI,SI
                MOV     AX,_SCREEN      
                MOV     ES,AX                   ; Destination ES:[DI]
                XOR     DI,DI                 
                MOV     CX,kScrWidth*kScrHeight*2       ; Ganzer Bildschirmbereich
                REP     MOVSB

                ; Speicher wieder freigeben
                MOV     AX,DS
                MOV     ES,AX
                MOV     AH,49h
                INT     21h

                POP     SI
                POP     ES
                POP     DI
                POP     DS
                POP     CX
                RET
RestoreScreen   ENDP

_CODE           ENDS

END
                                                                                
