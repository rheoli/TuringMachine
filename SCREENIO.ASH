;**=================================================================**
;             Allgemeine Bildschirmausgabeprozeduren
;**=================================================================**

;***********************************************************************
;********              Konstanten von ScreenIO                  ********
;***********************************************************************

; Screendefinitionen
kScrWidth       EQU 80
kScrHeight      EQU 25

; Textausrichtungsparameter
kTALeft         EQU 1
kTACenter       EQU 2
kTARight        EQU 3

; Farbendefinition f�r Bildschirmausgabe
kColBlack        EQU 0
kColBlue         EQU 1
kColGreen        EQU 2
kColCyan         EQU 3
kColRed          EQU 4
kColMagenta      EQU 5
kColBrown        EQU 6
kColGray         EQU 7
kColLightBlack   EQU 8
kColLightBlue    EQU 9
kColLightGreen   EQU 10
kColLightCyan    EQU 11
kColLightRed     EQU 12
kColLightPurple  EQU 13
kColYellow       EQU 14
kColWhite        EQU 15


;***********************************************************************
;********              Prozeduren von ScreenIO                  ********
;***********************************************************************

;***********************************************************************
; CalcPoint
;   - Offsetadresse f�r direkter Bildschirmzugriff berechnen
;-----------------------------------------------------------------------
; Input:
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert:
;   - DI: Offsetadresse auf Bildschirm
;-----------------------------------------------------------------------
CalcPoint       PROTO

;***********************************************************************
; SetColor
;   - Text- und Hintergrundfarbe festlegen f�r die Zeichen
;-----------------------------------------------------------------------
; Input:
;   - AL: Neue Text- und Hintergrundfarbe
; Returnwert: -
;-----------------------------------------------------------------------
SetColor        PROTO

;***********************************************************************
; InitScreen
;   - VideoMode initialisieren
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
InitScreen      PROTO

;***********************************************************************
; ResetScreen
;   - Normaler DOS-Screen wiederherstellen
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
ResetScreen     PROTO

;***********************************************************************
; ChangeAttrib
;   - Farbe und Hintergrund von Zeichen �ndern
;-----------------------------------------------------------------------
; Input:
;   - AL: Attribut
;   - AH: Anzahl Zeichen
;   - BL: x-Koord
;   - BH: y-Koord
; Returnwert: -
;-----------------------------------------------------------------------
ChangeAttrib    PROTO

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
PrintChar       PROTO

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
PrintString     PROTO

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
PrintAlignString PROTO

;***********************************************************************
; PrintTitle
;   - Schreiben eines Programmtitels
;   - der Titel wird immer auf die erste Bildschirmzeile geschrieben
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
; Returnwert: -
;-----------------------------------------------------------------------
PrintTitle      PROTO

;***********************************************************************
; PrintMenu
;   - Schreiben eines Men�s auf der von BH angegebenen Zeile
;   - den Zeichen mit vorhergehenden '&' werden mit dem Attribut von
;     BL geschrieben
;-----------------------------------------------------------------------
; Input:
;   - AX: Offset des ASCIIZ-String
;   - BL: Farbe f�r Zeichen nach '&'
;   - BH: Zeile
; Returnwert: -
;-----------------------------------------------------------------------
PrintMenu       PROTO

;***********************************************************************
; ChangeMenu
;   - Men� �ndern
;-----------------------------------------------------------------------
; Input:
;   - AL: Men�farbe
;   - AH: Selektierte Buchstabenfarbe
;   - BX: OFFSET von oberem Men�
;   - CX: OFFSET von unterem Men�
; Returnwert: -
;-----------------------------------------------------------------------
ChangeMenu      PROTO

;***********************************************************************
; ClrScr
;   - L�schen des Bildschirms
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
ClrScr          PROTO

;***********************************************************************
; SaveScreen
;   - aktueller Bildschirminhalt speichern
;   - �ber DOS-Alloc wird Speicher angefordert, f�r den Bildschirm-
;     bereich
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: Speichersegment f�r Bildschirminhalt
;-----------------------------------------------------------------------
SaveScreen      PROTO

;***********************************************************************
; GuruScreen
;   - der in AX gespeicherte Bildschirmbereich wird um 5 Zeilen nach
;     unten verschoben dargestellt
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse f�r Speicherblock mit Bildschirminhalt
; Returnwert: -
;-----------------------------------------------------------------------
GuruScreen      PROTO

;***********************************************************************
; RestoreScreen
;   - gespeicherter Bildschirmbereich wieder auf Bildschirm schreiben
;   - angeforderter Speicher wieder freigeben
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse f�r Speicherblock mit Bildschirminhalt
; Returnwert: -
;-----------------------------------------------------------------------
RestoreScreen   PROTO


;**============================= Ende ==============================**
