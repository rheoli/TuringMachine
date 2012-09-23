;**=================================================================**
;             TapeIO Verarbeitung der Turingmaschine
;**=================================================================**


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
TapePrintAll    PROTO

;***********************************************************************
; TapePrint
;   - aktuelle Tapedaten in Tapebereich schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapePrint       PROTO

;***********************************************************************
; TapeReadChar
;   - Zeichen von aktueller Tapepointer-Position lesen und zurÅckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: Zeichen der aktuellen Position
;-----------------------------------------------------------------------
TapeReadChar    PROTO

;***********************************************************************
; TapeWriteChar
;   - Zeichen an aktueller Tapepointer-Position schreiben
;-----------------------------------------------------------------------
; Input:
;   - AL: das Zeichen
; Returnwert:
;-----------------------------------------------------------------------
TapeWriteChar   PROTO

;***********************************************************************
; TapeLeftShift
;   - Positionszeiger des Tapes um eins nach links (<) verschieben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeLeftShift   PROTO

;***********************************************************************
; TapeRightShift
;   - Positionszeiger des Tapes um eins nach rechts (>) verschieben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeRightShift  PROTO

;***********************************************************************
; TapeEdit
;   - Edit-Modus aufrufen
;   - neues MenÅ ausgeben
;   - eigene Eingabeschleife
;   - TapeEdit kann mit ESC verlassen werden
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
TapeEdit        PROTO

;***********************************************************************
; TapeWrite
;   - aktuelle Banddaten in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fÅr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert: -
;-----------------------------------------------------------------------
TapeWrite       PROTO

;***********************************************************************
; TapeRead
;   - Banddaten von File lesen
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET fÅr Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert:
;-----------------------------------------------------------------------
TapeRead        PROTO


;**============================= Ende ==============================**
