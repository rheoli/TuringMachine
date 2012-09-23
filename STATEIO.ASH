;**=================================================================**
;              StateIO f�r Behandlung des Turing-Stateliste
;**=================================================================**

;***********************************************************************
;********           Globale Konstanten von StateIO              ********
;***********************************************************************
kImpossibleState        EQU 0FFFFh
kHALTState              EQU 0FFFEh


;***********************************************************************
;********              Prozeduren von StateIO                   ********
;***********************************************************************

;***********************************************************************
; StateGetFirst
;   - pStateFirst zur�ckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: pStateFirst
;-----------------------------------------------------------------------
StateGetFirst   PROTO

;***********************************************************************
; StatePrintAll
;   - Rahmen von Statezeilen zeichnen
;   - aktuelle States ausgeben
;   - Cursor an aktuelle Statezeile schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrintAll   PROTO

;***********************************************************************
; StatePrint
;   - State-Tabelle in Rahmen schreiben
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrint      PROTO

;***********************************************************************
; StateGetThis
;   - aktueller ThisState zur�ckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: ThisState zur�ckgeben
;-----------------------------------------------------------------------
StateGetThis    PROTO

;***********************************************************************
; StateGetNext
;   - aktueller NextState zur�ckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AX: NextState zur�ckgeben
;-----------------------------------------------------------------------
StateGetNext    PROTO

;***********************************************************************
; StateGetDirect
;   - aktuelles Direction zur�ckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: aktuelles Direction-Zeichen
;-----------------------------------------------------------------------
StateGetDirect  PROTO

;***********************************************************************
; StateGetOutput
;   - aktuelles Output-Zeichen zur�ckgeben
;-----------------------------------------------------------------------
; Input: -
; Returnwert:
;   - AL: aktuelles Output-Zeichen
;-----------------------------------------------------------------------
StateGetOutput  PROTO

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
;   - AX: 0 f�r Programm stoppen, 1 alles klar
;-----------------------------------------------------------------------
StateSetThis    PROTO

;***********************************************************************
; StateAddLine
;   - neue Zeile zur Stateliste hinzuf�gen
;-----------------------------------------------------------------------
; Input:
;   - AX: ThisState-Wert
;   - BL: Input-Wert
; Returnwert:
;   - AX: Speichersegment f�r neue Werte
;-----------------------------------------------------------------------
StateAddLine    PROTO

;***********************************************************************
; StateInsertLine
;   - ein aus der State-Liste entferntes Element wird wieder ein-
;     gef�gt
;-----------------------------------------------------------------------
; Input:
;   - AX: Speichersegment f�r State
; Returnwert:
;   - AL: AL==0 Alles OK, AL==1 State doppelt
;-----------------------------------------------------------------------
StateInsertLine PROTO

;***********************************************************************
; StateDelLine
;   - das Element mit Segmentadresse AX wird aus der State-Liste ent-
;     fernt
;-----------------------------------------------------------------------
; Input:
;   - AX: Segmentadresse des zu entfernenden Elementes
; Returnwert: -
;-----------------------------------------------------------------------
StateDelLine    PROTO

;***********************************************************************
; StatePrintEdit
;   - State-Liste in Tabelle schreiben (f�r Edit-Modus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StatePrintEdit  PROTO

;***********************************************************************
; StateChngAttrib
;   - Attribute einer State-Zeile �ndern (f�r Edit-Modus)
;-----------------------------------------------------------------------
; Input:
;   - AL: Farbe
; Returnwert: -
;-----------------------------------------------------------------------
StateChngAttrib PROTO

;***********************************************************************
; StateCursorDown
;   - eine Statezeile nach unten (im Editmodus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateCursorDown PROTO

;***********************************************************************
; StateCursorUp
;   - eine Statezeile nach oben (im Editmodus)
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateCursorUp   PROTO

;***********************************************************************
; StateEditNumber
;   - Editieren einer State-Nummer auf aktueller Zeile in Spalte AL
;   - Zugelassen sind die Zahlen 0-65533
;   - die beiden Zahlen 65534 und 65535 sind f�r ImpossibleState und
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
StateEditNumber PROTO

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
StateEditChar   PROTO

;***********************************************************************
; StateEdit
;   - Edit-Modus f�r State-Liste
;-----------------------------------------------------------------------
; Input: -
; Returnwert: -
;-----------------------------------------------------------------------
StateEdit       PROTO

;***********************************************************************
; StateRead
;   - State-Liste von File lesen
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET f�r Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert: -
;-----------------------------------------------------------------------
StateRead       PROTO

;***********************************************************************
; StateWrite
;   - aktuelle State-Liste in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET f�r Filename, wenn 0FFFFh dann wird Standardfilename
;         genommen
; Returnwert: -
;-----------------------------------------------------------------------
StateWrite      PROTO


;**============================= Ende ==============================**
