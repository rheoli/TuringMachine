;**=================================================================**
;                     Fehlermeldungen ausgeben
;**=================================================================**

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
FatalError      PROTO

;***************************************************************************
; NormalError
;   - Fehlermeldung Ausgeben
;   - nach der Fehlermeldung wird wieder in die aufrufende Prozedur
;     zurÅckgesprungen
;-----------------------------------------------------------------------
; Input:
;   - AX: Fehlermeldung
; Returnwert: -
;-----------------------------------------------------------------------
NormalError     PROTO


;**============================= Ende ==============================**
