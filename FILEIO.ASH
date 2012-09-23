;**=================================================================**
;                  Allgemeine FileIO Verarbeitung
;**=================================================================**

;***********************************************************************
;********               Prozeduren von FileIO                   ********
;***********************************************************************

;***********************************************************************
; FileGetLength
;   - Filel�nge ab aktuellem Filepointer bestimmen
;-----------------------------------------------------------------------
; Input:
;   - BX: FileHandle
; Returnwert:
;   - BX: Filelaenge oder BX=0FFFFh wenn Fehler aufgetreten
;-----------------------------------------------------------------------
FileGetLength   PROTO

;***********************************************************************
; FileReadLine
;   - Textzeile aus File lesen
;   - Zeilenenden m�ssen ein 0x0D, 0x0A enthalten, genau in dieser
;     Reihenfolge geschrieben sein.
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET f�r Zeichenkette aus File
;   - BX: FileHandle
; Returnwert:
;   - AX: 0x0000 f�r alles OK, 0x0001 = EOF und 0xFFFF = Fehler
;-----------------------------------------------------------------------
FileReadLine    PROTO

;***********************************************************************
; FileWriteLine
;   - Textzeile in File schreiben
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET des Textes in ASCIIZ
;   - BX: FileHandle
; Returnwert:
;   - AX: 0x0000 f�r alles OK, 0xFFFF f�r Fehler
;-----------------------------------------------------------------------
FileWriteLine   PROTO


;**============================= Ende ==============================**

