;**=================================================================**
;               Allgemeine Prozeduren f�r Turingmaschine
;**=================================================================**

;***********************************************************************
;********               Prozeduren von Utils                    ********
;***********************************************************************

;***********************************************************************�
; StringToWORD
;   - Zeichenkette in WORD-Zahl wandeln
;-----------------------------------------------------------------------
; Input:
;   - AX: Zeichenkette ASCIIZ
; Returnwert:
;   - AX: 0xFFFF wenn Fehler aufgetreten, Gewandelte Zahl sonst
;-----------------------------------------------------------------------
StringToWORD    PROTO

;***********************************************************************
; SetFree
;   - den nicht belegten Speicherplatz freigeben 
;   - Info:  Da das Stack-Segment immer das letzte Segment in einer
;            EXE-Datei ist, deutet ES:0000 auf den Anfang und SS:SP
;            auf das Ende des Programms im Speicher. Dadurch kann die
;            L�nge des Programms berechnet werden
;-----------------------------------------------------------------------
; Input:
;   - ES: Adresse des PSP
; Returnwert: -
; Register:
;   - AX, BX, CL und FLAGS werden ver�ndert
;-----------------------------------------------------------------------
SetFree         PROTO

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
WORDToString    PROTO

;***********************************************************************
; GetFilename
;   - Filename eingeben �ber Dialogbox
;-----------------------------------------------------------------------
; Input:
;   - AX: OFFSET auf String
;   - BX: OFFSET auf TitelString
; Returnwert: -
;-----------------------------------------------------------------------
GetFilename     PROTO

;**============================= Ende ==============================**

