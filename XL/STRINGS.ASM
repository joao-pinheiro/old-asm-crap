;ีออ XDOS DOS EXTENDER อออออออออออออออออออออออ FILE: STRINGS.ASM ออออออออออธ
;ณ                                                                         ณ
;ณ                   STRING OPERATION ROUTINES                             ณ
;ณ                                                                         ณ
;ณ  Copyright (c) 1997  Joo Manuel Pinheiro                               ณ
;ณ  V.N.Gaia - Portugal                                                    ณ
;ณ                                                                         ณ
;ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ


.MODEL LARGE,PASCAL
.386P

INCLUDE SEGS.INC
INCLUDE DEFS.INC
INCLUDE EXTERN.INC

;===========================================================================
_TEXT32
;===========================================================================
PUBLIC BASE_NUMBER, FIELD_LEN, PAD_CHAR, SUFFIX, TERMINATOR

BASE_NUMBER DD 10         ;Base do Nง a ser convertido para string
FIELD_LEN   DW 1          ;Tamanho mกnimo do Campo
PAD_char    Db ' '        ;Caracter de PAD
SUFFIX      DB 0          ;Sufixo
Terminator  DB 0          ;Terminador da string

;===========================================================================
ALIGN 4

;---------------------------------------------
; LONG STRLEN(S:POINTER)
;---------------------------------------------
PUBLIC STRLEN

STRLEN PROC NEAR
	ARG @S:POINTER
	
	USES EAX,ECX,EDI,ES
	LPTR ES,EDI,@S
	MOV ECX,-1
	XOR EAX,EAX
	CLD
	REPNE SCASB
	MOV EDX,-2	
	SUB EDX,ECX
	RET
	ENDP

;---------------------------------------------
; BOOL STRCOMP(S1:POINTER, S2:POINTER)
;---------------------------------------------
PUBLIC STRCOMP

STRCOMP PROC NEAR
	ARG @S1:POINTER, @S2:POINTER
	
	USES EAX,ECX,ESI,EDI,ES,DS
	LPTR ES,EDI,@S1
	LPTR DS,ESI,@S2
	CALL STRLEN PASCAL,ES,EDI
	MOV EAX,EDX
	CALL STRLEN PASCAL,DS,ESI
	CMP EAX,EDX
	JBE @@NO_SWITCH
	XCHG EAX,EDX
 @@NO_SWITCH:
	XOR EDX,EDX
	MOV ECX,EAX
	CLD
 	REPE CMPSB	
	SETE DL
	RET
	ENDP
	

;-----------------------------------------------------
;VOID INT2STR(LONG @N, POINTER @S)
;-----------------------------------------------------
PUBLIC INT2STR

INT2STR PROC NEAR
	ARG @N:LONG, @S:POINTER

	USES EAX,EBX,ECX,EDX,EDI,ESI,ES
	LPTR ES,EDI,@S
	MOV EAX,@N
    	MOV ESI,EAX
    	XOR ECX,ECX
	OR ESI,ESI
	JNS @@ST_1
	NEG EAX
	
 @@ST_1:
	XOR EDX,EDX
	DIV CS:[BASE_NUMBER]
	PUSH EDX
	INC CX
	CMP AX,0
	JNE @@ST_1
	OR ESI,ESI
	JNS @@ST_2
	MOV AL,'-'
	STOSB
 @@ST_2:
	PUSH ECX
	MOV AX,CS:FIELD_LEN
	CMP AX,CX
	JBE @@ST_STORE
	MOV BX,CX
	SUB AX,CX
	MOV CX,AX	
	MOV AL,CS:PAD_CHAR
	CLD
	REP STOSB
	MOV CX,BX
	
 @@ST_STORE:
 	POP ECX
	CLD

 @@ST_STORE1:
	POP EAX
	ADD AL,30H
	STOSB
	LOOP @@ST_STORE1

	
	MOV AL,CS:SUFFIX
	CMP AL,0
	JE @@ST_TERMINATOR
	STOSB
 
 @@ST_TERMINATOR:
 	MOV AL,CS:TERMINATOR
	STOSB 	

 	RET
 	ENDP


;----------------------------------------------------------
; VOID UPCASE(POINTER STR)
;----------------------------------------------------------
PUBLIC UPCASE

UPCASE PROC NEAR
	ARG @S:POINTER
	
	USES EAX,ECX,EDX,ESI,EDI,DS,ES
	LPTR DS,ESI,@S
	LPTR ES,EDI,@S
	CALL STRLEN PASCAL,ES,EDI
	MOV ECX,EDX
	CMP ECX,0
	JE @@Q_UCASE
	CLD

 @@UCASE_LL1:	
	LODSB
	CMP AL,97
	JB @@UCASE_L3
	CMP AL,122
	JA @@UCASE_L3
	SUB AL,20H

 @@UCASE_L3:
	STOSB
 	LOOP @@UCASE_LL1

 @@Q_UCASE:
	RET
	ENDP

	
;----------------------------------------------------------
; VOID DELETE(POINTER STR, LONG START, LONG LEN)
;----------------------------------------------------------
PUBLIC DELETE

DELETE PROC NEAR
	ARG @S:POINTER, @START:LONG, @LEN:LONG
	
	USES EAX,ECX,EDX,ESI,EDI,DS,ES
	LPTR ES,EDI,@S
	CALL STRLEN PASCAL,ES,EDI
	MOV ECX,EDX
	MOV EAX,@START
	CMP ECX,0
	JB @@Q_DELETE
	CMP ECX,EAX
	JB @@Q_DELETE			;IF LEN<START TEN EXIT
	ADD EAX,@LEN
	CMP ECX,EAX
	JA @@DELETE_L1
	SUB EAX,ECX
	SUB @LEN,EAX
	
 @@DELETE_L1:
	SUB ECX,@START
	SUB ECX,@LEN			;ECX - CARACTERES A COPIAR
	INC ECX
	MOV AX,ES
	MOV DS,AX
	ADD EDI,@START
	MOV ESI,EDI
	ADD ESI,@LEN
	CLD
	REP MOVSB
	
 @@Q_DELETE:
 	RET
 	ENDP			 	

;----------------------------------------------------------
; VOID STRCOPY(POINTER SRC,POINTER DEST, LONG COUNT)
;----------------------------------------------------------
PUBLIC STRCOPY

STRCOPY PROC NEAR
	ARG @SRC:POINTER, @DEST:POINTER, @COUNT:LONG
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LPTR DS,ESI,@SRC
	LPTR ES,EDI,@DEST
	MOV ECX,@COUNT	
	REP MOVSB
	RET
	ENDP


;----------------------------------------------------------
; VOID STRAPPEND(POINTER SRC,POINTER DEST)
;----------------------------------------------------------
PUBLIC STRAPPEND

STRAPPEND PROC NEAR
	ARG @SRC:POINTER, @DEST:POINTER
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LPTR DS,ESI,@SRC
	LPTR ES,EDI,@DEST
	CALL STRLEN PASCAL,ES,EDI
	ADD EDI,EDX
	CALL STRLEN PASCAL,DS,ESI
	CMP EDX,0
	JE @Q_STRAPPEND
	MOV ECX,EDX
	CLD
	REP MOVSB

 @Q_STRAPPEND:
	RET
	ENDP

;===========================================================================
_TEXT32_END

END