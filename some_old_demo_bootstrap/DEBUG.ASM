.586P
.MODEL LARGE,PASCAL


INCLUDE DPMI.INC
INCLUDE SYSTEM.INC
INCLUDE MACROS.INC


CODESTART

;===========================================================================
PUBLIC BASE_NUMBER, FIELD_LEN, PAD_CHAR, SUFFIX, TERMINATOR

BASE_NUMBER DD 10         ;Base do Nº a ser convertido para string
FIELD_LEN   DW 1          ;Tamanho mInimo do Campo
PAD_char    Db ' '        ;Caracter de PAD
SUFFIX      DB 0          ;Sufixo
Terminator  DB 0          ;Terminador da string


INTERN_BUF DB 256 DUP(0)  ;BUFFER INTERNO DA STRING
TERM	   DB 0AH,0DH,'$' ;TERMINADOR DA STRING

;===========================================================================
;===========================================================================
;CONVERTE UM Nº INTEIRO EM STRING
;ASSUME DS-SEGMENT32

PUBLIC INT2STR

INT2STR PROC NEAR
	ARG @N:DWORD, @S:POINTER

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
	DIV CS:BASE_NUMBER
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
	CMP AL,9
	JBE @DEC_1
	SUB AL,10
	ADD AL,'A'
    	JMP @GO_2
    @DEC_1:
	ADD AL,30H
    @GO_2:
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

;===========================================================================
;EXIBE UM INTEIRO NO ECRÂ USANDO INT 21 FN 09
;

PUBLIC SHOWVAL 

SHOWVAL PROC NEAR
	ARG @NO:DWORD,@SUF:DWORD,@B:DWORD

	USES EAX, EDX
	MOV EAX,@SUF
	MOV SUFFIX,AL
	PUSH @B
	POP BASE_NUMBER
	MOV TERMINATOR,'$'
	CALL INT2STR PASCAL,@NO,DS,OFFSET INTERN_BUF
	MOV EDX,OFFSET INTERN_BUF
	CALL PRINT
	MOV EDX,OFFSET TERM
	CALL PRINT
	RET
       ENDP

CODEENDS
END
