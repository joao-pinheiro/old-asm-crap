.586P
.MODEL LARGE,PASCAL


INCLUDE DPMI.INC
INCLUDE SYSTEM.INC
INCLUDE MACROS.INC

CODESTART

;------------------------------------------------------------------------
;########################################################################[RANDOM GENERATOR]
;------------------------------------------------------------------------

RANDSEED   DD ?  ;SEED OF RANDOM GENERATOR

PUBLIC RANDOMIZE,RANDOM,RANDSEED

;-------------------------------------------------------
; INICIA O GERADOR DE NºS ALEATÓRIOS

RANDOMIZE PROC NEAR

	USES EAX,ECX,EDX
	MOV AH,02Ch
	INTR 21H
	ROR EDX,CL
	MOV RANDSEED,EDX
	RET
       ENDP
 
;-------------------------------------------------------
; GERA UM Nº ALEATÓRIO
RANDOM  PROC NEAR
	ARG @MAXN:DWORD

	USES EDX,EBX
	XOR EDX,EDX
	MOV EAX,RANDSEED  ;New := 8088405H * Old + 1
	MOV EBX,8088405h
	MUL EBX
	INC EAX
	MOV RANDSEED,EAX
	MOV EBX,@MAXN
	XOR EAX,EAX
	XCHG EAX,EDX
	DIV EBX
	XCHG EAX,EDX
	RET
       ENDP

;------------------------------------------------------------------------
;########################################################################[TRIGONOMETRIC TABLES]
PUBLIC SINTABLE, COSTABLE
PUBLIC ALLOCTRIGTABLES
;------------------------------------------------------------------------
   ALIGN 4
   INCLUDE TRIG.INC

TABLESIZE = (4096*4)+8

SINTABLE DD ?   ;PTR PARA SINTABLE
COSTABLE DD ?   ;PTR PARA COSTABLE

;------------------------------------------------------------------------
; CREATES SINE TABLE
;

CREATE_SIN_TABLE PROC NEAR

	USES DS
	MOV EDI,SINTABLE
	MOV ESI,OFFSET ST_ENTRYS
	MOV ECX,1025
	CLD
	REP MOVSD		;SIN 0-90

	SUB ESI,8
	MOV ECX,1024
	CLD

 @@SIN_L1:
	MOV EAX,DS:[ESI]	;SIN 90-180
	STOSD			;INC EDI
	SUB ESI,4		;DEC ESI
	LOOP @@SIN_L1	

	MOV ESI,EDI		;SIN 180-360
	MOV DS,SDATA0
	SUB ESI,8	
	MOV ECX,2049
	CLD

 @@SIN_L2:
	MOV EAX,DS:[ESI]
	NEG EAX
	STOSD
	SUB ESI,4
	LOOP @@SIN_L2	

	RET
ENDP

;------------------------------------------------------------------------
; CREATES COSINE TABLE BASED UPON SINE TABLE
;

CREATE_COS_TABLE PROC NEAR

	USES DS				
	MOV EDI,COSTABLE	;COPY FROM SIN TABLE
	MOV ESI,SINTABLE
	PUSH ESI
	MOV DS,SDATA0
	ADD ESI,1024*4		;COS 0..90 = SIN 90..0 = SIN 90..180
	MOV ECX,3072
	CLD 
	REP MOVSD
	POP ESI
	MOV ECX,1025		;COS 270..360 (INCLUSIVE)
	CLD
	REP MOVSD
	
	RET
ENDP


;------------------------------------------------------------------------
; INIT TRIGONOMETRIC TABLES (SINE & COSINE)
;
 
ALLOCTRIGTABLES PROC NEAR

	CALL MALLOC PASCAL,TABLESIZE	;ALLOC TABLES SIZE
	MOV SINTABLE,EDI

	CALL MALLOC PASCAL,TABLESIZE
	MOV COSTABLE,EDI

	CALL CREATE_SIN_TABLE		;CREATE TABLES FROM DATA
	CALL CREATE_COS_TABLE

	RET
       ENDP


CODEENDS

END
