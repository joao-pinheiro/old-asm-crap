;[·]------------------------------------------------------------------------------------------[·]
; |											       |
; |   PALETTE HACKING MODULE - CODED BY JOÃO PINHEIRO					       |
; |											       |
; |   Copyright (c) 1998, João Pinheiro							       |
; |   All rights reserved                                                                      |
; |											       |
;[·]------------------------------------------------------------------------------------------[·]

.586P
.MODEL LARGE,PASCAL


INCLUDE DPMI.INC
INCLUDE SYSTEM.INC
INCLUDE MACROS.INC

CODESTART

;##############################################################################################
;# PALETTE FUNCTIONS									      #
;##############################################################################################

RGBSTRUC STRUC		;RGB ENTRY FOR PAL
     	   RED   DB ?
     	   GREEN DB ?
     	   BLUE  DB ?
RGBSTRUC ENDS


PALSTRUC STRUC		;PALETTE STRUCTURE
	 DB 768 DUP(0)
PALSTRUC ENDS
	
;========================================================================
PUBLIC VRETRACE,SETRGB,SETPAL,GETPAL,FADEIN,FADEOUT
;------------------------------------------------------------------------	

VRETRACE:
   	MOV DX,03DAh
   @@L1:
   	IN AL,DX
   	TEST AL,8
	JNZ @@L1
   @@L2:
	IN AL,DX
	TEST AL,8
	JZ @@L2
       RETN


;------------------------------------------------------------------------
; SETA AS COMPONENTES DE UMA COR 
;


SETRGB PROC NEAR
	ARG @I:BYTE,@R:BYTE,@G:BYTE,@B:BYTE
	
	MOV DX,03C8h
	MOV AL,@I
	OUT DX,AL
	INC DX
	MOV AL,@R
	OUT DX,AL
	MOV AL,@G
	OUT DX,AL
	MOV AL,@B
	OUT DX,AL
	RET
      ENDP 


;------------------------------------------------------------------------
; SETA A PALETTE INTEIRA
;

SETPAL PROC NEAR
	ARG @PAL:POINTER

        USES DS,ESI
	LDS ESI,SS:[OFFSET @PAL]
	MOV ECX,768
	CALL VRETRACE
	MOV EDX,03C8h
	XOR AL,AL
  	OUT DX,AL
	INC EDX
	CLD
	REP OUTSB
	RET
       ENDP


;------------------------------------------------------------------------	
;LÊ A PALETTE INTEIRA
;

GETPAL PROC NEAR
	ARG @PAL:POINTER

        USES ES,EDI
	LES EDI,SS:[OFFSET @PAL]
	MOV ECX,768
	CALL VRETRACE
	MOV EDX,03C7h
	XOR AL,AL
  	OUT DX,AL
	INC DX
	INC DX
	CLD
	REP INSB
	RET
       ENDP

;------------------------------------------------------------------------
; FAZ UM FADE IN
;

FADEIN PROC NEAR
	ARG @PAL:POINTER

	USES DS,ES
	LDS ESI,SS:[OFFSET @PAL]
	LES EDI,SS:[OFFSET @PAL]
	CALL GETPAL PASCAL,DS,ESI
	MOV BL,64

 @@LOOP_INT:
	CALL VRETRACE
	CALL SETPAL PASCAL,ES,EDI
	PUSH ESI EDI
	MOV ECX,768
	CLD
	
 @@LOOP_COLORS:
        LODSB
	MUL BL 	   ;AX=AX*INTENSITY
	SHR AX,6   ;AX:=AX DIV 64	
	STOSB
	LOOP @@LOOP_COLORS
	POP EDI ESI
	DEC BL
	JNS @@LOOP_INT
	RET
       ENDP

;------------------------------------------------------------------------
; FAZ UM FADE OUT
; NOTA - @PAL É A PALETE PARA A QUAL SE VAI FAZER FADE, E @PAL2 É A TEMPORÁRIA

FADEOUT PROC NEAR
	ARG @PAL:POINTER,@PAL2:POINTER

	USES DS,ES
	LDS ESI,SS:[OFFSET @PAL]
	LES EDI,SS:[OFFSET @PAL2]
	PUSH ESI EDI
	MOV ECX,192			;MAKE @PAL2=@PAL
	CLD
	REP MOVSD
	XOR BL,BL	
	POP EDI ESI

 @@FADEOUT_INT:
	PUSH  ESI EDI 
	PUSH ES EDI			;SETPAL STACK FRAME
	MOV ECX,768
	CLD

 @@FADEOUT_COLORS:
	LODSB
	MUL BL     			;AX=AX*INTENSITY
	SHR AX,6			;AX:=AX DIV 64
	STOSB
	LOOP @@FADEOUT_COLORS

	CALL SETPAL 			;SETA PALETTE
	POP EDI ESI
	INC BL
	CMP BL,65
	JNE @@FADEOUT_INT
	RET
      ENDP

CODEENDS
END
