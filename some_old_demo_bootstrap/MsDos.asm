;##############################################################################################
;# MS-DOS INTERFACE FUNCTIONS	   							      #
;# COPYRIGHT (C) 1998, JOÃO PINHEIRO							      #
;##############################################################################################

.586P
.MODEL LARGE,PASCAL


INCLUDE DPMI.INC
INCLUDE SYSTEM.INC
INCLUDE MACROS.INC

CODESTART

DOSBUFSIZE = 07FFFh
FREADONLY  = 00h
FWRITEONLY = 01h
FREADWRITE = 02h

;##############################################################################################
;# INIT MODULE										      #
;##############################################################################################

DOSBUFFER  DD ?   ;LINEAR ADDX DE DOSBUFFER
DOSBUFSEL  DD ?   ;SELECTOR DE DOSBUFFER
DOSBUFSEG  DD ?   ;SEGMENTO DO BUFFER

PUBLIC DOSBUFFER,DOSBUFSEL,DOSBUFSEG

FILE TYPEDEF WORD

;---------------------------------------------------------------------------------------------
;COPY COUNT BYTES DE SRC TO DEST

COPY PROC NEAR
	ARG @SRC:POINTER, @DEST:POINTER,@COUNT:DWORD

	USES ES,DS
	LPTR DS,ESI,@SRC
	LPTR ES,EDI,@DEST
	MOV ECX,@COUNT
	CLD
	REP MOVSB
	RET
       ENDP

;---------------------------------------------------------------------------------------------
;COPY COUNT DWORDS DE SRC TO DEST

COPYD PROC NEAR
	ARG @SRC:POINTER, @DEST:POINTER,@COUNT:DWORD

	USES ES,DS,ECX
	LPTR DS,ESI,@SRC
	LPTR ES,EDI,@DEST
	MOV ECX,@COUNT
	CLD
	REP MOVSD
	RET
       ENDP

;---------------------------------------------------------------------------------------------
;INITIALIZES DOS-FN INTERNAL BUFFER

INIT_DOSBUFFER PROC NEAR
	
	CALL GETDOSMEM PASCAL,DOSBUFSIZE+1024   ;ALLOC DOS BUF
	MOV DOSBUFSEL,EDX
	MOV DOSBUFSEG,EAX
	SHL EAX,4			     ;CALC LINEAR ADDX FROM SEGMENT
	MOV DOSBUFFER,EAX
	RET
      ENDP 

;---------------------------------------------------------------------------------------------
;FREES ALLOCATED BUFFER

FREE_DOSBUFFER PROC NEAR

	CALL FREEDOSMEM PASCAL,DOSBUFSEL    ;FREE DOS BUFFER
	RET
      ENDP

;##############################################################################################
;# FILE NICE CODE				      					      #
;##############################################################################################
;==========================================================================
; EXECUTA UMA CHAMADA A INT 21h
; 

DOSINT PROC NEAR

        USES ES,EDI
        MOV DWORD PTR REGS._AX,EAX
        MOV DWORD PTR REGS._BX,EBX
        MOV DWORD PTR REGS._CX,ECX
        MOV DWORD PTR REGS._DX,EDX
        MOV DWORD PTR REGS._SI,ESI
        MOV DWORD PTR REGS._DI,EDI
        XOR EAX,EAX
        MOV DWORD PTR REGS._BP,EAX  
	MOV EDX,DOSBUFSEG
	MOV REGS._DS,DX
        MOV REGS._FS,AX
        MOV REGS._GS,AX     
        MOV AX,0202H
        MOV REGS.FLAGS,AX           
        MOV AX,OLDSS    
        MOV REGS._SS,AX
        MOV AX,OLDSP
        MOV REGS._SP,AX
        MOV EBX,021h
        XOR ECX,ECX             
        MOV ES,SDATA32
        MOV EDI,OFFSET REGS
        MOV EAX,0300H
        INT 31H
        SETC ERRORFLAG
        JNC @L_19
        CALL [ERRORHANDLER] PASCAL,0014
        JMP @Q_0300

 @L_19:
        MOV AX,REGS.FLAGS
        XCHG AH,AL
        SAHF
        MOV EAX,DWORD PTR REGS._AX
        MOV EBX,DWORD PTR REGS._BX
        MOV ECX,DWORD PTR REGS._CX
        MOV EDX,DWORD PTR REGS._DX

 @Q_0300:
        RET
      ENDP

;---------------------------------------------------------------------------------------------
;OPEN FILE - RETS HANDLE IN AX, 0 IF ERR

FOPEN PROC NEAR
	ARG @NAME:DWORD,@R:DWORD

	MOV EAX,@R
	MOV AH,3Dh
	MOV EDX,@NAME
       INT 21h
	JNC @@FN3D_QUIT
	XOR EAX,EAX

  @@FN3D_QUIT:
	RET
       ENDP

;---------------------------------------------------------------------------------------------
;CLOSE FILE

FCLOSE PROC NEAR
	ARG @HANDLE:DWORD

	MOV AH,3Eh
	MOV EBX,@HANDLE
	CALL DOSINT 
	RET
       ENDP
	
;---------------------------------------------------------------------------------------------
;READ FROM FILE COUNT BYTES 

FREAD PROC NEAR
	ARG @HANDLE:DWORD,@DEST:POINTER,@COUNT:DWORD

	LOCAL @M:DWORD

	XOR EDX,EDX
	MOV EAX,@COUNT
	MOV EBX,DOSBUFSIZE	;CALC COUNT DIV 32767
	DIV EBX
	MOV @M,EDX
	MOV ECX,EAX		;EAX=COUNT MOD 32767
	XOR EDX,EDX
	MOV EBX,@HANDLE
	JECXZ @@MOD_LOOP ;IF JUST MOD THEN MOD_LOOP
	CLD

   @@READ_LOOP:

	PUSH ECX
	MOV ECX,DOSBUFSIZE
	PUSH ECX
	MOV AX,3F00h
	CALL DOSINT
	CALL COPY PASCAL,DOSBUFSEL,0,@DEST.SELECTOR,@DEST.OFSET,DOSBUFSIZE
	POP ECX
	ADD @DEST.OFSET,ECX
	POP ECX
	LOOP @@READ_LOOP

   @@MOD_LOOP:
	MOV ECX,@M
	JECXZ @@READ_QUIT
	MOV AX,3F00h
	CALL DOSINT
	CALL COPY PASCAL,DOSBUFSEL,0,@DEST.SELECTOR,@DEST.OFSET,@M

   @@READ_QUIT:
	RET
       ENDP



;---------------------------------------------------------------------------------------------
;SEEK TO OFSET FROM BEGINNING OF FILE
;RETS EAX - CURR FP

FROMSTART = 00h
FROMEND   = 02h
FROMPOS   = 01h

FSEEK PROC NEAR
	ARG @HANDLE:DWORD,@SEEKPOS:DWORD,@FROM:DWORD

	MOV EAX,@FROM
	MOV AH,42H
	MOV EBX,@HANDLE
	MOV CX,WORD PTR [@SEEKPOS+2]
	MOV DX,WORD PTR [@SEEKPOS]

	CALL DOSINT
	SHL EDX,16
	MOV DX,AX
	MOV EAX,EDX

	RET
       ENDP


;---------------------------------------------------------------------------------------------
;RETS FILE SIZE IN EAX

GETFILESIZE PROC NEAR
	     ARG @HANDLE:DWORD

	     LOCAL FPOS:DWORD

	     CALL FSEEK PASCAL,@HANDLE,0,FROMPOS
	     MOV FPOS,EAX
	     CALL FSEEK PASCAL,@HANDLE,0,FROMEND
             PUSH EAX
	     CALL FSEEK PASCAL,@HANDLE,FPOS,FROMSTART
	     POP EAX
             RET
            ENDP


PUBLIC FOPEN,FCLOSE,FREAD,INIT_DOSBUFFER, FREE_DOSBUFFER,FSEEK,GETFILESIZE, COPY

CODEENDS
END

