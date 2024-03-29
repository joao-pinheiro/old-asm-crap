;###########################################################################
;#                                                                         #
;#  VIDEO TEXT MODE ROUTINES                                               #
;#                                                                         #
;#  Copyright (c) 1997  Jo�o Manuel Pinheiro                               #
;#  V.N.Gaia - Portugal                                                    #
;#                                                                         #
;###########################################################################


.MODEL LARGE,PASCAL
.386P

INCLUDE GLOBALS.INC



;##########################################################################
CODE16START
;##########################################################################
ASSUME DS:_TEXT
;----------------------------------------------------------------
;| Inicializa as rotinas modo texto, e salva o modo de entrada  |
;| VOID INIT_VIDEO()						|
;| Aten��o!! - Modo real					|
;----------------------------------------------------------------
PUBLIC INIT_VIDEO

INIT_VIDEO PROC NEAR

        USES EAX,EBX,ECX,EDX,ES,DS
	PUSH _TEXT
	POP DS
        MOV AX,01A00H
	INT 10H
        CMP AL,1AH
        JNE @@CHECK_EGA         	;ERRO NA CHAMADA, N�O � VGA
                
        CMP BL,2                	;SE VGA COMO SECUND�RIO
        JE @@CHECK_CGA               	;PROCURA VGA COMO PRIM�RIO
        JB @@IS_MONO
        
        CMP BL,5                    	;EGA � PRIM�RIO?
        JBE @@CHECK_EGA
        
 @@CHECK_MCGA:
        MOV ADAPTER,MCGA        	;ASSUME MCGA
        MOV DISPL,COR   
        CMP BL,8
        JA @@MODE_DETECTED
        
 @@CHECK_EGA:
        MOV AX,1200H
        MOV BX,10H             ;OBTER PARAMETROS EGA
        XOR ECX,ECX
	INT 10H
        JCXZ @@CHECK_CGA        ;N�O � EGA
        
        MOV ADAPTER,EGA
        MOV DISPL,MONO          ;ASSUME MONO
        CMP BH,01
        JE @@MODE_DETECTED

        MOV DISPL,COR           ;AFINAL � COR
        JMP @@MODE_DETECTED     

 @@CHECK_CGA:
	INT 11H
        AND AL,30H              		;ISOLA OS BITS DA PLACA DE V�DEO
        CMP AL,30H              		;SE ESTIVEREM OS 2 A 1 � MONO
        JE @@IS_MONO
        
        MOV ADAPTER,CGA
        MOV DISPL,COR
        JMP @@MODE_DETECTED
        
 @@IS_MONO:
        MOV ADAPTER,MDA
        MOV DISPL,MONO
        
 @@MODE_DETECTED:
        MOV AX,0F00H
	INT 10H
        MOV INIT_MODE,AL
        MOV CURRPAGE,BH
        XOR EAX,EAX
        MOV AL,DS:DISPL           		;CALCULA O FRAME BUFFER
        SHL EAX,15
        ADD EAX,0B0000H
        MOV TXT_LINADDX,EAX
        
	PUSH 0
	POP ES
        MOV AX,ES:[0044AH]      		;OBT�M O N� DE COLUNAS
        MOV COLS,AL
        MOV ROWS,24
        CMP ADAPTER,EGA
        JB @@QUIT_VINIT
        
        MOV AX,01130H          			;GET CHAR INFO
        XOR BX,BX
	INT 10H
        MOV ROWS,DL
        
 @@QUIT_VINIT:
        RET
        ENDP

;##########################################################################
CODE16ENDS
;##########################################################################
;##########################################################################
CODESTART
;##########################################################################

PUBLIC INIT_MODE, DISPL, ADAPTER, CURRPAGE, ROWS, COLS, TXT_LINADDX

;##########################################################################

INIT_MODE       DB   -1 	;INITIAL VIDEO MODE
DISPL           DB   0  	;TIPO DE DISPLAY (MONO/COR)
ADAPTER         DB   0  	;TIPO DE PLACA
CURRPAGE        DB   0  	;P�GINA DE V�DEO CORRENTE
ROWS            DB   0  	;N� DE LINHAS 
COLS            DB   0  	;N� DE COLUNAS
TXT_LINADDX     DD   0  	;ENDERE�O LINEAR DA JANELA DE TEXTO

;##########################################################################

PUBLIC SIMPLES, DUPLA, DUPLAHOR, DUPLAVERT, CLASSICA, DOTTED

;##########################################################################

SIMPLES		DB '�Ŀ��ٳ�'
DUPLA		DB '�ͻ�ͼ��'
DUPLAHOR	DB '�͸�;��'
DUPLAVERT	DB '�ķ�Ľ��'
CLASSICA	DB '��������'
DOTTED		DB '+-++-+||'

;##########################################################################
EXTRN _SDATA0:WORD
EXTRN _SDATA32:WORD
EXTRN STRLEN:NEAR
;===========================================================================

SCREEN STRUC GLOBAL METHOD{

	T_INIT		:DWORD = SCRN_INIT
	T_GOTOXY	:DWORD = SCRN_GOTOXY
	T_SETRECT	:DWORD = SCRN_SETRECT
	T_SETCOLOR	:DWORD = SCRN_COLOR
	T_CLS		:DWORD = SCRN_CLS
	T_FILL		:DWORD = SCRN_FILL
	T_PRINTXY	:DWORD = SCRN_PRINTXY
	T_PRINT		:DWORD = SCRN_PRINT	
	T_BOX		:DWORD = SCRN_BOX
	T_SCROLLUP	:DWORD = SCRN_SCROLLUP
	T_SCROLLDOWN	:DWORD = SCRN_SCROLLDOWN
	T_GETCHAR	:DWORD = SCRN_GETCHAR
	T_PUTCHAR	:DWORD = SCRN_PUTCHAR	
	T_GETATR	:DWORD = SCRN_GETATR
	T_PUTATR	:DWORD = SCRN_PUTATR
	T_MUDA		:DWORD = SCRN_MUDA
	T_LIMPA		:DWORD = SCRN_LIMPA
	T_GETAREA	:DWORD = SCRN_GETAREA
	T_PUTAREA	:DWORD = SCRN_PUTAREA

	}

   WINDOW	TRECT <>	;COORDS DA JANELA
   AT   	TPOINT <>	;POSI��O DO CURSOR NA JANELA
   ATR          DB ?,?          ;ATRIBUTO
   WOFS         DW ?            ;OFFSET DA JANELA
ENDS

;==========================================================================
ASSUME DS:_TEXT
LOCALS @@

ALIGN 4


;----------------------------------------------------------
; VOID SCREEN.INIT(POINTER32 SELF)
;----------------------------------------------------------
PUBLIC SCRN_INIT

SCRN_INIT PROC NEAR				;METHOD DE INICIALIZA��O
        ARG @SELF:POINTER32

	USES EAX,EDI,ES
	LES EDI,FWORD PTR @SELF
	XOR EAX,EAX
	MOV ES:[EDI][SCREEN.WOFS],AX		;OFFSET DA JANELA=0
	MOV ES:[EDI][SCREEN.WINDOW.A.X],AX	;XY DA JANELA
	MOV ES:[EDI][SCREEN.WINDOW.A.Y],AX
	MOV ES:[EDI][SCREEN.AT.X],Ax
	MOV ES:[EDI][SCREEN.AT.Y],Ax

	MOV AL,CS:COLS				;X1Y1 DA JANELA
	MOV ES:[EDI][SCREEN.WINDOW.B.X],AX
	MOV AL,CS:ROWS
	MOV ES:[EDI][SCREEN.WINDOW.B.Y],AX
	MOV AL,7				;ATRIBUTO DEFAULT
	MOV ES:[EDI][SCREEN.ATR],AL

	RET
	ENDP
	

;----------------------------------------------------------
;VOID SCREEN.GOTOXY(POINTER3232 SELF, LONG XY)
;----------------------------------------------------------
PUBLIC SCRN_GOTOXY

SCRN_GOTOXY PROC NEAR			;SETS CURSOR POSITION
        ARG @SELF:POINTER32, @XY:DWORD
	
	USES EAX,EDI,ES
	LES EDI,FWORD PTR @SELF
	MOV EAX,@XY
	MOV ES:[EDI.SCREEN.AT.X],AX
	SHR EAX,16
	MOV ES:[EDI.SCREEN.AT.Y],AX

	RET
	ENDP
	
;-----------------------------------------------------
;VOID SCREEN.SETRECT(POINTER32 SELF, LONG XY, LONG X1Y1)
;------------------------------------------------------
PUBLIC SCRN_SETRECT

SCRN_SETRECT PROC NEAR				;SETS WINDOW COORDS
        ARG @SELF:POINTER32,@XY:DWORD,@X1Y1:DWORD

	USES EAX,EBX,EDX,EDI,ES
	LES EDI, FWORD PTR @SELF
	MOV AX,WORD PTR @XY[0]			;X
	MOV BX,WORD PTR @XY[2]			;Y
	MOV ES:[EDI][SCREEN.WINDOW.A.X],AX
	MOV ES:[EDI][SCREEN.WINDOW.A.Y],BX
	SHL AX,1
	PUSH EAX
	XOR AX,AX
	MOV AL,CS:COLS
	SHL AL,1
	MUL BL
	POP EBX
	ADD BX,AX
	MOV ES:[EDI][SCREEN.WOFS],BX 
	MOV EAX,@X1Y1
	MOV ES:[EDI][SCREEN.WINDOW.B.X],AX
	SHR EAX,16
	MOV ES:[EDI][SCREEN.WINDOW.B.Y],AX

	RET
	ENDP
	
;-------------------------------------------------
;VOID SCREEN.SETCOLOR(POINTER32 SELF,DWORD FGBG)
;-------------------------------------------------
PUBLIC SCRN_COLOR

SCRN_COLOR PROC NEAR
        ARG @SELF:POINTER32, @FGBG:DWORD
	
	USES EAX,EBX,EDI,ES
	LES EDI,FWORD PTR @S
	MOV EAX,@FGBG
	MOV EBX,EAX
	SHR EAX,16
	SHL BX,4
	ADD BX,AX
	MOV ES:[EDI][SCREEN.ATR],BL	
	
	RET
	ENDP


;---------------------------------------------
;VOID CLS(POINTER32 SELF)
;---------------------------------------------
PUBLIC SCRN_CLS

SCRN_CLS PROC NEAR
        ARG @SELF:POINTER32
	
	USES EAX,EBX,ECX,EDX,EDI,ESI,ES
	LES EDI,FWORD PTR @SELF
	MOVZX EDX,ES:[EDI][SCREEN.WINDOW.B.X]	;DX:=WIN.X1-WIN.X
	SUB DX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX ECX,ES:[EDI][SCREEN.WINDOW.B.Y]	;CX:=WIN.Y1-WIN.Y
	SUB CX,ES:[EDI][SCREEN.WINDOW.A.Y]
	
	XOR EBX,EBX
	MOV BL,CS:COLS
	SUB BX,DX				;BX:=COLS-DX
	SHL BX,1				;BX:=BX*2
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]
	MOV AH,ES:[EDI][SCREEN.ATR]	
	MOV AL,20H
	MOV EDI,ESI
	MOV ES,CS:_SDATA0
	CLD

  @@CLS_S1:
	PUSH ECX
  	MOV ECX,EDX
  	CLD
  	REP STOSW
  	ADD EDI,EBX
	POP ECX
	LOOP @@CLS_S1
	
	RET
	ENDP
	

;-----------------------------------------------------------------
;VOID SCREEN.FILL(POINTER32 SELF, DWORD XY,DWORD X1Y1,DWORD CAR)
;-----------------------------------------------------------------
PUBLIC SCRN_FILL

SCRN_FILL PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1Y1:DWORD, @CAR:DWORD

	USES EAX,EBX,ECX,EDX,ESI,EDI,ES
	LES EDI,FWORD PTR @SELF

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;X>AX?
	JG @@Q_FILL

	CMP WORD PTR @X1Y1[0],AX		;X1
	JBE @@FILL_L1
	
	MOV WORD PTR @X1Y1[0],AX		;AX:=XMAX ;LARGURA DA JANELA

 @@FILL_L1:
	CMP WORD PTR @XY[2],BX			;BX>Y?
	JG @@Q_FILL
	CMP WORD PTR @X1Y1[2],BX		;BX>Y1?
	JBE @@FILL_L2

	MOV WORD PTR @X1Y1[2],BX		;Y1:=AX

 @@FILL_L2:
	MOV AX,WORD PTR @XY[2]			;Y
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BL,1
	PUSH EBX				;PRESERVE GAP
	MUL BX
	MOV DX,WORD PTR @XY[0]			;X
	SHL DX,1
	ADD AX,DX
	MOV ESI,CS:TXT_LINADDX
	ADD SI,AX
	ADD SI,ES:[EDI][SCREEN.WOFS]
	
	MOVZX ECX,WORD PTR @X1Y1[2]		;Y1	;CX:=Y1-Y
	SUB CX,WORD PTR @XY[2]			;Y
	MOVZX EDX,WORD PTR @X1Y1[0]		;X1
	SUB DX,WORD PTR @XY[0]			;X
						;DX:=X1-X
	MOV AX,DX				
	SHL AX,1
	POP EBX
	SUB BX,AX				;BX:=(COLS*2-((X1-X)*2))
	MOV EAX,@CAR
	MOV AH,ES:[EDI][SCREEN.ATR]
	MOV EDI,ESI
	MOV ES,CS:_SDATA0
	CLD
	
 @@FILL_L3:
	PUSH ECX
	MOV ECX,EDX
	CLD
	REP STOSW
	ADD EDI,EBX
	POP ECX
	LOOP @@FILL_L3

 @@Q_FILL:
 	RET
 	ENDP


;--------------------------------------------------------
;VOID SCREEN.PRINTXY(POINTER32 SELF, DWORD XY, POINTER32 S)
;--------------------------------------------------------
PUBLIC SCRN_PRINTXY

SCRN_PRINTXY PROC NEAR
        ARG @SELF:POINTER32,@XY:DWORD,@S:POINTER32

	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JG @@Q_PRINTXY
	
	CMP WORD PTR @XY[2],BX			;IF B>AX THEN QUIT
	JG @@Q_PRINTXY

	MOV BX,AX				
	LDS ECX,FWORD PTR @S
	CALL STRLEN PASCAL,DS,ECX		;GET STRING LEN IN EDX
	MOV EAX,EDX				;LEN IN AX
	CMP AL,0				;IF @S='' THEN QUIT
	JE @@Q_PRINTXY
	XOR AH,AH
	MOV CX,WORD PTR @XY[0]			;CX:=X+LEN
	ADD CX,AX
	CMP BX,CX				;IF CX<(WIN.X1-WIN.X) 
	JG @@PRINTXY_L1				;NO PROBLEM

	MOVZX EDX,ES:[EDI][SCREEN.WINDOW.A.X]
	ADD DX,WORD PTR @XY[0]			;X
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;
	SUB AX,DX				;TRUNCATE LEN
						;AX:=WIN.X1-(WIN.X+X)
 @@PRINTXY_L1:
	PUSH EAX 
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]
	MUL BX					;AX:=(COLS*2)*Y
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX				;AX:=AX+(X*2)
	ADD SI,AX
	POP ECX
	CLD
	MOV AH,ES:[EDI][SCREEN.ATR]
	MOV EDI,ESI
	LDS ESI,FWORD PTR @S
	MOV ES,CS:_SDATA0
 
 @@PRINTXY_L2:
 	LODSB
 	STOSW
 	LOOP @@PRINTXY_L2

 @@Q_PRINTXY:
 	RET
 	ENDP



;----------------------------------------------------------------
;VOID SCREEN.PRINT(POINTER32 SELF,POINTER32 STRING)
;----------------------------------------------------------------
PUBLIC SCRN_PRINT

SCRN_PRINT PROC NEAR
        ARG @SELF:POINTER32,@S:POINTER32

	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP ES:[EDI][SCREEN.AT.X],AX		;IF X>AX THEN QUIT
	JG @@Q_PRINT
	
	CMP ES:[EDI][SCREEN.AT.Y],BX		;IF B>AX THEN QUIT
	JG @@Q_PRINT

	MOV BX,AX				
	LDS ECX,FWORD PTR @S
	CALL STRLEN PASCAL,DS,ECX		;GET STRING LEN IN EDX
	MOV EAX,EDX				;LEN IN AX
	CMP AL,0				;IF @S='' THEN QUIT
	JE @@Q_PRINT

	XOR AH,AH
	MOV CX,ES:[EDI][SCREEN.AT.X]		;CX:=X+LEN
	ADD CX,AX
	CMP BX,CX				;IF CX<(WIN.X1-WIN.X) 
	JG @@PRINT_L1				;NO PROBLEM

	MOVZX EDX,ES:[EDI][SCREEN.WINDOW.A.X]
	ADD DX,ES:[EDI][SCREEN.AT.X]		;X
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;
	SUB AX,DX				;TRUNCATE LEN
						;AX:=WIN.X1-(WIN.X+X)
 @@PRINT_L1:
	PUSH EAX 
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,ES:[EDI][SCREEN.AT.Y]
	MUL BX					;AX:=(COLS*2)*Y
	MOV CX,ES:[EDI][SCREEN.AT.X]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	POP ECX
	CLD
	MOV AH,ES:[EDI][SCREEN.ATR]
	MOV EDI,ESI
	LDS ESI,FWORD PTR @S
	MOV ES,CS:_SDATA0
 
 @@PRINT_L2:
 	LODSB
 	STOSW
 	LOOP @@PRINT_L2

 @@Q_PRINT:
 	RET
 	ENDP

;--------------------------------------------------------------
; VOID SCREEN.BOX(POINTER32 SELF, DWORD XY, DWORD X1Y1, DWORD TIPO)
;--------------------------------------------------------------
PUBLIC SCRN_BOX

SCRN_BOX PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1Y1:DWORD, @TIPO:DWORD

	LOCAL ISDOWN:BYTE
	LOCAL ISRIGHT:BYTE
	LOCAL K1:DWORD,K2:DWORD,K3:DWORD
	LOCAL WIDT:WORD,HEIGH:WORD
	LOCAL GAP:WORD
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JA @@Q_BOX
	CMP WORD PTR @XY[2],BX			;IF Y>BX THEN QUIT
	JA @@Q_BOX

	CMP WORD PTR @X1Y1[0],AX		;X1>(WIN.X1-WIN.X)?
	SETBE ISRIGHT
	JBE @@BOX_L1	
	MOV WORD PTR @X1Y1[0],AX
	
 @@BOX_L1:
 	CMP WORD PTR @X1Y1[2],BX		;Y1>(WIN.Y1-WIN.Y)?
	SETBE ISDOWN
	JBE @@BOX_L2	
	MOV WORD PTR @X1Y1[2],BX

 @@BOX_L2:					
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]
 						;CALC � ADDX
	XOR EBX,EBX
	MOV BL,CS:COLS
	MOVZX ECX,WORD PTR @XY[0]		;X
	MOVZX EAX,WORD PTR @XY[2]		;Y
	SHL BX,1				;BX:=COLS*2
	SHL CX,1				;CX:=X*2
	MOV GAP,BX
	MUL BX
	ADD AX,CX				;AX:=(BX*Y)+CX
	ADD EAX,ESI
	MOV K1,EAX				;GUARDA K1
	MOV K3,EAX
						;CALC � ADDX

	MOVZX EAX,WORD PTR @X1Y1[2]		;Y1
	MUL BX
	ADD AX,CX				;AX:=(BX*Y1)+CX
	ADD EAX,ESI				;EAX:=AX+WOFS+TXT_LINADDX
	MOV K2,EAX				;GUARDA K2

	MOV EAX,@X1Y1				;CALC WIDT, HEIGH
	MOV ECX,@XY
	SUB AX,CX
	DEC AX
	MOV WIDT,AX				;WIDT:=X1-X
	SHR EAX,16
	SHR ECX,16
	SUB AX,CX
	DEC AX
	MOV HEIGH,AX				;HEIGH:=Y1-Y
	
	MOV DS,CS:_SDATA32
	MOV ESI,@TIPO
	MOV AH,ES:[EDI][SCREEN.ATR]
	MOV EDI,K1
	MOV ES,CS:_SDATA0
	MOV AL,DS:[ESI]
	STOSW					;ESCREVE �
	MOV AL,DS:[ESI+1]
	MOVZX ECX,WIDT
	PUSH ECX
	CLD
	REP STOSW				;ESCREVE -
	CMP ISRIGHT,1
	JNE @@BOX_L3

	MOV AL,DS:[ESI+2]
	MOV ES:[EDI],AX				;ESCREVE �
	MOV K1,EDI
		
 @@BOX_L3:
	POP ECX
	MOV EDI,K2
	CMP ISDOWN,1
	JNE @@BOX_L4
	MOV AL,DS:[ESI+3]
 	STOSW					;ESCREVE �
	MOV AL,DS:[ESI+4]
	CLD
	REP STOSW				;ESCREVE -
	CMP ISRIGHT,1
	JNE @@BOX_L4

	MOV AL,DS:[ESI+5]
	STOSW					;ESCREVE �

 @@BOX_L4:
	MOV AL,DS:[ESI+6]			;ESCREVE � ESQUERDA
	MOVZX ECX,HEIGH	
	PUSH ECX
	MOV EDI,K3
	MOV BX,GAP
	ADD DI,BX
	DEC BX
	DEC BX
	CLD
 @@BOX_LL4:
	STOSW
	ADD DI,BX
	LOOP @@BOX_LL4
	
	POP ECX
	CMP ISRIGHT,1
	JNE @@Q_BOX
	
	MOV AL,DS:[ESI+7]		;ESCREVE � DIREITA
	MOV EDI,K1
	ADD DI,BX
	INC DI
	INC DI
	CLD
 @@BOX_LL5:
 	STOSW
 	ADD DI,BX
 	LOOP @@BOX_LL5
	

 @@Q_BOX:
 	RET
 	ENDP


;---------------------------------------------------------------
;VOID SCREEN.SCROLLUP(POINTER32 SELF, DWORD XY, DWORD X1Y1, DWORD LINES)
;---------------------------------------------------------------
PUBLIC SCRN_SCROLLUP

SCRN_SCROLLUP PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1Y1:DWORD, @LINES:DWORD
	
	USES EAX,EBX,ECX,EDX,EDI,ESI,ES,DS
	LES EDI,FWORD PTR @SELF

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.A.Y]
	ADD WORD PTR @XY[0],AX
	ADD WORD PTR @X1Y1[0],AX
	ADD WORD PTR @XY[2],BX
	ADD WORD PTR @X1Y1[2],BX

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]
	CMP WORD PTR @XY[0],AX
	JA @@Q_SCROLLUP
	
	CMP WORD PTR @X1Y1[0],AX
	JBE @@SUP_L1

	MOV WORD PTR @X1Y1[0],AX
 
 @@SUP_L1:
 	MOV AX,ES:[EDI][SCREEN.WINDOW.B.Y]
 	CMP WORD PTR @XY[2],AX
 	JA @@Q_SCROLLUP
	CMP WORD PTR @X1Y1[2],AX
	JBE @@SUP_L2
	MOV WORD PTR @X1Y1[2],AX

 @@SUP_L2:
	MOV CL,BYTE PTR ES:[EDI][SCREEN.ATR]
	PUSH ECX
	MOVZX ECX,WORD PTR @X1Y1[2]
	MOV AX,WORD PTR @XY[2]
	SUB CX,AX
	SUB ECX,@LINES[0]			;CX:=(Y1-Y)-LINES
	MOV ESI,CS:TXT_LINADDX
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	PUSH EBX
	MUL BX
	ADD SI,AX
	MOV AX,WORD PTR @XY[0]
	SHL AX,1
	ADD SI,AX
	MOV EDI,ESI

	POP EBX					;ENDERE�O DE ORIGEM
	MOV EAX,@LINES
	MUL BX
	ADD SI,AX	
	
	MOVZX EAX,WORD PTR @X1Y1[0]		;WORDS/LINHA
	SUB AX,WORD PTR @XY[0]	
	INC AX
	PUSH EAX
	MOV EDX,EAX
	SHL AX,1
	SUB BX,AX
	MOV ES,CS:_SDATA0
	MOV DS,CS:_SDATA0
	CLD
	
 @@SUP_LL1:
 	PUSH ECX
	MOV CX,DX
	CLD
	REP MOVSW
	ADD DI,BX
	ADD SI,BX
	POP ECX
	LOOP @@SUP_LL1

	POP EDX
	POP EAX
	MOV AH,AL
	MOV AL,20H
	MOV ECX,@LINES
	CLD

 @@SUP_LL2:
 	PUSH ECX
 	MOV ECX,EDX
 	CLD
 	REP STOSW
 	ADD DI,BX
	POP ECX
	LOOP @@SUP_LL2

 @@Q_SCROLLUP:
 	RET
 	ENDP



;---------------------------------------------------------------
;VOID SCREEN.SCROLLDOWN(POINTER32 SELF, DWORD @XY, DWORD @X1Y1, DWORD LINES)
;---------------------------------------------------------------
PUBLIC SCRN_SCROLLDOWN

SCRN_SCROLLDOWN PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1Y1:DWORD, @LINES:DWORD
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.A.Y]
	ADD WORD PTR @XY[0],AX
	ADD WORD PTR @X1Y1[0],AX
	ADD WORD PTR @XY[2],BX
	ADD WORD PTR @X1Y1[2],BX

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]
	CMP WORD PTR @XY[0],AX			;IF X>(WIN.X1-WIN.X) THEN QUIT
	JA @@Q_SCROLLDOWN
	CMP WORD PTR @X1Y1[0],AX		;IF X1>(WIN.X1-WIN.X) THEN
	JBE @@SDN_L1
	MOV WORD PTR @X1Y1[0],AX		;X1:=(WIN.X1-WIN.X)
 
 @@SDN_L1:
 	MOV AX,ES:[EDI][SCREEN.WINDOW.B.Y]
 	CMP WORD PTR @XY[2],AX
 	JA @@Q_SCROLLDOWN
	CMP WORD PTR @X1Y1[2],AX
	JBE @@SDN_L2
	MOV WORD PTR @X1Y1[2],AX

 @@SDN_L2:
	MOV AL,ES:[EDI][SCREEN.ATR]
	PUSH EAX
	MOV ESI,CS:TXT_LINADDX			;DESTINO
	MOVZX EAX,WORD PTR @X1Y1[2]
	PUSH EAX
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	MOV EDI,ESI
	ADD DI,AX

	POP EAX				;ORIGEM
	SUB EAX,@LINES[0]
	MUL BX
	ADD AX,CX
	ADD SI,AX

	MOVZX EDX,WORD PTR @X1Y1[0]		;WORDS/LINHA
	SUB DX,WORD PTR @XY[0] 
	INC DX
	MOV CX,DX				;DX:=(X1-X)+1
	SHL CX,1
	ADD BX,CX
	
	MOV ES,CS:_SDATA0
	MOV DS,CS:_SDATA0		
	MOVZX ECX,WORD PTR @X1Y1[2]
	SUB CX,WORD PTR @XY[2]
	SUB ECX,@LINES				;ECX:=((Y1-Y)-LINES)+1
	INC CX
	CLD
	
 @@SDN_LL1:
	PUSH ECX
	MOV ECX,EDX
	CLD
	REP MOVSW
	SUB ESI,EBX
	SUB EDI,EBX
	POP ECX
	LOOP @@SDN_LL1	

	POP EAX
	MOV ECX,@LINES
	MOV AH,AL
	MOV AL,20H
	CLD
	
 @@SDN_LL2:
 	PUSH ECX
 	MOV ECX,EDX
 	CLD
 	REP STOSW
	SUB EDI,EBX
	POP ECX
	LOOP @@SDN_LL2

 @@Q_SCROLLDOWN:
 	RET
 	ENDP

;-------------------------------------------------------
; VOID SCREEN.PUTCHAR(POINTER32 SELF, DWORD @XY, DWORD CHAR)
;-------------------------------------------------------
PUBLIC SCRN_PUTCHAR

SCRN_PUTCHAR PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @CAR:DWORD

	USES EAX,EBX,ECX,EDX,EDI,ESI,ES
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JA @@Q_PUTCH
	
	CMP WORD PTR @XY[2],BX			;IF Y>BX THEN QUIT
	JA @@Q_PUTCH
	
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]			;Y
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	MOV ES,CS:_SDATA0
	MOV AX,ES:[ESI]
	MOV AL,BYTE PTR @CAR[0]
	MOV ES:[ESI],AX

 @@Q_PUTCH:
 	RET
 	ENDP


;-------------------------------------------------------
; DWORD SCREEN.GETCHAR(POINTER32 SELF, DWORD @XY)
;-------------------------------------------------------
PUBLIC SCRN_GETCHAR

SCRN_GETCHAR PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD

	USES EAX,EBX,ECX,EDI,ESI,ES
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JA @@Q_GETCH
	
	CMP WORD PTR @XY[2],BX			;IF Y>BX THEN QUIT
	JA @@Q_GETCH
	
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]			;Y
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	MOV ES,CS:_SDATA0
	XOR EDX,EDX
	MOV DL,ES:[ESI]

 @@Q_GETCH:
 	RET
 	ENDP

;-------------------------------------------------------
; DWORD SCREEN.GETATR(POINTER32 SELF, DWORD @XY)
;-------------------------------------------------------
PUBLIC SCRN_GETATR

SCRN_GETATR PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD

	USES EAX,EBX,ECX,EDI,ESI,ES
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JA @@Q_GETATR
	
	CMP WORD PTR @XY[2],BX			;IF Y>BX THEN QUIT
	JA @@Q_GETATR
	
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]			;Y
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	MOV ES,CS:_SDATA0
	XOR EDX,EDX
	INC ESI
	MOV DL,ES:[ESI]

 @@Q_GETATR:
 	RET
 	ENDP

;-------------------------------------------------------
; VOID SCREEN.PUTATR(POINTER32 SELF, DWORD @XY, DWORD ATR)
;-------------------------------------------------------
PUBLIC SCRN_PUTATR

SCRN_PUTATR PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @A:DWORD

	USES EAX,EBX,ECX,EDX,EDI,ESI,ES
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;IF X>AX THEN QUIT
	JA @@Q_PUTATR
	
	CMP WORD PTR @XY[2],BX			;IF Y>BX THEN QUIT
	JA @@Q_PUTATR
	
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]			;Y
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	MOV ES,CS:_SDATA0
	MOV AX,ES:[ESI]
	MOV AH,BYTE PTR @A[0]
	MOV ES:[ESI],AX

 @@Q_PUTATR:
 	RET
 	ENDP



;-----------------------------------------------------
; VOID SCREEN.MUDA(POINTER32 SELF, DWORD @XY, DWORD @X1A)
;-----------------------------------------------------
PUBLIC SCRN_MUDA

SCRN_MUDA PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1A:DWORD
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX ECX,ES:[EDI][SCREEN.WINDOW.B.X]	;CX:=WIN.X1-WIN.X
	SUB CX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],CX			;IF X>CX THEN QUIT
	JA @@Q_MUDA
	
	CMP WORD PTR @XY[2],BX			;IF B>AX THEN QUIT
	JA @@Q_MUDA
	
	MOVZX EAX,WORD PTR @X1A[0]		;X1
	SUB AX,WORD PTR @XY[0]
	JZ @@Q_MUDA
	PUSH ECX
	CMP AX,CX
	JBE @@MUDA_L1
	
	SUB CX,AX
	SUB WORD PTR @X1A[0],CX
	
 @@MUDA_L1:
	MOV CX,WORD PTR @XY[0]
	ADD CX,AX
	POP EBX
	CMP BX,CX
	JA @@MUDA_L2
	MOV AX,ES:[EDI][SCREEN.WINDOW.B.X]
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	SUB AX,WORD PTR @XY[0]

 @@MUDA_L2:
 	PUSH EAX
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	POP ECX	
	INC ECX
	CLD

	MOV DX,WORD PTR @X1A[2]
	MOV AX,CS:_SDATA0
	MOV ES,AX
	MOV DS,AX
	MOV EDI,ESI

 @@MUDA_LL1:
 	LODSW
 	MOV AH,DL
 	STOSW
 	LOOP @@MUDA_LL1
 	
 @@Q_MUDA:
 	RET
 	ENDP


;-----------------------------------------------------
; VOID SCREEN.LIMPA(POINTER32 SELF, DWORD @XY, DWORD @X1C)
;-----------------------------------------------------
PUBLIC SCRN_LIMPA

SCRN_LIMPA PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1C:DWORD
	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]

	MOVZX ECX,ES:[EDI][SCREEN.WINDOW.B.X]	;CX:=WIN.X1-WIN.X
	SUB CX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],CX			;IF X>CX THEN QUIT
	JA @@Q_LIMPA
	
	CMP WORD PTR @XY[2],BX			;IF B>AX THEN QUIT
	JA @@Q_LIMPA
	
	MOVZX EAX,WORD PTR @X1C[0]		;X1
	SUB AX,WORD PTR @XY[0]
	JZ @@Q_LIMPA
	PUSH ECX
	CMP AX,CX
	JBE @@LIMPA_L1
	
	SUB CX,AX
	SUB WORD PTR @X1C[0],CX
	
 @@LIMPA_L1:
	MOV CX,WORD PTR @XY[0]
	ADD CX,AX
	POP EBX
	CMP BX,CX
	JA @@LIMPA_L2
	MOV AX,ES:[EDI][SCREEN.WINDOW.B.X]
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	SUB AX,WORD PTR @XY[0]

 @@LIMPA_L2:
 	PUSH EAX
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MOV AX,WORD PTR @XY[2]
	MUL BX
	MOV CX,WORD PTR @XY[0]
	SHL CX,1
	ADD AX,CX
	ADD SI,AX
	POP ECX	
	INC ECX
	CLD

	MOV DX,WORD PTR @X1C[2]
	MOV AX,CS:_SDATA0
	MOV ES,AX
	MOV DS,AX
	MOV EDI,ESI

 @@LIMPA_LL1:
 	LODSW
 	MOV AL,DL
 	STOSW
 	LOOP @@LIMPA_LL1
 	
 @@Q_LIMPA:
 	RET
 	ENDP


;-----------------------------------------------------------------------
; VOID SCREEN.GETAREA(POINTER32 SELF, DWORD @XY, DWORD @X1Y1, POINTER32 BUF)
;-----------------------------------------------------------------------
PUBLIC SCRN_GETAREA

SCRN_GETAREA PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @X1Y1:DWORD, @BUF:POINTER32

	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR  @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;X>AX?
	JG @@Q_GETAREA

	CMP WORD PTR @X1Y1[0],AX		;X1
	JBE @@GETAREA_L1
	MOV WORD PTR @X1Y1[0],AX		;AX:=XMAX ;LARGURA DA JANELA

 @@GETAREA_L1:
	CMP WORD PTR @XY[2],BX			;BX>Y?
	JG @@Q_GETAREA
	CMP WORD PTR @X1Y1[2],BX		;BX>Y1?
	JBE @@GETAREA_L2
	MOV WORD PTR @X1Y1[2],BX		;Y1:=AX

 @@GETAREA_L2:
	MOVZX EAX,WORD PTR @XY[2]		;Y
	MOVZX ECX,WORD PTR @X1Y1[2]
	SUB CX,AX
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	PUSH EBX
	MUL BX
	ADD AX,WORD PTR @XY[0]
	ADD AX,WORD PTR @XY[0]
	ADD SI,AX	
	
	MOVZX EDX,WORD PTR @X1Y1[0]
	SUB DX,WORD PTR @XY[0]
	INC CX
	INC DX
	POP EBX
	LES EDI,FWORD PTR @BUF
	MOV AX,DX
	STOSW				;GUARDA (X1-X)
	MOV AX,CX
	STOSW				;GUARDA (Y1-Y)
	MOV AX,BX
	STOSW				;GUARDA GAP
	CLD
	MOV AX,CS:_SDATA0
	MOV DS,AX
		
  @@GETAREA_LL1:
  	PUSH ECX ESI
  	MOV ECX,EDX
  	CLD
  	REP MOVSW
	POP ESI ECX
	ADD ESI,EBX
	LOOP @@GETAREA_LL1

  @@Q_GETAREA:
  	RET
  	ENDP



;----------------------------------------------------------
; VOID SCREEN.PUTAREA(POINTER32 SELF, DWORD @XY, POINTER32 BUF)
;----------------------------------------------------------
PUBLIC SCRN_PUTAREA

SCRN_PUTAREA PROC NEAR
        ARG @SELF:POINTER32, @XY:DWORD, @BUF:POINTER32

	
	USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
	LES EDI,FWORD PTR @SELF
	MOV ESI,CS:TXT_LINADDX
	ADD SI,ES:[EDI][SCREEN.WOFS]
	MOVZX EAX,ES:[EDI][SCREEN.WINDOW.B.X]	;AX:=WIN.X1-WIN.X
	SUB AX,ES:[EDI][SCREEN.WINDOW.A.X]
	MOVZX EBX,ES:[EDI][SCREEN.WINDOW.B.Y]	;BX:=WIN.Y1-WIN.Y
	SUB BX,ES:[EDI][SCREEN.WINDOW.A.Y]

	CMP WORD PTR @XY[0],AX			;X>AX?
	JG @@Q_PUTAREA
	CMP WORD PTR @XY[2],BX			;BX>Y?
	JG @@Q_PUTAREA

	MOVZX EAX,WORD PTR @XY[2]		;Y
	XOR EBX,EBX
	MOV BL,CS:COLS
	SHL BX,1
	MUL BX
	ADD AX,WORD PTR @XY[0]
	ADD AX,WORD PTR @XY[0]
	ADD SI,AX	

	XOR EAX,EAX
	LDS EDI,FWORD PTR @BUF
	MOV ES,CS:_SDATA0
	XCHG ESI,EDI
	LODSW
	MOV EDX,EAX
	LODSW
	MOV ECX,EAX
	LODSW 
	MOV EBX,EAX	
	CLD
	
  @@PUTAREA_LL1:
  	PUSH ECX EDI
  	MOV ECX,EDX
  	CLD
  	REP MOVSW
	POP EDI ECX  	
	ADD EDI,EBX
	LOOP @@PUTAREA_LL1
	
  @@Q_PUTAREA:
  	RET
  	ENDP



;===========================================================================
CODEENDS

END
