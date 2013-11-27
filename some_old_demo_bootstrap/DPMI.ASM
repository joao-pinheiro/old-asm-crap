.386
MODEL LARGE, PASCAL
STACK 200H

INCLUDE DPMI.INC

ASSUME DS:_SEGMENT16, CS:_SEGMENT16

;##############################################################################      
;##############################################################################      
_SEGMENT16 SEGMENT PARA PUBLIC 'SEGMENT16' USE16
;##############################################################################      
ORG 0

COPYR     DB 'DPMI 32 BIT DOS EXTENDER',0Ah,0Dh
	  DB 'Copyright (c) Joao Pinheiro, 1998',0AH,0DH,0AH,0DH,'$'
ALLOC_ERR DB 'NOT ENOUGH MEMORY',0AH,0DH,7,7,7,'$'

CPU_386   DB 'i80386 CPU FOUND',0AH,0DH,'$'
CPU_486   DB 'i80486 CPU FOUND',0AH,0DH,'$'
CPU_586   DB 'iPENTIUM CPU FOUND',0AH,0DH,'$'
CPU_686   DB 'iPENTIUM PRO CPU FOUND',0AH,0DH,'$'
CPU_786   DB 'iPENTIUM II CPU FOUND',0AH,0DH,'$'
CPU_MMX	  DB 'MMX(R) TECNOLOGY PRESENT',0AH,0DH,'$'

CPU_TABLE  DW OFFSET CPU_386,OFFSET CPU_486
           DW OFFSET CPU_586,OFFSET CPU_686,OFFSET CPU_786

CPU_ERROR DB 'THIS PROGRAM REQUIRES AT LEAST A PENTIUM TO RUN!!!',0AH,0DH,'$'

DPMI_MSG1  DB 'DPMI v0.9 PRESENT',0AH,0DH,'$'
DPMI_MSG2  DB 'DPMI v1.0 PRESENT',0AH,0DH,'$'
DPMI_ERROR DB 'DPMI NOT FOUND!!',0AH,0DH,'$'
DPMI32_ERR DB '32 BIT API NOT SUPPORTED!',0AH,0DH,'$'
DPMI_NOMEM DB 'NOT ENOUGH BASE MEMORY TO INITIALIZE DPMI',0AH,0DH,'$'
DPMI_ABORT DB 'COUND`T SWITCH TO DPMI',0AH,0DH,7,7,7,'$'

DPMIFN_ERR DB 'FAIL DURING CALL TO DPMI SERVICE',0AH,0DH,7,7,7,'$'

; VARIÁVEIS INTERNAS DO ARRANQUE EM DPMI ======================================

DPMI_VERSION    DW -1   ;DPMI SERVER VERSION
DPMI_FLAGS      DW 0    ;DPMI FLAGS
DPMI_PDSIZE     DW 0    ;PRIVATE DATA SIZE IN PARAGR
DPMI_ENTRY      DW 0,0  ;DPMI ENTRY POINT

MASTER_VPIC     DB 0  ;VIRTUAL MASTER PIC SETTINGS
SLAVE_VPIC      DB 0  ;VIRTUAL SLAVE PIC SETTINGS
SEL_INCVAL      DW 0  ;SELECTORS INCREMENT VALUE

; VARIÁVEIS INTERNAS DIVERSAS =================================================

OLDCS		DW 0  ;SEGMENTO CS
OLDSS		DW 0  ;SEGMENTO SS
OLDSP		DW 0  ;VALOR DE SP
OLDBP		DW 0  ;VALOR DE BP

SCODE16         DW 0  ;SELECTOR CODE16
SDATA16  	DW 0  ;SELECTOR DATA16
SSTACK16  	DW 0  ;SELECTOR STACK16
SPSP	  	DW 0  ;SELECTOR PSP
STACKSEG	DW 0  ;SEGMENTO DA STACK32

RMREGS          REGISTERS <>     ;RM CALL REGISTER STRUCTURE

; VARIAVEIS DE DETECÇÃO DO CPU=================================================


CPU_TYPE        DB -1
FP_STATUS       DW -1
VENDOR_ID       DD 3 DUP (?)
MODELL          DB 0
STEPPING        DB 0
ID_FLAG         DB 0
FPU_TYPE        DB 0
FEATURE_FLAGS   DD 0
MMX_FLAG        DB 0



;000000000000000000000000000000000000000000000000000000000000000000000000000000


;------------------------------------------------------------------------------

@PRINT_MSG:
	PUSH AX
	MOV AH,09H
	INT 21H
	POP AX
	RETN


;+--------------------------------------------------------+
;! VOID CHECK_CPU()
;!
;! RETURNS: CPU_TYPE WITH CPU TYPE:
;!               0=8086 PROCESSOR
;!               2=INTEL 286 PROCESSOR
;!               3=INTEL386(TM) PROCESSOR
;!               4=INTEL486(TM) PROCESSOR
;!               5=PENTIUM(TM) PROCESSOR
;! NOTE: ALL REGS DESTROYED!!
;+-------------------------------------------------------+

CHECK_CPU PROC NEAR
          
          PUSHF               ; PUSH ORIGINAL FLAGS
          POP AX              ; GET ORIGINAL FLAGS
          MOV CX, AX          ; SAVE ORIGINAL FLAGS
          AND AX, 0FFFH       ; CLEAR BITS 12-15 IN FLAGS
          PUSH AX             ; SAVE NEW FLAGS VALUE ON STACK
          POPF                ; REPLACE CURRENT FLAGS VALUE
          PUSHF               ; GET NEW FLAGS
          POP AX              ; STORE NEW FLAGS IN AX
          AND AX, 0F000H      ; IF BITS 12-15 ARE SET, THEN CPU
          CMP AX, 0F000H      ;   IS AN 8086/8088
          MOV CPU_TYPE, 0     ; TURN ON 8086/8088 FLAG
          JNE CHECK_80286     ; JUMP IF CPU IS NOT 8086/8088
          JMP END_GET_CPUID

          ;       INTEL 286 CPU CHECK
          ;       BITS 12-15 OF THE FLAGS REGISTER ARE ALWAYS CLEAR ON THE
          ;       INTEL 286 PROCESSOR IN REAL-ADDRESS MODE.
          ;

   CHECK_80286:
          OR CX, 0F000H      ; TRY TO SET BITS 12-15
          PUSH CX            ; SAVE NEW FLAGS VALUE ON STACK
          POPF               ; REPLACE CURRENT FLAGS VALUE
          PUSHF              ; GET NEW FLAGS
          POP AX             ; STORE NEW FLAGS IN AX
          AND AX,0F000H      ; IF BITS 12-15 CLEAR, CPU=80286
          MOV CPU_TYPE, 2    ; TURN ON 80286 FLAG
          JNZ CHECK_80386    ; IF NO BITS SET, CPU IS 80286
          JMP END_GET_CPUID

          ;       INTEL386 CPU CHECK
          ;       THE AC BIT, BIT #18, IS A NEW BIT INTRODUCED IN THE EFLAGS
          ;       REGISTER ON THE INTEL486 DX CPU TO GENERATE ALIGNMENT FAULTS.
          ;       THIS BIT CANNOT BE SET ON THE INTEL386 CPU.
	.386
   CHECK_80386:

          MOV BX, SP          ; SAVE CURRENT STACK POINTER TO ALIGN
          AND SP, NOT 3       ; ALIGN STACK TO AVOID AC FAULT
          PUSHFD              ; PUSH ORIGINAL EFLAGS
          POP EAX             ; GET ORIGINAL EFLAGS 
          MOV ECX, EAX        ; SAVE ORIGINAL EFLAGS
          XOR EAX, 40000H     ; FLIP AC BIT IN EFLAGS
          PUSH EAX
          POPFD               ; REPLACE CURRENT EFLAGS VALUE
          PUSHFD              ; GET NEW EFLAGS
          POP EAX             ; STORE NEW EFLAGS IN EAX
          XOR EAX, ECX        ; CAN'T TOGGLE AC BIT, CPU=80386
          MOV CPU_TYPE, 3     ; TURN ON 80386 CPU FLAG
          MOV SP, BX          ; RESTORE ORIGINAL STACK POINTER
          JZ END_GET_CPUID    ; JUMP IF 80386 CPU
          AND SP, NOT 3       ; ALIGN STACK TO AVOID AC FAULT
          PUSH ECX
          POPFD               ; RESTORE AC BIT IN EFLAGS FIRST
          MOV SP, BX          ; RESTORE ORIGINAL STACK POINTER

          ;       INTEL486 DX CPU, INTEL487 SX NDP, AND INTEL486 SX CPU CHECK
          ;       CHECKING FOR ABILITY TO SET/CLEAR ID FLAG (BIT 21) IN EFLAGS
          ;       WHICH INDICATES THE PRESENCE OF A PROCESSOR
          ;       WITH THE ABILITY TO USE THE CPUID INSTRUCTION.
          
          MOV CPU_TYPE, 4     ; TURN ON 80486 CPU FLAG
          MOV EAX, ECX        ; GET ORIGINAL EFLAGS
          XOR EAX, 200000H    ; FLIP ID BIT IN EFLAGS
          PUSH EAX            ; SAVE NEW EFLAGS VALUE ON STACK
          POPFD               ; REPLACE CURRENT EFLAGS VALUE
          PUSHFD              ; GET NEW EFLAGS
          POP EAX             ; STORE NEW EFLAGS IN EAX
          XOR EAX,ECX         ; CAN'T TOGGLE ID BIT,
          JE END_GET_CPUID    ;   CPU=80486

          ;       EXECUTE CPUID INSTRUCTION TO DETERMINE VENDOR, FAMILY,
          ;       MODEL AND STEPPING.
        
          .586
          MOV ID_FLAG,TRUE    ; SET FLAG INDICATING USE OF CPUID INST.
          XOR EAX,EAX         ; SET UP INPUT FOR CPUID INSTRUCTION
          CPUID               ; MACRO FOR CPUID INSTRUCTION
          MOV DWORD PTR VENDOR_ID,EBX  ; SETUP TO TEST FOR VENDOR ID
          MOV DWORD PTR VENDOR_ID+4,EDX
          MOV DWORD PTR VENDOR_ID+8,ECX


          CMP EAX, 1          ; MAKE SURE 1 IS A VALID INPUT VALUE FOR CPUID
          JL END_GET_CPUID    ; IF NOT, JUMP TO END
          XOR EAX,EAX         ; OTHERWISE, USE AS INPUT TO CPUID
          INC EAX             ; AND GET STEPPING, MODEL AND FAMILY
          CPUID
          MOV STEPPING, AL
          AND STEPPING, 0FH   ; ISOLATE STEPPING INFO
          AND AL, 0F0H        ; ISOLATE MODEL INFO
          SHR AL, 4
          MOV     MODELL, AL
          AND AX,0F00H        ; MASK EVERYTHING BUT FAMILY
          SHR AX,8
          MOV CPU_TYPE, AL    ; SET CPU_TYPE WITH FAMILY
          MOV FEATURE_FLAGS,EDX       ; SAVE FEATURE FLAG DATA

          TEST EDX,00800000H  ; IF IA MMX TECHNOLOGY, BIT 23 IN FEATURE FLAG
          JZ END_GET_CPUID
          MOV MMX_FLAG,TRUE
          
          .286

   END_GET_CPUID:
          RET
        ENDP 


;------------------------------------------------------------------------------
DETECT_DPMI PROC NEAR

           MOV AX,1687H    ;CHECK DPMI
           INT 2FH
           CMP AX,0
           JE @DPMI_01
           MOV DX,OFFSET DPMI_ERROR
	   CALL @PRINT_MSG
	   CALL @DIE2_DOS	

  @DPMI_01:
           CMP DX,005AH     ;IF DPMI.VERSION=0.9 THEN WRITELN(SYS_MSG1)
	   MOV DPMI_VERSION,DX ;SETA VERSAO    	   
	   JNE @DPMI_V1
           MOV DX,OFFSET DPMI_MSG1
	   JMP @DPMI_CONT
  @DPMI_V1:
	   MOV DX,OFFSET DPMI_MSG2

  @DPMI_CONT:
	   CALL @PRINT_MSG 
           MOV DPMI_FLAGS,BX   ;SETA FLAGS DO SERVIDOR
           MOV DPMI_PDSIZE,SI  ;BLOCO DE MEMORIA REQUERIDO
           MOV DPMI_ENTRY,DI   ;ENDEREÇO DE ENTRADA
           MOV DPMI_ENTRY+2,ES
          
           TEST DPMI_FLAGS,1   ;SE DPMI_FLAGS<>1 THEN EXIT;
           JNZ @DPMI_EXIT
	   MOV DX,OFFSET DPMI32_ERR
	   CALL @PRINT_MSG
	   CALL @DIE2_DOS

  @DPMI_EXIT:
	   RET
	ENDP
	  
;##########################################################################
;##########################################################################

.486P
ALIGN 4
;==========================================================================
;------------------------------------------------
; PROCEDURE RMINT
;  EXECUTA UMA CHAMADA A UMA INT EM MODO REAL
;------------------------------------------------

RMINT PROC NEAR
      ARG @INTNO:WORD

        USES EAX,EBX,ECX,EDX,ESI,EDI,ES
        MOV DWORD PTR RMREGS._DI,EDI
        MOV DWORD PTR RMREGS._SI,ESI
        MOV DWORD PTR RMREGS._BP,0
        MOV DWORD PTR RMREGS._BX,EBX
        MOV DWORD PTR RMREGS._DX,EDX
        MOV DWORD PTR RMREGS._CX,ECX
        MOV DWORD PTR RMREGS._AX,EAX
        MOV EAX,0300H
        MOVZX EBX,@INTNO
        PUSH 0
        POP RMREGS._SS
        MOV WORD PTR RMREGS._SP,0
        PUSH 0202H
        POP RMREGS.FLAGS
        MOV ECX,0
        PUSH DS
        POP ES
        MOV EDI,OFFSET RMREGS
        INT 31H
        JC @RM21_2
      
        MOV AX,RMREGS.FLAGS
        SAHF

 @RM21_2:
        RET
        ENDP

;==========================================================================
@PMMSG:
        USES EAX
        PUSH OLDCS
        POP DS:RMREGS._DS
        MOV AH,09H
        CALL RMINT PASCAL,021H
        RET


; DESCRIPTORS DOS SEGMENTOS DE 32 BITS DO SISTEMA, ESTILO GDT==================
ALIGN 8

DESCR1  DESCRIPTOR <0FFFFH,0,0,10011011b,11011111b,0>   ;CODE32 DESCRIPTOR
DESCR2  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;DATA32 DESCRIPTOR
DESCR3  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;STACK32 DESCRIPTOR
DESCR4  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;DATA0 DESCRIPTOR

;============================================================================
;------------------------------------------------
;Procedure SETUP_SELECTORS;
;
; Seta os descriptors e os selectors
;------------------------------------------------

SETUP_SELECTORS PROC NEAR

	MOV EDX,SEG _SEGMENT32
	SHL EDX,4
        MOV DESCR1.BASE_15,DX		;CODE32 BASE 0-15
        MOV DESCR2.BASE_15,DX		;DATA32 BASE 0-15
        SHR EDX,16
        MOV DESCR1.BASE_23,DL           ;CODE32 BASE 16-23
        MOV DESCR2.BASE_23,DL           ;DATA32 BASE 16-23

	MOVZX EDX,STACKSEG		;SET UP STACK32
	SHL EDX,4
        MOV DESCR3.BASE_15,DX		;STACK32 BASE 0-15
        SHR EDX,16
        MOV DESCR3.BASE_23,DL		;STACK32 BASE 16-23

        MOV BX,CS
        LAR CX,BX                       ;CURR CPL
        JNZ @Q_08

        AND CH,01100000b
        OR DESCR1.ACESSR,CH		;SET ACCESS RIGHTS
        OR DESCR2.ACESSR,CH
        OR DESCR3.ACESSR,CH
        OR DESCR4.ACESSR,CH

        MOV EAX,3                       ;SERVIÇO DPMI 3 - GET INCVAL
        INT 31H
        MOV SEL_INCVAL,AX
        JC @Q_07
        
        MOV DX,AX
        MOV AX,0                ;SERVIÇO DPMI 0 - ALLOC DESCRIPTORS
        MOV CX,4                ;CODE32,DATA32,STACK32,DATA0
        INT 31H
        JC @Q_07
        
        ASSUME FS:_SEGMENT32

        MOV FS,AX               ;FS -> FUTURO DATA32 
        PUSH DS
	POP ES
        MOV BX,AX
        MOV AX,000Ch            ;SETA O DESCRIPTOR PARA QUE 
        MOV EDI,OFFSET DESCR2   ;FS->DATA32
        INT 31h
        JC @Q_07
        MOV SDATA32,BX          ;SETA O DESCRIPTOR DATA32

        ADD BX,SEL_INCVAL
        MOV SCODE32,BX          ;SETA O DESCRIPTOR CODE32
        MOV EDI,OFFSET DESCR1
        INT 31h
        JC @Q_07

        ADD BX,SEL_INCVAL
        MOV SSTACK32,BX         ;SETA O DESCRIPTOR STACK32
        MOV EDI,OFFSET DESCR3
        INT 31h
        JC @Q_07

        ADD BX,SEL_INCVAL
        MOV SDATA0,BX         ;SETA O DESCRIPTOR DATA0
        MOV EDI,OFFSET DESCR4
        INT 31h
        JNC @Q_08

  @Q_07: 
        MOV EDX,OFFSET DPMIFN_ERR 
	CALL @PMMSG 
	CALL @DIE2_DOS

  @Q_08:
        RET
       ENDP

;##########################################################################
;##########################################################################
.286
ALIGN 2
;============================================================================
;------------------------------------------------------------------------------


START:
	PUSH CS
	PUSH CS
	POP DS
	POP OLDCS		;PRESERVE CS SEG
	PUSH SS
	POP OLDSS	
	MOV AH,4AH		;ADJUST PSP SIZE
	MOV BX,PROGRAMSIZE
	INT 21H

	MOV DX,OFFSET COPYR
	CALL @PRINT_MSG		;COPYRIGHT MSG

	CALL CHECK_CPU
	XOR BX,BX
	MOV BL,CPU_TYPE
	AND BL,0111B
	PUSH BX
	SUB BX,3
	SHL BX,1
	MOV DX,WORD PTR CPU_TABLE[BX]
	CALL @PRINT_MSG		;CPU TYPE
	POP BX
	CMP BL,CPUMIN
	JGE @CPU_OK
	MOV DX,OFFSET CPU_ERROR
	CALL @PRINT_MSG		;CPU ERROR	
	CALL @DIE2_DOS

 @CPU_OK:
	CMP MMX_FLAG,TRUE
	JNE @L0001
	MOV DX,OFFSET CPU_MMX	;MMX TECH PRESENT
	CALL @PRINT_MSG

	.486P		
 @L0001:
	MOV AH,48H		;ALLOC 32 BIT STACK
	MOV BX,STACKSIZE
	INT 21H
	JNC @L0003
	MOV DX,OFFSET ALLOC_ERR	
	CALL @DIE2_DOS
 @L0003:
	MOV STACKSEG,AX
	CALL DETECT_DPMI
	MOV AH,48H
	MOV BX,DPMI_PDSIZE
	INT 21H
	JNC @L0002
	MOV DX,OFFSET DPMI_NOMEM
	CALL @PRINT_MSG
	CALL @DIE2_DOS

 @L0002:
	MOV ES,AX	
	MOV AX,1	;32 BIT APP
        CALL DWORD PTR DPMI_ENTRY	;DPMI PM!!!
        CLI                   
	JNC @NOFUCKINERROR
	MOV DX,OFFSET DPMI_ABORT	;IF TROUBLED FUCK OFF
	CALL @PRINT_MSG
	CALL @DIE2_DOS
	

 @NOFUCKINERROR:
        
        MOV DS:SPSP,ES                ;SELECTOR DO PSP
        MOV DS:SCODE16,CS             ;SELECTOR DO CS
        MOV DS:SDATA16,DS             ;SELECTOR DO DS 
        MOV DS:SSTACK16,SS            ;SELECTOR DO SS
        PUSH DS                       ;FS=DS
        POP FS
        MOV AX,0400H                  ;GET DPMI VERSION 
        INT 31H                  
        MOV DS:DPMI_FLAGS,BX
        MOV DS:MASTER_VPIC,DH         ;PIC MAPPINGS
        MOV DS:SLAVE_VPIC,DL

	CALL SETUP_SELECTORS	      ;FIX SELECTORS

        MOV ESP,STACKSIZE
        XOR EBP,EBP
        MOV SS,SSTACK32         ;STACK SWITCH

	MOV GS,SDATA0
        MOV AX,FS
        MOV FS,SDATA16          ;FS ->DATA16
        MOV DS,AX               ;DS ->DATA32

;------------------------------------------------------------        
    ASSUME DS:_SEGMENT32, SS:_SSEGMENT32 ;32-BIT PUSH'S
;------------------------------------------------------------        

	PUSH DS:SCODE32    
 	PUSH OFFSET CODE32_STARTUP 
        DB 66H                   ;32-BIT RETF
        RETF                     ;GOTO KINGDOM COME


 @GO2_DOS:
	MOV AX,04C00H		    ;GOOD ...GOODBYE!!
	INT 21H

 @DIE2_DOS:
	MOV AX,04C01H		    ;FAIL - QUIT WITH ERROR CODE	
	INT 21H

;------------------------------------------------------------------------------

_SEGMENT16 ENDS


.586P

	ASSUME CS:_SEGMENT32, DS:_SEGMENT32, SS:_SSEGMENT32
	ASSUME ES:NOTHING   , FS:_SEGMENT16, GS:NOTHING

;#############################################################################
_SEGMENT32 SEGMENT PARA PUBLIC 'SEGMENT32' USE32
;#############################################################################
;#############################################################################
ORG 0
ALIGN 8

;=============================================================================
INCLUDE PUBLICS.INC
INCLUDE MACROS.INC
;=============================================================================

SCODE32         DW 0  ;SELECTOR CODE32
SDATA32  	DW 0  ;SELECTOR DATA32
SSTACK32  	DW 0  ;SELECTOR STACK32
SDATA0		DW 0  ;SELECTOR FLAT DATA	

REGS		REGISTERS <> ;RM CALL REGISTER STRUCTURE
DPMIMEM		DPMI_MEM <>   ;BLOCO DE INFORMAÇÃO SOBRE A MEMÓRIA

HEAP_ADDX	DD 0  ;ENDEREÇO LINEAR DA HEAP

EXITPROC	DD 0  ;OFFSET OF EXIT PROC
EXITCOD 	DW 0  ;EXIT CODE 
	
ERRORFLAG	DB 0,0 ;ERROR FLAG (2 BYTES PARA ALINHAR)
ERRORHANDLER	DD 0   ;OFFSET DO ERRORHANDLER
VERIFY		DW INT31CHECK ;VERIFICA ERROS

;=============================================================================
INIT_MODE       DB 3  ;MODO DE VÍDEO INICIAL

;=============================================================================
LOCALS @@

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$[ERROR HANDLERS]

ALIGN 8

DPMI_F1   DB 'SYSTEM FAIL AT DPMI CALL THROUGH INT 31h - $'
DPMI_F2	  DB 0AH,0DH,'EXECUTION ABORTED',0AH,0DH,7,7,7,'$'

ALIGN 8

DPMI_FNTBL  DB 'FN 000h$', 'FN 001h$','FN 007h$','FN 008h$','FN 009h$','FN 00Bh$','FN 00Ch$'
  	    DB 'FN 100h$', 'FN 101h$','FN 102h$','FN 202h$','FN 203h$','FN 204h$','FN 205h$'
  	    DB 'FN 300h$', 'FN 301h$','FN 303h$','FN 304h$','FN 500h$','FN 501h$','FN 502h$'
            DB 'FN 503h$', 'FN 600h$','FN 601h$','FN 602h$','FN 900h$','FN 901h$','FN 902h$'


MESSAGECOUNT = 27   ;Nº DE MENSAGENS EXISTENTES

;=============================================================================
; PROCEDURE ERROR_HANDLER(FCODE:DWORD)
; WARNING - DS NEEDED!!!

ERROR_HANDLER PROC NEAR
	ARG @FCODE:DWORD

	CMP CS:VERIFY,TRUE
	JNE @@E0001		;NO ABORT TO DOS
	MOV EAX,@FCODE
	CMP EAX,MESSAGECOUNT
	JG @@E0001		;INVALID ERROR CODE 
	PUSH EAX

	MOV EDX,OFFSET DPMI_F1	;SHOW MSG
	MOV AH,09H
	CALL RM_INT PASCAL,0021H

	POP EDX
	SHL EDX,3
	ADD EDX,OFFSET DPMI_FNTBL
	MOV AH,09H
	CALL RM_INT PASCAL,0021H
	
	CALL FAIL PASCAL,OFFSET DPMI_F2

  @@E0001:
	RET
       ENDP
	

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

;=============================================================================
; PROCEDURE PRINT(DX:OFFSET)

PRINT:
	PUSH AX BX
	MOV AH,09H
	MOV BX,SEG _SEGMENT32
	MOV REGS._DS,BX
	CALL RM_INT PASCAL,021H
	POP BX AX
	RETN		


;=============================================================================
; ABORTA COM MENSAGEM DE ERRO

FAIL PROC NEAR
	ARG @MSG:DWORD

	MOV EDX,@MSG
	CALL PRINT
	MOV AX,1
	MOV EXITCOD,AX	
	CALL EXITPROC
      ENDP


;#############################################################################[DPMI ROUTINES]

;===========================================================================
; INT ALLOC_DESCR(LONG N_DESCR)
; ALOCA N_DESCR DESCRIPTORS, DEVOLVENDO O PRIMEIRO


ALLOC_DESCR PROC NEAR
        ARG @ND:DWORD

        USES ECX
        XOR EAX,EAX
        MOV ECX,@ND
        INT 31H
        SETC ERRORFLAG
        MOVZX EAX,AX
        JNC @Q_0000     
        CALL [ERRORHANDLER] PASCAL,0000
        XOR EAX,EAX

 @Q_0000:
        RET
        ENDP
        
;==========================================================================     
; VOID FREE_DESCRIPTOR(LONG DESCR)

FREE_DESCR PROC NEAR
        ARG @DESCR:DWORD

        USES EAX,EBX    
        MOV EAX,001H
        MOV EBX,@DESCR
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0001
        CALL [ERRORHANDLER] PASCAL,0001    

 @Q_0001:
        RET
        ENDP
        
;=============================================================================
; VOID SET_BASE(LONG SELECTOR, LONG BASE)

SET_BASE PROC NEAR
        ARG @SELECTOR:DWORD, @BASE:DWORD

        USES EAX,EBX,ECX,EDX
        MOV EAX,007H
        MOV EBX,@SELECTOR
        MOV ECX,@BASE
        MOV DX,CX
        SHR ECX,16
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0007
        CALL [ERRORHANDLER] PASCAL,0002      

 @Q_0007:
        RET
        ENDP
        
;=============================================================================
; VOID SET_LIMIT(LONG SELECTOR, LONG LIMIT)


SET_LIMIT PROC NEAR
        ARG @SELECTOR:DWORD, @LIMIT:DWORD

        USES EAX,EBX,ECX,EDX
        MOV EAX,008H
        MOV EBX,@SELECTOR
        MOV ECX,@LIMIT
        MOV DX,CX
        SHR ECX,16
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0008
        CALL [ERRORHANDLER] PASCAL,0003        

 @Q_0008:
        RET
        ENDP

;=============================================================================
; VOID SET_AR(LONG SELECTOR, LONG AR)

SET_AR PROC NEAR
        ARG @SELECTOR:DWORD, @AR:DWORD

        USES EAX,EBX,ECX,EDX
        MOV EAX,009H
        MOV EBX,@SELECTOR
        MOV ECX,@AR                     ;CL - ACCESS RIGHTS
        SHR ECX,16                      ;CH - 386 EXTENDED ACCESS RIGHTS
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0009
        CALL [ERRORHANDLER] PASCAL,0004       

 @Q_0009:
        RET
       ENDP

;=============================================================================
; VOID GET_DESCR(LONG SELECTOR, POINTER BUF)

GET_DESCR PROC NEAR
        ARG @SELECTOR:DWORD, @BUF:POINTER

        USES EAX,EBX,EDI,ES
        MOV EAX,00BH
        MOV EBX,@SELECTOR
        MOV ES,WORD PTR @BUF.SELECTOR[0]        ;NEAT!!!
        MOV EDI,@BUF.OFSET
        INT 31H
        SETC ERRORFLAG
        JNC @Q_000B
        CALL [ERRORHANDLER] PASCAL,0005       

 @Q_000B:
        RET
        ENDP

;=============================================================================
; VOID SET_DESCR(LONG SELECTOR, POINTER BUF)

SET_DESCR PROC NEAR
        ARG @SELECTOR:DWORD, @BUF:POINTER

        USES EAX,EBX,EDI,ES
        MOV EAX,00CH
        MOV EBX,@SELECTOR
        MOV ES,WORD PTR @BUF.SELECTOR[0]        ;NEAT!!!
        MOV EDI,@BUF.OFSET
        INT 31H
        SETC ERRORFLAG
        JNC @Q_000C
        CALL [ERRORHANDLER] PASCAL,0006       

 @Q_000C:
        RET
        ENDP

;=============================================================================
; (LONG SELECTOR, LONG(EAX) SEGMENT) GETDOSMEM(LONG SIZE)

GETDOSMEM PROC NEAR
        ARG @SIZE:DWORD
        
        USES EBX
        MOV EAX,0100H
        MOV EBX,@SIZE
        SHR EBX,4
        INT 31H
        MOVZX EAX,AX
        MOVZX EDX,DX
        SETC ERRORFLAG
        JNC @Q_0100
        CALL [ERRORHANDLER] PASCAL,0007      
        XOR EAX,EAX
        MOV EDX,EAX
        
 @Q_0100:
        RET
        ENDP

;=============================================================================
; VOID FREEDOSMEM(LONG SELECTOR)

FREEDOSMEM PROC NEAR
        ARG @SELECTOR:DWORD
        
        USES EAX,EDX
        MOV EAX,0101H
        MOV EDX,@SELECTOR
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0101
        CALL [ERRORHANDLER] PASCAL,0008         
        
 @Q_0101:
        RET
        ENDP

;=============================================================================
; VOID RESIZEDOSMEM(LONG SELECTOR, LONG SIZE)

RESIZEDOSMEM PROC NEAR
        ARG @SELECTOR:DWORD, @SIZE:DWORD
        
        USES EAX,EBX,EDX
        MOV EAX,0102H
        MOV EBX,@SIZE
        SHR EBX,4
        MOV EDX,@SELECTOR
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0102
        CALL [ERRORHANDLER] PASCAL,0009        
        
 @Q_0102:
        RET
        ENDP

;=============================================================================
; POINTER GET_EXCEPTION(LONG I)
; RETURNS VECTOR OF EXCEPTION I

GET_EXCEPTION PROC NEAR
        ARG @I:DWORD
        
        USES EAX,EBX
        MOV EAX,0202H
        MOV EBX,@I
        INT 31H
        SETC ERRORFLAG
        MOVZX ECX,CX            ;SELECTOR EM ECX, OFFSET EM EDX
        JNC @L_0202
        CALL [ERRORHANDLER] PASCAL,0010       
        XOR ECX,ECX		;MAYBE UNECESSARY... 
        MOV EDX,ECX

  @L_0202:
        RET
        ENDP

;=============================================================================
; VOID SET_EXCEPTION(LONG I,POINTER VEC)
; SETS VECTOR OF EXCEPTION I TO VEC

SET_EXCEPTION PROC NEAR
        ARG @I:DWORD, @P:POINTER
        
        USES EAX,EBX,ECX,EDX
        MOV EAX,0203H
        MOV EBX,@I
        MOV ECX,DWORD PTR @P.SELECTOR           ;SELECTOR DO HANDLER
        MOV EDX,DWORD PTR @P.OFSET              ;OFFSET DO HANDLER
        INT 31H
        SETC ERRORFLAG
        JNC @L_0203
        CALL [ERRORHANDLER] PASCAL,0011     

 @L_0203:
        RET
       ENDP

;=============================================================================
; POINTER GET_INTVEC(LONG I)
; RETURNS PM VECTOR OF INT I

GET_INTVEC PROC NEAR
        ARG @I:DWORD
        
        USES EAX,EBX
        MOV EAX,0204H
        MOV EBX,@I
        INT 31H
        SETC ERRORFLAG
        MOVZX ECX,CX            ;SELECTOR
        JNC @L_0204
        CALL [ERRORHANDLER] PASCAL,0012        
        XOR ECX,ECX		
        MOV EDX,ECX

 @L_0204:
        RET
        ENDP

;=============================================================================
; VOID SET_INTVEC(LONG I,POINTER VEC)
; SETS PM VECTOR OF INT I TO VEC


SET_INTVEC PROC NEAR
        ARG @I:DWORD, @P:POINTER
        
        USES EAX,EBX,ECX,EDX
        MOV EAX,0205H
        MOV EBX,@I
        MOV ECX,DWORD PTR @P.SELECTOR           ;SELECTOR IN CX
        MOV EDX,DWORD PTR @P.OFSET              ;OFFSET IN EDX
        INT 31H
        SETC ERRORFLAG
        JNC @L_0205
        CALL [ERRORHANDLER] PASCAL,0013      

 @L_0205:
        RET
       ENDP

;==========================================================================
; VOID RM_INT(LONG I)
; EXECUTA A INT I EM MODO REAL

RM_INT PROC NEAR
        ARG @I:DWORD

        USES ES,EDI
        MOV DWORD PTR REGS._AX,EAX
        MOV DWORD PTR REGS._BX,EBX
        MOV DWORD PTR REGS._CX,ECX
        MOV DWORD PTR REGS._DX,EDX
        MOV DWORD PTR REGS._SI,ESI
        MOV DWORD PTR REGS._DI,EDI
	MOV AX,SEG _SEGMENT32
        MOV REGS._DS,AX   ;ASSUME REGS._DS<-SEGMENT32
        XOR EAX,EAX
        MOV DWORD PTR REGS._BP,EAX  
        MOV REGS._FS,AX
        MOV REGS._GS,AX     
        MOV AX,0202H
        MOV REGS.FLAGS,AX           
        MOV AX,OLDSS    
        MOV REGS._SS,AX
        MOV AX,OLDSP
        MOV REGS._SP,AX
        MOV EBX,@I
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


;==========================================================================
; VOID CALL_RMPROC(ADDX:POINTER)
; EXECUTA UM PROC FAR EM MODO REAL
; VALORES DOS REGISTOS NA ESTRUTURA REGS

CALL_RMPROC PROC NEAR
        ARG @ADDX:POINTER

        USES EAX,EBX,ECX,EDX,EDI,ES
        MOV EAX,@ADDX.SELECTOR
        MOV EBX,@ADDX.OFSET
        MOV REGS._CS,AX             ;SETA O ENDEREÇO NA ESTRUTURA
        MOV REGS._IP,BX
        MOV AX,OLDSS    
        MOV REGS._SS,AX
        MOV AX,OLDSP
        MOV REGS._SP,AX
        MOV EAX,0202
        MOV REGS.FLAGS,AX
        MOV EAX,0301H
        XOR EBX,EBX
        XOR ECX,ECX
        MOV ES,SDATA32
        MOV EDI,OFFSET REGS
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0301
        CALL [ERRORHANDLER] PASCAL,0015

 @Q_0301:
        RET
      ENDP
        
;==========================================================================
; POINTER ALLOC_CALLBACK(PROC2CALL:POINTER, REGSTRUC:POINTER)
; ALOCA UMA CALLBACK E RETORNA O ENDEREÇO DE MODO REAL A SER CHAMADO
; VALORES DOS REGISTOS NA ESTRUTURA REGS

ALLOC_CALLBACK PROC NEAR
        ARG @PROC2CALL:POINTER, @RSTRUC:POINTER

        USES EAX,ESI,EDI,ES,DS
        MOV EAX,@PROC2CALL.SELECTOR
        MOV ESI,@PROC2CALL.OFSET
        MOV DS,AX
        MOV EDI,@RSTRUC.OFSET
        MOV ES,WORD PTR @RSTRUC.SELECTOR[0]
        MOV AX,0303H    
        INT 31H
        MOVZX ECX,CX
        MOVZX EDX,DX
        MOV DS,CS:SDATA32       	;RESTAURA DS PARA SETAR FLAG DE ERRO
        SETC ERRORFLAG
        JNC @Q_0303
        CALL [ERRORHANDLER] PASCAL,0016 
        XOR ECX,ECX
        MOV EDX,ECX

 @Q_0303:
        RET
       ENDP    

;==========================================================================        
; VOID FREE_CALLBACK(RMADDX:POINTER)
; LIBERTA UMA CALLBACK ALOCADA

FREE_CALLBACK PROC NEAR
        ARG @RMADDX:POINTER
        
        USES EAX,ECX,EDX
        MOV AX,0304H
        MOV ECX,@RMADDX.SELECTOR
        MOV EDX,@RMADDX.OFSET
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0304
        CALL [ERRORHANDLER] PASCAL,0017

 @Q_0304:
        RET
        ENDP    
        
;==========================================================================     
; VOID DPMI_MEMINFO(POINTER(DPMI_MEM) BLOCK)
; PREENCHE A ESTRUTURA BLOCK COM A INFO DE MEMORIA


DPMI_MEM_INFO PROC NEAR
        ARG @BLOCK:POINTER
        
        USES EAX,EDI,ES
        MOV EDI,@BLOCK.OFSET
        MOV ES,WORD PTR @BLOCK.SELECTOR[0]
        MOV EAX,0500H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0500
        CALL [ERRORHANDLER] PASCAL,0018

 @Q_0500:
        RET
        ENDP    
        

;==========================================================================
; LONG GETMEM(LONG SIZE)
; ALOCA UM BLOCO DE MEMORIA
; O HANDLE É COLOCADO NOS BYTES 4 A 8 DO BLOCO

GETMEM PROC NEAR
        ARG @SIZE:DWORD

        USES EAX,EBX,ECX,ESI,EDI,ES
        MOV EBX,@SIZE
        CMP EBX,32              ;TAMANHO MINIMO DE 32 BYTES
        JGE @L_0501
        MOV EBX,32
                
 @L_0501:
        ADD EBX,8               ;ADICIONA  ESPAÇO PARA O HANDLE
        MOVZX ECX,BX            ;CX - LSW
        SHR EBX,16              ;BX - MSW
        MOV EAX,0501H
        INT 31H
        SETC ERRORFLAG
        JC @QE_0501
        MOV ES,CS:SDATA0                ;ES - DATA0 PQ O ENDEREÇO É LINEAR
        SHL EBX,16                      ;HANDLE EM EBX
        MOV BX,CX
        SHL ESI,16                      ;ENDEREÇO EM ESI
        MOV SI,DI
        XCHG EBX,ESI
        MOV EAX,0F010E0AH
        MOV ES:[ESI],EAX                ;GUARDA ASSINATURA
        MOV ES:[ESI+4],EBX              ;GUARDA HANDLE
        ADD ESI,8                       ;SALTA O CABEÇALHO
        MOV EDX,ESI
        JMP @Q_0501

 @QE_0501:
        CALL [ERRORHANDLER] PASCAL,0019  
        XOR EDX,EDX     
 @Q_0501:
        RET
      ENDP
        
;==========================================================================
; VOID FREEMEM(LONG ADDX)
; LIBERTA UM BLOCO DE MEMORIA
; O HANDLE é COLOCADO NOS 1ºS 4 BYTES DO BLOCO

FREEMEM PROC NEAR
        ARG @ADDX:DWORD
        
        USES EAX,EBX,ESI,EDI,ES
        MOV ES,CS:SDATA0
        MOV EDI,@ADDX
        SUB EDI,8
        CMP ES:[EDI],0F010E0AH          ;VERIFICA ASSINATURA
        SETNE ERRORFLAG
        JNE @QE_0502

        MOV ESI,ES:[EDI+4]              ;LÊ O HANDLE
        MOV DI,SI
        SHR ESI,16                      ;EM SI:DI
        MOV AX,0502H
        INT 31H 
        SETC ERRORFLAG
        JNC @Q_0502

 @QE_0502:
        CALL [ERRORHANDLER] PASCAL,0020       

 @Q_0502:
        RET
       ENDP    

;==========================================================================
; LONG RESIZEMEM(LONG ADDX, LONG SIZE)
; REDIMENSIONA UM BLOCO DE MEMORIA


RESIZEMEM PROC NEAR
        ARG @ADDX:DWORD,@SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI,ES
        MOV EBX,@SIZE
        CMP EBX,32              ;TAMANHO MINIMO DE 32 BYTES
        JGE @L_0503
        MOV EBX,32
                
 @L_0503:
        ADD EBX,8               ;ADICIONA  ESPAÇO PARA O HANDLE
        MOVZX ECX,BX            ;CX - LSW
        SHR EBX,16              ;BX - MSW

        MOV ES,CS:SDATA0        ;LE O HANDLE DO BLOCO
        MOV EDI,@ADDX
        SUB EDI,8
        CMP ES:[EDI],0F010E0AH  ;VERIFICA ASSINATURA
        SETNE ERRORFLAG
        JNE @QE_0503

        MOV ESI,ES:[EDI+4]              ;LE O HANDLE
        MOVZX EDI,SI
        SHR ESI,16                      ;EM SI:DI
        MOV AX,0503H
        INT 31H 
        SETC ERRORFLAG                  ;ASSINALA ERRO
        JC @QE_0503

        SHL EBX,16                      ;HANDLE EM EBX
        MOV BX,CX
        SHL ESI,16                      ;ENDEREÇO EM ESI
        MOV SI,DI
        XCHG EBX,ESI
        MOV ES,CS:SDATA0
        MOV EAX,0F010E0AH       
        MOV ES:[ESI],EAX
        MOV ES:[ESI+4],EBX
        ADD ESI,8
        MOV EDX,ESI                     ;RETORNA ENDEREÇO EM EDX
        JMP @Q_0503

 @QE_0503:
        CALL [ERRORHANDLER] PASCAL,0021      
        XOR EDX,EDX     

 @Q_0503:
        RET
        ENDP
        
;==========================================================================     
; VOID LOCK_REGION(LONG LINADDX, LONG SIZE)
; NAO PERMITE PAGING NA REGIAO ESPECIFICADA

LOCK_REGION PROC NEAR
        ARG @LINADDX:DWORD, @SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI
        MOV EBX,@LINADDX
        MOV CX,BX
        SHR EBX,16
        MOV ESI,@SIZE
        MOV DI,SI
        SHR ESI,16
        MOV AX,0600H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0600
        CALL [ERRORHANDLER] PASCAL,0022    

 @Q_0600:
        RET
        ENDP
        
;==========================================================================     
; VOID UNLOCK_REGION(LONG LINADDX, LONG SIZE)

UNLOCK_REGION PROC NEAR
        ARG @LINADDX:DWORD, @SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI
        MOV EBX,@LINADDX
        MOV CX,BX
        SHR EBX,16
        MOV ESI,@SIZE
        MOV DI,SI
        SHR ESI,16
        MOV AX,0601H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0601
        CALL [ERRORHANDLER] PASCAL,0023     

 @Q_0601:
        RET
       ENDP

;==========================================================================     
; VOID MARK_RM_PAGEABLE(LONG LINADDX, LONG SIZE)
; MARCA UMA ZONA REAL COMO PAGINAVEL

MARK_RM_PAGEABLE PROC NEAR
        ARG @LINADDX:DWORD, @SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI
        MOV EBX,@LINADDX
        MOV CX,BX
        SHR EBX,16
        MOV ESI,@SIZE
        MOV DI,SI
        SHR ESI,16
        MOV AX,0602H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0602
        CALL [ERRORHANDLER] PASCAL,0024  

 @Q_0602:
        RET
        ENDP

;==========================================================================     
; VOID VCLI()

VCLI PROC NEAR
        
        USES EAX
        MOV EAX,0900H
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0900
        CALL [ERRORHANDLER] PASCAL,0025    
        
 @Q_0900:
        RET
        ENDP

;==========================================================================     
; VOID VSTI()

VSTI PROC NEAR
        
        USES EAX
        MOV EAX,0901H
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0901
        CALL [ERRORHANDLER] PASCAL,0026        
        
 @Q_0901:
        RET
        ENDP

;==========================================================================     
; LONG GET_VI()

GET_VI PROC NEAR
        
        USES EAX
        MOV EAX,0902H
        INT 31H
        MOV EDX,EAX
        SETC ERRORFLAG  
        JNC @Q_0902
        CALL [ERRORHANDLER] PASCAL,0027         
        MOV EDX,-1      

 @Q_0902:
        RET
        ENDP
  
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CODE32_STARTUP:

;--------------------------------------------------------
	MOV ES,SDATA0
	PUSH OFFSET Q00001
	POP EXITPROC			;EXIT ROUTINE

	PUSH OFFSET ERROR_HANDLER	;DEFAULT ERROR HANDLER ROUTINE
	POP ERRORHANDLER

	MOV AL,ES:[449H]		;MODO DE VÍDEO INICIAL
	MOV INIT_MODE,AL

;--------------------------------------------------------
	MOV EAX,SEG _SEGMENT32		;PREVINE PAGING EM _SEGMENT32
	SHL EAX,4
	MOV EDX,OFFSET SAFE2PAGING	;EM TODO O CÓDIGO QUE POSSA SER EXECUTADO DURANTE
					;UMA EXCEPTION 
        CALL LOCK_REGION PASCAL,EAX,EDX
        CALL SWAP_EXCEPT_VECTORS        ;REDEFINE OS VECTORES DAS EXCEPÇÕES

	PUSH OFFSET Q00002
	POP EXITPROC			;EXIT ROUTINE

;--------------------------------------------------------
	CALL GETMEM PASCAL, HEAPSIZE*1024 ;INITIALIZE HEAP
	MOV HEAP_ADDX,EDX
	PUSH OFFSET Q00003
	POP EXITPROC			;EXIT ROUTINE
	CALL HEAP_INIT

;--------------------------------------------------------
EXTRN _MAIN:NEAR

	CALL _MAIN

ALIGN 8

;--------------------------------------------------------
Q00003:	
	CALL FREEMEM PASCAL,HEAP_ADDX

;--------------------------------------------------------
Q00002:	
        CALL RESTORE_EXCEPT_VECTORS     ;RESTORE ORIGINAL EXCEPTION VECTORS
	MOV EAX,SEG _SEGMENT32		;HABILITA PAGING EM _SEGMENT32
	SHL EAX,4
	MOV EDX,OFFSET SAFE2PAGING
        CALL UNLOCK_REGION PASCAL,EAX,EDX

;--------------------------------------------------------
Q00001:
	
	MOVZX EBX,SSTACK16	;FREE SYSTEM SELECTORS
	MOV EAX,001H
        INT 31H

	MOVZX EBX,SDATA16
	MOV EAX,001H
        INT 31H
	
	MOVZX EBX,SDATA32
	MOV EAX,001H
        INT 31H

	MOV AX,CS:EXITCOD
	MOV AH,04Ch			;THE END!!!
	INT 21H



;##########################################################################
; EXCEPTION HANDLERS
;##########################################################################

;=========================================================================
;VARIAVEIS DOS HANDLERS

ALIGN 8

EXCEP_REGS REGISTERS    <>      ;EXCEPTION HANDLER REGISTER STRUC
EXCEP_SF   EXCEP_SF32   <>      ;EXCEPTION STACK FRAME PARAMETERS
EXCEP_NO   DD 255             	;EXCEPTION NUMBER

;=========================================================================
;=========================================================================
; ORIGINAL SYSTEM EXCEPTION HANDLER VECTOR TABLES

ALIGN 4

OLD_EX00 POINTER <>     ;EXCEPTION 0 - DIVIDE ERROR
OLD_EX01 POINTER <>     ;EXCEPTION 1 - DEBUG EXCEPTION
OLD_EX02 POINTER <>     ;EXCEPTION 2 - NMI INTERRUPT
OLD_EX03 POINTER <>     ;EXCEPTION 3 - ONE BYTE INT
OLD_EX04 POINTER <>     ;EXCEPTION 4 - INT ON OVERFLOW
OLD_EX05 POINTER <>     ;EXCEPTION 5 - BOUND CHECK
OLD_EX06 POINTER <>     ;EXCEPTION 6 - INVALID OPCODE
OLD_EX07 POINTER <>     ;EXCEPTION 7 - FPU EXCEPTION
OLD_EX08 POINTER <>     ;EXCEPTION 8 - DOUBLE FAULT
OLD_EX09 POINTER <>     ;EXCEPTION 9 - FPU SEGMENT OVERRUN
OLD_EX10 POINTER <>     ;EXCEPTION 10 - INVALID TSS
OLD_EX11 POINTER <>     ;EXCEPTION 11 - NO SEGMENT
OLD_EX12 POINTER <>     ;EXCEPTION 12 - STACK FAULT
OLD_EX13 POINTER <>     ;EXCEPTION 13 - GENERAL PROTECTION FAULT
OLD_EX14 POINTER <>     ;EXCEPTION 14 - RESERVED
OLD_EX15 POINTER <>     ;EXCEPTION 15 - PAGE FAULT

;=========================================================================
;HIGH LEVEL EXCEPTION HANDLERS OFFSET

EXCEP_HANDLER00 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 00
EXCEP_HANDLER01 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 01
EXCEP_HANDLER02 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 02
EXCEP_HANDLER03 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 03
EXCEP_HANDLER04 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 04
EXCEP_HANDLER05 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 05
EXCEP_HANDLER06 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 06
EXCEP_HANDLER07 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 07
EXCEP_HANDLER08 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 08
EXCEP_HANDLER09 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 09
EXCEP_HANDLER10 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 10
EXCEP_HANDLER11 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 11
EXCEP_HANDLER12 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 12
EXCEP_HANDLER13 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 13
EXCEP_HANDLER14 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 14
EXCEP_HANDLER15 DD OFFSET EX_DEF      ;HANDLER DA EXCEPTION 15

;=========================================================================
;MENSAGENS DE ERRO

EXCEP00MSG DB 0AH,0DH,'ERRO #200 : EXCEPTION 00h - DIVIDE BY ZERO.',0Ah,0Dh,7,'$'
EXCEP01MSG DB 0AH,0DH,'ERRO #201 : EXCEPTION 01h - DEBUG TRAP.',0Ah,0Dh,7,'$'
EXCEP02MSG DB 0AH,0DH,'ERRO #202 : EXCEPTION 02h - NMI OCCURED.',0Ah,0Dh,7,'$'
EXCEP03MSG DB 0AH,0DH,'ERRO #203 : EXCEPTION 03h - ONE BYTE INTERRUPT.',0Ah,0Dh,7,'$'
EXCEP04MSG DB 0AH,0DH,'ERRO #204 : EXCEPTION 04h - OVERFLOW OCCURED.',0Ah,0Dh,7,'$'
EXCEP05MSG DB 0AH,0DH,'ERRO #205 : EXCEPTION 05h - BOUND ERROR.',0Ah,0Dh,7,'$'
EXCEP06MSG DB 0AH,0DH,'ERRO #206 : EXCEPTION 06h - INVALID OPCODE.',0Ah,0Dh,7,'$'
EXCEP08MSG DB 0AH,0DH,'ERRO #208 : EXCEPTION 08h - DOUBLE FAULT ERROR.',0Ah,0Dh,7,'$'
EXCEP0AMSG DB 0AH,0DH,'ERRO #210 : EXCEPTION 0Ah - INVALID TSS.',0Ah,0Dh,7,'$'
EXCEP0BMSG DB 0AH,0DH,'ERRO #211 : EXCEPTION 0Bh - SEGMENT FAULT.',0Ah,0Dh,7,'$'
EXCEP0CMSG DB 0AH,0DH,'ERRO #212 : EXCEPTION 0Ch - STACK ERROR.',0Ah,0Dh,7,'$'
EXCEP0DMSG DB 0AH,0DH,'ERRO #213 : EXCEPTION 0Dh - GENERAL PROTECTION FAULT.',0Ah,0Dh,7,'$'
EXCEP0EMSG DB 0AH,0DH,'ERRO #214 : EXCEPTION 0Eh - PAGE FAULT.',0Ah,0Dh,7,'$'

;=========================================================================
;TABELA DE OFFSETS
ALIGN 4

ERROR_TABLE DD Offset EXCEP00MSG,Offset EXCEP01MSG,Offset EXCEP02MSG
            DD Offset EXCEP03MSG,Offset EXCEP04MSG,Offset EXCEP05MSG
            DD Offset EXCEP06MSG,0,Offset EXCEP08MSG, 0
            DD Offset EXCEP0AMSG,Offset EXCEP0BMSG,Offset EXCEP0CMSG
            DD Offset EXCEP0DMSG,Offset EXCEP0EMSG

;=========================================================================
;MACRO DOS NOVOS EXCEPTION HANDLERS 

SYS_EH MACRO NOME,NUM,HANDLER

        NOME PROC FAR

                ALIGN 4         
                CALL ESAVE_REGS PASCAL,SS,ESP
                MOV EAX,NUM
                MOV DS:EXCEP_NO,EAX
                CALL [HANDLER]
                XOR EAX,EAX
                MOV EBX,ESP
                MOV AX,SS
                CALL ERESTORE_REGS PASCAL,EAX,EBX
                ALIGN 4         
                RET
                ENDP
        ENDM


;=========================================================================
;#########################################################################
;=========================================================================

;==========================================================================     
; VOID ESAVE_REGS(POINTER E_STRUC)
; SALVA OS REGISTOS AO ENTRAR NA EXCEPTION HANDLER

ESAVE_REGS PROC NEAR                    ;RETS WITH DS<-SDATA32!!!
        ARG @E_STRUC:POINTER
                
        PUSH DS
        MOV DS,CS:SDATA32
        MOV DWORD PTR DS:EXCEP_REGS._AX,EAX
        MOV EAX,0900H                           ;VCLI
        INT 31H
        MOV DWORD PTR DS:EXCEP_REGS._BX,EBX
        MOV DWORD PTR DS:EXCEP_REGS._CX,ECX
        MOV DWORD PTR DS:EXCEP_REGS._DX,EDX
        MOV DWORD PTR DS:EXCEP_REGS._DI,EDI
        MOV DWORD PTR DS:EXCEP_REGS._SI,ESI
        MOV DWORD PTR DS:EXCEP_REGS._ES,ES
        MOV DWORD PTR DS:EXCEP_REGS._FS,FS      
        MOV DWORD PTR DS:EXCEP_REGS._GS,GS      
        POP AX
        MOV WORD PTR DS:EXCEP_REGS._DS,AX
        PUSH EDI ESI ES DS
        MOV AX,DS
        MOV ES,AX
        LPTR DS,ESI,@E_STRUC
        ADD ESI,8                               ;REMOVE PARAMS FROM STACK
        MOV EDI,OFFSET EXCEP_SF
        MOV ECX,SIZE EXCEP_SF32
        CLD
        REP MOVSB
        POP DS ES ESI EDI
        RET 
 ENDP

;==========================================================================     
;VOID ERESTORE_REGS(POINTER E_STRUC)
;RESTORE REGISTERS AFTER EXCEPTION HANDLER

ERESTORE_REGS PROC NEAR
        ARG @E_STRUC:POINTER
        
        LPTR ES,EDI,@E_STRUC
        MOV AX,CS:SDATA32
        MOV DS,AX
        MOV ESI,OFFSET EXCEP_SF
        MOV ECX,SIZE EXCEP_SF32
        CLD
        REP MOVSB
        MOVZX EAX,WORD PTR DS:EXCEP_REGS._DS
        PUSH EAX                                ;GONNA BE DS!!
        MOV ES,WORD PTR DS:EXCEP_REGS._ES
        MOV FS,WORD PTR DS:EXCEP_REGS._FS
        MOV GS,WORD PTR DS:EXCEP_REGS._GS
        MOV EDI,DWORD PTR DS:EXCEP_REGS._DI
        MOV ESI,DWORD PTR DS:EXCEP_REGS._SI
        MOV EAX,0901H                           ;VSTI
        INT 31H
        MOV EAX,DWORD PTR DS:EXCEP_REGS._AX
        MOV EBX,DWORD PTR DS:EXCEP_REGS._BX     
        MOV ECX,DWORD PTR DS:EXCEP_REGS._CX
        MOV EDX,DWORD PTR DS:EXCEP_REGS._DX
        POP DS                                  ;RESTAURA DS
        RET
        ENDP

;=========================================================================
;=========================================================================

;=========================================================================
;DEFAULT EXCEPTION HANDLER TO ALL EXCEPS

EX_DEF PROC NEAR

        XOR EAX,EAX
        MOV AL,3			;INITIAL VIDEO MODE
        CALL RM_INT PASCAL,010H
        MOV ESI,OFFSET ERROR_TABLE	;ERROR MSG
        MOV EAX, EXCEP_NO
        SHL AX,1
        ADD ESI,EAX
        LODSW
        MOVZX EDX,AX
        MOV EAX,0900H
        CALL RM_INT PASCAL,021H 	;PRINT ERROR MSG
        MOV AX,CS
        MOV DS:EXCEP_SF.RET_CS,AX
        MOV EAX,EXITPROC
        MOV DS:EXCEP_SF.RET_EIP,EAX
        RET
        ENDP

;=========================================================================
;LOW-LEVEL EXCEPTION HANDLERS 
;DEFINIÇÃO DOS PROCEDIMENTOS ATRAVÉS DE MACROS - NEAT!!

;SYS_EH NOME_PROC, NUM_EXCEP, HANDLER_PROC

SYS_EH SYS_EXCEP_00, 00, EXCEP_HANDLER00
SYS_EH SYS_EXCEP_01, 01, EXCEP_HANDLER01
SYS_EH SYS_EXCEP_02, 02, EXCEP_HANDLER02
SYS_EH SYS_EXCEP_03, 03, EXCEP_HANDLER03
SYS_EH SYS_EXCEP_04, 04, EXCEP_HANDLER04
SYS_EH SYS_EXCEP_05, 05, EXCEP_HANDLER05
SYS_EH SYS_EXCEP_06, 06, EXCEP_HANDLER06
SYS_EH SYS_EXCEP_07, 07, EXCEP_HANDLER07
SYS_EH SYS_EXCEP_08, 08, EXCEP_HANDLER08
SYS_EH SYS_EXCEP_09, 09, EXCEP_HANDLER09
SYS_EH SYS_EXCEP_10, 10, EXCEP_HANDLER10
SYS_EH SYS_EXCEP_11, 11, EXCEP_HANDLER11
SYS_EH SYS_EXCEP_12, 12, EXCEP_HANDLER12
SYS_EH SYS_EXCEP_13, 13, EXCEP_HANDLER13
SYS_EH SYS_EXCEP_14, 14, EXCEP_HANDLER14
SYS_EH SYS_EXCEP_15, 15, EXCEP_HANDLER15

ALIGN 8

;=========================================================================
;TABELA DE OFFSETS DOS EXCEPTION HANDLERS

SYS_EH_TABLE DD OFFSET SYS_EXCEP_00, OFFSET SYS_EXCEP_01, OFFSET SYS_EXCEP_02 
	     DD OFFSET SYS_EXCEP_03
             DD OFFSET SYS_EXCEP_04, OFFSET SYS_EXCEP_05, OFFSET SYS_EXCEP_06
             DD OFFSET SYS_EXCEP_07, OFFSET SYS_EXCEP_08, OFFSET SYS_EXCEP_09
             DD OFFSET SYS_EXCEP_10, OFFSET SYS_EXCEP_11, OFFSET SYS_EXCEP_12
             DD OFFSET SYS_EXCEP_13, OFFSET SYS_EXCEP_14, OFFSET SYS_EXCEP_15

;=========================================================================
; VOID SWAP_EXCEPT_VECTORS()

SWAP_EXCEPT_VECTORS PROC NEAR

        CALL VCLI
        MOV ES,CS:SDATA32
        MOV DS,CS:SDATA32		;FAIL SAFE
        MOV EDI,OFFSET OLD_EX00		;TABELA DOS VECTORES ANTIGOS
        MOV ESI,OFFSET SYS_EH_TABLE	;TABELA DE OFFSETS DOS NOVOS HANDLERS
        MOV ECX,0

  @@SWAP_EXCEP_L1:
        PUSH ECX
        CALL GET_EXCEPTION PASCAL,ECX		;SALVA OLD VECTORS
        MOV ES:[EDI][POINTER.SELECTOR],ECX
        MOV ES:[EDI][POINTER.OFSET],EDX
        ADD EDI,8
        POP ECX
        LODSD
        CMP ECX,7				;VECTORES A NÃO SETAR
        JE @@SWAP_SKIP				;EXISTEM NA TABELA, MAS NÃO DEVEM SER USADOS
        CMP ECX,9				
        JE @@SWAP_SKIP
        CMP ECX,15
        JE @@SWAP_SKIP
        CALL SET_EXCEPTION PASCAL,ECX,CS,EAX	;SETA O NOVO VECTOR

 @@SWAP_SKIP:
        INC ECX
        CMP ECX,16
        JNE @@SWAP_EXCEP_L1
        
        CALL VSTI				;HABILITA INTERRUPÇÕES
        RET
        ENDP    

;=========================================================================
; VOID RESTORE_EXCEPT_VECTORS()

RESTORE_EXCEPT_VECTORS PROC NEAR

        CALL VCLI				;DESACTIVA INTERRUPÇÕES
        MOV DS,CS:SDATA32
        MOV ESI,OFFSET OLD_EX00			;TABELA DE VECTORES ANTIGOS
        MOV ECX,15
        CLD

  @@RESTORE_EXCEP_L1:
        MOV EBX,DS:[ESI][POINTER.SELECTOR]
        MOV EAX,DS:[ESI][POINTER.OFSET]
        CALL SET_EXCEPTION PASCAL,ECX,EBX,EAX	;RESTAURA VECTORES ANTIGOS
        ADD ESI,8
        LOOP @@RESTORE_EXCEP_L1
        
        CALL VSTI				;HABILITA INTERRUPÇÕES
        RET
      ENDP    


;##########################################################################
; HEAP MANAGER 
;##########################################################################

MAGIC   = 0F0A12BDFh
HDRSIZE = SIZE BLOCKHDR

;=========================================================================

BLOCKHDR STRUC 
	   SIGNATURE DD ?
	   BLOCKSIZE DD ?
	   NEXTBLOCK DD ?	    
           USED      DD ?
BLOCKHDR ENDS

;=========================================================================

HEAP_PTR  DD ?   ;START OF HEAP

;=========================================================================
; INICIALIZA A HEAP

HEAP_INIT PROC NEAR

	MOV ES,SDATA0
	MOV EDI,HEAP_ADDX
	MOV ES:[EDI][BLOCKHDR.SIGNATURE],MAGIC
	MOV ES:[EDI][BLOCKHDR.NEXTBLOCK],0
	MOV ES:[EDI][BLOCKHDR.BLOCKSIZE],HEAPSIZE*1024
	MOV ES:[EDI][BLOCKHDR.USED],FALSE
	MOV HEAP_PTR,EDI
	RET 
      ENDP

;=========================================================================
;DIVIDE UM BLOCO EM 2
;PARAMS: EDI->BLOCO INICIAL, EDX->TAMANHO DO BLOCO. RETS EDX->NOVO BLOCO
;DESTRÓI EAX

BLOCK_SPLIT:
	
	PUSH ES:[EDI][BLOCKHDR.NEXTBLOCK]	;SALVA ADDX DO BLOCO SEGUINTE
	MOV EAX,ES:[EDI][BLOCKHDR.BLOCKSIZE]	
	SUB EAX,EDX				;TAMANHO DO NOVO BLOCO
	SUB EAX,HDRSIZE
	MOV ES:[EDI][BLOCKHDR.BLOCKSIZE],EDX	;TAMANHO DO BLOCO INICIAL
	ADD EDX,EDI				;OFFSET DO NOVO BLOCO
	ADD EDX,HDRSIZE
	MOV ES:[EDI][BLOCKHDR.NEXTBLOCK],EDX		;FST.NEXT=@NEXT
	MOV ES:[EDX][BLOCKHDR.BLOCKSIZE],EAX
	MOV ES:[EDX][BLOCKHDR.SIGNATURE],MAGIC
	POP ES:[EDX][BLOCKHDR.NEXTBLOCK]
	MOV ES:[EDI][BLOCKHDR.USED],TRUE
	MOV ES:[EDX][BLOCKHDR.USED],FALSE
      RET

;=========================================================================
;PROCURA O PRIMEIRO BLOCO LIVRE, MAIOR OU IGUAL AO ESPECIFICADO
;PARAMS: ECX->TAMANHO. RETS AL - TRUE/FALSE;EDI->ADDX

BLOCK_FINDFREE:
	MOV EDI,HEAP_PTR

  @@LOOP_FIND:
	CMP ES:[EDI][BLOCKHDR.USED],TRUE     ;SE EM USO,SALTA
	JE @@SKIP_BLOCK
	CMP ES:[EDI][BLOCKHDR.BLOCKSIZE],ECX ;SE NÃO FOR GRANDE O SUFICIENTE,SALTA
	JB @@SKIP_BLOCK
	MOV AL,TRUE
	RET
	
  @@SKIP_BLOCK:
	PUSH ES:[EDI][BLOCKHDR.NEXTBLOCK]     ;OBTÉM O PRÓXIMO BLOCO
	POP EDI
	CMP EDI,0			     ;SE NÃO FOR O ÚLTIMO, CONTINUA 
        JNE @@LOOP_FIND	

 @@BAD_QUIT:
	MOV AL,FALSE
	RET

;=========================================================================
;CALCULA A MEMÓRIA LIVRE
;RETS EDX - TAMANHO; EBX - Nº DE BLOCOS ALOCADOS; ECX - Nº DE BL. LIVRES

MEMAVAIL PROC NEAR

	USES ES,EDI
	MOV ES,SDATA0
	MOV EDI,HEAP_PTR
	XOR EDX,EDX                ;ESPAÇO LIVRE
	XOR EBX,EBX		   ;BLOCOS ALOCADOS
	XOR ECX,ECX		   ;BLOCOS LIVRES

  @@MEM_LOOP:
        CMP ES:[EDI][BLOCKHDR.USED],TRUE
	JE @@IN_USE
	INC ECX
	ADD EDX,ES:[EDI][BLOCKHDR.BLOCKSIZE]
	JMP @@SEEK_NEXT
  @@IN_USE:
	INC EBX

  @@SEEK_NEXT:
        MOV EDI,ES:[EDI][BLOCKHDR.NEXTBLOCK]
	CMP EDI,0
	JNE @@MEM_LOOP
	RET
      ENDP

;=========================================================================
;ALOCA UM BLOCO DE MEMÓRIA
;RETS EDI -  ADDX (0 SE ERRO)

MALLOC PROC NEAR
	ARG @SZ:DWORD

	USES ES,EDX,EBX,EAX,ECX
	MOV ES,SDATA0
	MOV ECX,@SZ 
	CALL BLOCK_FINDFREE
	CMP AL,TRUE
	JNE @@MALLOC_FAIL
	MOV EAX,ES:[EDI][BLOCKHDR.BLOCKSIZE]
	MOV EDX,ECX
	SUB EAX,ECX
	CMP EAX,64			;SE FOR <64 BYTES A DIFERENÇA, NÃO PARTE O BLOCO
	JB @@NOSPLIT

	CALL BLOCK_SPLIT

   @@NOSPLIT:
	ADD EDI,HDRSIZE
	JMP @@MALLOC_QUIT

   @@MALLOC_FAIL:
	XOR EDI,EDI
	
   @@MALLOC_QUIT:	
	RET
      ENDP

;=========================================================================
;LIBERTA UM BLOCO DE MEMÓRIA PREVIAMENTE ALOCADO
;

MDALLOC PROC NEAR
	ARG @ADDX:DWORD

	USES ES,EDX,EAX,EDI
	MOV ES,SDATA0
	MOV EDI,@ADDX
	SUB EDI,HDRSIZE
	CMP ES:[EDI][BLOCKHDR.SIGNATURE],MAGIC		;O BLOCO É VÁLIDO
	JNE @@MDALLOC_QUIT
	MOV ES:[EDI][BLOCKHDR.USED],FALSE		;SETA A FLAG
	MOV EDX,ES:[EDI][BLOCKHDR.NEXTBLOCK]		;O PRÓXIMO BLOCO ESTÁ LIVRE?
	CMP EDX,0
	JE @@MDALLOC_QUIT				;NÃO EXISTE PRÓXIMO?
	CMP ES:[EDX][BLOCKHDR.USED],FALSE		;
	JNE @@MDALLOC_QUIT
	
	MOV EAX,ES:[EDX][BLOCKHDR.BLOCKSIZE]		;FUNDE OS 2 BLOCOS
	ADD EAX,HDRSIZE
	ADD ES:[EDI][BLOCKHDR.BLOCKSIZE],EAX
	PUSH ES:[EDX][BLOCKHDR.NEXTBLOCK]
	POP ES:[EDI][BLOCKHDR.NEXTBLOCK]		;ACTUALIZA OS POINTERS

  @@MDALLOC_QUIT:
	RET
ENDP	

;=========================================================================
;=========================================================================
SAFE2PAGING:
;=========================================================================

_SEGMENT32 ENDS


;#############################################################################
_SSEGMENT32 SEGMENT PARA PUBLIC 'SSEGMENT32' USE32
;#############################################################################
;#############################################################################
       DD 0
_SSEGMENT32 ENDS


END START
