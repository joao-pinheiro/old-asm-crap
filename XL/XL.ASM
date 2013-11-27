;i-- XDOS DOS EXTENDER ----------------------- FILE: XL.ASM   -------------∏
;›                                                                         ›
;›  PM MODE SWITCH ROUTINES                                                ›
;›                                                                         ›
;›  Copyright (c) 1997  JoÑo Manuel Pinheiro                               ›
;›  V.N.Gaia - Portugal                                                    ›
;›                                                                         ›
;‘-------------------------------------------------------------------------æ

MODEL LARGE,PASCAL
.386P

INCLUDE DEFS.INC
STACK RMSTACKSIZE

INCLUDE SEGS.INC
INCLUDE STRINGS.INC
INCLUDE VCRT.INC


;============================================================================
_TEXT16
;============================================================================
ORG 0

COPYRIGHT       CHAR 0AH,0DH,'-- XDOS V2.0b --',0AH,0DH
                CHAR '32 bit DOS Extender Coded by JoÑo Pinheiro, (c) 1997 All Rights Reserved',0AH,0DH,0AH,0DH,'$'

CPU_MSG         CHAR 'CPU TYPE : ','$'
CPU_386         CHAR 'i80386 or compatible',0AH,0DH,0AH,0DH,'$'
CPU_486         CHAR 'i80486 or compatible',0AH,0DH,0AH,0DH,'$'
CPU_586         CHAR 'iPENTIUM or compatible',0AH,0DH,0AH,0DH,'$'
CPU_686         CHAR 'iPENTIUM PRO or compatible',0AH,0DH,0AH,0DH,'$'
CPU_TABLE       INTEGER OFFSET CPU_386,0,OFFSET CPU_486,OFFSET CPU_586,OFFSET CPU_686

;==================================================

SYS_MSG1        CHAR 'DPMI Server detected',0AH,0DH,'$'
SYS_MSG2        CHAR 'VCPI Server detected',0AH,0DH,'$'
SYS_MSG3        CHAR 'XMS driver detected',0AH,0DH,'$'
SYS_MSG4        CHAR 'A20 Enabled for accessing HMA',0AH,0DH,'$'

;==================================================

ERRO1           CHAR 'INIT ERROR #01: 32 bit CPU not detected !!!',7,0AH,0DH,'$'
ERRO2           CHAR 'INIT ERROR #02: MS-DOS 3.00 or higher required!',7,0AH,0DH,'$'

;==================================================

DPMI_ERRO1      CHAR 'DPMI INIT ERROR #01: DPMI Server does not support 32-bit clients.',7,0AH,0DH,'$'
DPMI_ERRO2      CHAR 'DPMI INIT ERROR #02: Error during switch call to DPMI Server.',7,0AH,0DH,'$'
DPMI_ERRO3      CHAR 'DPMI INIT ERROR #03: DPMI Server not found.',7,0AH,0DH,'$'
DPMI_ERRO4      CHAR 'DPMI INIT ERROR #04: Fail trying to allocate needed selectors.',7,0AH,0DH,'$'

;==================================================

QUITPROC        INTEGER -1  ;Offset do Proc de sa°da
PRINTPROC       INTEGER -1  ;Offset do proc de print msg

System_Flags    Db 0    ; Bits: 0-XMS/1-VCPI/2-DPMI
Free_Paragraphs DW 0    ; Nß de par†grafos livres
DOS_version     DW 0    ; VersÑo do MS-DOS

PSP_Seg         DW 0    ; Segmento do PSP do programa
OLDCS           DW 0    ; Segmento CS RM
OLDSS           DW 0    ; Segmento SS RM
OLDSP           DW 0    ; Valor de SP RM

SEG16_OFSET     LONG 0  ; Endereáo linear do seg SEGMENT16
SEG32_OFSET     LONG 0  ; Endereáo linear do seg SEGMENT32
STACK32_OFSET   LONG 0  ; Endereáo linear do seg STACK32

SPSP            UINT 0 ;PSP selector
SCODE16         UINT 0 ;SEGMENT16 code selector
SDATA16         UINT 0 ;SEGMENT16 data selector
SSTACK16        UINT 0 ;Initial Stack Selector

;==================================================

DPMI_VERSION    DW -1   ;DPMI server version
DPMI_FLAGS      DW 0    ;DPMI flags
DPMI_PDSIZE     DW 0    ;Private data size in paragr
DPMI_ENTRY      DW 0,0  ;DPMI Entry point
SYS_RMREGS      REGISTERS <>    ;RM call register structure

MASTER_VPIC     CHAR 0  ;Virtual master pic settings
SLAVE_VPIC      CHAR 0  ;Virtual slave pic settings
DPMI_XTENDED    BOOL FALSE              ;DPMI extended capabilities (V1.0+)
DPMI_XSTRUC     DPMI_CAPABILITIES <>    ;DPMI extended capabilities info
SEL_INCVAL      DW 0    ;Selectors increment value

EMMMGR          CHAR 'EMMXXXX0',0
VCPI_VERSION    DW 0    ;VCPI driver version

XMS_DRIVER      DW 0,0  ;XMS driver entry point


;==================================================
ALIGN 8

GDT     DESCRIPTOR <0, 0, 0, 0, 0, 0>                   ;NULL DESCRIPTOR
DESCR1  DESCRIPTOR <0FFFFH,0,0,10011011b,11011111b,0>   ;CODE32 DESCRIPTOR
DESCR2  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;DATA32 DESCRIPTOR
DESCR3  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;STACK32 DESCRIPTOR
DESCR4  DESCRIPTOR <0FFFFH,0,0,10010011b,11011111b,0>   ;DATA0 DESCRIPTOR

;==================================================

CPU_TYPE     CHAR -1
CPUID_ABLE   BOOL FALSE
CPU_FEATURES LONG 0
CPU_MODEL    CHAR -1
CPU_FAMILY   CHAR -1
CPU_STEPPING CHAR -1

;============================================================================
PUBLIC System_Flags, Free_Paragraphs, DOS_version
PUBLIC PSP_Seg, OLDCS, OLDSS, OLDSP
PUBLIC SPSP, SCODE16, SDATA16, SSTACK16
PUBLIC DPMI_VERSION, DPMI_FLAGS, MASTER_VPIC, SLAVE_VPIC
PUBLIC DPMI_XTENDED, DPMI_XSTRUC, SEL_INCVAL
PUBLIC CPU_TYPE, CPUID_ABLE, CPU_FEATURES, CPU_MODEL, CPU_STEPPING, CPU_FAMILY
;============================================================================

;------------------------------------------------
; Char What_CPU()
;------------------------------------------------

WHAT_CPU PROC NEAR

        .286
        USES BX,CX,DX
        XOR DX,DX       ;ASSUME CPU=8086
        PUSHF
        POP AX
        AND AH,0FH
        PUSH AX
        POPF
        PUSHF
        POP AX
        CMP AH,0F0H
        JAE @Q_01       ;ê UM 8086
        INC DX
        OR AH,0F0H
        PUSH AX
        POPF
        PUSHF
        POP AX
        AND AH,0F0H
        JZ @Q_01        ;ê UM 186/286
        INC DX

        .386
        MOV BX,SP
        AND SP,NOT 3
        PUSHFD
        POP EAX
        MOV ECX,EAX
        XOR EAX,040000
        PUSH EAX
        POPFD
        PUSHFD
        POP EAX
        AND SP,NOT 3
        XOR EAX,ECX
        JE @Q_01        ;ê UM 386

        INC DX
        INC DX
        PUSH ECX
        POPFD
        MOV SP,BX
        MOV EAX,ECX
        XOR EAX,0200000H
        PUSH EAX
        POPFD
        PUSHFD
        POP EAX
        XOR EAX,ECX
        JE @Q_01        ;ê um 486

        XOR EAX,EAX
        PUSH EDX
        DB 0FH,0A2H     ;CPUID
        CMP AL,1
        POP EDX
        JE @L_03       ;ERRO NA INSTRUÄéO

  @Q_01: JMP @Q_02
  @L_03:
        SETE CPUID_ABLE
        MOV EAX,1
        DB 0FH,0A2H
        MOV CPU_FEATURES,EDX
        MOV ECX,EAX
        AND AL,0FH
        MOV CPU_STEPPING,AL
        MOV AL,CL
        SHR AL,4
        MOV CPU_MODEL,AL
        MOV AL,CH
        AND AL,0FH
        MOV CPU_FAMILY,AL
        XOR AH,AH
        MOV DX,AX
 @Q_02:
        .286
        MOV AX,DX
        RET
        ENDP


;------------------------------------------------
; VOID DOS_MSG(INT MSG)
;------------------------------------------------

DOS_MSG PROC NEAR
        ARG @MSG:WORD

        USES DS,DX,AX
        MOV AH,09H
        PUSH CS
        POP DS
        MOV DX,@MSG
        INT 21H
        RET
        ENDP


;------------------------------------------------
; VOID Check_SYSTEM()
;------------------------------------------------
CHECK_SYSTEM Proc near

        MOV AX,1687h    ;CHECK DPMI
        INT 2FH
        CMP AX,0
        JNE @L_06

        CALL [PRINTPROC] PASCAL,OFFSET SYS_MSG1
        MOV DPMI_VERSION,DX
        MOV DPMI_FLAGS,BX
        MOV DPMI_PDSIZE,SI
        MOV DPMI_ENTRY,DI
        MOV DPMI_ENTRY+2,ES
        OR SYSTEM_FLAGS,4

 @L_06:
        MOV DX,OFFSET EMMMGR            ;CHECK FOR VCPI
        MOV AX,03D00H                   ;OPEN FILE OR DEVICE
        INT 21H
        JC @L_07
        MOV BX,AX
        MOV AX,04400H                   ;IOCTL INFORMATION
        INT 21H
        JC @L_07
        TEST DX,80H                     ;BIT 7 - 0:FILE/1:DEVICE
        JZ @L_07
        
        MOV AX,0DE00H
        INT 67H
        CMP AH,0
        JNE @L_07
        CALL [PRINTPROC] PASCAL,OFFSET SYS_MSG2
        MOV VCPI_VERSION,BX
        OR SYSTEM_FLAGS,2

 @L_07:
        MOV AX,4300H            ;CHECK FOR XMS
        INT 2FH
        CMP AL,80H
        JNE @Q_03
        OR SYSTEM_FLAGS,1
        CALL [PRINTPROC] PASCAL,OFFSET SYS_MSG3
        MOV AX,4310H
        INT 2FH
        MOV XMS_DRIVER[0],BX
        MOV XMS_DRIVER[2],ES
 @Q_03:
        RET
        ENDP
.386
;------------------------------------------------
; BOOL ENABLE_A20()
;------------------------------------------------

ENABLE_A20 PROC NEAR

        USES FS, GS
        TEST SYSTEM_FLAGS,1     ;DETECTA XMS?
        JZ @L_008

        MOV AH,03H              ;XMS FN - SERVIÄO 03 - LIGAR A20
        CALL DWORD PTR XMS_DRIVER
        CMP AX,TRUE
        JE @Q_04
 @L_008:
        IN AL,092H              ;PS/2 ENABLE
        OR AL,2
        JMP SHORT $+2
        OUT 92H,AL

        MOV AX,0
        MOV FS,AX
        MOV AX,-1
        MOV GS,AX
        MOV BX,FS:[0]
        MOV WORD PTR FS:[0],1234
        CMP WORD PTR GS:[16],1234
        JE @L_08
        MOV WORD PTR FS:[0],4321
        CMP WORD PTR GS:[16],4321
        JE @L_08
        MOV FS:[0],BX
        MOV AX,TRUE
        JMP @Q_04

 @L_08:         
        IN AL,64H               ;ACTIVA ATRAVêS DO HARDWARE
        AND AL,2
        JNZ @L_08
        MOV AL,0D1H
        OUT 64H,AL
 @L_09:
        IN AL,64H
        AND AL,2
        JNZ @L_09
        MOV AL,0DFH
        OUT 60H,AL
 @L_10:
        IN AL,64H
        AND AL,2
        JNZ @L_10

        MOV AX,0
        MOV FS,AX
        MOV AX,-1
        MOV GS,AX
        MOV BX,FS:[0]
 @A20_LOOP:
        MOV WORD PTR FS:[0],1234
        CMP WORD PTR GS:[16],1234
        JE @A20_LOOP
        MOV WORD PTR FS:[0],4321
        CMP WORD PTR GS:[16],4321
        JE @A20_LOOP
        MOV FS:[0],BX
        MOV AX,TRUE
 @Q_04:
        RET
        ENDP
.286
;------------------------------------------------
; VOID QUIT_01(AL:RETCODE)
;------------------------------------------------

QUIT_01 PROC NEAR

        MOV AH,04CH
        INT 21H

       ENDP


;============================================================================

FATAL_ERR MACRO MSG

        CALL [PRINTPROC] PASCAL,OFFSET MSG
        MOV AL,01H
        CALL [QUITPROC]

        ENDM

;============================================================================
;============================================================================
.386P
ALIGN 4

;------------------------------------------------
;BOOL DPMI_START()
;------------------------------------------------

DPMI_START PROC NEAR

        TEST DPMI_FLAGS,1       ;SE NéO EXISTE DPMI32 SERVER
        JNZ @L_11
        FATAL_ERR DPMI_ERRO1
 @L_11:
        MOV BX,DPMI_PDSIZE
        CMP BX,0
        JZ @L_12
        MOV AH,48H
        INT 21H
        MOV ES,AX
        MOV AX,FALSE
        JC @Q_05
 @L_12:
        MOV AX,1
        CALL DWORD PTR DPMI_ENTRY
        CLI
        JC @Q_05
        MOV SPSP,ES
        MOV SCODE16,CS
        MOV SDATA16,DS
        MOV SSTACK16,SS
        PUSH DS
        POP FS
        MOV AX,0400H
        INT 31H                         ;GET DPMI VERSION
        MOV DPMI_FLAGS,BX
        MOV MASTER_VPIC,DH
        MOV SLAVE_VPIC,DL
        CMP AH,0
        JE @L_14
        MOV AX,0401H                    ;GET DPMI CAPABILITIES
        MOV ES,SDATA16
        MOV EDI,OFFSET DPMI_XSTRUC
        INT 31H
        MOV DPMI_XSTRUC.CAPABILITIES,AX
        JC @L_14
        SETNC DPMI_XTENDED
 @L_14:
        MOV AX,TRUE
 @Q_05:
        RET
        ENDP

;------------------------------------------------
; VOID PM_INT21()
;------------------------------------------------

PM_INT21 PROC NEAR

        USES EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
        MOV DWORD PTR SYS_RMREGS._DI,EDI
        MOV DWORD PTR SYS_RMREGS._SI,ESI
        MOV DWORD PTR SYS_RMREGS._BP,0
        MOV DWORD PTR SYS_RMREGS._BX,EBX
        MOV DWORD PTR SYS_RMREGS._DX,EDX
        MOV DWORD PTR SYS_RMREGS._CX,ECX
        MOV DWORD PTR SYS_RMREGS._AX,EAX
        MOV EAX,0300H
        MOV EBX,21H
        PUSH 0
        POP SYS_RMREGS._SS
        MOV WORD PTR SYS_RMREGS._SP,0
        PUSH 0202H
        POP SYS_RMREGS.FLAGS
        MOV ECX,0
        PUSH DS
        POP ES
        MOV EDI,OFFSET SYS_RMREGS
        INT 31H
        JNC @L_13
        STC
        JMP @Q_06

 @L_13:
        MOV AX,SYS_RMREGS.FLAGS
        SAHF

 @Q_06:
        RET
        ENDP

;------------------------------------------------
; VOID DPMI_MSG(INT MSG)
;------------------------------------------------

DPMI_MSG PROC NEAR
        ARG @MSG:WORD

        USES EAX
        MOV AX,OLDCS
        MOV SYS_RMREGS._DS,AX
        MOVZX EDX,@MSG
        MOV AH,09H
        CALL PM_INT21

        RET
        ENDP


;------------------------------------------------
;VOID SETUP_SELECTORS()
;------------------------------------------------

SETUP_SELECTORS PROC NEAR

        MOV EDX,SEG32_OFSET             ;SET UP DESCRIPTORS
        MOV DESCR1.BASE_15,DX
        MOV DESCR2.BASE_15,DX
        SHR EDX,16
        MOV DESCR1.BASE_23,DL   ;CODE
        MOV DESCR2.BASE_23,DL   ;DATA

        MOV EDX,STACK32_OFSET           ;SET UP STACK32
        MOV DESCR3.BASE_15,DX
        SHR EDX,16
        MOV DESCR3.BASE_23,DL

        MOV BX,CS
        LAR CX,BX                       ;CURR CPL
        JNZ @Q_08

        AND CH,01100000b
        OR DESCR1.ACESSR,CH
        OR DESCR2.ACESSR,CH
        OR DESCR3.ACESSR,CH
        OR DESCR4.ACESSR,CH

        MOV EAX,3               ;SERVIÄO DPMI 3 - GET INCVAL
        INT 31H
        MOV SEL_INCVAL,AX
        JC @Q_07
        
        MOV DX,AX
        MOV AX,0                ;SERVIÄO DPMI 0 - ALLOC DESCRIPTORS
        MOV CX,4                ;CODE32,DATA32,STACK32,DATA0
        INT 31H
        JC @Q_07
        
        ASSUME FS:SEGMENT32

        MOV FS,AX               ;FS -> FUTURO DATA32 
        MOV ES,SDATA16
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
        MOV SDATA0,BX           ;SETA O DESCRIPTOR DATA0
        MOV EDI,OFFSET DESCR4
        INT 31h
        JNC @Q_08

 @Q_07: 
        FATAL_ERR DPMI_ERRO4

 @Q_08:
        RET
        ENDP


;============================================================================
;============================================================================

Start:
        .286
        MOV AX,CS
        MOV DS,AX
        MOV OLDCS,AX
        PUSH ES
        POP PSP_SEG
        MOV OLDSS,SS
        MOV OLDSP,SP

        MOV BX,PROGSIZE                         ;Resize program psp
        SHR BX,4
        MOV AH,4AH
        INT 21H

        MOV AX,OFFSET QUIT_01                   ;SETA O QUITPROC
        MOV QUITPROC,AX
        MOV AX,OFFSET DOS_MSG
        MOV PRINTPROC,AX

        Call [PRINTPROC] Pascal,offset CopyRight        ;Exibe a msg de copyright

        CALL WHAT_CPU                           ;ObtÇm o CPU
        MOV CPU_TYPE,AL
        CMP AL,i386
        JAE @L_04
        MOV AX,OFFSET ERRO1
        FATAL_ERR ERRO1                         ;Erro se nÑo for >=386

 @L_04:
        CALL [PRINTPROC] PASCAL,OFFSET CPU_MSG  ;Exibe info do CPU
        CMP AL,6
        JBE @L_04_1
        MOV AL,6

 @L_04_1:
        SUB AL,i386
        XOR AH,AH
        ADD AX,AX
        MOV BX,OFFSET CPU_TABLE
        ADD BX,AX
        CALL [PRINTPROC] PASCAL,DS:[BX]

        MOV AH,30H                      ;Get DOS version
        INT 21H
        XCHG AL,AH
        MOV DOS_VERSION,AX
        CMP AH,3
        JAE @L_05
        FATAL_ERR ERRO2

@L_05:
        MOV AH,48H                      ;GET FREE MEM INFO
        MOV BX,-1                       ;SHURE TO FAIL!!
        INT 21H
        CLC                             ;I HATE CARRYING...
        MOV FREE_PARAGRAPHS,BX          ;SAVE INFO

        CALL CHECK_SYSTEM
        CALL ENABLE_A20                 ;ACTIVANDO LINHA A20
        CALL [PRINTPROC] PASCAL,OFFSET SYS_MSG4

        .386P
        ALIGN 4

        XOR EAX,EAX             ;Preenche os campos de linear adDX
        MOV AX,CS
        SHL EAX,4
        MOV SEG16_OFSET,EAX
        MOV EAX,SEGMENT32
        SHL EAX,4
        MOV SEG32_OFSET,EAX
        MOV EAX,STACK32
        SHL EAX,4
        MOV STACK32_OFSET,EAX

        TEST SYSTEM_FLAGS,4
        JNZ @T_12
        FATAL_ERR DPMI_ERRO3

 @T_12:
        CALL DPMI_START         ;ENTRA EM PM, SISTEMA DPMI
        CMP AX,TRUE
        JE @T_13
        FATAL_ERR DPMI_ERRO2

 @T_13:
        CALL SETUP_SELECTORS    ;FS->SDATA32
        MOV GS,SDATA0

        MOV OLDSP,SP            ;SALVA O SP
        MOV ESP,STACKSIZE
        XOR EBP,EBP
        MOV SS,SSTACK32         ;STACK SWITCH

        MOV AX,FS
        MOV FS,SDATA16          ;FS ->DATA16
        MOV DS,AX               ;DS ->DATA32

        ASSUME SS:STACK32, DS:SEGMENT32 ;32-BIT PUSH'S

        PUSH SCODE32    
        PUSH OFFSET CODE32_STARTUP
        DB 66H                  ;32-BIT RETF
        RETF


MSG DB 'fuck off asshole!!',0AH,0DH,'$'

RTEST PROC FAR

        PUSH CS
        POP DS
        MOV DX,OFFSET MSG
        MOV AH,09H
        INT 21H
        
        RET
        ENDP            

_TEXT16_END

;#########################################################################
;#########################################################################
_TEXT32
;#########################################################################
;#########################################################################
ORG 0

;=========================================================================

SDATA32         UINT 0 ;SEGMENT32 data selector
SCODE32         UINT 0 ;SEGMENT32 code selector
SSTACK32        UINT 0 ;Stack32 Selector
SDATA0          UINT 0 ;FLAT segment

SEG16           LONG 0 ;LINEAR SEG16 ADDX

;=========================================================================

SYS_ERRORHANDLER LONG ?                 ;SYSTEM ERROR HANDLER

ERRORFLAG       BOOL 0                  ;SYSTEM ERROR FLAG
DPMI1_ERROR     UINT 0                  ;DPMI 1.0+ ERROR CODE
DPMI_ERROR      UINT 0                  ;DPMI 0.9 FN CODE

EXITPROC        LONG ?                  ;EXIT PROC

DIVERR          LONG ?                  ;OFFSET DO PROC DE DIVIDE ERR
OLD_INT00       POINTER <>              ;OLD INT 0 VECTOR

;=========================================================================

REGSTRUC        REGISTERS <>            ;ESTRUTURA INTERNA DOS REGISTOS
R_RES           UINT 0                  ;REGISTO ES
R_RDS           UINT 0                  ;REGISTO DS
R_RCS           UINT 0                  ;REGISTO CS
R_IP            UINT 0                  ;REGISTO IP


;=========================================================================

PUBLIC SDATA32, SCODE32, SSTACK32, SDATA0, SEG16
PUBLIC SYS_ERRORHANDLER, ERRORFLAG, DPMI1_ERROR, DPMI_ERROR
PUBLIC REGSTRUC, R_RES, R_RDS, R_RCS, R_IP

;=========================================================================
LOCALS @@

SND MACRO
        MOV AL,33H
        OUT 61H,AL
        MOV ECX,100000H
W_LOOP:
        LOOPD W_LOOP
        MOV AL,032H
        OUT 61H,AL
        MOV ECX,100000H
W_LOOP1:
        LOOP W_LOOP1    
        ENDM
        
;--------------------------------------------------
; VOID _EH_DPMI_01(INT SN, INT AX)
; DEFAULT ERROR HANDLER WHEN ENTERING SEGMENT32
;--------------------------------------------------
PUBLIC _EH_DPMI_01

_EH_DPMI_01 PROC NEAR
        ARG @SN:DWORD, @RAX:DWORD

        USES EAX,DS
        MOV DS,CS:SDATA32
        MOV EAX,@SN     
        MOV DPMI_ERROR,AX
        MOV AX,TRUE
        MOV ERRORFLAG,AL
        CMP BYTE PTR DPMI_VERSION[0],0
        JE @L_18
        MOV EAX,@RAX
        MOV DPMI1_ERROR,AX
 @L_18:
        RET
        ENDP
        

;==========================================================================

;--------------------------------------------------
; POINTER GET_RMINTVEC(LONG I)
; RETURNS RM VECTOR OF INT I
;--------------------------------------------------
PUBLIC GET_RMINTVEC

GET_RMINTVEC PROC NEAR
        ARG @I:DWORD
        
        USES EAX,EBX
        MOV EAX,0200H
        MOV EBX,@I
        INT 31H
        SETC ERRORFLAG
        MOVZX ECX,CX            ;RM SEGMENT (16 BIT)
        MOVZX EDX,DX            ;RM OFSET (16 BIT)
        JNC @L_0200
        CALL [SYS_ERRORHANDLER] PASCAL,0200h,EAX        
        XOR ECX,ECX
        MOV EDX,ECX

 @L_0200:
        RET
        ENDP


;--------------------------------------------------
; VOID SET_RMINTVEC(LONG I,POINTER VEC)
; SETS RM VECTOR OF INT I TO VEC
;--------------------------------------------------
PUBLIC SET_RMINTVEC

SET_RMINTVEC PROC NEAR
        ARG @I:DWORD, @P:POINTER
        
        USES EAX,EBX,ECX,EDX
        MOV EAX,0201H
        MOV EBX,@I
        MOV ECX,DWORD PTR @P.SELECTOR           ;NOT SELECTOR, BUT RM SEG!!
        MOV EDX,DWORD PTR @P.OFSET
        INT 31H
        SETC ERRORFLAG
        JNC @L_0201
        CALL [SYS_ERRORHANDLER] PASCAL,0201h,EAX        

 @L_0201:
        RET
        ENDP


;--------------------------------------------------
; POINTER GET_EXCEPTION(LONG I)
; RETURNS VECTOR OF EXCEPTION I
;--------------------------------------------------
PUBLIC GET_EXCEPTION

GET_EXCEPTION PROC NEAR
        ARG @I:DWORD
        
        USES EAX,EBX
        MOV EAX,0202H
        MOV EBX,@I
        INT 31H
        SETC ERRORFLAG
        MOVZX ECX,CX            ;SELECTOR EM ECX, OFFSET EM EDX
        JNC @L_0202
        CALL [SYS_ERRORHANDLER] PASCAL,0202h,EAX        
        XOR ECX,ECX
        MOV EDX,ECX

 @L_0202:
        RET
        ENDP


;--------------------------------------------------
; VOID SET_EXCEPTION(LONG I,POINTER VEC)
; SETS VECTOR OF EXCEPTION I TO VEC
;--------------------------------------------------
PUBLIC SET_EXCEPTION

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
        CALL [SYS_ERRORHANDLER] PASCAL,0203h,EAX        

 @L_0203:
        RET
        ENDP

;--------------------------------------------------
; POINTER GET_INTVEC(LONG I)
; RETURNS PM VECTOR OF INT I
;--------------------------------------------------
PUBLIC GET_INTVEC

GET_INTVEC PROC NEAR
        ARG @I:DWORD
        
        USES EAX,EBX
        MOV EAX,0204H
        MOV EBX,@I
        INT 31H
        SETC ERRORFLAG
        MOVZX ECX,CX            ;SELECTOR
        JNC @L_0204
        CALL [SYS_ERRORHANDLER] PASCAL,0204h,EAX        
        XOR ECX,ECX
        MOV EDX,ECX

 @L_0204:
        RET
        ENDP


;--------------------------------------------------
; VOID SET_INTVEC(LONG I,POINTER VEC)
; SETS PM VECTOR OF INT I TO VEC
;--------------------------------------------------
PUBLIC SET_INTVEC

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
        CALL [SYS_ERRORHANDLER] PASCAL,0205h,EAX        

 @L_0205:
        RET
        ENDP

;==========================================================================

;--------------------------------------------------
; VOID RM_INT(LONG I)
; EXECUTA A INT I EM MODO REAL
;--------------------------------------------------
PUBLIC RM_INT

RM_INT PROC NEAR
        ARG @I:DWORD

        USES ES,EDI
        MOV DWORD PTR REGSTRUC._AX,EAX
        MOV DWORD PTR REGSTRUC._BX,EBX
        MOV DWORD PTR REGSTRUC._CX,ECX
        MOV DWORD PTR REGSTRUC._DX,EDX
        MOV DWORD PTR REGSTRUC._SI,ESI
        MOV DWORD PTR REGSTRUC._DI,EDI
        MOV AX,R_RES
        MOV BX,R_RDS
        MOV CX,R_RCS
        MOV DX,R_IP
        MOV REGSTRUC._ES,AX
        MOV REGSTRUC._DS,BX
        MOV REGSTRUC._CS,CX
        MOV REGSTRUC._IP,DX
        XOR EAX,EAX
        MOV DWORD PTR REGSTRUC._BP,EAX  
        MOV REGSTRUC._FS,AX
        MOV REGSTRUC._GS,AX     
        MOV AX,0202H
        MOV REGSTRUC.FLAGS,AX           
        MOV AX,OLDSS    
        MOV REGSTRUC._SS,AX
        MOV AX,OLDSP
        MOV REGSTRUC._SP,AX
        MOV EBX,@I
        XOR ECX,ECX             
        MOV ES,CS:SDATA32
        MOV EDI,OFFSET REGSTRUC
        MOV EAX,0300H
        INT 31H
        SETC ERRORFLAG
        JNC @L_19
        CALL [SYS_ERRORHANDLER] PASCAL,0300h,EAX        
        JMP @Q_0300

 @L_19:
        MOV EAX,DWORD PTR REGSTRUC._AX
        MOV EBX,DWORD PTR REGSTRUC._BX
        MOV ECX,DWORD PTR REGSTRUC._CX
        MOV EDX,DWORD PTR REGSTRUC._DX
        MOV AX,REGSTRUC.FLAGS
        XCHG AH,AL
        SAHF

 @Q_0300:
        RET
        ENDP


;--------------------------------------------------
; VOID CALL_RMPROC(ADDX:POINTER)
; EXECUTA UM PROC FAR EM MODO REAL
; VALORES DOS REGISTOS NA ESTRUTURA REGSTRUC
;--------------------------------------------------
PUBLIC CALL_RMPROC

CALL_RMPROC PROC NEAR
        ARG @ADDX:POINTER

        USES EAX,EBX,ECX,EDX,EDI,ES
        MOV EAX,@ADDX.SELECTOR
        MOV EBX,@ADDX.OFSET
        MOV REGSTRUC._CS,AX             ;SETA O ENDEREÄO NA ESTRUTURA
        MOV REGSTRUC._IP,BX
        MOV AX,OLDSS    
        MOV REGSTRUC._SS,AX
        MOV AX,OLDSP
        MOV REGSTRUC._SP,AX
        MOV EAX,0202
        MOV REGSTRUC.FLAGS,AX
        MOV EAX,0301H
        XOR EBX,EBX
        XOR ECX,ECX
        MOV ES,CS:SDATA32
        MOV EDI,OFFSET REGSTRUC
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0301
        CALL [SYS_ERRORHANDLER] PASCAL,0301h,EAX        

 @Q_0301:
        RET
        ENDP
        

;------------------------------------------ ROTINA DO SISTEMA --------
; VOID CALL_RMINT(ADDX:POINTER)
; CHAMA UM PROC REAL QUE TERMINA COM IRET
; VALORES DOS REGISTOS NA ESTRUTURA REGSTRUC
;---------------------------------------------------------------------
PUBLIC CALL_RMINT

CALL_RMINT PROC NEAR
        ARG @ADDX:POINTER

        USES EAX,EBX,ECX,EDI,ES
        MOV EAX,@ADDX.SELECTOR          ;SEGMENTO
        MOV EBX,@ADDX.OFSET             ;OFSET
        MOV REGSTRUC._CS,AX             
        MOV REGSTRUC._IP,BX
        MOV EAX,0202H                   ;SETA DEFAULT FLAGS
        MOV REGSTRUC.FLAGS,AX           
        MOV EAX,0302H
        XOR EBX,EBX
        XOR ECX,ECX
        MOV ES,CS:SDATA32                       ;ENDEREÄO DA ESTRUTURA REGSTRUC
        MOV EDI,OFFSET REGSTRUC
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0302
        CALL [SYS_ERRORHANDLER] PASCAL,0302h,EAX        

 @Q_0302:
        RET
        ENDP    

;==========================================================================

;--------------------------------------------------
; POINTER ALLOC_CALLBACK(PROC2CALL:POINTER, REGSTRUC:POINTER)
; ALOCA UMA CALLBACK E RETORNA O ENDEREÄO DE MODO REAL A SER CHAMADO
; VALORES DOS REGISTOS NA ESTRUTURA REGSTRUC
;--------------------------------------------------
PUBLIC ALLOC_CALLBACK

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
        MOV DS,CS:SDATA32       ;RESTAURA DS PARA SETAR FLAG DE ERRO
        SETC ERRORFLAG
        JNC @Q_0303
        CALL [SYS_ERRORHANDLER] PASCAL,0303h,EAX        
        XOR ECX,ECX
        MOV EDX,ECX

 @Q_0303:
        RET
        ENDP    
        
;--------------------------------------------------
; VOID FREE_CALLBACK(RMADDX:POINTER)
; LIBERTA UMA CALLBACK ALOCADA
; 
;--------------------------------------------------
PUBLIC FREE_CALLBACK

FREE_CALLBACK PROC NEAR
        ARG @RMADDX:POINTER
        
        USES EAX,ECX,EDX
        MOV AX,0304H
        MOV ECX,@RMADDX.SELECTOR
        MOV EDX,@RMADDX.OFSET
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0304
        CALL [SYS_ERRORHANDLER] PASCAL,0304h,EAX        

 @Q_0304:
        RET
        ENDP    
        
;==========================================================================     

;----------------------------------------------------
; VOID DPMI_MEMINFO(POINTER(DPMI_MEM) BLOCK)
; PREENCHE A ESTRUTURA BLOCK COM A INFO DE MEMORIA
;----------------------------------------------------
PUBLIC DPMI_MEM_INFO

DPMI_MEM_INFO PROC NEAR
        ARG @BLOCK:POINTER
        
        USES EAX,EDI,ES
        MOV EDI,@BLOCK.OFSET
        MOV ES,WORD PTR @BLOCK.SELECTOR[0]
        MOV EAX,0500H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0500
        CALL [SYS_ERRORHANDLER] PASCAL,0500h,EAX        

 @Q_0500:
        RET
        ENDP    
        

;--------------------------------------------------
; LONG GETMEM(LONG SIZE)
; ALOCA UM BLOCO DE MEMORIA
; O HANDLE ê COLOCADO NOS BYTES 4 A 8 DO BLOCO
;--------------------------------------------------
PUBLIC GETMEM

GETMEM PROC NEAR
        ARG @SIZE:DWORD

        USES EAX,EBX,ECX,ESI,EDI,ES
        MOV EBX,@SIZE
        CMP EBX,32              ;TAMANHO MãNIMO DE 32 BYTES
        JGE @L_0501
        MOV EBX,32
                
 @L_0501:
        ADD EBX,8               ;ADICIONA  ESPAÄO PARA O HANDLE
        MOVZX ECX,BX            ;CX - LSW
        SHR EBX,16              ;BX - MSW
        MOV EAX,0501H
        INT 31H
        SETC ERRORFLAG
        JC @QE_0501
        MOV ES,CS:SDATA0                ;ES - DATA0 PQ O ENDEREÄO ê LINEAR
        SHL EBX,16                      ;HANDLE EM EBX
        MOV BX,CX
        SHL ESI,16                      ;ENDEREÄO EM ESI
        MOV SI,DI
        XCHG EBX,ESI
        MOV EAX,0F010E0AH
        MOV ES:[ESI],EAX                ;GUARDA ASSINATURA
        MOV ES:[ESI+4],EBX              ;GUARDA HANDLE
        ADD ESI,8                       ;SALTA O CABEÄALHO
        MOV EDX,ESI
        JMP @Q_0501

 @QE_0501:
        CALL [SYS_ERRORHANDLER] PASCAL,0501h,EAX         
        XOR EDX,EDX     
 @Q_0501:
        RET
        ENDP
        

;--------------------------------------------------
; VOID FREEMEM(LONG ADDX)
; LIBERTA UM BLOCO DE MEMORIA
; O HANDLE ê COLOCADO NOS 1ßS 4 BYTES DO BLOCO
;--------------------------------------------------
PUBLIC FREEMEM

FREEMEM PROC NEAR
        ARG @ADDX:DWORD
        
        USES EAX,EBX,ESI,EDI,ES
        MOV ES,CS:SDATA0
        MOV EDI,@ADDX
        SUB EDI,8
        CMP ES:[EDI],0F010E0AH          ;VERIFICA ASSINATURA
        SETNE ERRORFLAG
        JNE @QE_0502

        MOV ESI,ES:[EDI+4]              ;Lâ O HANDLE
        MOV DI,SI
        SHR ESI,16                      ;EM SI:DI
        MOV AX,0502H
        INT 31H 
        SETC ERRORFLAG
        JNC @Q_0502

 @QE_0502:
        CALL [SYS_ERRORHANDLER] PASCAL,0502h,EAX        

 @Q_0502:
        RET
        ENDP    

;--------------------------------------------------
; LONG RESIZEMEM(LONG ADDX, LONG SIZE)
; REDIMENSIONA UM BLOCO DE MEMORIA
; O HANDLE ê COLOCADO NOS 1ßS 4 BYTES DO BLOCO
;--------------------------------------------------
PUBLIC RESIZEMEM

RESIZEMEM PROC NEAR
        ARG @ADDX:DWORD,@SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI,ES
        MOV EBX,@SIZE
        CMP EBX,32              ;TAMANHO MãNIMO DE 32 BYTES
        JGE @L_0503
        MOV EBX,32
                
 @L_0503:
        ADD EBX,8               ;ADICIONA  ESPAÄO PARA O HANDLE
        MOVZX ECX,BX            ;CX - LSW
        SHR EBX,16              ;BX - MSW

        MOV ES,CS:SDATA0        ;LE O HANDLE DO BLOCO
        MOV EDI,@ADDX
        SUB EDI,8
        CMP ES:[EDI],0F010E0AH  ;VERIFICA ASSINATURA
        SETNE ERRORFLAG
        JNE @QE_0503

        MOV ESI,ES:[EDI+4]              ;Lâ O HANDLE
        MOVZX EDI,SI
        SHR ESI,16                      ;EM SI:DI
        MOV AX,0503H
        INT 31H 
        SETC ERRORFLAG                  ;ASSINALA ERRO
        JC @QE_0503

        SHL EBX,16                      ;HANDLE EM EBX
        MOV BX,CX
        SHL ESI,16                      ;ENDEREÄO EM ESI
        MOV SI,DI
        XCHG EBX,ESI
        MOV ES,CS:SDATA0
        MOV EAX,0F010E0AH       
        MOV ES:[ESI],EAX
        MOV ES:[ESI+4],EBX
        ADD ESI,8
        MOV EDX,ESI                     ;RETORNA ENDEREÄO EM EDX
        JMP @Q_0503

 @QE_0503:
        CALL [SYS_ERRORHANDLER] PASCAL,0503h,EAX         
        XOR EDX,EDX     

 @Q_0503:
        RET
        ENDP
        
;==========================================================================     

;--------------------------------------------------
; VOID LOCK_REGION(LONG LINADDX, LONG SIZE)
; NéO PERMITE PAGING NA REGIéO ESPECIFICADA
;
;--------------------------------------------------
PUBLIC LOCK_REGION

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
        CALL [SYS_ERRORHANDLER] PASCAL,0600h,EAX         

 @Q_0600:
        RET
        ENDP
        
;--------------------------------------------------
; VOID UNLOCK_REGION(LONG LINADDX, LONG SIZE)
;
;
;--------------------------------------------------
PUBLIC UNLOCK_REGION

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
        CALL [SYS_ERRORHANDLER] PASCAL,0601h,EAX         

 @Q_0601:
        RET
        ENDP

;--------------------------------------------------
; VOID MARK_RM_PAGEABLE(LONG LINADDX, LONG SIZE)
; MARCA UMA ZONA REAL COMO PAGINÜVEL
;
;--------------------------------------------------
PUBLIC MARK_RM_PAGEABLE

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
        CALL [SYS_ERRORHANDLER] PASCAL,0602h,EAX         

 @Q_0602:
        RET
        ENDP
        
;--------------------------------------------------
; VOID RELOCK_REGION(LONG LINADDX, LONG SIZE)
; NéO PERMITE PAGING NA REGIéO ESPECIFICADA
;
;--------------------------------------------------
PUBLIC RELOCK_REGION

RELOCK_REGION PROC NEAR
        ARG @LINADDX:DWORD, @SIZE:DWORD
        
        USES EAX,EBX,ECX,ESI,EDI
        MOV EBX,@LINADDX
        MOV CX,BX
        SHR EBX,16
        MOV ESI,@SIZE
        MOV DI,SI
        SHR ESI,16
        MOV AX,0603H
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0603
        CALL [SYS_ERRORHANDLER] PASCAL,0603h,EAX         

 @Q_0603:
        RET
        ENDP
        

;--------------------------------------------------
; LONG GET_PAGESIZE()
;
;--------------------------------------------------
PUBLIC GET_PAGESIZE

GET_PAGESIZE PROC NEAR
        
        USES EAX,EBX,ECX
        MOV EAX,0604H
        INT 31H
        SETC ERRORFLAG
        JC @QE_0604
        SHL EBX,16
        MOV BX,CX
        MOV EDX,EBX     
 @QE_0604:
        CALL [SYS_ERRORHANDLER] PASCAL,0604h,EAX         
        XOR EDX,EDX
        
 @Q_0604:
        RET
        ENDP


;--------------------------------------------------
; LONG MAP_PHISICAL_ADDX(LONG PHIS_ADDX, LONG SIZE)
;
;--------------------------------------------------
PUBLIC MAP_PHISICAL_ADDX

MAP_PHISICAL_ADDX PROC NEAR
        ARG @PA:DWORD, @SIZE:DWORD

        USES EAX,EBX,ECX,ESI,EDI
        MOV EBX,@PA
        MOV CX,BX
        SHR EBX,16
        MOV ESI,@SIZE
        MOV DI,SI
        SHR ESI,16
        MOV AX,0800H
        INT 31H
        SETC ERRORFLAG
        JC @QE_0800
        SHL EBX,16
        MOV BX,CX
        MOV EDX,EBX
        JMP @Q_0800
        
 @QE_0800:
        CALL [SYS_ERRORHANDLER] PASCAL,0800h,EAX         
        XOR EDX,EDX

 @Q_0800:
        RET
        ENDP
        
;===========================================================================

;--------------------------------------------------
; INT ALLOC_DESCRIPTORS(LONG N_DESCR)
; ALOCA N_DESCR DESCRIPTORS, DEVOLVENDO O PRIMEIRO
;--------------------------------------------------
PUBLIC ALLOC_DESCRIPTORS

ALLOC_DESCRIPTORS PROC NEAR
        ARG @ND:DWORD

        USES ECX
        XOR EAX,EAX
        MOV ECX,@ND
        INT 31H
        SETC ERRORFLAG
        MOVZX EAX,AX
        JNC @Q_0000     
        CALL [SYS_ERRORHANDLER] PASCAL,0000,EAX
        XOR EAX,EAX

 @Q_0000:
        RET
        ENDP
        

;--------------------------------------------
; VOID FREE_DESCRIPTOR(LONG DESCR)
;--------------------------------------------
PUBLIC FREE_DESCRIPTOR

FREE_DESCRIPTOR PROC NEAR
        ARG @DESCR:DWORD

        USES EAX,EBX    
        MOV EAX,001H
        MOV EBX,@DESCR
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0001
        CALL [SYS_ERRORHANDLER] PASCAL,0001h,EAX         

 @Q_0001:
        RET
        ENDP
        
        
;--------------------------------------------
; LONG SEG2DESCR(LONG SEGM)
;--------------------------------------------
PUBLIC SEG2DESCR

SEG2DESCR PROC NEAR
        ARG @SEGM:DWORD

        USES EAX,EBX    
        MOV EAX,002H
        MOV EBX,@SEGM
        INT 31H
        MOVZX EDX,AX
        SETC ERRORFLAG
        JNC @Q_0002
        CALL [SYS_ERRORHANDLER] PASCAL,0002h,EAX         
        XOR EDX,EDX
        
 @Q_0002:
        RET
        ENDP
        


;--------------------------------------------
; LONG GET_BASE(LONG SELECTOR)
;--------------------------------------------
PUBLIC GET_BASE

GET_BASE PROC NEAR
        ARG @SELECTOR:DWORD

        USES EAX,EBX,ECX        
        MOV EAX,006H
        MOV EBX,@SELECTOR
        INT 31H
        SETC ERRORFLAG
        JC @QE_0006
        SHL ECX,16
        MOV CX,DX
        MOV EDX,ECX
        JMP @Q_0006

 @QE_0006:
        CALL [SYS_ERRORHANDLER] PASCAL,0006h,EAX         
        XOR ECX,ECX
        
 @Q_0006:
        RET
        ENDP
        

;--------------------------------------------
; VOID SET_BASE(LONG SELECTOR, LONG BASE)
;--------------------------------------------
PUBLIC SET_BASE

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
        CALL [SYS_ERRORHANDLER] PASCAL,0007h,EAX         

 @Q_0007:
        RET
        ENDP
        

;--------------------------------------------
; VOID SET_LIMIT(LONG SELECTOR, LONG LIMIT)
;--------------------------------------------
PUBLIC SET_LIMIT

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
        CALL [SYS_ERRORHANDLER] PASCAL,0008h,EAX         

 @Q_0008:
        RET
        ENDP


;--------------------------------------------
; VOID SET_AR(LONG SELECTOR, LONG AR)
;--------------------------------------------
PUBLIC SET_AR

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
        CALL [SYS_ERRORHANDLER] PASCAL,0009h,EAX         

 @Q_0009:
        RET
        ENDP


;--------------------------------------------
; LONG MAKE_ALIAS(LONG SELECTOR)
;--------------------------------------------
PUBLIC MAKE_ALIAS

MAKE_ALIAS PROC NEAR
        ARG @SELECTOR:DWORD

        USES EAX,EBX
        MOV EAX,00AH
        MOV EBX,@SELECTOR
        INT 31H
        MOVZX EDX,AX    
        SETC ERRORFLAG
        JNC @Q_000A
        CALL [SYS_ERRORHANDLER] PASCAL,000Ah,EAX         
        XOR EDX,EDX
        
 @Q_000A:
        RET
        ENDP

;--------------------------------------------
; VOID GET_DESCR(LONG SELECTOR, POINTER BUF)
;--------------------------------------------
PUBLIC GET_DESCR

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
        CALL [SYS_ERRORHANDLER] PASCAL,000BH,EAX         

 @Q_000B:
        RET
        ENDP

;--------------------------------------------
; VOID SET_DESCR(LONG SELECTOR, POINTER BUF)
;--------------------------------------------
PUBLIC SET_DESCR

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
        CALL [SYS_ERRORHANDLER] PASCAL,000CH,EAX         

 @Q_000C:
        RET
        ENDP


;--------------------------------------------
; (LONG SELECTOR, LONG(EAX) SEGMENT) GETDOSMEM(LONG SIZE)
;--------------------------------------------
PUBLIC GETDOSMEM

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
        CALL [SYS_ERRORHANDLER] PASCAL,0100H,EAX         
        XOR EAX,EAX
        MOV EDX,EAX
        
 @Q_0100:
        RET
        ENDP

;--------------------------------------------
; VOID FREEDOSMEM(LONG SELECTOR)
;--------------------------------------------
PUBLIC FREEDOSMEM

FREEDOSMEM PROC NEAR
        ARG @SELECTOR:DWORD
        
        USES EAX,EDX
        MOV EAX,0101H
        MOV EDX,@SELECTOR
        INT 31H
        SETC ERRORFLAG
        JNC @Q_0101
        CALL [SYS_ERRORHANDLER] PASCAL,0101H,EAX         
        
 @Q_0101:
        RET
        ENDP

;--------------------------------------------
; VOID RESIZEDOSMEM(LONG SELECTOR, LONG SIZE)
;--------------------------------------------
PUBLIC RESIZEDOSMEM

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
        CALL [SYS_ERRORHANDLER] PASCAL,0102H,EAX         
        
 @Q_0102:
        RET
        ENDP

;===========================================================================

;--------------------------------------------
; LONG SET_WATCHPOINT(LONG LINADDX, LONG INFO)
;
; INFO AS: LSW-> LSB - SIZE (1, 2 OU 4 BYTES)
;                MSB - TYPE (00 -EXEC, 01 - WRITE, 02 - READ/WRITE)
;          MSW-> XXXX
;--------------------------------------------
PUBLIC SET_WATCHPOINT

SET_WATCHPOINT PROC NEAR
        ARG @LINADDX:DWORD, @INFO:DWORD
        
        USES EAX,EBX,ECX
        MOV EAX,0B00H
        MOV EBX,@LINADDX
        MOVZX ECX,BX
        SHR EBX,16
        MOV EDX,@INFO
        INT 31H
        MOVZX EDX,BX
        SETC ERRORFLAG  
        JNC @Q_0B00
        CALL [SYS_ERRORHANDLER] PASCAL,0B00H,EAX         
        XOR EDX,EDX
        
 @Q_0B00:
        RET
        ENDP

;--------------------------------------------
; VOID CLEAR_WATCHPOINT(LONG HANDLE)
;--------------------------------------------
PUBLIC CLEAR_WATCHPOINT

CLEAR_WATCHPOINT PROC NEAR
        ARG @HANDLE:DWORD
        
        USES EAX,EBX
        MOV EAX,0B01H
        MOV EBX,@HANDLE
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0B01
        CALL [SYS_ERRORHANDLER] PASCAL,0B01H,EAX         
        
 @Q_0B01:
        RET
        ENDP


;--------------------------------------------
; LONG GET_WATCHPOINT_STATE(LONG HANDLE)
;--------------------------------------------
PUBLIC GET_WATCHPOINT_STATE

GET_WATCHPOINT_STATE PROC NEAR
        ARG @HANDLE:DWORD
        
        USES EAX,EBX
        MOV EAX,0B02H
        MOV EBX,@HANDLE
        INT 31H
        MOVZX EDX,AX
        SETC ERRORFLAG  
        JNC @Q_0B02
        CALL [SYS_ERRORHANDLER] PASCAL,0B02H,EAX         
        
 @Q_0B02:
        RET
        ENDP

;--------------------------------------------
; VOID RESET_WATCHPOINT(LONG HANDLE)
;--------------------------------------------
PUBLIC RESET_WATCHPOINT

RESET_WATCHPOINT PROC NEAR
        ARG @HANDLE:DWORD
        
        USES EAX,EBX
        MOV EAX,0B03H
        MOV EBX,@HANDLE
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0B03
        CALL [SYS_ERRORHANDLER] PASCAL,0B03H,EAX         
        
 @Q_0B03:
        RET
        ENDP

;===========================================================================

;-------------------------------------------------
; VOID VCLI()
;-------------------------------------------------
PUBLIC VCLI

VCLI PROC NEAR
        
        USES EAX
        MOV EAX,0900H
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0900
        CALL [SYS_ERRORHANDLER] PASCAL,0900H,EAX         
        
 @Q_0900:
        RET
        ENDP

;-------------------------------------------------
; VOID VSTI()
;-------------------------------------------------
PUBLIC VSTI

VSTI PROC NEAR
        
        USES EAX
        MOV EAX,0901H
        INT 31H
        SETC ERRORFLAG  
        JNC @Q_0901
        CALL [SYS_ERRORHANDLER] PASCAL,0901H,EAX         
        
 @Q_0901:
        RET
        ENDP

;-------------------------------------------------
; LONG GET_VI()
;-------------------------------------------------
PUBLIC GET_VI

GET_VI PROC NEAR
        
        USES EAX
        MOV EAX,0902H
        INT 31H
        MOV EDX,EAX
        SETC ERRORFLAG  
        JNC @Q_0902
        CALL [SYS_ERRORHANDLER] PASCAL,0902H,EAX         
        MOV EDX,-1      

 @Q_0902:
        RET
        ENDP

;===========================================================================

;-------------------------------------------------
; VOID MOVE(POINTER SRC, POINTER DEST, LONG COUNT)
;-------------------------------------------------
PUBLIC MOVE

MOVE PROC NEAR
        ARG @SRC:POINTER, @DEST:POINTER, @CNT:DWORD
        
        USES ESI,EDI,ECX,DS,ES
        MOV DS,WORD PTR @SRC.SELECTOR[0]        
        MOV ES,WORD PTR @DEST.SELECTOR[0]       
        MOV ESI,@SRC.OFSET
        MOV EDI,@DEST.OFSET
        MOV ECX,@CNT
        CLD
        REP MOVSB
        
        RET
        ENDP

;-------------------------------------------------
; VOID MOVEW(POINTER SRC, POINTER DEST, LONG COUNT)
;-------------------------------------------------
PUBLIC MOVEW

MOVEW PROC NEAR
        ARG @SRC:POINTER, @DEST:POINTER, @CNT:DWORD
        
        USES ESI,EDI,ECX,DS,ES
        MOV DS,WORD PTR @SRC.SELECTOR[0]        
        MOV ES,WORD PTR @DEST.SELECTOR[0]       
        MOV ESI,@SRC.OFSET
        MOV EDI,@DEST.OFSET
        MOV ECX,@CNT
        CLD
        REP MOVSW
        
        RET
        ENDP

;-------------------------------------------------
; VOID MOVED(POINTER SRC, POINTER DEST, LONG COUNT)
;-------------------------------------------------
PUBLIC MOVED

MOVED PROC NEAR
        ARG @SRC:POINTER, @DEST:POINTER, @CNT:DWORD
        
        USES ESI,EDI,ECX,DS,ES
        MOV DS,WORD PTR @SRC.SELECTOR[0]        
        MOV ES,WORD PTR @DEST.SELECTOR[0]       
        MOV ESI,@SRC.OFSET
        MOV EDI,@DEST.OFSET
        MOV ECX,@CNT
        CLD
        REP MOVSD
        
        RET
        ENDP

;===========================================================================


;##########################################################################
; CRT INITIALIZATION
;##########################################################################

PUBLIC INIT_MODE, DISPL, ADAPTER, CURRPAGE, ROWS, COLS, TXT_LINADDX

ALIGN 4
;---------------------------------------------------------------
INIT_MODE       CHAR -1 ;INITIAL VIDEO MODE
DISPL           CHAR 0  ;TIPO DE DISPLAY (MONO/COR)
ADAPTER         CHAR 0  ;TIPO DE PLACA
CURRPAGE        CHAR 0  ;PÜGINA DE VãDEO CORRENTE
ROWS            CHAR 0  ;Nß DE LINHAS 
COLS            CHAR 0  ;Nß DE COLUNAS
TXT_LINADDX     LONG 0  ;ENDEREÄO LINEAR DA JANELA DE TEXTO

;----------------------------------------------------------------
; Inicializa as rotinas modo texto, e salva o modo de entrada
; VOID INIT_VIDEO()
; ASSUME DS:SEGMENT32
;----------------------------------------------------------------

INIT_VIDEO PROC NEAR

        USES EAX,EBX,ECX,EDX,ES
        MOV EAX,01A00H
        CALL RM_INT PASCAL,10H
        CMP AL,1AH
        JNE @@CHECK_EGA         ;ERRO NA CHAMADA, NéO ê VGA
                
        CMP BL,2                ;SE VGA COMO SECUNDÜRIO
        JE @@CHECK_CGA          ;PROCURA VGA COMO PRIMÜRIO
        JB @@IS_MONO
        
        CMP BL,5                ;EGA ê PRIMÜRIO?
        JBE @@CHECK_EGA
        
 @@CHECK_MCGA:
        MOV ADAPTER,MCGA        ;ASSUME MCGA
        MOV DISPL,COR   
        CMP BL,8
        JA @@MODE_DETECTED
        
 @@CHECK_EGA:
        MOV EAX,1200H
        MOV EBX,10H             ;OBTER PARèMETROS EGA
        XOR ECX,ECX
        CALL RM_INT  PASCAL,10H
        JCXZ @@CHECK_CGA        ;NéO ê EGA
        
        MOV ADAPTER,EGA
        MOV DISPL,MONO          ;ASSUME MONO
        CMP BH,01
        JE @@MODE_DETECTED

        MOV DISPL,COR           ;AFINAL ê COR
        JMP @@MODE_DETECTED     

 @@CHECK_CGA:
        CALL RM_INT PASCAL,11H
        AND AL,30H              ;ISOLA OS BITS DA PLACA DE VãDEO
        CMP AL,30H              ;SE ESTIVEREM OS 2 A 1 ê MONO
        JE @@IS_MONO
        
        MOV ADAPTER,CGA
        MOV DISPL,COR
        JMP @@MODE_DETECTED
        
 @@IS_MONO:
        MOV ADAPTER,MDA
        MOV DISPL,MONO
        
 @@MODE_DETECTED:
        MOV EAX,0F00H
        CALL RM_INT PASCAL,10H
        MOV INIT_MODE,AL
        MOV CURRPAGE,BH
        XOR EAX,EAX
        MOV AL,DISPL            ;CALCULA O FRAME BUFFER
        SHL EAX,15
        ADD EAX,0B0000H
        MOV TXT_LINADDX,EAX
        
        MOV ES,SDATA0
        MOV AX,ES:[0044AH]      ;OBTêM O Nß DE COLUNAS
        MOV COLS,AL
        MOV ROWS,24
        CMP ADAPTER,EGA
        JB @@QUIT_VINIT
        
        MOV EAX,01130H          ;GET CHAR INFO
        XOR EBX,EBX
        CALL RM_INT PASCAL,10H
        MOV ROWS,DL
        
 @@QUIT_VINIT:
        RET
        ENDP

;##########################################################################
; EXCEPTION HANDLERS
;##########################################################################

;=========================================================================
PUBLIC EXCEP_REGS, EXCEP_SF, EXCEP_NO

;=========================================================================
;VARIÜVEIS DOS HANDLERS

ALIGN 4

EXCEP_REGS REGISTERS    <>      ;EXCEPTION HANDLER REGISTER STRUC
EXCEP_SF   EXCEP_SF32   <>      ;EXCEPTION STACK FRAME PARAMETERS
EXCEP_NO   LONG 255             ;EXCEPTION NUMBER

;=========================================================================
PUBLIC OLD_EX00,OLD_EX01,OLD_EX02,OLD_EX03,OLD_EX04,OLD_EX05,OLD_EX06
PUBLIC OLD_EX07,OLD_EX08,OLD_EX09,OLD_EX10,OLD_EX11,OLD_EX12,OLD_EX13
PUBLIC OLD_EX14,OLD_EX15

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
PUBLIC EXCEP_HANDLER00,EXCEP_HANDLER01,EXCEP_HANDLER02,EXCEP_HANDLER03
PUBLIC EXCEP_HANDLER04,EXCEP_HANDLER05,EXCEP_HANDLER06,EXCEP_HANDLER07
PUBLIC EXCEP_HANDLER08,EXCEP_HANDLER09,EXCEP_HANDLER10,EXCEP_HANDLER11
PUBLIC EXCEP_HANDLER12,EXCEP_HANDLER13,EXCEP_HANDLER14,EXCEP_HANDLER15

;=========================================================================
;HIGH LEVEL EXCEPTION HANDLERS OFFSET

EXCEP_HANDLER00 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 00
EXCEP_HANDLER01 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 01
EXCEP_HANDLER02 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 02
EXCEP_HANDLER03 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 03
EXCEP_HANDLER04 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 04
EXCEP_HANDLER05 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 05
EXCEP_HANDLER06 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 06
EXCEP_HANDLER07 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 07
EXCEP_HANDLER08 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 08
EXCEP_HANDLER09 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 09
EXCEP_HANDLER10 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 10
EXCEP_HANDLER11 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 11
EXCEP_HANDLER12 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 12
EXCEP_HANDLER13 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 13
EXCEP_HANDLER14 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 14
EXCEP_HANDLER15 LONG OFFSET EX_DEF      ;HANDLER DA EXCEPTION 15

;=========================================================================
;MENSAGENS DE ERRO

EXCEP00MSG DB 0AH,0DH,'ERRO #200 : EXCEPTION 00h - DivisÑo por zero.',0Ah,0Dh,7,'$'
EXCEP01MSG DB 0AH,0DH,'ERRO #201 : EXCEPTION 01h - Debug TRAP.',0Ah,0Dh,7,'$'
EXCEP02MSG DB 0AH,0DH,'ERRO #202 : EXCEPTION 02h - Ocorreu uma NMI.',0Ah,0Dh,7,'$'
EXCEP03MSG DB 0AH,0DH,'ERRO #203 : EXCEPTION 03h - InterrupáÑo de 1 byte.',0Ah,0Dh,7,'$'
EXCEP04MSG DB 0AH,0DH,'ERRO #204 : EXCEPTION 04h - Ocorreu um Overflow.',0Ah,0Dh,7,'$'
EXCEP05MSG DB 0AH,0DH,'ERRO #205 : EXCEPTION 05h - Erro de BOUND.',0Ah,0Dh,7,'$'
EXCEP06MSG DB 0AH,0DH,'ERRO #206 : EXCEPTION 06h - Opcode inv†lido.',0Ah,0Dh,7,'$'
EXCEP08MSG DB 0AH,0DH,'ERRO #208 : EXCEPTION 08h - Erro de DOUBLE FAULT.',0Ah,0Dh,7,'$'
EXCEP0AMSG DB 0AH,0DH,'ERRO #210 : EXCEPTION 0Ah - TSS inv†lido.',0Ah,0Dh,7,'$'
EXCEP0BMSG DB 0AH,0DH,'ERRO #211 : EXCEPTION 0Bh - Segmento nÑo presente.',0Ah,0Dh,7,'$'
EXCEP0CMSG DB 0AH,0DH,'ERRO #212 : EXCEPTION 0Ch - Erro de Stack.',0Ah,0Dh,7,'$'
EXCEP0DMSG DB 0AH,0DH,'ERRO #213 : EXCEPTION 0Dh - General Protection Fault.',0Ah,0Dh,7,'$'
EXCEP0EMSG DB 0AH,0DH,'ERRO #214 : EXCEPTION 0Eh - Page Fault.',0Ah,0Dh,7,'$'

;TABELA DE OFFSETS
ALIGN 4

ERROR_TABLE UINT Offset EXCEP00MSG,Offset EXCEP01MSG,Offset EXCEP02MSG
            UINT Offset EXCEP03MSG,Offset EXCEP04MSG,Offset EXCEP05MSG
            UINT Offset EXCEP06MSG,0,Offset EXCEP08MSG, 0
            UINT Offset EXCEP0AMSG,Offset EXCEP0BMSG,Offset EXCEP0CMSG
            UINT Offset EXCEP0DMSG,Offset EXCEP0EMSG

;=========================================================================


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

;-------------------------------------------------
; VOID ESAVE_REGS(POINTER E_STRUC)
;-------------------------------------------------

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

;--------------------------------------------------
;VOID ERESTORE_REGS(POINTER E_STRUC)
;--------------------------------------------------

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
;SYSTEM EXCEPTION HANDLERS

;----------------------------------------------
;DEFAULT EXCEPTION HANDLER TO ALL EXCEPS
;----------------------------------------------

EX_DEF PROC NEAR

        XOR EAX,EAX
        MOV AL,CS:INIT_MODE
        CALL RM_INT PASCAL,010H
        MOV ESI,OFFSET ERROR_TABLE
        MOV EAX, EXCEP_NO
        SHL AX,1
        ADD ESI,EAX
        LODSW
        MOVZX EDX,AX
        MOV AX,SEG ERROR_TABLE
        MOV R_RDS,AX
        MOV EAX,0900H
        CALL RM_INT PASCAL,021H 
        MOV AX,CS
        MOV DS:EXCEP_SF.RET_CS,AX
        MOV EAX,EXITPROC
        MOV DS:EXCEP_SF.RET_EIP,EAX
        RET
        ENDP

;-------------------------------------
;LOW-LEVEL EXCEPTION HANDLERS

SYS_EH SYS_EXCEP_00,00,EXCEP_HANDLER00
SYS_EH SYS_EXCEP_01,01,EXCEP_HANDLER01
SYS_EH SYS_EXCEP_02,02,EXCEP_HANDLER02
SYS_EH SYS_EXCEP_03,03,EXCEP_HANDLER03
SYS_EH SYS_EXCEP_04,04,EXCEP_HANDLER04
SYS_EH SYS_EXCEP_05,05,EXCEP_HANDLER05
SYS_EH SYS_EXCEP_06,06,EXCEP_HANDLER06
SYS_EH SYS_EXCEP_07,07,EXCEP_HANDLER07
SYS_EH SYS_EXCEP_08,08,EXCEP_HANDLER08
SYS_EH SYS_EXCEP_09,09,EXCEP_HANDLER09
SYS_EH SYS_EXCEP_10,10,EXCEP_HANDLER10
SYS_EH SYS_EXCEP_11,11,EXCEP_HANDLER11
SYS_EH SYS_EXCEP_12,12,EXCEP_HANDLER12
SYS_EH SYS_EXCEP_13,13,EXCEP_HANDLER13
SYS_EH SYS_EXCEP_14,14,EXCEP_HANDLER14
SYS_EH SYS_EXCEP_15,15,EXCEP_HANDLER15

ALIGN 4

;TABELA DE OFFSETS

SYS_EH_TABLE LONG OFFSET SYS_EXCEP_00, OFFSET SYS_EXCEP_01, OFFSET SYS_EXCEP_02, OFFSET SYS_EXCEP_03
             LONG OFFSET SYS_EXCEP_04, OFFSET SYS_EXCEP_05, OFFSET SYS_EXCEP_06
             LONG OFFSET SYS_EXCEP_07, OFFSET SYS_EXCEP_08, OFFSET SYS_EXCEP_09
             LONG OFFSET SYS_EXCEP_10, OFFSET SYS_EXCEP_11, OFFSET SYS_EXCEP_12
             LONG OFFSET SYS_EXCEP_13, OFFSET SYS_EXCEP_14, OFFSET SYS_EXCEP_15

;=========================================================================
;--------------------------------------
; VOID SWAP_EXCEPT_VECTORS()
;--------------------------------------

SWAP_EXCEPT_VECTORS PROC NEAR

        USES EAX,EBX,ECX,EDX,EDI,ESI,ES,DS
        CALL VCLI
        MOV AX,CS:SDATA32
        MOV ES,AX
        MOV DS,AX
        MOV EDI,OFFSET OLD_EX00
        MOV ESI,OFFSET SYS_EH_TABLE
        MOV ECX,0

  @@SWAP_EXCEP_L1:
        PUSH ECX
        CALL GET_EXCEPTION PASCAL,ECX
        MOV ES:[EDI][POINTER.SELECTOR],ECX
        MOV ES:[EDI][POINTER.OFSET],EDX
        ADD EDI,8
        POP ECX
        LODSD
        CMP ECX,7
        JE @@SWAP_SKIP
        CMP ECX,9
        JE @@SWAP_SKIP
        CMP ECX,15
        JE @@SWAP_SKIP
        CALL SET_EXCEPTION PASCAL,ECX,CS,EAX

 @@SWAP_SKIP:
        INC ECX
        CMP ECX,16
        JNE @@SWAP_EXCEP_L1
        
        CALL VSTI
        RET
        ENDP    


;--------------------------------------
; VOID RESTORE_EXCEPT_VECTORS()
;--------------------------------------

RESTORE_EXCEPT_VECTORS PROC NEAR

        USES EAX,EBX,ECX,EDX,ESI,DS
        CALL VCLI
        MOV DS,CS:SDATA32
        MOV ESI,OFFSET OLD_EX00
        MOV ECX,15
        CLD

  @@RESTORE_EXCEP_L1:
        MOV EBX,DS:[ESI][POINTER.SELECTOR]
        MOV EAX,DS:[ESI][POINTER.OFSET]
        CALL SET_EXCEPTION PASCAL,ECX,EBX,EAX
        ADD ESI,8
        LOOP @@RESTORE_EXCEP_L1
        
        CALL VSTI
        RET
        ENDP    

;=========================================================================
;#########################################################################
;=========================================================================
BLOCK_USED = 1
BLOCK_FREE = 0
;ATENÄéO - USAR ES <-SDATA0 NAS CHAMADAS!!!

MEMBLOCK STRUC METHOD {
        
        INIT:DWORD      = OFFSET MB_INIT        ;INICIALIZA O BLOCO
        GETNEXT:DWORD   = OFFSET MB_GETNEXT     ;DEVOLVE O PROX BLOCO
        GETPREV:DWORD   = OFFSET MB_GETPREV     ;DEVOLVE O BLOCO ANTERIOR
        SPLIT:DWORD     = OFFSET MB_SPLIT       ;DIVIDE O BLOCO EM 2
        FINDNEXT_FREE:DWORD = OFFSET MB_FINDNEXT_FREE ;DEVOLVE O PROX BLOCO LIVRE

        }
        NEXT    LONG ?    ;NEXT BLOCK
        PREV    LONG ?    ;PREV BLOCK
        BSIZE   LONG ?    ;BLOCK SIZE
        STATUS  LONG ?    ;0 - FREE, 1 - IN USE
ENDS

;-----------------------------------------
;VOID MB_INIT(LONG SELF)
;-----------------------------------------

MB_INIT PROC NEAR
        ARG @SELF:LONG, @SIZE:LONG
        
        USES EAX,EDI
        MOV EAX,NIL                             ;SETS NEXT & PREV TO NIL
        MOV ES:[EDI][MEMBLOCK.NEXT],EAX 
        MOV ES:[EDI][MEMBLOCK.PREV],EAX         
        MOV EAX,@SIZE
        MOV ES:[EDI][MEMBLOCK.BSIZE],EAX                
        MOV EAX,BLOCK_FREE
        MOV ES:[EDI][MEMBLOCK.STATUS],EAX                       
        RET
ENDP    


;----------------------------------------
; LONG MB_GETNEXT(LONG SELF)
;----------------------------------------
MB_GETNEXT PROC NEAR
        ARG @SELF:LONG
        
        USES EDI
        MOV EDI,@SELF
        MOV EDX,ES:[EDI][MEMBLOCK.NEXT]
        RET
        ENDP

;----------------------------------------
; LONG MB_GETNEXT(LONG SELF)
;----------------------------------------
MB_GETPREV PROC NEAR
        ARG @SELF:LONG
        
        USES EDI
        MOV EDI,@SELF
        MOV EDX,ES:[EDI][MEMBLOCK.PREV]
        RET
        ENDP
        
;----------------------------------------
; LONG MB_SPLIT(LONG SELF,LONG SIZE)
;----------------------------------------
MB_SPLIT PROC NEAR
        ARG @SELF:LONG, @SIZE:LONG
        
        USES EAX,EBX,EDI
        MOV EDI,@SELF
        MOV EAX,ES:[EDI][MEMBLOCK.BSIZE]        ;EAX:=BLOCKSIZE-SIZE
        MOV EBX,@SIZE
        SUB EAX,EBX                             ;EBX:=SIZE-HEADER
        SUB EBX,SIZE MEMBLOCK           
        CALL EAX METHOD MEMBLOCK:INIT PASCAL,EAX,EBX    ;INICIALIZA O NOVO BLOCO
        MOV ES:[EAX][MEMBLOCK.PREV],EDI         ;NEW.PREV:=@CURR
        MOV EBX,ES:[EDI][MEMBOCK.NEXT]
        MOV ES:[EAX][MEMBLOCK.NEXT],EBX         ;NEW.NEXT:=CURR.NEXT
        MOV ES:[EDI][MEMBLOCK.NEXT],EAX         ;CURR.NEXT:=@NEW
        MOV EDX,EAX
        RET
ENDP

;------------------------------------
; LONG FINDNEXT_FREE(LONG SELF)
;------------------------------------

MB_FINDNEXT_FREE PROC NEAR
        ARG @SELF:LONG
        
        USES EAX,ECX,EDI        
        MOV EDI,@SELF
        MOV EDX,NIL                             ;VALOR DE ERRO

 @@FINDNEXT_L1:
        MOV ECX,ES:[EDI][MEMBLOCK.NEXT]
        JECXZ @@Q_FINDNEXT                      ;ABORTA SE CHEGOU AO FIM        

        MOV EDI,ECX                             ;CURR:=CURR.NEXT
        CMP ES:[EDI][MEMBLOCK.STATUS],BLOCK_FREE
        JNE @@FINDNEXT_L1
        MOV EDX,EDI             

 @@Q_FINDNEXT:
        RET
        ENDP


;--------------------------------------
; POINTER MALLOC(LONG SIZE)
;--------------------------------------

MALLOC PROC NEAR
        ARG @SZ:LONG
        
        USES EAX,EBX,ECX,EDX,EDI,ES
        MOV AX,CS:SDATA0
        MOV ES,AX
        MOV EAX,@SZ
        CMP EAX,16              ;TAMANHO MãNIMO
        JAE @@MALLOC_L1
        MOV EAX,16

 @@MALLOC_L1:
        ADD EAX,16              ;TAMANHO DO CABEÄALHO
        MOV EBX,EAX
        AND EBX,3               ;ALINHAMENTO A 4 BYTES (BITS 0 E 1 A ZERO)
        CMP EBX,3
        JE @@MALLOC_L2          ;NéO ê PRECISO ALINHAR

        AND EAX,NOT 3           ;ALINHA A 4
        ADD EAX,4               
        
 @@MALLOC_L2:
        MOV EDI,HEAP_ADDX       


 @@MALLOC_L3:
        MOV ECX,ES:[EDI][MEMBLOCK.STATUS]
        
        
@@Q_MALLOC:
        RET
        ENDP



;=========================================================================
COMMENT %
;-------------------------------------
; BOOL HEAP_INIT()
;-------------------------------------

HEAP_INIT PROC NEAR

        USES EAX,EBX,EDI,ES
        MOV AX,CS:SDATA0
        MOV ES,AX
        MOV EBX,HEAPSIZE
        MOV HEAP_SIZE,EBX
        CALL GETMEM PASCAL,EBX
        MOV HEAP_ADDX,EDX
        MOV EDI,EDX
        XOR EAX,EAX
        MOV ES:[EDI][BLOCK_FREE.NEXT],EAX
        MOV ES:[EDI][BLOCK_FREE.SIZE],EBX
        MOV ES:[EDI][BLOCK_FREE.PREV],EAX
        MOV ES:[EDI][BLOCK_FREE.PREV_FREE],EAX
        ADD EDX,EBX
        MOV HEAP_TOP,EDX
        XOR EDX,EDX
        MOV DX,ERRORFLAG
        RET
        ENDP
                
;-------------------------------------
; VOID HEAP_FREE()
;-------------------------------------

HEAP_FREE PROC NEAR

        CALL FREEMEM PASCAL,HEAP_ADDX
        RET
        ENDP


%
;=========================================================================

;--------------------------------------
; VOID INIT_VECTORS()
;--------------------------------------

INIT_VECTORS PROC NEAR

        MOV EDX,OFFSET CODE32_STARTUP   ;PREVINE PAGING DO SEGMENTO DE CODIGO
        MOV EAX,SEGMENT32               ;POR CAUSA DOS EXCEPTION HANDLERS
        SHL EAX,4
        CALL LOCK_REGION PASCAL,EAX,EDX

        CALL SWAP_EXCEPT_VECTORS        ;REDEFINE OS VECTORES DAS EXCEPÄôES
        MOV EAX,OFFSET _QUIT_02         ;EXIT PROCEDURE
        MOV EXITPROC,EAX

        RET
        ENDP

;--------------------------------------
; VOID DONE_VECTORS()
;--------------------------------------

DONE_VECTORS PROC NEAR

        USES EAX,DS
        MOV AX,SDATA32
        MOV DS,AX
        CALL RESTORE_EXCEPT_VECTORS     ;RESTORE ORIGINAL EXCEPTION VECTORS

        RET
        ENDP    

;=========================================================================

ALIGN 4
TTEST   CHAR 'TU êS O MEU SOL A MINHA ILUSéO O MEU PECADO A MINHA TENTAÄéO',0,0AH,0DH,'$'
TST POINTER <>
TSST DD ?
PTEST POINTER <>
STS     SCREEN {}
TBUF DB 5000 DUP(0)
OLDGPF POINTER <>

;--------------------------------------
; VOID _QUIT_01()
;--------------------------------------

_QUIT_01 PROC NEAR
        ARG @C:LONG
        
        MOV EAX,@C
        MOV AH,4CH
        INT 21H
        ENDP

;--------------------------------------
; VOID _QUIT_02()
;--------------------------------------

_QUIT_02 PROC NEAR
        ARG @C:LONG
        
        CALL DONE_VECTORS
        MOV EAX,@C
        MOV AH,4CH
        INT 21H
        ENDP

;=========================================================================
CODE32_STARTUP:                         ;PONTO DE ENTRADA NO SEGMENTO 32

        MOV EAX,SEG16_OFSET
        MOV SEG16,EAX                   ;SALVA O ENDEREÄO LINEAR DO SEG16

        MOV EAX,OFFSET _QUIT_01         ;EXIT PROCEDURE
        MOV EXITPROC,EAX
        
        MOV EAX,OFFSET _EH_DPMI_01      ;SETA ROTINA DE ERRO TEMPORÜRIA
        MOV SYS_ERRORHANDLER,EAX

        CALL INIT_VIDEO                 ;INICIA AS VARIÜVEIS DE MODO TEXTO
        CALL INIT_VECTORS               ;INICIALIZA OS VECTORES DE EXCEPTION
                                        ;E DE INT


;       MOV ES,SDATA16
;       MOV ESI,0F0000H
;       MOV AX,ES:[ESI]

        MOV EDX,0FE00FE00H
        PUSH EDX

;------------------------------------------------------------------------
        CALL [SYS_ERRORHANDLER] PASCAL,00H,EAX
        CALL _EH_DPMI_01 PASCAL,0000H,EAX

        CALL GET_RMINTVEC PASCAL,021H
        MOV TST.SELECTOR,ECX
        MOV TST.OFSET,EDX
        CALL SET_RMINTVEC PASCAL,021H,TST

;{===========================================================}

        MOV EAX,64      
        CALL GETMEM PASCAL,EAX
        CMP ERRORFLAG,TRUE
        JE @QQQ
        MOV TSST,EDX

        MOV EAX,OFFSET TTEST
        MOV EBX,SEG TTEST

;       CALL TSST METHOD SCRT:PROC1 PASCAL,EBX,EAX
;       CALL TSST METHOD SCRT:PROC2 PASCAL,EBX,EAX

;       MOV ECX,OFFSET MSG22
;       CALL ECX PASCAL,EbX,EAX

        MOV EDI,OFFSET STS
        CALL SCRN_INIT PASCAL,DS,EDI

        MOV EAX,10
        MOV BASE_NUMBER,EAX
        XOR AX,AX
        MOV TERMINATOR,AL
        MOV EBX,SIZE MEMBLOCK
        MOV EDX,OFFSET TBUF
        CALL INT2STR PASCAL,EBX,DS,EDX
        CALL DS:[EDI] METHOD SCREEN:T_PRINT PASCAL,DS,EDI,DS,EDX

        MOV AX,5
        SHL EAX,16
        MOV AX,2

        MOV BX,20
        SHL EBX,16
        MOV BX,70

        CALL FREEMEM PASCAL,TSST
        CMP ERRORFLAG,TRUE
        JE @QQQ
;{===========================================================}
        MOVZX EAX,OLDCS
        MOV EBX,OFFSET RTEST
        CALL CALL_RMPROC PASCAL,EAX,EBX


        MOV EAX,32000   
        CALL GETMEM PASCAL,EAX
        CMP ERRORFLAG,TRUE
        JE @QQQ
        MOV EAX,EDX
        CALL RESIZEMEM PASCAL,EAX,2000
        CMP ERRORFLAG,TRUE
        JE @QQQ
        CALL FREEMEM PASCAL,EDX
        CMP ERRORFLAG,TRUE
        JE @QQQ

        MOV ES,CS:SDATA0
        MOV EDX,0F10F50F3H
        MOV PTEST.OFSET,EDX
        MOV AX,ES
        MOVZX EAX,AX
        MOV PTEST.SELECTOR,EAX
        MOV EBX,OFFSET PTEST
        LES EDI,DS:EBX
        MOV AX,WORD PTR PTEST.SELECTOR[0]
        CMP AX,DS:SDATA0
        JNE @QQQ
        CMP EDI,0F10F50F3H
        JNE @QQQ
        
        POP EDX
        CMP EDX,0FE00FE00H
        JNE @QQQ
        SND
        
@QQQ:
        CALL RESTORE_EXCEPT_VECTORS
@QQQ1:
        CALL EXITPROC

        

_TEXT32_END
;===================================================================


;===================================================================
_STACK32
;===================================================================


DD STACKSIZE DUP(0)

_STACK32_END

END start
