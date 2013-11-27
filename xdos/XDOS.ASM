.286p
Model LArge,PASCAL

stack 200h

noJUMPS

Include DPMI.INC
;INCLUDE NODES.ASO
INCLUDE CRT.ASO

SEGMENT CODE16 PARA PUBLIC USE16
CODE16 ENDS



ASSUME DS:CODE16, CS:code16
;-------------------------------------------------------------------
; SEGMENTO DE DADOS DE 16 BITS
;===================================================================

CODE16 SEGMENT Para public use16


PUBLIC SDATA16, Scode16, DOSMB_seg ,RSS,Psp_sel



;---------------------------------------------------
; Mensagens de Erro

CopyR  db 'XDOS  DOS extender v1.2b - Programado por JoÑo Pinheiro',0ah,0dh
       db 'Copyright (c) 1997 JoÑo Pinheiro ',0ah,0dh,0ah,0dh,'$'
Error1 db 'CPU >=386 nÑo encontrado !!',0ah,0dh,7,'$'
Error2 db 'Servidor DPMI nÑo encontrado!!',0ah,0dh,7,'$'
Error3 DB 'Erro de alocaáÑo de mem¢ria !!',0ah,0dh,7,'$'
Error4 DB 'Servidor DPMI nÑo suporta 32 bit !!',0ah,0dh,7,'$'
Error5 DB 'Erro aquando a chamada de entrada em modo protegido !!',0ah,0dh,7,'$'
Error6 DB 'Erro activando linha A20 !!',0ah,0dh,7,'$'
Error7 db 'Erro aquando a alocaáÑo da mem¢ria baixa necess†ria !!',0ah,0dh,7,'$'
Error8 DB 'N„o foi possÌvel alocar memÛria para a stack!',0ah,0dh,7,'$'
Error9 DB 'Erro na alocaáÑo dos descriptors necess†rios.',0ah,0dh,7,'$'
ErrorA DB 'Erro de inicializaáÑo',0ah,0dh,7,'$'


;Mensagens de inicializaáÑo

MSG1   DB 'Detectando CPU... $'
MSG2   DB 'Detectando DPMI... $'
MSG3   DB 'Activando A20... $'
MSG4   DB 'Alocando bloco de $'
MSG5   DB 'par†grafos de mem¢ria baixa',0ah,0dh,'$'
MSG6   DB 'par†grafos de mem¢ria baixa para stack',0ah,0dh,'$'
MSG7   DB 'Inicializando DPMI...',0ah,0dh,'$'
MSG8   DB 'V0.9',0ah,0dh,'$'
MSG9   DB 'OK.',0ah,0dh,'$'


TempSTR DB 5 DUP(?)  ;String Tempor†ria
TempSTR2 DB 5 DUP(?)  ;String Tempor†ria


DPMI_Entry DW ?,?    ;ADDX de entrada em PM

ALIGN 4

DOSMB_SEG DW ?   ;Segmento do bloco de mem¢ria Baixa alocado
RSS       DW ?   ;Segmento da pilha para usar na RM CallBack Struct
PSP_Seg   DW ?   ;Segmento do PSP
PSP_Sel   DW ?   ;Selector do PSP


SDATA16 DW ?    ;Selector do segmento Data16
SCode16 DW ?    ;Selector do segmento Code16
Scode32 DW ?    ;Selector do segmento Code32
Sdata32 DW ?    ;Selector do segmento data32 
SDATA0  DW ?    ;Selector do segmento flat

SINCVAL DW ?	;Selector increment val

oldSP	DW ?    ;Code16 SP
oldSS	DW ?	;Code16 SS

;------------------------------------------------------------------------
GLOBAL SDATa16:Word
Global Scode16:Word
Global Scode32:word
Global Sdata32:Word
Global Sdata0:word
Global Sincval:Word
Global PSP_sel:Word
Global RSS:word
Global RM_int10:far
;-----------------------------------------------
; Dados para V86 switch

REGS16 Registers <>

RDS16 DW ?  ;Registo DS modo V86
RES16 DW ?  ;REgisto ES modo V86



;**********************************************
; Converte uma Word numa String ASCIIZ
;**********************************************

Tstr PROC FAR
      Arg Numb:Word, S:RMPOINTER

      local CNT:Word

      Uses AX, BX, CX, DX, ES, DI
      Mov Ax,numb
      Xor dx,dx
      mov cnt,dx  
      mov cx,dx
      mov bx,10
    @Tstr_L1:
      DIV BX 
      inc cnt
      PUSH DX
      MOV dx,cx
      cmp ax,0
      jne @Tstr_L1

      Mov es,S.RSEG
      Mov di,S.Rofset
      Mov cx,cnt
      cld
    @Tstr_L2: 
      pop ax
      add al,30h
      stosb
      loop @Tstr_l2
      mov al,0
      stosb
      ret
Endp        



;**********************************************
; Detecta se existe CPU>=386
; RET AX - TRUE/False
;**********************************************

Proc Is386 far

        PushF
        Pop ax
        And Ah,0fh
        push ax
        popf
        pushf
        pop ax
        cmp ah,0f0h     ;IS an oldie 8086
        jae @No_386
        or ah,0f0h              
        push ax
        popf
        pushf
        pop ax
        and ah,0f0h
        jz @NO_386      ;Is a shitty 286
        mov ax,true
        ret

    @no_386:
        mov ax,false
        ret
endp



;**********************************************
; Activa a linha A20
;**********************************************

Proc EnableA20 far

        local xms_adr:Dword
        Uses bx,es
	mov ax,4300h
	int 2fh
        cmp al,80h      ;Xms presente?
        jne @Hard_way   ;NÑo, usar hardware
	mov ax,4310h
	int 2fh
	mov ah,03h
        mov word ptr xms_adr[0],bx
        Mov word ptr xms_adr[2],es
        call dword ptr xms_adr
        jmp @A20_quit
  @Hard_way:
  	in al,64h
  	and al,2
  	jnz @hard_way
  	mov al,0d1h
  	out 64h,al
  @hard_way2:  	
  	in al,64h
  	and al,2
  	jnz @hard_way2
  	mov al,0dfh
  	out 60h,al
  @hard_way3:
  	in al,64h
  	and al,2
        jnz @hard_way3
        mov ax,noerror
  @a20_quit:
  	ret
endp  		




;**********************************************
; ESCREVE Uma Tstring Terminada em $
;**********************************************

Proc Print far 
        ARG msg:RMpointer

        uses ax,DS,dx
        Mov ds,Msg.RSEG
        Mov dx,MSG.Rofset
        mov ah,09h
        int 21h
        ret
endp


;**********************************************
; Termina a execuáÑo com msg de erro
;**********************************************

Proc ErrorQuit far
        ARG MSG:RMPointer
        
        Mov ax,MSG.RSEG
        Mov bx,MSG.Rofset
        call Print pascal,ax,bx
        mov ax,04c01h
        int 21h
endp    


;**********************************************
; Aloca o bloco de mem¢ria real requerido
;**********************************************

Proc GET_DOSMEM far

        uses ax,bx      
        mov bx,dosmem
        cmp bx,0
        je @ALocDos_quit
        mov ah,48h
        int 21h
        jnc @ALOC_cont
        call Errorquit pascal, @Data, offset Error7

   @ALoc_Cont:
        mov dosmB_SEG,ax

   @Alocdos_quit:
        ret
endp            
                

;**********************************************
; Liberta o bloco de mem¢ria real alocado
;**********************************************

Proc Free_dosmem Far

        uses ax,bx,es
        mov bx,dosmem
        cmp bx,0
        je @FreeDos_quit
        Mov es,dosmb_seg
        mov ah,49h
        int 21h

  @freedos_quit:
        ret
endp    



;**********************************************
; Aloca a pilha para as chamadas V86
;**********************************************

Proc Alloc_RMSTACK far

        uses ax,bx      
        mov bx,RMSTACK
        cmp bx,0
        je @ALocrss_quit
        mov ah,48h
        int 21h
        jnc @@sscont
        Mov ax,iserror
        ret
   @@ssCont:
        mov RSS,ax

        mov ax,noerror
   @Alocrss_quit:
        ret
endp            
                
;**********************************************
; Liberta a pilha para as chamadas V86
;**********************************************

Proc Free_RMStack Far

        uses ax,bx,es
        mov bx,rmstack
        cmp bx,0
        je @Freerss_quit
        Mov es,RSS
        mov ah,49h
        int 21h

  @freerss_quit:
        ret
Endp



;ÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆ
;In°cio do Programa

CODE16_Start:

        push seg code16                ;DS = code16
	pop ds

        mov ah,62h                     ;Encolher o PSP
        int 21h
        mov es,bx
        mov ah,4ah
        mov bx,PROGRAM_SZ
        int 21h

        MOv PSP_Seg,ES                 ;Salva o PSP segm

        Call print Pascal,DS,offset copyr
        Call Print Pascal,DS,Offset MSG1

	call is386 pascal
	cmp al,true
        je @Cont1                       ;>=386 detected

        call errorquit pascal,DS,offset error1

  @cont1:
        Call Print Pascal,Ds,offset msg9
        Call Print Pascal,DS,offset MSG2

	mov ax,1687h
	int 2fh
	cmp ax,0
        je @cont2                       ;DPMI server detected

        Call errorquit pascal,ds,offset error2

  @cont2:
        add dh,30h
        mov byte ptr msg8[1],dh
        cmp dl,'Z'
        jne @@Cont1
        sub dl,81
@@Cont1:
        add dl,30h
        mov byte ptr msg8[3],dl
        call print pascal,ds,offset msg8

        test bx,1
        jnz @cont3                      ;Dpmi 32 server
        Call errorquit pascal,ds,offset error4

  @Cont3:
	mov word ptr dpmi_entry[0],di
	mov word ptr dpmi_entry[2],es
	cmp si,0
	je @cont4

        mov bx,si                       ;Aloca Private data
	mov ah,48h
	int 21h
	mov es,ax
	jnc @cont4

        Call errorquit pascal,DS, offset error3
  @cont4:
        Call Print pascal,Ds,offset MSG3
        call enableA20
        cmp al,false

        jne @cont5
        Call errorquit pascal,DS,offset error6

 @cont5:
        Call Print Pascal,DS,Offset MSG9
        CALL Print Pascal,ds,offset MSG4
        CALL Tstr pascal,DOsmem,DS,offset Tempstr
        mov al,'$'
        MOv byte ptr tempstr[4],al
        Call Print Pascal,Ds,offset Tempstr
        Call print pascal,ds,offset msg5
        call get_dosmem
        
        CALL Print Pascal,ds,offset MSG4
        CALL Tstr pascal,rmstack,DS,offset Tempstr2
        mov al,'$'     
        MOv byte ptr tempstr2[4],al
        Call Print Pascal,Ds,offset Tempstr2
        Call print pascal,ds,offset msg6
        Call Alloc_RMstack
        Call Print Pascal,ds,offset MSG7

	Mov ax,1
.386p
	call dword ptr Dpmi_entry

	jnc @cont6
        call free_dosmem
        Call free_rMstack
        Call errorquit pascal,DS,offset error5

 @cont6:
 	mov psp_sel,es
	Mov Scode16,cs
	Mov sdata16,DS
			;In°cio da zona de inicializaáÑo

	
	Call rGetincVal Pascal ;ObtÇm o incremento de selector
	Mov SINCVAL,AX
	
        CAll RAlloc_descr pascal,1	;Aloca 2 descriptors
	mov dx,offset error9
     	jc @quiterr
     	
 @cont7:
	Mov Scode32,ax

	Mov Ebx,seg code32
	shl ebx,4
 	Call RSetBase pascal,ax,EBX	;seta a base de Scode32
	mov dx,offset errorA
 	jc @quiterr2
     	
	mov ebx,0FFFFFFFFh
     	Call RSetlimit pascal,scode32,EBX ;Seta o limite
        jc @quiterr2

	mov bx,cs
	lar CX,bx
	jnz @quiterr2

	and cl,01100000b
	or cl,11111011b
	mov ch,11010000b
	Call RSetar Pascal,scode32,cx  ;seta o Type byte
	jc @quiterr2

	mov ax,0ah
	mov bx,scode32
	int 31h			;cria alias de code32
	jc @quiterr2
	Mov sdata32,ax	


	mov ax,0ah
	mov bx,scode32
	int 31h			;cria alias de code32
	jc @quiterr2
	Mov sdata0,ax	

	Mov Ebx,0
 	Call RSetBase pascal,sdata0,EBX	 ;seta a base de Sdata0
 	jc @quiterr2
	
	Mov ax,002h
	Mov bx,seg stack32		;Inicia a stack 32 bit
	int 31h
	jc @quit

	mov ax,sp
	Mov oldsp,ax			;salva o SP
	mov ax,ss
	mov oldss,ax			;sava o SS inicial

	mov ax,es
	Mov ss,ax			;stack switch

	Mov esp,pmstack
	mov ebp,0

	push dword ptr scode32
	push offset dpmi_init
	DB 66h,0cbh			;RETF 32
	
 @quit:
	mov ax,4c00h
	int 21h

 @quitERR:
     	Call Pmprint pascal,seg code16,dx
     	jmp @quit

 @quiterr2:
	Call rFreeDescriptor pascal,scode32
	jmp @quiterr


;------------------------------------------------------------------------
;Rotinas DPMI PM DE INICIALIZAÄÑo
;------------------------------------------------------------------------

;************************************
; Aloca um ou Mais descriptors
;************************************

RAlloc_Descr Proc far
	  ARG count:word
	  
	  Uses cx
	  Mov ax,00h
	  mov cx,Count
	  Int 31h
	  ret
Endp
	    

;*********************************
; ObtÇm O Selector incval
;*********************************

RGetincval Proc far
	  
	  MOv ax,03h
	  int 31h
	  ret
Endp


;********************************
; Liberta um descriptor
;********************************

RFreeDescriptor Proc Far
	ARG Sel:word
	
	Uses AX,BX
	mOv ax,01h
	Mov bx,sel
	Int 31h
	ret
Endp		  

;*********************************************
; Seta a base de um selector
;*********************************************

RSetbase Proc far
	Arg Sel:word, Base:Dword
	
	Uses AX,bx,cx,dx
	Mov ax,07h
	Mov bx,Sel
	Mov cx,word ptr Base[2]
	Mov dx,word ptr Base[0]
	Int 31h
	ret
Endp	


;*********************************************
; Seta o limite de um selector
;*********************************************

RSetlimit Proc far
	Arg Sel:word, Base:Dword
	
	Uses AX,bx,cx,dx
	Mov ax,08h
	Mov bx,Sel
	Mov cx,word ptr Base[2]
	Mov dx,word ptr Base[0]
	Int 31h
	ret
Endp	


;*********************************************
; Seta Direitos de acesso de um descriptor
;*********************************************

RSetAR Proc far
	Arg Sel:word, AR:word
	
	Uses AX,bx,cx
	Mov ax,09h
	Mov bx,Sel
	Mov cx,ar
	Int 31h
	ret
Endp	

;**********************************************
;Executa uma int em modo real
;**********************************************

RMINT16 Proc Far
      ARG R_int:byte

      uses EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
      MOV dword ptr Regs16._DI,EDI
      MOV dword ptr Regs16._sI,EsI
      MOV dword ptr Regs16._bp,0
      MOV dword ptr Regs16._bx,ebx
      MOV dword ptr Regs16._dx,edx
      MOV dword ptr Regs16._cx,ecx
      MOV dword ptr Regs16._ax,eax
      Mov Ax,0300h
      Mov bl,R_int
      mov bh,0                
      PUSH res16
      pop regs16._es
      push RDS16
      Pop regs16._ds
      PUSH RSS
      Pop regs16._ss
      Mov word ptr regs16._sp,0
      push 0202h
      Pop Regs16.flags
      Mov  cx,0
      push Sdata16
      pop es
      Mov edi,offset regs16
      Int 31h
      jnc @RI16_noproblem
      stc
      jmp @ri16_quit

@Ri16_noproblem:
      MOV AX,Regs16.flags
      SAhf

@RI16_quit:
      ret
Endp


INTR16 Macro Intno
       CAll RMint16 pascal,Intno
       Endm


;*****************************************
; Rotina de Print em PM
;*****************************************

PMPrint Proc far
	ARg Msg:RMpointer

	Uses ax,dx
	Mov ah,09h
	Push MSG.rSeg
	pop RDS16
	Mov dx,Msg.rofset
	INTR16 21h
	ret
endp		


rm_iNT10 PROC FAR

         int 10H
         RETF
eNDP


CODE16 ENDS



;...........................................................................
;---------------------------------------------------------------------------
;...........................................................................
;---------------------------------------------------------------------------




ASSUME DS:CODE32, CS:code32, FS:code16, ES:nothing,SS:STACK32
;...........................................................................
;---------------------------------------------------------------------------

CODE32 Segment Para public use32

org 0
ALIGN 4


extrn doserror:WORD

;------------------------------------------
;Mensagens de Erro


PUBLIC dpmi_init



;----------------------------------------------
;Dados Gerais
;

MEMINFOSTRUC DpMI_MEM <>  ;Vari†vel de info da mem¢ria

;-----------------------------------------------
;Estruturas de Erro

ErrorFlag    		DD ?  ;Flag de Erro
SystemErrorHandler 	DD ?  ;Offset do system Error handler


REGS	     Registers <> ;Var de RMint
RES	     DW ?	  ;Real Mode ES
RDS	     DW ?	  ;Real Mode DS
RCS	     DW ?	  ;Real Mode CS

MEssageZ DB 'ISTO ê UM TESTE † rotina PRINT!!',0
TST1 DB '÷ƒ∑∫Ωƒ”∫'

BUff DB 5000 dup(0)


;------------------------------------------------

Global Errorflag:Dword
Global REGS:Registers
Global RES:Word
Global RDS:Word
GLOBAL RCS:WORD

;------------------------------------------------

extrn Simples:Byte
extrn Dupla:Byte
extrn Duplahor:Byte
extrn Duplavert:Byte
extrn Classica:Byte
extrn Dotted:byte



;ÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆ

Global Sys_seterrorHandler:far
Global Alloc_Descriptor:Near
Global Free_Descriptor:Near
Global Set_base:Near
Global Set_limit:Near
Global Set_AccessRights:Near
Global GETMEMINFO:Near
Global SEG2Descriptor:Near
Global GET_BASe:Near
Global CreaTe_Alias:Near
Global Get_descriptor:Near
Global Set_descriptor:Near
Global ALLOC_DOSMEM:Near
Global Dalloc_DOSMEM:Near
Global Resize_DOSMEM:Near
Global GET_RMIntvec:Near
Global SET_RMintvec:Near
Global GET_Except_handler:Near
Global SET_Except_handler:Near
Global GetintVEC:Near
Global SETINTVEC:Near
Global GetVerSion:Near
Global Getmem:Near
Global Freemem:Near
Global RESIZE:Near
Global DOLock:Near
Global UnLock:Near
Global MARK_RMPage:Near
Global RELOCKRM:Near
Global GETPAGESIZE:Near
Global Disable_VIS:Near
Global Enable_VIS:Near
Global Get_VIS:Near
Global RMINT:Near
Global ErrorPrint:Near
Global Quit:Near
Global Call_RM_Far:Near

;ÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆÆ

;EXTRN _main:near
EXTRN MKDIR:NEAR
EXTRN CHDIR:NEAR

EXTRN FNEW:NEAR
EXTRN FOPEN:NEAR
EXTRN FCLOSE:NEAR
EXTRN BLOCKWRITE:NEAR
EXTRN Fseek:near

EXTRN IORESULT:DWORD
EXTRN IO_errorHandler:DWORD

EXTRN SETDOSERRORHANDLER:NEAR
EXTRN DEF_DOSError_handler:near

;===============================================
; Default Error Handler

Sys_DeferrorHandler Proc near
	ARG @Err:dword

	Uses EAX,DS
        Mov ds,sdata32
	Xor Eax,eax
	Mov Eax, @Err
	Mov Errorflag,EAX
	ret
endp

;-----------------------------------------------
;Set error handler

Sys_seterrorHandler Proc FAR
	Arg @OFS:Dword

	Uses EAX
	MOv eax,@OFS
	Mov SystemErrorHandler,EAX
	ret
Endp


;-----------------------------------------------
;MACROS DO SISTEMA

ClrError Macro
	Push 0
	Pop Errorflag
	endm


SetError Macro CD
	push EDI
	Mov Edi,SystemErrorHandler
	Push cd
	Call EDI
	pop EDI
	endm



;=================================================
; Aloca um ou Mais descriptors
; Rets AX- initial sel
;=================================================

Alloc_Descriptor Proc Near
	  	ARG count:word
	  
	  	Uses cx
		ClrError
	  	Mov ax,00h
	  	mov cx,Count
	  	Int 31h
		jnc @AD_quit
		seterror 25
	@ad_quit:
		  ret
Endp
	    

;=================================================
; Liberta um descriptor
;=================================================

Free_Descriptor Proc Near
	ARG Sel:word

	Uses AX,BX
	clrerror
	mOv ax,01h
	Mov bx,sel
	Int 31h
	jnc @FD_quit
	seterror 01

@fd_quit:
	ret
Endp

;=================================================
; Seta a base de um selector
;=================================================

Set_base Proc Near
	Arg Sel:word, Base:Dword
	
	Uses AX,bx,cx,dx
	clrerror
	Mov ax,07h
	Mov bx,Sel
	Mov cx,word ptr Base[2]
	Mov dx,word ptr Base[0]
	Int 31h
	JNC @SB_QUIT
	seterror 7
	
@SB_QUIT:
	ret
Endp	


;=================================================
; Seta o limite de um selector
;=================================================

Set_limit Proc Near
	Arg Sel:word, Base:Dword
	
	Uses AX,bx,cx,dx
	clrerror
	Mov ax,08h
	Mov bx,Sel
	Mov cx,word ptr Base[2]
	Mov dx,word ptr Base[0]
	Int 31h
	JNC @sL_QUIT

	seterror 8
		
@SL_QUIT:	
	ret
Endp


;=================================================
; Seta Direitos de acesso de um descriptor
;=================================================

Set_AccessRights Proc Near
		Arg Sel:word, AR:word
	
		Uses AX,bx,cx
		clrerror
		Mov ax,09h
		Mov bx,Sel
		Mov cx,ar
		Int 31h
		jnc @SAR_quit
		seterror 9
	@sar_quit:	
		ret
Endp


;=================================================
; ObtÇm Info da mem¢ria - retorna em MeminfoStruc
;=================================================

GETMEMINFO PROC Near
	   ARG @p:pointer

	   Uses EDI,EAX,ES
	   clrerror
	   Mov Ax,0500h
	   Mov es,@p.selector
	   Mov edi,@P.ofset
	   Int 31h
	   JNC @GETMI_quit
	   seterror 0500h
	@GetMI_quit:
	   REt
Endp

;DPMI_mem Struc
;	 LB   DD ?	;largest available block in bytes
;	 MUPA DD ?	;maximum unlocked page allocation
;	 MLPA DD ?	;maximum locked page allocation
;	 TLAS DD ?	;total linear address space in pages
;	 TUP  DD ?	;total unlocked pages
;	 FP   DD ?	;free pages
;	 TFP  DD ?	;total physical pages
;	 FLAS DD ?	;free linear address space in pages
;	 SPF  DD ?	;size of paging file/partition in pages
;	 PAD  DD 3 dup(?)  ;reserved
;ENDS

;=================================================
;Retorna um descriptor do RM seg
;=================================================

SEG2Descriptor Proc Near
		ARG @SEGm:word

		Uses BX
		clrerror
		Mov ax,02h
		int 31h
		jnc @S2D_quit
		seterror 2
		
	@s2d_quit:
		Ret
Endp		


;=================================================
;Retorna A base do Selector
;=================================================

GET_BASe Proc Near
	 Arg @S:word

	 Uses Bx,Cx,dx
	 ClrError
	 Mov ax,06h
	 Mov bx,@s
	 Int 31h
	 jnc @GB_quit
	 SetError 6
	
      @GB_quit:
         Mov AX,cx
         SHL EAX,16
         MOV AX,dx
         ret
Endp         


;=================================================
;Cria um seg Alias code
;=================================================

CreaTe_Alias Proc Near
	  Arg @S:word
	  
	  Uses BX
	  Clrerror
	  Mov ax,0ah
	  mov bx,@s
	  int 31h
	  jnc @CG_quit
	  seterror 0Ah
	@Cg_quit:
	  Ret
Endp

;=================================================
;ObtÇm um descriptor
;=================================================

Get_descriptor Proc Near
		Arg @s:word, @PS:WORD,@PO:DWORD

		Uses EAx,Ebx,edi,ES
		clrerror
		Mov Eax,00bh
		Mov bx,@s
		Mov ES,@pS
		Mov Edi,@Po
		Int 31h
		jnc @GD_quit
		Seterror 0bh

	@GD_quit:
		Ret
Endp

;=================================================
;Seta um descriptor
;=================================================

Set_descriptor Proc Near
		Arg @s:word, @P:pointer
		
		Uses Ax,bx,es,edi
		clrerror
		Mov ax,00ch
		Mov bx,@s
		Mov ES,@p.Selector
		Mov Edi,@P.ofset
		Int 31h
		jnc @sD_quit
		Seterror 0ch
		
	@sD_quit:
		Ret
Endp

;=================================================
;Aloca Um bloco de mem¢ria baixa
; Retorna AX- segm, DX-selector, BX-largest Free block
;=================================================

ALLOC_DOSMEM Proc Near
	     Arg @Paragr:word
	     
	     Uses ES
	     clrerror
	     Mov ax,0100h
	     Mov bx,@paragr
	     int 31h
	     jnc @ADM_quit
	     seterror 0100h
	     
	   @Adm_quit:
	     Ret
Endp

;=================================================
;Liberta Um bloco de mem¢ria baixa
;=================================================

Dalloc_DOSMEM Proc Near
	     Arg @sel:word
	     
	     Uses Ax,DX
	     clrerror
	     Mov ax,0101h
	     Mov dx,@sel
	     int 31h
	     jnc @fDM_quit
	     seterror 0101h
	     
	   @fdm_quit:
	     Ret
Endp	       


;=================================================
;Resize Um bloco de mem¢ria baixa
;=================================================

Resize_DOSMEM Proc Near
	     Arg @sel:word,NSZ:Word
	     
	     Uses Ax,BX,dx
	     clrerror
	     Mov bx,nsz
	     Mov ax,0102h
	     Mov dx,@sel
	     int 31h
	     jnc @rDM_quit
	     seterror 0102h
	     
	   @rdm_quit:
	     Ret
Endp

;=================================================
;Retorna CX:DX RM Int Vect
;=================================================

GET_RMIntvec proc Near
	     Arg @I:byte
	     
	     Uses Ax,bx
	     Clrerror
	     mov bl,@i
	     Mov ax,0200h
	     int 31h
	     jnc @GRMIV_q
	     Seterror 0200h
	  @GRMIV_Q:
	     ret
Endp


;=================================================
;SET RM Int Vect
;=================================================

SET_RMintvec proc Near
	Arg @I:byte, @P:RMpointer
	
	Uses Ax,bx,cx,dx
	clrerror 
	Mov ax,0201h
	mov bl,@i
	Mov cx,@p.rSeg
	Mov dx,@p.rofset
	Int 31h
	jnc @SRMIV_q
	seterror 0201h
	
      @SRMIV_q:
        Ret
Endp

;=================================================
;Get exception handler vec (00-1fh)
;RETS CX:EDX
;=================================================

GET_Except_handler proc Near
	Arg @H:word

	Uses AX,BX
	clrerror
	Mov ax,0202h
	mov bx,@h
	Int 31h
	jnc @GEH_Q
	seterror 0202h

      @GEH_q:
        Ret
EndP

;=================================================
;Set exception handler vec (00-1fh)
;=================================================

SET_Except_handler proc Near
	Arg @H:word, @P:pointer

	Uses AX,BX,CX,EDX
	clrerror
	Mov ax,0203h
	mov cx,@p.selector
	mov EDX,@p.Ofset
	mov bx,@h
	Int 31h
	jnc @SEH_Q
	seterror 0203h

      @SEH_q:
        Ret
EndP

;the handler should return using a Far return
;all fault stack frames contain an error code, but it is only valid for
;exceptions 08h and 0Ah-0Eh
;handlers will only be called if the exception occurs in protected mode,
;and the DPMI host does not transparently handle the exception
;the handler may change certain values on the stack frame

;Format of stack frame for 32-bit programs: (offset from SS:ESP)
;Offset	Size	Description	(Table 2297)
; 00h	DWORD	return EIP (do not change)
; 04h	WORD	return CS selector (do not change)
; 06h	WORD	reserved (do not change)
; 08h	DWORD	error code
; 0Ch	DWORD	EIP of exception
; 10h	WORD	CS selector of exception
; 12h	WORD	reserved (do not change)
; 14h	DWORD	EFLAGS
; 18h	DWORD	ESP
; 1Ch	WORD	SS
; 1Eh	WORD	reserved (do not change)


;=================================================
;GET PM Intvec
; RETS Cx:EDX
;=================================================

GetintVEC proc Near
	  Arg @i:Dword

	Uses AX,BX
	clrerror
	Mov ax,0204h
	mov ebx,@i
	Int 31h
	jnc @giv_Q
	seterror 0204h

      @giv_q:
        Ret
EndP


;=================================================
;Set PM intvec
;=================================================

SETINTVEC proc Near
	 Arg @i:dword, @P:pointer

	Uses AX,BX,CX,EDX
	clrerror
	Mov ax,0205h
	mov cx,@p.selector
	mov EDX,@p.Ofset
	mov ebx,@i
	Int 31h
	jnc @siv_Q
	seterror 0205h

      @siv_q:
        Ret
EndP


;=================================================
;GET Dpmi Version
;AH = major version of DPMI spec supported
;AL = two-digit minor version of DPMI spec supported
;BX = DPMI host flags (see #2302)
;CL = processor type (02h=80286, 03h=80386, 04h=80486)
;DH = curr value of virtual master interrupt controller base interrupt
;DL = curr value of virtual slave interrupt controller base interrupt
;=================================================

GetVerSion Proc Near
		
	Mov Ax,0400h
	clrerror
	int 31h
	jnc @GV_q
	Seterror 0400h

	@GV_Q:
	ret
Endp	

;------------------------------------------------------------------
;Alloc Mem Block
;RETS EDX-linadr
;------------------------------------------------------------------

Msignature DD 0ABC3FF00h       ;Assinatura no bloco
Minsize    DD 32               ;Tamanho m°nimo do bloco
ReservedSZ DD 16               ;Bytes a mais por bloco reservados



Getmem proc Near
 	Arg @S:Dword

	Uses EAX,EBX,ECX,Esi,Edi,DS,ES
	clrerror
        Mov ES,Sdata0
        Mov Ds,sData32
        MOV EBX,@S
        Add Ebx,ReservedSz
        Cmp Ebx,Minsize
        jnb @@GETMEM_Go2

        Mov Ebx,Minsize

   @@GETMEM_go2:
	Mov ax,0501h
	Mov Cx,bx
	SHR ebx,16
	Int 31h
	jc @gm_E
	SHL ebx,16
	Mov bx,cx
	mov eax,ebx
	SHL EDI,16
	Mov di,si
	mov EDX,EDI
        Mov EDi,MSignature
        Mov ES:[EAX],EDI
        MOV ES:[EAX+4],EDX
        Add EAX,ReservedSz
        Mov EDX,EAX
	jmp @gm_q
      @gm_e:
	seterror 0501h

      @gm_q:
        Ret
EndP


;------------------------------------------------------------------
;Free Mem Block
;------------------------------------------------------------------

Freemem proc Near
 	Arg @o:Dword

	Uses Eax,Esi,Edi,ES
	clrerror

        Mov Es,Sdata0
        MOV ESI,@O
        SUB ESI,ReservedSZ
        Mov EAX,ES:[ESI]
        Cmp Eax,Msignature
        JNE @FM_q
        MOV EAX,ES:[ESI+4]
        Mov Di,AX
        SHR EAX,16
        mov si,ax
	Mov ax,0502h
	Int 31h
	jnc @fm_q
	seterror 0502h

      @fm_q:
        Ret

EndP


;=================================================
;Resize Mem block
;Rets EAX-linadr
;EDX-handle
;=================================================

RESIZE proc Near
 	Arg @h:Dword,@newsz:dword
	
	Uses BX,CX,si,di
	clrerror
	Mov ax,0503h
	Mov Bx,word ptr @newsz[2]
	Mov CX,word ptr @newsz[0]
	Mov si,word ptr @h[2]
	Mov di,word ptr @h[0]
	Int 31h
	jc @rsm_e
	Mov Ax,bx
	shl eax,16
	mov ax,cx
	Mov dx,si
	shl edx,16
	mov dx,di
	jmp @rsm_q
      @rsm_e:
	seterror 0503h

      @rsm_q:
        Ret
EndP

;any selectors pointing at the block must be updated
;the previous memory block handle becomes invalid
;an error is returned if the new size is 0


;=================================================
;Lock linear region
;=================================================

DOLock proc Near
 	Arg @linadr:Dword,@Sz:Dword
	
	Uses ax,bx,cx,si,di
	clrerror
	Mov ax,0600h
	Mov si,word ptr @sz[2]
	Mov di,word ptr @sz[0]
	Mov bx,word ptr @linadr[2]
	Mov cx,word ptr @linadr[0]
	Int 31h
	jnc @dlm_q
	seterror 0600h

      @dlm_q:
        Ret

EndP
;pages at beginning and end will be locked if the region overlaps them
;may be called multiple times for a given page; the DPMI host keeps a
;lock count for each page


;=================================================
;unLock linear region
;=================================================

UnLock proc Near
 	Arg @linadr:Dword,@Sz:Dword

	Uses ax,bx,cx,si,di
	clrerror 
	Mov ax,0601h
	Mov si,word ptr @sz[2]
	Mov di,word ptr @sz[0]
	Mov bx,word ptr @linadr[2]
	Mov cx,word ptr @linadr[0]
	Int 31h
	jnc @lm_q
	seterror 0601h

      @lm_q:
        Ret

EndP

;=================================================
;MARK rm region as pageable
;=================================================

MARK_RMPage proc Near
        Arg @linadr:Dword,@Sz:Dword
	
	Uses ax,bx,cx,si,di
	clrerror
	Mov ax,0602h
	Mov si,word ptr @sz[2]
	Mov di,word ptr @sz[0]
	Mov bx,word ptr @linadr[2]
	Mov cx,word ptr @linadr[0]
	Int 31h
	jnc @mrmp_q
	seterror 0602h
	
      @mrmp_q:
        Ret

EndP	

;=================================================
; RELOCK REAL MODE REGION
;=================================================

RELOCKRM proc Near
 	Arg @linadr:Dword,@Sz:Dword

	Uses ax,bx,cx,si,di
	clrerror
	Mov ax,0603h
	Mov si,word ptr @sz[2]
	Mov di,word ptr @sz[0]
	Mov bx,word ptr @linadr[2]
	Mov cx,word ptr @linadr[0]
	Int 31h
	jnc @rlrm_q
	seterror 0603h

      @rlrm_q:
        Ret

EndP	

;=================================================
;GET PAGE SIZE
;EAX-page Size
;=================================================

GETPAGESIZE proc Near

	Uses bx,cx
	clrerror
	Mov ax,0604h
	Int 31h
	jc @gpsz_e
	mov ax,bx
	shl EAX,16
	mov ax,cx
	jmp @gpsz_q

      @gpsz_e:
        Seterror 0604h
         
      @gpsz_q:
        Ret

EndP


;=================================================
;GET AND DISABLE VIRTUAL INTERRUPT STATE
;Al- prev Vstate
;=================================================

Disable_VIS proc Near

	clrerror
	Mov ax,0900h
	Int 31h
	mov ah,0
	jnc @dvis_q
        Seterror 0900h
         
      @dvis_q:
        Ret

EndP


;=================================================
;GET AND ENABLE VIRTUAL INTERRUPT STATE
;Al- prev Vstate
;=================================================

Enable_VIS proc Near

	clrerror
	Mov ax,0901h
	Int 31h
	mov ah,0
	jnc @evis_q
        Seterror 0901h
         
      @evis_q:
        Ret

EndP


;=================================================
;GET VIRTUAL INTERRUPT STATE
;Al- Vstate
;=================================================

Get_VIS proc Near

	clrerror
	Mov ax,0902h
	Int 31h
	mov ah,0
	jnc @gvis_q
        Seterror 0902h
         
      @gvis_q:
        Ret

EndP



;=================================================
;Executa uma int em modo real
;=================================================

RMINT Proc Near
      ARG R_int:word

      uses ES,DS
      clrError
      MOV dword ptr Regs._DI,EDI
      MOV dword ptr Regs._sI,EsI
      MOV dword ptr Regs._bp,0
      MOV dword ptr Regs._bx,ebx
      MOV dword ptr Regs._dx,edx
      MOV dword ptr Regs._cx,ecx
      MOV dword ptr Regs._ax,eax
      Mov Ax,0300h
      Mov bx,R_int
      mov bh,0
      PUSH res
      pop regs._es
      push RDS
      Pop regs._ds
      PUSH RSS
      Pop regs._ss
      Mov word ptr regs._sp,0
      Push 202h
      Pop Regs.flags
      Mov  cx,0
      push Sdata32
      pop es
      push edi
      Mov edi,offset regs
      Int 31h
      pop EDI
      jnc @RI_noproblem
      SetError 300h
      jmp @ri_quit

@Ri_noproblem:

	mov ax,Regs.Flags
	xchg ah,al
	SAHF
	Mov EAx,dword ptr Regs._AX
	mov EBX,dword ptr regs._BX
	Mov ECX,dword ptr regs._CX
	Mov EDX,dword ptr regs._DX
	Mov ESI,dword ptr regs._si
	Mov EDi,dword ptr regs._di

@RI_quit:
      ret
Endp



;HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
; R O T I N A S  D E  I N I C I A L I Z A Ä A O
;HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH


;*****************************************
; Rotina de Print em PM
;*****************************************

ErrorPrint Proc Near
	ARg Msg:rmpointer

	Uses ax,dx
	Mov ah,09h
	Push MSG.Segm
	pop RDS
	Mov dx,Msg.rofset
	INTR 21h
	ret
endp

;##########################################################################
; RMBUFFER ROUTINES
;##########################################################################

RMbuf_seg    DW ?      ;real mode buf segm
RMBUF_OFFSET DD ?      ;Linear Addx of buf segm
RMBUF_Sel    DW ?      ;RM buf selector

;---------------------------------------------------------------------------

;------------------------------------------------
;Function RMbuf_init:Boolean;
;------------------------------------------------

Rmbuf_init proc near

           Uses EBX,ECX,EDX

           MOV Bx,4095
           Mov Ax,0100h
           Int 31h
           jc @RMinit_err

           Mov RMbuf_seg,AX
           Mov RMbuf_sel,DX
           MOVZX eBx,ax
           shl ebx,4
           Mov Rmbuf_offset,EBX
           mov AX,true
           jmp @RMBUF_quit

 @RMinit_err: mov ax,false

 @RMBuf_quit: ret
Endp

;----------------------------------------------
;Procedure RMbuf_free;
;----------------------------------------------

RMBUF_Free Proc near

           Uses EAX,EDX

           MOV AX,0101h
           Mov dx,RMbuf_sel
           cmp dx,0
           je @RMFREE_quit
           int 31h

 @RMfree_quit:
           ret
Endp


;------------------------------------------------
;Procedure CopyReal(SRC:pointer;Count:Dword);
;------------------------------------------------

CopyReal Proc Near
         Arg @SRC:pointer,@cnt:Dword

         USES ESI,EDI,EAX,EBX,ECX,edx,DS,es

         Mov bx,sdata0
         Mov es,bx
         moV Edi,Rmbuf_Offset
         Mov ds,@SRC.selector
         Mov ESI,@SRC.OFSET
         Xor ecx,ecx
         Mov EAX,@CNT
         mov cx,ax
         CLD
         REP movsb
         ret
EndP

;-------------------------------------------
; Procedure DosPrint(MSG:Pointer);
;-------------------------------------------

DOSprint Proc Near
         Arg @MSG:Pointer

         Uses EAX,EDX
         Call CopyReal pascal,@MSG.Selector,@Msg.ofset,80
         Mov ah,09h
         push RMbuf_seg
         pop RDS
         Mov dx,0
         Intr 21h
         ret
Endp
;##########################################################################
; EXCEPTION ROUTINES
;##########################################################################
ALIGN 4

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

ERROR_TABLE DW Offset EXCEP00MSG,Offset EXCEP01MSG,Offset EXCEP02MSG
            DW Offset EXCEP03MSG,Offset EXCEP04MSG,Offset EXCEP05MSG
            DW Offset EXCEP06MSG,0,Offset EXCEP08MSG,0
            DW Offset EXCEP0AMSG,Offset EXCEP0BMSG,Offset EXCEP0CMSG
            DW Offset EXCEP0DMSG,Offset EXCEP0EMSG


;----------------------------------------------
; Procedure Harsh_quit;
;----------------------------------------------

Harsh_quit Proc Near

           Mov ds,FS:Sdata32
           CALL DUMP_REGS
           Mov ax,02h
           Call Quit pascal,02h
Endp



Save_regs :
          push Bp
          Mov bp,SP                    ;SF AT BP+4
          push ds                      ;SF AT BP+6
          MOV DS,FS:Special_data32
          MOV dword ptr ERegs._DI,EDI
          MOV dword ptr ERegs._sI,EsI
          MOV dword ptr ERegs._bx,ebx
          MOV dword ptr ERegs._dx,edx
          MOV dword ptr ERegs._cx,ecx
          MOV dword ptr ERegs._ax,eax
          MOV word ptr ERegs._fs,fs
          MOV word ptr ERegs._gs,gs
          MOV word ptr ERegs._es,es

          mov eax,EBP
          Mov ax,[bp+2]
          MOV dword ptr ERegs._bp,eax
          Mov ax,[bp+4]
          Mov word ptr Eregs._DS,ax
          MOV ax,[BP+22]
          Mov word ptr Eregs._CS,ax
          Mov ax,[bp+34]
          Mov word ptr Eregs._SS,ax
          MOV EAX,[BP+18]
          Mov E_IP,EAX
          MOV EAX,[Bp+30]
          Mov E_SP,EAX
          MOV EAX,[BP+26]
          MOV E_FLAGS,EAX
          POP DS
          POP BP
          RETN

;----------------------------------------------
;Exception Handlers
;----------------------------------------------

EXCEption_00 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,0
             jmp @General_handler
ENDP

EXCEption_01 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,1
             jmp @General_handler
ENDP

EXCEption_02 Proc far

             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,2
             jmp @General_handler
ENDP

EXCEption_03 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,3
             jmp @General_handler
ENDP

EXCEption_04 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,4
             jmp @General_handler
ENDP

EXCEption_05 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,5
             jmp @General_handler
ENDP

EXCEption_06 Proc far

             CALL near ptr save_regs
             push BP
             Mov bp,sp
             Push ESI EAX EBX ECX EDX DS ES
             Mov ebx,6
             jmp @General_handler
ENDP

;---------------------------------------------------
;Exception handler para as excep. 08,0A,0B,0C,0D,0E
;---------------------------------------------------

EXCEPTION_HANDLER Proc FAR

                  CALL near ptr save_regs
                  push BP
                  Mov bp,sp
                  Push ESI EAX EBX ECX EDX DS ES
                  MOv EBX,[BP+10]
@General_handler:
                  Mov CX,FS:Sdata32
                  MOV DX,CS
                  Mov [BP+6],DX
                  MOV EDX,offset HARSH_QUIT
                  MOV [BP+2],EDX
                  sti
                  MOV ax,0901h
                  Int 31h

                  cmp ebx,0eh
                  jg @Excep_quit

                  MOV Esi,offset Error_table
                  SHL ebx,1
                  ADD esi,ebx
                  Xor ebx,ebx
                  MOV BX,CS:[ESI]
                  cmp EBX,0
                  je @Excep_quit

                  xor ax,ax
                  Mov al,initial_mode
                  Intr 10h
                  Call Dosprint Pascal,CX,EBX

  @EXCEP_quit:
                  POP ES DS EDX ECX EBX EAX ESI
                  POP BP
                  ret
Endp

;=========================================================================
ALIGN 4

EREGS Registers <>    ;Exception struc REGS
E_flags DD ?          ;Exception flags
E_IP    DD ?          ;Exception locus EIP
E_SP    DD ?          ;Exception locus ESP
ALIGN 4

STR_MSG1 DB 0AH,0DH,'Register contents',0ah,0dh,0ah,0dh,'$'
STR_EAX  DB '   EAX =                       ',0AH,0DH,'$'
STR_EBX  DB '   EBX =                       ',0AH,0DH,'$'
STR_ECX  DB '   ECX =                       ',0AH,0DH,'$'
STR_EDX  DB '   EDX =                       ',0AH,0DH,'$'
STR_CS  DB '   CS  =                       ',0AH,0DH,'$'
STR_IP  DB '   EIP =                       ',0AH,0DH,'$'
STR_DS  DB '   DS  =                       ',0AH,0DH,'$'
STR_si  DB '   ESI =                       ',0AH,0DH,'$'
STR_ES  DB '   ES  =                       ',0AH,0DH,'$'
STR_DI  DB '   EDI =                       ',0AH,0DH,'$'
STR_SS  DB '   SS  =                       ',0AH,0DH,'$'
STR_Sp  DB '   ESP =                       ',0AH,0DH,'$'
STR_FS  DB '   FS  =                       ',0AH,0DH,'$'
STR_GS  DB '   GS  =                       ',0AH,0DH,'$'
STR_BP  DB '   EBP =                       ',0AH,0DH,'$'
STR_FLags  DB '   flags =                    ',0AH,0DH,'$'

LINEbr db 0AH,0DH,'$'

REGSTR DB 20 dup(0)

;-----------------------------------------------------------------
;Procedure DumpREgs;
;-----------------------------------------------------------------

Dump_regs Proc Near

         uses eax,EBX,ECX,EDX,DS
         MOV DS,FS:SDATA32
         MOV AL,'0'
         MOV PAD_CHAR,al
         mov EAX,16
         MOV BASE_NUMBER,16
         mOV AL,'h'
         MOV SUFFIX,Al
         mov ax,8
         Mov field_len,ax
         mov al,' '
         mov terminator,al

         Mov ax,offset STR_MSG1
         Call Dosprint Pascal,dS,EAX

         Mov eax,offset STR_EAX
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,dword ptr eRegs._AX
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_EbX
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,dword ptr eRegs._BX
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_ECX
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,dword ptr eRegs._CX
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_EDX
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,dword ptr eRegs._DX
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,4
         Mov field_len,ax

         Mov eax,offset STR_CS
         mOV EBX,EAX
         ADD EAX,11
         MovZX EDX,word ptr eRegs._CS
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,8
         Mov field_len,ax

         Mov eax,offset STR_ip
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,E_IP
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,4
         Mov field_len,ax

         Mov eax,offset STR_DS
         mOV EBX,EAX
         ADD EAX,11
         MovZX EDX,word ptr eRegs._DS
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,8
         Mov field_len,ax

         Mov eax,offset STR_SI
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,DWOrD PTR EREGS._SI
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,4
         Mov field_len,ax

         Mov eax,offset STR_ES
         mOV EBX,EAX
         ADD EAX,11
         MovZX EDX,word ptr eRegs._ES
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,8
         Mov field_len,ax

         Mov eax,offset STR_dI
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,DWOrD PTR EREGS._dI
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,4
         Mov field_len,ax

         Mov eax,offset STR_FS
         mOV EBX,EAX
         ADD EAX,11
         MovZX EDX,word ptr eRegs._FS
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_GS
         mOV EBX,EAX
         ADD EAX,11
         Movzx EDX,word ptr eRegs._GS
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_sS
         mOV EBX,EAX
         ADD EAX,11
         Movzx EDX,word ptr eRegs._sS
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         mov ax,8
         Mov field_len,ax

         Mov eax,offset STR_SP
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,E_sp
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset STR_bp
         mOV EBX,EAX
         ADD EAX,11
         Mov EDX,dword ptr Eregs._bp
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         Mov eax,offset str_FLAGS
         mOV EBX,EAX
         ADD EAX,13
         Mov EDX,e_flags
         Call INT2STR Pascal,EDX,DS,EaX
         CALL DOsprint Pascal,dS,EBX

         RET
eNDP


;=========================================================================

OLDEXCEPTION_TABLE DW 84 dup(0)

EXCEPTION_HANDLER_TABLE DW Offset Exception_00,Offset Exception_01
                        DW Offset Exception_02,Offset Exception_03
                        DW Offset Exception_04,Offset Exception_05
                        DW Offset Exception_06,0
                        DW Offset Exception_handler,0
                        DW Offset Exception_handler,Offset Exception_handler
                        DW Offset Exception_handler,Offset Exception_handler
                        DW Offset Exception_handler,0
;=========================================================================

;------------------------------------------------------
; Procedure SetsysExceptions;
;------------------------------------------------------

SetSysExceptions Proc near

                 USES EBX,ECX,EDX,EDI,ESI,es
                 MOV edi,offset OLDexception_table
                 Mov ESI,Offset exception_handler_table
                 MOV ES,sdata32

                 Mov bx,0

      @SETSYsExCEP_loop:

                 Mov ax,0202h
                 int 31h
                 jc @SETEXCEP_err

                 MOV Ax,cx
                 STOSW
                 MOV EAX,EDX
                 STOSW
                 MOVZX EDX,word ptr ES:[ESI]
                 inc esi
                 inc esi
                 cmp edx,0
                 je @Loop_skip

                 Mov ax,0203h
                 Mov Cx,cs
                 int 31h
                 jc @SETEXCEP_err
      @loop_skip:
                 INC bX
                 cmp bx,15
                 jb @SETSYsExCEP_loop

                 mov ax,true
                 jmp @SetExcep_quit

  @SetEXcep_ERR:
                 Mov ax,false

  @SetExcep_quit:
                 ret
Endp

;------------------------------------------------------
;Procedure RestoresysExceptions;
;------------------------------------------------------

RESTORESysExceptions Proc near

                 USES EAX,EBX,ECX,EDX,EsI,ds

                 MOV esi,offset OLDexception_table
                 MOV DS,sdata32
                 Mov bx,0

      @RESTOREExCEP_loop:

                 LODSW
                 Mov cX,ax
                 LODSD
                 MOV edx,eax
                 Mov AX,0203h
                 int 31h
                 jc @RESTORE_err
                 INC bX
                 cmp bx,15
                 jb @RESTOREExCEP_loop

                 mov ax,true
                 jmp @RESTOREExcep_quit

  @RESTORE_ERR:
                 Mov ax,false

  @RESTOREExcep_quit:
                 ret
Endp

;##########################################################################
; EXTRY/EXIT ROUTINES
;##########################################################################

Error100 DB 'ERRO #100: Erro alocando bloco de mem¢ria Baixa.',0ah,0dh,7,'$'
Error101 DB 'ERRO #101: Mem¢ria extendida insuficiente para correr a aplicaáÑo.',0ah,0dh,7,'$'
Error102 DB 'ERRO #102: NÑo foi poss°vel setar todos os vectores de EXCEPTION.',0ah,0dh
         DB 'Se estiver numa janela DOS, Feche-a. Consulte a documentaáÑo para',0ah,0dh
         DB 'mais informaáîes.',0ah,0dh,7,'$'
;------------------------------------------------------------------

;------------------------------------------------
;Quit Routines
;------------------------------------------------

Quit Proc Near
         Arg @RetCode:Word

        Mov Ds,sdata32
        MOV ES,SDAta0
        Call Call_links
        Call Disposeproclist

        MOV Cx,Sdata0
        Mov dx,sdata16
	Push cs
	Push CS
	POP ES
	POP FS
	mOv ax,01h
	Mov bx,cx
	Int 31h
        Mov ax,01h
        Mov bx,dx
        int 31h
        mov ax,@Retcode
	MOV Ah,04ch
        int 21h
        ret
Endp


;------------------------------------------------
;Procedure SetupThings;
;------------------------------------------------

SetupThings Proc Near

            USES EAx,EBX,ECX,EDX,ES
            Call Initproclist                  ;inicializa a lista de sa°da

            Xor eax,eax
            Mov rmbuf_sel,ax
	    mov EAX,offset sys_deferrorhandler
	    Call sys_seterrorHandler Pascal,eax	        ;DPMI Error Handler

            Call RMbuf_init
            cmp ax,true
            je @@THINGS_GO1
            Mov Ax,offset Error100
            jmp @@Things_quit

 @@Things_Go1:
            MOV EAX,Offset RMbuf_free
            Call AddProcnode Pascal,EAX

            MOV EDI,offset Meminfostruc
            MOV Ax,0500h
            Push ds
            Pop es
            Int 31h
            Mov ES,Sdata0
            jc @MEM_error
            mov Eax,heapmin
	    Cmp meminfoStruc.LB,eax                         ;Chek memory
	    jge @@Things_GO2

 @Mem_error:
            Mov ax,Offset Error101
            jmp @@Things_quit

 @@Things_Go2:
            Call SetSysExceptions
            cmp al,false
            jne @@THings_go3
            Mov ax,Offset Error102

 @@THINGS_quit:
    	    Call Errorprint pascal,code32,ax
            mov ax,01h
            Call quit pascal,ax

 @@Things_Go3:
            MOV EAX,Offset RestoreSysexceptions
            Call AddProcnode Pascal,eax
            ret

Endp

;##########################################################################
; MISC ROUTINES
;##########################################################################


GLOBAL STRINGLEN:NEAR
GLOBAL COPYREAL:NEAR
GLOBAL MOVE:NEAR
GLOBAL DOSPRINT:NEAR

GLOBAL RMBUF_SEG:WORD
GLOBAL RMBUF_OFFSET:DWORD

;----------------------------------------------
;function StringLen(S:pointer):Word;
;----------------------------------------------

StringLen proc near
          Arg @S:Pointer

          USES EDI,ECX,ES
          MOV es,@S.selector
          mOv EDI,@S.ofset
          Mov ECX,0FFFFh
          xor ax,ax
          cld
          repne SCASB
          Mov EAX,0fffeh
          sub EAX,ECX
          ret
Endp


;--------------------------------------------------
;function SGETChar(S:pointer;I:dWord):Char(al);
;-------------------------------------------------

SGETCHAR PRoC NEAR
          Arg @S:Pointer,@I:dWORD

          USES ESI,ECX,DS
          call stringlen pascal,@S.selector,@S.ofset
          cmp eax,@I
          jb @STRINGCHAR_Quiterr

          MOV Ds,@S.selector
          mOv ESI,@S.ofset
          Mov ECX,@I
          cld
          rep Lodsb
          jmp @Stringchar_quit

 @STRINGCHAR_Quiterr:
          Mov ax,0

 @STRINGCHAR_Quit:
          ret
Endp

;--------------------------------------------------
;Procedure SSETChar(S:pointer;I:dWord;C:WORD);
;-------------------------------------------------

SSETCHAR PRoC NEAR
          Arg @S:Pointer,@I:dWORD,@C:WORD

          USES ESI,ECX,DS
          call stringlen pascal,@S.selector,@S.ofset
          cmp eax,@I
          jb @STRINGSCHAR_Quit

          MOV Ds,@S.selector
          mOv ESI,@S.ofset
          Mov ECX,@I
          cld
          rep Lodsb
          Mov ax,@C
          MOV DS:[ESI],AL

 @STRINGsCHAR_Quit:
          ret
Endp


;--------------------------------------------------
;Function PSTRING(L:dWord):OBJPTR(EDX);
;-------------------------------------------------

PSTRING PRoC NEAR
          Arg @L:dWORD

          USES ES,EDI,EAX,ECX,EBX
          MOV EDX,@L
          add EDX,17
          CALL GETmem Pascal,EDX
          cmp edx,0
          jne @Pstring_INIT
          XOR edx,edx
          XOR EBx,ebx
          jmp @Pstring_quit

 @Pstring_init:
          MOV EDI,EDX
          MOV ES,Sdata0
          MOV AX,0
          Mov Ecx,@L
          INC ECX
          CLD
          Rep Stosb

 @PSTRING_Quit:
          ret
Endp


;-------------------------------------------------------------------

BASE_NUMBER DW 10         ;Base do Nß a ser convertido para string
FIELD_LEN   DW 1          ;Tamanho m°nimo do Campo
PAD_char    Db ' '        ;Caracter de PAD
SUFFIX      DB 0          ;Sufixo
Terminator  DB 0          ;Terminador da string

align 4

;-----------------------------------------------------
;Procedure INT2STR(Number:DWord;;SDEST:Pointer);
;-----------------------------------------------------

INT2STR Proc near
    Arg @N:DWORD,@S:Pointer

    USES EAX,ebx,ecx,edx,edi,es
    MOV EAX,@N
    MOVZX EBX,BASE_number
    Xor eCX,ecx

@SL1:
    XOR eDX,eDX
    Div EBX
    PUsh DX
    INC CX
    cmp ax,0
    jne @SL1

    MOV ES,@S.Selector
    MOV EDI,@S.OFSET

    Mov ax,Field_len
    Cmp ax,cx
    jbe @S_store

    Mov Bx,CX
    SUB ax,CX
    Movzx Ecx,ax
    Mov al,Pad_char
    cld
    REP STOSB
    MOV Cx,bx

 @S_Store:
    Pop AX
    Add al,30h
    Cmp al,57
    jbe @S_GONEXT
    add al,7

 @S_gonext:
   STOSB
   Loop @S_store

   Mov al,suffix
   cmp al,0
   je @S_Terminator
   Stosb

@S_terminator:
   Mov al,terminator
   stosb
   ret
Endp

;------------------------------------------------------------
;Procedure Set_Conv_info(c:dword);
; Formato de C: 0->(L-Padchar,H-suffix) 16->(L-BASE,H-Flen)
;------------------------------------------------------------

SET_Conv_info Proc Near
          ARG @D:Dword

          USES EAX,EDX,DS
          Mov ds,sdata32
          Mov EAX,@D

          Xor Dx,dx
          Mov Pad_char,al
          Mov suffix,ah
          SHR EAX,16
          Mov dl,al
          Mov Base_number,DX
          Mov dl,ah
          Mov Field_len,DX
          ret
Endp

ALIGN 4
;--------------------------------------------------------
;Procedure PString2DOS(S:pointer);
;--------------------------------------------------------

Pstring2DOS Proc near
            ARG @S:pointer

          USES EAX,EDI,ECX,ES
          MOV es,@S.selector
          mOv EDI,@S.ofset
          xor ax,ax
          cld
          repne SCASB
          Mov al,'$'
          Stosb
          ret
Endp

;--------------------------------------------------------
;Procedure DOS2Pstring(S:pointer);
;--------------------------------------------------------

DOS2PSTRING Proc near
            ARG @S:pointer

          USES EAX,EDI,ECX,ES
          MOV es,@S.selector
          mOv EDI,@S.ofset
          Mov ECX,0FFFFh
          Mov al,'$'
          cld
          repne SCASB
          Xor ax,ax
          Stosb
          ret
Endp



;--------------------------------------------------------
;Procedure MOVE(SRC:POINTER;DEst:POINTER;count:DWORD);
;--------------------------------------------------------

MOVE proc near
     ARG @S:Pointer,@D:pointer,@C:dword

     USES ECX,ESI,EDI,ES,DS
     MOV DS,@S.selector
     MOV ES,@D.selector
     MOV ESI,@S.ofset
     MOV EDI,@D.ofset
     MOV ECX,@C
     CLD
     REP movsb
     ret
ENDP

;##########################################################################
; REAL MODE CallBack
;##########################################################################

CallBack_STRUCt REGISTERS <>


;-----------------------------------------------------------------
;Function ALLOc_RM_callback(Proc2call:pointer):RMpointer(CX:DX)
;-----------------------------------------------------------------

ALLOC_RM_CallBACK Proc NEAR
                  ARG @P:Pointer

                  USES EAX,EBX,ESI,EDI,ES,DS
                  CLRERROR
                  MOV AX,0303h
                  Mov ES,SDAta32
                  MOV EDI,offset CallBack_Struct
                  Mov Ds,@P.selector
                  MOV ESI,@P.ofset
                  Int 31h
                  jnc @A_CallBK_quit

                  MOV DS,sdata32
                  SetError 303h
                  Xor CX,cx
                  Xor Dx,dx

        @A_callBK_quit:
                  RET
ENDP

;real mode call structure is static, causing reentrancy problems
;the called procedure must modify the real mode CS:IP before returning
;values are returned to real mode by modifying the real mode call struc
;real-mode callback procedure is called with:
;	DS:(E)SI = selector:offset of real mode SS:SP
;	ES:(E)DI = selector:offset of real mode call structure
;	SS:(E)SP = locked protected mode API stack
;	interrupts disabled
;Return: (with IRET)
;	ES:(E)DI = selector:offset of real mode call structure to restore


;-----------------------------------------------------------------
;Procedure Free_RM_callBack(RSEG:DWORd,ROFS:DWORD);
;-----------------------------------------------------------------

Free_RM_CallBack Proc Near
                 Arg @RS:DWORd,@RO:DWORD

                 USES EAX,ECX,EDX
                 CLRERROR
                 Mov ax,0304h
                 Mov Ecx,@RS
                 MOV EDX,@RO
                 Int 31h
                 jnc @FCBK_quit
                 Seterror 304h

          @FCBK_Quit:
                 RET
Endp


;--------------------------------------------------------------
;Procedure Call_RM_far proc Near
;--------------------------------------------------------------

Call_RM_far Proc Near

      uses ES,DS
      clrError
      Mov ds,FS:sdata32
      MOV dword ptr Regs._DI,EDI
      MOV dword ptr Regs._sI,EsI
      MOV dword ptr Regs._bp,0
      MOV dword ptr Regs._bx,ebx
      MOV dword ptr Regs._dx,edx
      MOV dword ptr Regs._cx,ecx
      MOV dword ptr Regs._ax,eax
      Mov Ax,0301h
      mov bh,0
      PUSH res
      pop regs._es
      push RDS
      Pop regs._ds
      PUSH RSS
      Pop regs._ss
      Mov word ptr regs._sp,0
      Push 202h
      Pop Regs.flags
      Mov  cx,0
      push Sdata32
      pop es
      push edi
      Mov edi,offset regs
      Int 31h
      pop EDI
      jnc @CRF_noproblem
      SetError 301h
      jmp @CRF_quit

@CRF_noproblem:

	mov ax,Regs.Flags
	xchg ah,al
	SAHF
	Mov EAx,dword ptr Regs._AX
	mov EBX,dword ptr regs._BX
	Mov ECX,dword ptr regs._CX
	Mov EDX,dword ptr regs._DX
	Mov ESI,dword ptr regs._si
	Mov EDi,dword ptr regs._di

@CRF_quit:
      ret
Endp


;##########################################################################
; DEBUG Traps
;##########################################################################

OLDINT03  Pointer <>

DEBUG_trap Proc near
           PushA
           STI
           Call DumP_regs
;           Xor ax,ax
;           intr 16h
           POPA
           ret
Endp


;##########################################################################
; EXTERNAL MODULES/HEADERS
;##########################################################################

INCLUDE CRT.INC

;--------------------------------------------------------------------------


EXTRN IORESULT:DWORD

TESTMSG DB 'ê o caos que rege a harmonia do ser.',0Ah,0Dh,'$'


special_data32 dw ?  ;SYSTEM CODE ALIAS
Flat_data32    DD ?  ;Physical CODE32 ADDX

;##########################################################################
; DPMI ENTRY POINT
;##########################################################################

DPMI_INIT:
	push ds
	pop fs			;FS- code16

	MOV AX,sdata32
	mov ds,ax			;DS - data32
        MOV SPECIAL_DATA32,AX

	MOV AX,sdata0
	MOV es,ax			;ES - DATa0

        MOV EAX,Code32
        SHL EAX,4
        MOV Flat_data32,EAX

	Call Video_init		;Inicializa video
        Call SetupThings        ;Inicializa o sistema

        MOV EAX,offset Testmsg
        Call Dosprint pascal,ds,eax

        Call SETDOSERRORHANDLER Pascal,offset DEF_DOSERROR_Handler

        Mov eax,32
        Call Getmem pascal,EAx
        cmp errorflag,0
        jne @quitt
        Mov PN,EDX
        Call Scrn_init Pascal,pn
        mOv AX,0
        Mov bx,7
        Call Scrn_color pascal,pn,ax,bx

        Mov EAx,4100
        Call getmem pascal,EAX
        mov vbuf,eax
	MOv ecx,00010001h
        Mov edx,00100010h
        Mov bx,es
        shl ebx,16
        mov bx,es
	CALL Scrn_Getarea Pascal,pn,ecx,edx,sdata0,vbuf


        Call Scrn_cls Pascal,pn
        Mov ax,5
        mov r1,ax
        mov r2,ax
        Mov cx,'c'
        Mov ax,60
        mov bx,19
        Call Scrn_fill Pascal,pn,r1,r2,ax,bx,cx
        mov cx,3
        Call Scrn_scrollup Pascal,pn,r1,r2,ax,bx,cx
        Call Scrn_scrolldown Pascal,pn,r1,r2,ax,bx,cx
        Movzx ECx,sdata32
        Call Scrn_box Pascal,pn,r1,r2,ax,bx,sdata32,offset dupla

	MOv cx,10
	Call Scrn_getchar Pascal,pn,cx,cx
        Mov cx,1
        Call scrn_setchar Pascal,PN,CX,CX,dx
	Call scrn_getatr Pascal,pn,cx,cx
	mov cx,1
        xchg dh,dl
	Call Scrn_setatr Pascal,pn,cx,cx,dx

	mov cx,2
	mov dx,'C'
	mov bx,10
	Call scrn_limpa Pascal,pn,cx,cx,bx,dx
        Mov dx,7
        add cx,3
        Call Scrn_muda Pascal,pn,cx,cx,bx,dx








;	Call ES:[EAX] method screen:init pascal,EAX
;	Call ES:[EAX] method Screen:CLS Pascal,EAX
;	MOv cx,1
;	CALL ES:[EAX] method Screen:putarea Pascal,EAX,cx,cx,sdata32,offset buff
;	mov dx,0
;	MOv f1,dx
;	mov dx,79
;	mov cx,24
;	mov bx,'a'
;	CALL ES:[EAX] method screen:fill Pascal,EAX,f1,f1,dx,cx,bx
;	mov dx,1
;	mov f1,dx
;	Call ES:[EAx] method Screen:printxy Pascal,EAX,f1,f1,Sdata32,Offset MessageZ
;	Call ES:[EAx] method Screen:print Pascal,EAX,Sdata32,Offset MessageZ
;	mov bx,16
;	dec f1
;	Call ES:[EAX] method Screen:Box Pascal,EAX,f1,f1,bx,bx,sdata32,Offset simples
;	MOv cx,1
;	Call ES:[EAX] method Screen:getchar Pascal,eax,cx,cx
;	Call ES:[EAX] method screen:getatr Pascal,eax,cx,cx
;	mov cx,10
;	Call ES:[EAX] method Screen:Setchar Pascal,EAX,cx,cx,dx
;	xchg dl,dh
;	Call ES:[EAX] method Screen:Setatr Pascal,EAX,cx,cx,dx
;	mov cx,2
;	mov dx,'C'
;	mov bx,5
;	Call ES:[EAX] method Screen:limpa Pascal,EAX,cx,cx,bx,dx
;	OBJFree TST
;	mov ax,0
;	intr 16h
;	Call free_descriptor pascal,0ffffh

        MOV EAX,080A6830h
        Call SEt_conv_info Pascal,EAX
        MOV EAX,30
        Mov ebx,255
        Call Getmem Pascal,ebx
        CALL int2str pASCAL,eax,es,edx


        Mov cx,3
        mov r1,cx
        Call Scrn_gotoxy Pascal,pn,r1,r1

        Mov cx,ds
        Call Scrn_Print Pascal,pn,ecx,Offset MessageZ


        xor ebx,ebx
	mov bx,es
        MOv ecx,010001h

	CALL Scrn_putarea Pascal,pn,ecx,sdata0,vbuf
        call freemem pascal,vbuf

        Call Pstring2DOS Pascal,es,pn
        Call DOSprint Pascal,es,pn
        Call ADDPROCNODE Pascal,offset dummy1

        Call Freemem Pascal,EDX
        cmp errorflag,0
        jne @quitt

        Call Freemem pascal,PN
        MOV ES,sdata0
        MOV AX,1
        MOV R1,AX
;       CALL GOTOXY PASCAL,R1,R1
;       cALL SETCURSOR PASCAL,r1
        call Debugint
       Xor eax,eax
        intr 16h

@Quitt:
        Xor eax,eax
	Call Quit Pascal,ax
;---------------------------------------------------------------------------
DStruc Longint 0
Dptr   Longint 0


;------------------------------------------------------
;Function DebugW_Init:boolean(dx);
;------------------------------------------------------

Debug_WInit Proc near

       Uses EAX,EBX,ECX,EDX
       Mov eax,2500
       Call Getmem Pascal,eax
       cmp errorflag,0
       jne @Di_quitE
       Mov dptr,EDX
       Mov eax,40
       Call Getmem Pascal,eax
       cmp errorflag,0
       jne @Di_quitE
       Mov dstruc,EDX
       mov dx,true
       jmp @Di_quit

   @Di_quitE:
      Mov dx,false

   @Di_quit:
      Ret
Endp

_F_CTR DD ?


FOR MACRO L,V1,V2

     PUSH ECX
     xor ecx,ecx
     MOV ECX,V1
     PUSH ECX
     MOV ecx,v2
  &L:
    ENDm


FNEXT MACRO L1
     POP _F_CTR
     PUSH _F_CTR
     CMP ECX,_F_CTR
     INC ECX
     JB &L1
     pop ecx
     pop ECX
     ENDM

;----------------------------------------------------------------------
;PROCEDURE HLINE(X,Y,X1:WORD);
;----------------------------------------------------------------------


hLINE PROC NEAR
      ARG @X:WORD,@Y:WORD,@X1:WORD

      USES EAX,EBX,ECX,EDX
      MOV EDX,Dstruc
      Call scrn_gotoxy Pascal,dstruc,@X,@Y
      Mov al,ES:[EDX][scrn.atr]
      Call Scrn_muda Pascal,Dstruc,@X,@Y,@x1,ax
      mov al,'√'
      Call scrn_setchar Pascal,dstruc,@x,@y,ax
      Mov al,'¥'
      Call scrn_setchar Pascal,dstruc,@x1,@y,ax
      inc @x
      dec @x1
      mov al,'ƒ'
      mov cx,@X
      cld
 @HL_l1:
      Call scrn_setchar Pascal,dstruc,cx,@y,ax
      cmp cx,@X1
      inc cx
      jb @HL_l1

      ret
ENDP

;----------------------------------------------------------------------
;PROCEDURE vLINE(X,Y,Y1:WORD);
;----------------------------------------------------------------------


VLINE PROC NEAR
      ARG @X:WORD,@Y:WORD,@Y1:WORD

      USES EAX,EBX,ECX,EDX
      MOV EDX,Dstruc
      Call scrn_gotoxy Pascal,dstruc,@X,@Y
      Mov al,ES:[EDX][scrn.atr]
      Call Scrn_muda Pascal,Dstruc,@X,@Y,@x1,ax
      mov al,'¬'
      Call scrn_setchar Pascal,dstruc,@x,@y,ax
      Mov al,'¡'
      Call scrn_setchar Pascal,dstruc,@x,@y1,ax
      inc @y
      dec @y1
      mov al,'≥'
      mov cx,@y
      cld
 @vL_l1:
      Call scrn_setchar Pascal,dstruc,@x,cx,ax
      cmp cx,@y1
      inc cx
      jb @vL_l1

      ret
ENDP

;---------------------------------------------------------------------
;PROCEDURE PRINTMASK(X,Y:WORD;l:DWORD);
;---------------------------------------------------------------------


PRINTMASK PROC NEAR
          ARG @X:WORD,@y:WORD,@l:DWORD

          USES EAX,EBX,ECX,EDX
          MOV ECX,32
          MOV EBX,1
          MOV AX,@X
          ADD AX,32
          MOV @X,AX

    @PM_L1:
          MOV EDX,@L
          AND EDX,EBX
          CMP EDX,0
          JNE @PM_IS1
          MOV AL,'0'
          JMP @pm_LOOP

     @PM_IS1:
           MOV AL,'1'

     @PM_LOOP:
          CALL SCRN_SETCHAR PASCAL,DSTRUC,@X,@Y,AX
          DEC @X
          SHL EBX,1
          LOOP @PM_L1

          RET
eNDP



;========================================================================
; DEBUG DATA
;========================================================================

DM_EAX DB 'EAX :',0
DM_EBX DB 'EBX :',0
DM_ECX DB 'ECX :',0
DM_EDX DB 'EDX :',0
DM_ESP DB 'ESP :',0
DM_EBP DB 'EBP :',0
DM_EIP DB 'EIP :',0
DM_CS  DB 'CS :',0
DM_ES  DB 'ES :',0
DM_DS  DB 'DS :',0
DM_SS  DB 'SS :',0
DM_FS  DB 'FS :',0
DM_GS  DB 'GS :',0
DM_VF  DB 'VF :',0
DM_F   DB 'FLAGS :',0
DM_DW  DB '¥ DEBUG WINDOW √',0
DM_FL  DB '¥ --Reservado-- VR NplODITSZ A P C √Ÿ',0
DM_DES DB 'B 24-31 GD0AL16 PplS_T_A√ƒƒƒƒƒƒ Base 0-23 ƒƒƒƒƒ¥√ƒƒ Lim 0-15 ƒƒ¥',0

DM_TS DB 20 DUP(0)

D_REGS    REGISTERS <>
D_EIP     LONGINT ?
D_ESP     LONGINT ?
D_EBP     LONGINT ?
D_EFLAGS  LONGINT ?

D_DESCR   DD ?,?                ;DUMMY DESCRIPTOR

;-------------------------------------------------------------------------

@Descr_CLeaR proc near

          Xor eax,eax
          mov dword ptr d_descr[0],EAX
          mov dword ptr d_descr[4],EAX
          ret
endp


;-------------------------------------------------------------------------
; PROCEDURE sHOWmASKS;
;-------------------------------------------------------------------------

SHOWMASKS PROC NEAR

          USES EAX,EBX,ECX,EDX
          MOV EAX,D_EFLAGS
          MOV BX,44
          MOV CX,23
          CALL PRINTMASK PASCAL,BX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._CS,SDATA32,OFFSET D_DESCR
          MOV DX,45
          MOV BX,13
          MOV CX,16
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._dS,SDATA32,OFFSET D_DESCR
          MOV CX,17
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._ES,SDATA32,OFFSET D_DESCR
          MOV CX,18
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._FS,SDATA32,OFFSET D_DESCR
          MOV CX,19
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._GS,SDATA32,OFFSET D_DESCR
          MOV CX,20
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          call @descr_clear
          CALL GET_DESCRIPTOR PASCAL,D_REGS._SS,SDATA32,OFFSET D_DESCR
          MOV CX,21
          MOV EAX,DWORD PTR D_DESCR[4]
          CALL PRINTMASK PASCAL,BX,CX,EAX
          MOV EAX,DWORD PTR D_DESCR[0]
          CALL PRINTMASK PASCAL,DX,CX,EAX

          ret
endp


;-------------------------------------------------------------------------
; PROCEDURE sHOWVALS;
;-------------------------------------------------------------------------


SHOWVALS PROC NEAR

         USES EAX,EBX,ECX,EDX
         MOV EAX,080A6830h
         Call SEt_conv_info Pascal,EAX
         mov al,0
         mov terminator,al

         MOV EDX,dword ptr D_regs._AX
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         mov ax,9
         mov bx,12
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,dword ptr D_regs._BX
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         inc bx
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,dword ptr D_regs._DX
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         mov ax,28
         call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,dword ptr D_regs._cX
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         dec bx
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,D_ESP
         mov ax,47
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,D_EBP
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         inc bx
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts
         dec bx

         MOV EDX,dword ptr D_eip
         mov ax,66
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         MOV EDX,dword ptr D_eflags
         mov ax,29
         mov bx,23
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         Mov field_len,4

         MOVzx EDX,D_regs._CS
         mov ax,7
         mov bx,16
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         inc bx
         MOVzx EDX,D_regs._dS
         mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         inc bx
         MOVzx EDX,D_regs._eS
         mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         inc bx
         MOVzx EDX,D_regs._fS
         mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         inc bx
         MOVzx EDX,D_regs._gS
         mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         inc bx
         MOVzx EDX,D_regs._sS
         mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts
         inc bx
         inc bx
         mov ax,0902h
         int 31h
         xor ah,ah
         movzx edx,ax
         Mov ax,7
         CALL int2str pASCAL,EDX,DS,offset dm_ts
         Call scrn_printxy pascal,dstruc,ax,bx,sdata32,offset dm_ts

         ret


endp


;-------------------------------------------------------------------------
;pROCEDURE dEBUG_WDRAW;
;-------------------------------------------------------------------------

DEbug_Wdraw Proc near

       Uses EAX,EBX,ECX,EDX
       Call Scrn_init Pascal,Dstruc
       mOv AX,0
       Mov bx,7
       Call Scrn_color pascal,dstruc,ax,bx
       Mov r1,0
       Mov r2,11
       Mov r3,79
       mov r4,24
       Call Scrn_getarea pascal,dstruc,r1,r2,r3,r4,sdata0,dptr
       mov cl,' '
       Call Scrn_fill pascal,dstruc,r1,r2,r3,r4,cx
       Call Scrn_box pascal,Dstruc,r1,r2,r3,r4,sdata32,offset simples
       Mov ax,0
       mov bx,14
       mov cx,79
       Call Hline Pascal,ax,bx,cx
       Mov ax,0
       mov bx,22
       mov cx,79
       Call Hline Pascal,ax,bx,cx

       Mov ax,19
       mov bx,11
       mov cx,14
       Call Vline Pascal,ax,bx,cx
       Mov ax,38
       Call Vline Pascal,ax,bx,cx
       mov ax,58
       Call Vline Pascal,ax,bx,cx

       mov AX,3
       mOV BX,12
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eax
       inc BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eBx
       MOV AX,22
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eDx
       dec bx
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eCx
       mov aX,41
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eSP
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_ebP
       dec bx
       mOV AX,60
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_eIP
       mOV BX,16
       mOV AX,3
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_CS
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_DS
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_ES
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_FS
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_GS
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_SS
       INC BX
       INC BX
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_VF

       mov bx,11
       MOV AX,3
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_DW
       mOV AX,21
       MOV BX,23
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_F
       INC BX
       MOV AX,43
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_FL
       MOV AX,14
       MOV BX,15
       CALL SCRN_PRINTXY PASCAL,DSTRUC,AX,BX,SDATA32,oFFSET dm_DES
       MOV CX,'√'
       XOR AX,AX
       CALL SCRN_SETCHAR PASCAL,DSTRUC,AX,BX,CX
       MOV CX,'¥'
       MOV AX,79
       CALL SCRN_SETCHAR PASCAL,DSTRUC,AX,BX,CX

 @d_quit:


       ret
Endp



DebugInt Proc Far

         USES EAX,EBX,ECX,EDX,DS
         push ds
         Mov ds,sdata32
         Mov dword ptr D_Regs._AX,EAX
         Mov dword ptr D_Regs._bX,EbX
         Mov dword ptr D_Regs._cX,EcX
         Mov dword ptr D_Regs._dX,EdX
         mov ax,cs
         mov D_Regs._cs,ax
         mov ax,es
         mov D_Regs._es,ax
         mov ax,fs
         mov D_Regs._fs,ax
         mov ax,gs
         mov D_Regs._gs,ax
         mov ax,ss
         mov D_Regs._ss,ax
         pop D_Regs._ds
         PushFD
         pop EAX
         Mov D_eflags,eax
         Mov eax,ebp
         mov ax,SS:[esp+20]
         mov d_ebp,eax
         Mov eax,ESP
         add eax,24
         MOV D_ESP,eax
         MOV EAx,ss:[esp+20]
         MOV D_EIP,eax

         Call Debug_Winit
         Call Debug_Wdraw
         Call showmasks
         call showvals

;         Call Freemem pascal,dptr
;         Call Freemem pascal,dstruc

;         mov ah,08h
;         intr 21h

         ret
Endp




;...........................................................................
;---------------------------------------------------------------------------

Q1 DD ?
Q2 DD ?
PN Longint ?
Vbuf longint ?

NODE1 OBJPTR <>
NODE2 OBJPTR <>
NODE3 OBJPTR <>
NODE4 OBJPTR <>

ST1   DB 255 dup(0)

R1 DW ?,?
R2 DW ?,?
R3 DW ?,?
R4 DW ?,?


;############################################################################
; PROCnode Routines
;############################################################################

Procnode Struc
          Thandler <>
          Link     Longint ?
          Next     Longint ?
ENDS


ProcNodeList  Longint ?
;----------------------------------------------------------------------------

;-------------------------------------------------
;Procedure InitProclist;
;-------------------------------------------------

InitProclist Proc near

                 Uses EAX
                 Xor eax,eax
                 Mov Procnodelist,eax
                 Ret
Endp

;-------------------------------------------------
;Function NewProcnode:Dword(EDX);
;-------------------------------------------------
NewProCNode Proc near

          Uses EAX
          Mov eax,32
          Call getmem pascal,eax
          Cmp errorflag,0
          je @@NN_quit

      @@NN_quit:
          Ret
Endp

;-------------------------------------------------
;Function ADDprocnode:Dword(ECX);
;-------------------------------------------------

ADDProcNODE proc near
       Arg @L:Dword

       Uses Eax,ebx,edx,ESI,EDI
       Call Newprocnode
       cmp Errorflag,0
       jne @@ADD_quit

       Mov ESi,EDX
       Mov EAX,@L
       MOV ES:[ESI][procnode.link],EAX
       Cmp Procnodelist,0
       jne @@ADD_notnil

       MOv EAx,0
       Mov ES:[ESI][procnode.next],eax
       Mov ECX,1
       MOv ES:[ESI][procnode.handle],ecx
       Mov procnodelist,ESI
       jmp @@ADD_quit

 @@ADD_notNIL:
       Mov EDi,Procnodelist
       Mov ECX,ES:[EDI][Procnode.handle]
       Inc eCX
       Mov ES:[ESI][Procnode.next],EDI
       Mov ES:[esi][procnode.handle],ECX
       Mov procnodelist,ESI

 @@ADD_Quit:
      RET
ENDP

;-------------------------------------------------
; Procedure RemoveProcNode(Handle:Dword);
;-------------------------------------------------

RemoveProcNode Proc near
               Arg @O:DWORD

         USES EAX,EBX,ECX,EDX,ESI
         MOV ESi,Procnodelist
         cmp esi,0
         je @@REMOVE_quit

         Mov EBx,@O
         Cmp Ebx,ES:[ESI][Procnode.handle]
         jne @@Remove_l1

         Mov EBX,ES:[ESI][procnode.NEXT]
         Mov procnodelist,EBX

    @@Remove_L1:
         Mov EDi,ESI
         MOv ESi,ES:[EDI][procnode.next]
         Cmp ESi,0
         je @@Remove_quit
         Cmp ES:[ESI][procnode.handle],EBX
         jne @@Remove_l1
         Mov EAx,ES:[ESI][procnode.next]
         Mov ES:[EDi][procnode.next],eax

         Call Freemem pascal,EDI

   @@Remove_quit:
          ret
Endp



;-------------------------------------------------
;Procedure Call_Links;
;-------------------------------------------------

Call_lInks Proc near

         USES EAX,EBX,ECX,EDX,EDI
         Mov ESi,procnodelist

   @@CL_loop:
         Cmp Esi,0
         je @@CL_quit
         Mov EAX,ES:[ESI][Procnode.link]
         Call EAX
         Mov ESI,ES:[ESI][procnode.next]
         jmp @@CL_loop

   @@CL_Quit:
         ret
Endp

;-------------------------------------------------
;Procedure DisposeprocList;
;-------------------------------------------------

DisposeprocList Proc Near

        Uses EAX,EBX,ECX,EDX,ESI,EDI,DS

        Mov ESI,procnodelist

        cmp eSI,0
        je @@D_quit

    @@D_l1:
        Mov EAX,ES:[ESI][procnode.next]
        Call Freemem pascal,esi
        MOv ESI,EAX
        Cmp ESI,0
        jne @@D_l1

    @@D_quit:
         Ret
Endp

;############################################################################

DMSG1 DB '>>> ABOMINATIONS OF DESOLATION <<<',0ah,0dh,'$'
DMSG2 DB '>>> ALTARS OF MADNESS <<<',0ah,0dh,'$'
DMSG3 DB '>>> DOMINATION <<<',0ah,0dh,'$'

Align 4
Dummy1 proc near

        Uses EAX
        Mov ax,offset DMSG1
	Call Errorprint pascal,code32,ax
       ret
endp

Dummy2 proc near

        Uses EAX
        Mov ax,offset DMSG2
	Call Errorprint pascal,code32,ax
       ret
endp

Dummy3 proc near

        Uses EAX
        Mov ax,offset DMSG3
	Call Errorprint pascal,code32,ax
       ret
endp





CODE32 ENDS


;...........................................................................
;---------------------------------------------------------------------------
;...........................................................................
;---------------------------------------------------------------------------


STACK32 Segment para public use32

DB PMstack*16 dup(0)	;Espaáo de pilha

Stack32 ENDS



End Code16_start



