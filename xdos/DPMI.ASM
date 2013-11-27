.286p
Model large,pascal
Stack 200h

Global pascal Writeln:far
Global pascal Is386:far
Global pascal errorquit:far


include DPMI.INC

ISERROR =1
NOERROR =0

;----------------------------------------------------------------------------
segment data para public use16
ends data

segment code16 para public use16
ends code16

ASSUME CS:Code16, DS:data



segment data para public use16


;----------------------------------------------------------------------------
; Vars

;Error msgs

CopyR  db 'XDOS  DOS extender v1.0b - Coded by Jo„o Pinheiro',0ah,0dh
       db 'Copyright (c) 1997 Jo„o Pinheiro',0ah,0dh,0ah,0dh,'$'
Error1 db 'CPU >=386 n„o encontrado !!',0ah,0dh,7,'$'
Error2 db 'Servidor DPMI n„o encontrado!!',0ah,0dh,7,'$'
Error3 DB 'Erro de aloca‡„o de mem¢ria !!',0ah,0dh,7,'$'
Error4 DB 'Servidor DPMI n„o suporta 32 bit !!',0ah,0dh,7,'$'
Error5 DB 'Erro aquando a chamada de entrada em modo protegido !!',0ah,0dh,7,'$'
Error6 DB 'Erro activando linha A20 !!',0ah,0dh,7,'$'
Error7 db 'Erro aquando a aloca‡„o da mem¢ria baixa necess ria !!',0ah,0dh,7,'$'
Error8 DB 'Não foi possível alocar memória para a stack!',0ah,0dh,7,'$'

DOSmemBlock  dw ?

Server_Info DPMI_info <>

Dpmi_entry dw ?,?

RM_Pspseg  dw ?
Psp_sel    dw ?

Descr Descriptor <>
Block1 memory <>


REGS  Registers <>
RDS   DW ?
RES   DW ?
RSS   DW ?

DATA0 DW ?

FLAT_DATA Descriptor <0FFFFh,0000h,00h,0F9h,0CFh,00h>

;  Limit_15   dw ?
;  Base_15    dw ?
;  base_23    db ?
;  AcessR     db ?
;  Limit_19   db ?
;  Base_31    db ?
;


ends data



;----------------------------------------------------------------------------
;code
;----------------------------------------------------------------------------

segment code16 para public use16



;-------------------------------
Proc Writeln far 
        ARG msg:Word

        uses ax,dx
        mov ah,09h
        mov dx,Msg
        int 21h
        ret
endp
        

;-------------------------------
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
        

;-------------------------------
Proc ErrorQuit far
        arg msg:word
        
        mov ax,msg
        call writeln pascal, ax
        mov ax,04c01h
        int 21h
        ret
endp    

;-------------------------------
Proc AllocDosMem far

        uses ax,bx      
        mov bx,dosmem
        cmp bx,0
        je @ALocDos_quit
        mov ah,48h
        int 21h
        jnc @@cont
        call Errorquit pascal,offset Error7
   @@Cont:              
        mov dosmemblock,ax
   @Alocdos_quit:
        ret
endp            
                

;-------------------------------
Proc Freedosmem Far

        uses ax,bx,es
        mov bx,dosmem
        cmp bx,0
        je @FreeDos_quit
        push dosmemblock
        pop es
        mov ah,49h
        int 21h
  @freedos_quit:
        ret
endp    

;-------------------------------
Proc AllocRSTACK far

        uses ax,bx      
        mov bx,RMSTACK
        cmp bx,0
        je @ALocrss_quit
        mov ah,48h
        int 21h
        jnc @@sscont
        call Errorquit pascal,offset Error8
   @@ssCont:              
        mov RSS,ax
   @Alocrss_quit:
        ret
endp            
                

;-------------------------------
Proc FreeRStack Far

        uses ax,bx,es
        mov bx,rmstack
        cmp bx,0
        je @Freerss_quit
        push RSS
        pop es
        mov ah,49h
        int 21h
  @freerss_quit:
        ret
Endp

	


;-------------------------------	
Proc EnableA20 far

        local xms_adr:Dword
        Uses bx,es
	mov ax,4300h
	int 2fh
	cmp al,80h	;Xms present?
	jne @Hard_way	;No, go to hardway
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
  	mov ax,true
  @a20_quit:
  	ret
endp  		

;-------------------------------------------------------------
; FN GET_BASE(Selector:word);
; RETS: EBX - Base ADDX
;-------------------------------------------------------------
.386
GET_BASE Proc far pascal
	ARG sel:word
		
	mov ax,06h
	mov bx,sel
	int 31h
	mov ax,false
	jc @GB_quit
	Movzx EBX,cX	;Stores high word
	shr EBX,16
	MOVzx EBX,dx	;Stores low word
	mov ax,true
  @GB_quit:
	ret
Endp

;-------------------------------------------------------------
; FN GET_Descriptor(Want:word; Selector:word;Ofs:Dword);
; RETS Buffer SEL:OFS filled with want descriptor 
;-------------------------------------------------------------

GET_Descriptor Proc far pascal
	ARG @w:word,@s,@o:DWORD

	USES bx,es,edi
	mov ax,0bh
	mov bx,@W
	push @s
	pop es
	mov EDI,@o
	int 31h
	mov ax,false
	jc @gd_quit
	mov ax,true
  @gd_quit:
	ret
Endp

;-------------------------------------------------------------
; FN Malloc(STRUC Memory);
; RETS struc filled
;-------------------------------------------------------------

Malloc Proc far pascal
	ARG @S:word;@o:dword

	uses es,BX,CX,si,di
	mov ax,501h
	push @s
	pop es
	mov edi,@o
	mov bx,0f0h
	mov cx,0
	int 31h
	Mov ES:[@o].memory.address[2],bx
	Mov ES:[@o].memory.address[0],cx	
	Mov ES:[@o].memory.handle[2],si
	Mov ES:[@o].memory.handle[2],di
	mov ax,false
	jc @mal_quit
	mov ax,true

  @mal_quit:
	ret
Endp


;-------------------------------------------------------------
; FN Mdalloc(STRUC Memory);
;
;-------------------------------------------------------------

Mdalloc Proc far pascal
	ARG @s:word, @o:dword

	mov ax,502h
	push @s
	pop es
	Mov si,ES:[@o].memory.sz[2]
	Mov di,ES:[@o].memory.sz[0]
	int 31h
	mov ax,false
	jc @mdal_quit
	mov ax,true

  @mdal_quit:
	ret
Endp






.286


;-------------------------------
Proc Init_Dpmi near
	Push es
	Pop Rm_Pspseg
	call is386 pascal
	cmp al,true
	je @Cont1	;>=386 detected
	call errorquit pascal,offset error1
  @cont1: 
	mov ax,1687h
	int 2fh
	cmp ax,0
	je @cont2      ;DPMI server detected
	Call errorquit pascal,offset error2
  @cont2:
  	mov server_info.flags,bx
        test bx,1
        jnz @cont3      ;Dpmi 32 server
  	Call errorquit pascal,offset error4
  @Cont3:
  	mov Server_info.cpu,cl
	mov Server_info.ver,dx	
	mov word ptr dpmi_entry[0],di
	mov word ptr dpmi_entry[2],es
	cmp si,0
	je @cont4
	mov bx,si
	mov ah,48h
	int 21h
	mov es,ax
	jnc @cont4
	Call errorquit pascal,offset error3
  @cont4:
       call enableA20
       cmp al,false
       jne @cont5
       Call errorquit pascal,offset error6
 @cont5:
	call allocdosmem
        Call AllocRstack
	Mov ax,1
	.386p
	call dword ptr Dpmi_entry
	jnc @cont6
	call freedosmem
        Call freerstack
	Call errorquit pascal,offset error5
 @cont6:
 	mov psp_sel,es
	ret
endp 	

.386p

TESTe proc far
        ARG S:pointer

        Uses es,edi,cx,ax
        MOV ES,S.selector
        Mov EDI,S.Ofset
        Mov cx,1000
         Mov al,'A'
        mov ah,al
        cld
        rep stosb
        ret
Endp





RMINT Proc Far
      ARG R_int:byte

      uses EAX,EBX,ECX,EDX,ESI,EDI,ES,DS
      MOV dword ptr Regs._DI,EDI
      MOV dword ptr Regs._sI,EsI
      MOV dword ptr Regs._bp,0
      MOV dword ptr Regs._bx,ebx
      MOV dword ptr Regs._dx,edx
      MOV dword ptr Regs._cx,ecx
      MOV dword ptr Regs._ax,eax
      Mov Ax,0300h
      Mov bl,R_int
      mov bh,0                
      PUSH res
      pop regs._es
      push RDS
      Pop regs._ds
      push RSS
      Pop regs._ss
      Mov word ptr regs._sp,0
      push 0202h
      Pop Regs.flags
      Mov  cx,0
      push ds
      pop es
      Mov edi,offset regs
      Int 31h
      jnc @RI_noproblem
      stc
      jmp @ri_quit

@Ri_noproblem:
      MOV AX,Regs.flags
      SAhf

@RI_quit:
      ret
Endp

INTR Macro Intno
     CAll RMint pascal,Intno
     Endm
        


;-------------------------------------------------------------
; FN ALLOC_Descriptors(DNo:Word);
; RETS BX-Base selector
;-------------------------------------------------------------


ALLOC_DESCRIPTORS PROC FAR 
	ARG DNO:Word

        Uses CX
	Mov ax,00h
	mov cx,dno
	int 31h
        mov bx,ax
        mov ax,iserror
        jc @@AD_Quit
        Mov ax,noerror

  @@AD_quit:
	ret
Endp


;-------------------------------------------------------------
; FN FREE_Descriptor(Selector:Word);
;
;-------------------------------------------------------------

Free_Descriptor Proc far 
	ARG S:Word
	
	uses bx
	mov ax,01h
	mov bx,s
	int 31h
        mov ax,Iserror
	jc @FD_quit
        mov ax,noerror

  @Fd_quit:
	ret
Endp


;-------------------------------------------------------------
; FN SET_BASE(Selector:word;BASE:DWORD);
; 
;-------------------------------------------------------------

SET_BASE Proc far 
        ARG SEL:word, b:dword
		
	Uses bx,cx,dx
	mov ax,07h
        mov bx,sel
        MOV cx,word ptr b[2]
        mov dx,word ptr b[0]
	int 31h
        mov ax,Iserror
	jc @sB_quit
        mov ax,noerror
  @sB_quit:
	ret
Endp

;-------------------------------------------------------------
; FN SET_limit(Selector:word;limit:DWORD);
; 
;-------------------------------------------------------------

SET_limit Proc far
        ARG sel:word, b:Dword
		
	Uses bx,cx,dx
	mov ax,08h
        mov bx,sel
        MOV cx,word ptr b[2]
        mov dx,word ptr b[0]
	int 31h
        mov ax,Iserror
	jc @sl_quit
        mov ax,noerror
  @sl_quit:
	ret
Endp


;-------------------------------------------------------------
; FN CODE_ALIAS(Selector:word);
; RETS BX - DAta selector
;-------------------------------------------------------------

CODE_ALIAS Proc far 
	ARG sel:word
		
	mov ax,0Ah
	mov bx,sel
	int 31h
	mov bx,ax
        mov ax,Iserror
	jc @cl_quit
        mov ax,noerror
  @cl_quit:
	ret
Endp


;-------------------------------------------------------------
; FN SET_AR(Selector:word;Access rights:word);
; 
;-------------------------------------------------------------

SET_AR Proc far 
	ARG sel:word,ar:WORD
		
	Uses bx,cx
	mov ax,09h
	mov bx,sel
	MOV cx,ar
	int 31h
        mov ax,Iserror
	jc @sa_quit
        mov ax,noerror
  @sa_quit:
	ret
Endp




start:
        push seg data
	pop ds
        mov Rds,ds
        mov ah,62h
        int 21h
        mov ah,4ah
        mov es,bx
        mov bx,1000
        int 21h


	call writeln pascal, offset copyr
        call init_dpmi

        CALL SET_LIMIT pascal,CS,0ffffh,0ffffh
        cmp al,iserror
        je @quitter

        Call code_alias pascal,cs
        cmp al,iserror
        je @quitter

        Mov data0,bx
        push es

        mov eax,0
        CALL SET_Base pascal,data0,eax
        cmp al,iserror
        je @quitter

        CALL SET_LIMIT pascal,data0,0ffffh,0ffffh
        cmp al,iserror
        je @quitter

        mov es,Data0
        MOV Edi,0B0000h
        mov al,0F0h
        mov ah,al
        MOV ES:[EDI],AX
        INC EDI
        INC EDI
        MOV ES:[EDI],AX
        pop es

        mov ah,92
        mov al,ah
       

        mov ax,1
        mov bx,DATA0
        int 31h
        jc @quitter

        Mov ax,2
        mov bx,0b000h
        int 31h
        mov es,ax
        mov edi,1000

        Call teste pascal,ES,EDI
        Mov dx,offset Copyr
        Mov ah,09h
        Intr 21h

@quitter:
	mov ax,04c00h
	int 21h




ends code16
end start		





END

