.286p
Model LArge,PASCAL
.stack 200h


Include DPMI.INC
.386
INCLUDE SEGS.INC
.286


ASSUME DS:DATA16, CS:code16
;-------------------------------------------------------------------
; SEGMENTO DE DADOS DE 16 BITS
;===================================================================

SEGMENT DATA16 Para public use16


PUBLIC SDATA16, Scode16, DOSMB_seg ,RSS,Psp_sel

Extrn DPMI_Start:Near	

;---------------------------------------------------
; Mensagens de Erro

CopyR  db 'XDOS  DOS extender v1.0b - Programado por Jo„o Pinheiro',0ah,0dh
       db 'Copyright (c) 1997 Jo„o Pinheiro',0ah,0dh,0ah,0dh,'$'
Error1 db 'CPU >=386 n„o encontrado !!',0ah,0dh,7,'$'
Error2 db 'Servidor DPMI n„o encontrado!!',0ah,0dh,7,'$'
Error3 DB 'Erro de aloca‡„o de mem¢ria !!',0ah,0dh,7,'$'
Error4 DB 'Servidor DPMI n„o suporta 32 bit !!',0ah,0dh,7,'$'
Error5 DB 'Erro aquando a chamada de entrada em modo protegido !!',0ah,0dh,7,'$'
Error6 DB 'Erro activando linha A20 !!',0ah,0dh,7,'$'
Error7 db 'Erro aquando a aloca‡„o da mem¢ria baixa necess ria !!',0ah,0dh,7,'$'
Error8 DB 'Não foi possível alocar memória para a stack!',0ah,0dh,7,'$'



;Mensagens de inicializa‡„o

MSG1   DB 'Detectando CPU... $'
MSG2   DB 'Detectando DPMI... $'
MSG3   DB 'Activando A20... $'
MSG4   DB 'Alocando bloco de $'
MSG5   DB 'par grafos de mem¢ria baixa',0ah,0dh,'$'
MSG6   DB 'par grafos de mem¢ria baixa para stack',0ah,0dh,'$'
MSG7   DB 'Inicializando...',0ah,0dh,'$'
MSG8   DB 'V0.9',0ah,0dh,'$'
MSG9   DB 'OK.',0ah,0dh,'$'


DPMI_Entry DW ?,?    ;ADDX de entrada em PM

DOSMB_SEG DW ?   ;Segmento do bloco de mem¢ria Baixa alocado
RSS       DW ?   ;Segmento da pilha para usar na RM CallBack Struct
PSP_Seg   DW ?   ;Segmento do PSP
PSP_Sel   DW ?   ;Selector do PSP


SDATA16 DW ?    ;Selector do segmento Data16
SCode16 DW ?    ;Selector do segmento Code16


TempSTR DB 5 DUP(?)  ;String Tempor ria
TempSTR2 DB 5 DUP(?)  ;String Tempor ria

DATA16 ENDS


SEGMENT CODE16 PARA PUBLIC USE16

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
        jne @Hard_way   ;N„o, usar hardware
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
; Termina a execu‡„o com msg de erro
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


;®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®
;In¡cio do Programa

CODE16_Start:

        push seg data16                ;DS = Data16
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
	

        mov ax,002
        Mov bx,0b000h
        int 31h
        jc @quit
        mov es,ax
        Mov cx,1000
        Mov al,'3'
        mov ah,al
        mov di,0
        cld
        rep stosb

@quit:
        Mov ax,04c00h
        int 21h

CODE16 ENDS
End Code16_start


