.386P
MODEL LARge,Pascal

Include DPMI.INC
INCLUDE SEGS.INC
INCLUDE EXT.inc


data32 Segment Para pubic use32

;------------------------------------------
;Mensagens de Erro


PUBLIC RDS,RES,RCS,RIP,Sdata32,Scode32,sdata0,regs
PUBLIC dpmi_init


Error1 DB 'N„o foi poss¡vel alocar os descriptors pretendidos.',0ah,0dh,'$'
Error2 DB 'Mem¢ria extendida insuficiente para correr a aplica‡„o.',0ah,0dh,'$'



;-----------------------------------------------
; Dados para V86 switch

REGS Registers <>

RDS DW ?  ;Registo DS modo V86
RES DW ?  ;REgisto ES modo V86
RCS DW ?  ;REGISTO CS modo V86
RIP DW ?  ;REGISTO IP modo V86 

;----------------------------------------------
;Selectores do sistema

SDATA32 DW ?	;Selector do seg DATA32
SCODE32 DW ?	;Selector do seg Code32
SDATA0  DW ?	;Selector FLAT


data32 ENDS


code32 Segment Para pubic use32

ASSUME DS:data32, CS:code32

;**********************************************
;Executa uma int em modo real
;**********************************************

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
      push rss
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



DPMI_INIT:

    MOV ds,SDAta32
    MOV dx,offset Error1
    MOV RDS,seg data32
    MOV ah,09h
    intr 21h
    
    mov ax,04c00h
    int 21h
    
code32 ENDS

end