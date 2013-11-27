;###########################################################
;#                                                         #
;#  Ficheiro: MAIN.ASM                                     #
;#                                                         #
;#  (c) Jo„o Pinheiro, 1998                                #
;#                                                         #
;###########################################################

Model large, pascal
.386p

include globals.inc

;##############################################################################

PROGSIZE        = 8192  ;Tamanho do segmento de programa

SSTACK		= 10    ;Stack de inÌcio do programa (160B)
RMSTACK         = 512   ;Stack modo real, em par·grafos (8k)
PMSTACK		= 512	;Stack modo protegido, em par·grafos (8k)
EXPSTACK	= 16    ;Stack das exceptions, em par·grafos

GDT_SIZE	= (256*8)/16  ;Tamanho da gdt, em paragr
IDT_SIZE        = (256*8)/16  ;Tamanho da IDT, em paragr
HEAP_SIZE       = 1024*8192   ;8Mb de heap
INT_STACKGAP    = 100h

MINDOS		= 5	;Vers„o MÌnima do DOS

STACK SSTACK*16

;##############################################################################
CODE16START
;##############################################################################

copyright	db '32 BIT DOS EXTENDER',0ah,0dh
	  	db 'Copyright (c) Joao Pinheiro, 1998',0ah,0dh,0ah,0dh,'$'
cpu_error	db 'Fail: 32 bit cpu required, but not found!',0ah,0dh,'$'
memory_error 	db 'Erro: MemÛria DOS insuficiente para alocar vari·veis internas.',0ah,0dh
                db '      Liberte alguma memÛria e tente de novo.',0ah,0dh,7,'$'
dos_error	db 'Fail: A more recent version of MSDOS is required.',0ah,0dh,7,'$'
prot_error	db "Fail: Already in protected mode and VCPI/DPMI wasn't found",0ah,0dh,7,'$'

cpu_386   	db 'Cpu: 80386',0ah,0dh,'$'
cpu_486   	db 'Cpu: 80486',0ah,0dh,'$'
cpu_586   	db 'Cpu: pentium',0ah,0dh,'$'
cpu_686   	db 'Cpu: pentium pro',0ah,0dh,'$'
cpu_786   	db 'Cpu: pentium II',0ah,0dh,'$'
mmx_technology 	db 'MMX(r) technology present',0ah,0dh,'$'

cpu_table       dw offset cpu_386,offset cpu_486
                dw offset cpu_586,offset cpu_686,offset cpu_786

;###########################################################

ivtseg		dw ?	;segmento da cÛpia da ivt
inttable	dd ?    ;linear addx da cÛpia da tabela de ints
tss_seg		dw ?	;segmento do tss de sistema (VCPI/RAW)

;###########################################################

oldcs	     	dw ?    ;segmento cs inicial (modo real)
oldss	     	dw ?    ;segmento ss inicial (modo real)
oldesp	     	dd ?    ;reg esp inicial (modo real)
oldebp	     	dd ?    ;reg ebp inicial (modo real)
Seg_psp		dw ?    ;segmento de PSP
dosver		db ?,?  ;Vers„o do MS-DOS (lo-hi)


;###########################################################

sel_table  label WORD

sdata32         dw DATA32_SELECTOR    ;selector _TEXT
scode32		dw CODE32_SELECTOR    ;selector _TEXT
sdata0		dw DATA0_SELECTOR     ;Selector de dados Flat
Slomem		dw LOMEM_SELECTOR     ;Selector mapeado no 1∫ MB
sstack16	dw STACK16_SELECTOR   ;16 bit stack
sstack32	dw STACK32_SELECTOR   ;32 bit stack
sstack_e	dw STACK_E_SELECTOR   ;exception stack
sdata16         dw DATA16_SELECTOR    ;selector data16
scode16      	dw CODE16_SELECTOR    ;selector code16
spsp	     	dw ?    	      ;selector psp

start_stackbase dd ?		      ;endereÁo linear da base da stack de entrada no programa
start_stacktop  dd ?		      ;endereÁo linear do topo da stack de entrada no programa
start_type	db 0		      ;bit 0: DPMI, bit 1: VCPI, bit 2: XMS, bit 3:Raw

_switch2rm	dw 0		      ;offset da rotina para entrar em rm
_switch2pm	dw 0		      ;offset da rotina para entrar em pm
_set_a20        dw 0                  ;offset da rotina para setar A20
_reset_a20      dw 0                  ;rotina para resetar A20

_Pic_notify     dw 0                  ;rotina para mudar o PIC
_exitproc	dw 0    	      ;offset do proc de saÌda
_failproc       dw 0                  ;offset do proc de saÌda com erro
_exitcode	db 0	              ;CÛdigo de erro para o MSdos
_dos_bufseg     dw 0                  ;segmento de dados do dos
_dos_buflin     dd 0                  ;endereáo linear do _dos_bufseg

;###########################################################

Exception_count = 19		      ;N∫ de excepÁıes a tratar

_tss_linaddx    dd ? 	              ;endereÁo linear dos tss das exception's

_rmstack_base	dd ?                  ;stacks a usar pelo sistema
_rmstack_top	dd ?
_pmstack_base	dd ?
_pmstack_top	dd ?
_fstack_base	dd ?
_fstack_top	dd ?

;###########################################################

_Pic_Master	db 00h	              ;configuraÁ„o PIC Master
_Pic_Slave	db 00h	              ;configuraÁ„o Slave
_Old_Picmaps	db 08h,070h
_New_Picmaps	db 08h,070h

;###########################################################

_GDTR	DW GDT_LIMIT-1			;limite
	DD 0            		;Base

pad	dw 0

_IDTR	DW 0				;limite
	DD 0				;base
	dw 0

REALIDT dw 03ffh			;idt de modo real
	dd 0
        dw 0

;###########################################################

dpmi_version	db ?,?	;Vers„o do servidor dpmi
dpmi_entry 	dw ?,?	;far ptr dpmi_entry
dpmi_pdatasz	dw 0    ;tamanho do bloco de dados necess·rio
dpmi_flags	db 0	;flags dpmi
fail_code	dw 0	;CÛdigo de erro

;###########################################################

linker_mark	dw 0ABFEh	;marca para o linker
pmstacksz	dw PMSTACK      ;Tamanho da stack PM em paragr
rmstacksz	dw RMSTACK	;Tamanho da stack RM em paragr
excpstacksz	dw EXPSTACK	;tamando da exception stack, em paragr
forbid_startup  db 0		;bits de inibiÁ„o do tipo de startup
_heap_min       dd HEAP_SIZE    ;tamanho mÌnimo da heap,em bytes
_heap_max       dd HEAP_SIZE    ;tamanho m·ximo da heap,em bytes
_dos_bufsize    dw 8192         ;dos buffer size, em bytes

;###########################################################

include getcpu.inc
include dos.inc
include vcpi.inc
include xms.inc

;EXTRN INIT_VIDEO:NEAR
;extrn txt_linaddx:dword
;##############################################################################

copyw16 proc near
        arg @src:dword, @dest:dword, @count:word

	uses ds,es
        mov ds,word ptr @src
        mov si,word ptr @src+2
        mov es,word ptr @dest
        mov di,word ptr @dest+2

        lds si,@src
        les di,@dest
	movzx ecx,@count
	cld
	rep movsw
	ret
endp

fillchar proc near
	arg @dest:dword, @count:word,@car:word

	uses es
	les di,@dest
	mov cx,@count
	mov ax,@car
	cld
	rep stosb
	ret
endp

;##############################################################################
;===========================================================
;| void c_set_a20()        	                           |
;| Activa a linha A20                                	   |
;|                      				   |
;===========================================================

c_set_a20 proc near

	pushf
	cli
	in	al,092h		; 092h is the system control port "A"
				; for PS/2 models
	or	al,2		; this sets the A20 bit in register al
	jmp	SHORT $+2	; forget the instruction fetch
	out	092h,al		; set the A20 bit on

	mov	ax,0
	mov	fs,ax
	mov	ax,0ffffh
	mov	gs,ax
	mov	bx,fs:[0]
	mov	word ptr fs:[0],1234
	cmp	word ptr gs:[16],1234
	je	need_to_set_a20
	mov	word ptr fs:[0],4321
	cmp	word ptr gs:[16],4321
	je	need_to_set_a20
	mov	fs:[0],bx
	popf
	ret

need_to_set_a20:
	mov	fs:[0],bx
	call	waitkb
	mov	al,0d1h
	out	64h,al
	call	waitkb
	mov	al,0dfh
	out	60h,al
	call	waitkb
	mov	al,0ffh
	out	64h,al
	call	waitkb		
	mov	ax,0
	mov	fs,ax
	mov	ax,0ffffh
	mov	gs,ax
	mov	bx,fs:[0]

wait_for_valid_a20:
	mov	word ptr fs:[0],1234
	cmp	word ptr gs:[16],1234
	je	wait_for_valid_a20
	mov	word ptr fs:[0],4321
	cmp	word ptr gs:[16],4321
	je	wait_for_valid_a20

	mov	fs:[0],bx
	popf
	ret

waitkb:
	mov	cx,0
waitkb1:
	in	al,64h
	test	al,2
	loopnz  waitkb1
	je	waitkb3
waitkb2:
	in	al,64h
	test	al,2
	loopnz  waitkb1
waitkb3:
	ret
endp


;===========================================================
;| void c_reset_A20()      	                           |
;| Desactiva a Linha A20                             	   |
;|                      				   |
;===========================================================

c_reset_a20 proc near
	in	al,092h		; 092h is the system control port "A"
				; for PS/2 models
	and	al,not 2	; this resets the A20 bit in register al
        jmp	SHORT $+2	; forget the instruction fetch
	out	092h,al		; set the A20 bit off
	ret
endp

;##############################################################################

;===========================================================
;| bool is_prot()		                           | 
;| Devolve o estado do bit PE da MSW                 	   |
;|                      				   |
;===========================================================

is_prot proc near

	smsw ax			;A velha instruÁ„o dos 286
	and ax,1
	ret
Endp

;===========================================================
;| bool dpmi_detect()		                           | 
;| Devolve em si o tamanho do private buffer         	   |
;|                      				   |
;===========================================================


dpmi_detect proc near

	mov ax,1687h			;detect dpmi
	int 2fh
	or ax,ax
	setz al
	jnz @dpmi_detect_quit		;fail !!!

	mov word ptr dpmi_version,dx	;vers„o do host
	mov dpmi_entry,di		;far ptr de entrada
	mov dpmi_entry+2,es		
	mov dpmi_pdatasz,si		;guarda info sobre mem necess·ria
	mov dpmi_flags,bl

 @dpmi_detect_quit:
	ret
endp



;===========================================================
;| void pm_init()		                           | 
;| 						     	   |
;| os bits start_type setados de acordo com o encontrado   |
;===========================================================

pm_init proc near

	call dpmi_detect	;verifica dpmi
	mov start_type,al	;Dpmi Start

 @chek_for_vcpi:
	call vcpi_detect	;procura vcpi
	or al,al
	jz @chek_for_xms	;se n„o encontrado, continua
	or start_type,2		;Vcpi Start
	jmp @pm_quit		;e mais n„o interessa

 @chek_for_xms:
	test start_type,1	;j· detectou dpmi?
	jnz @pm_quit		;se sim, trabalho concluÌdo

	call is_prot		;est· em modo protegido?
	or al,al
	jnz @pm_quit		;se sim, sai (h· erro)

	call xms_detect		;procura driver xms
	shl al,2
	mov start_type,al	;seta o bit xms
	or al,al
	jnz @pm_quit

	mov start_type,8	;seta bit raw

 @pm_quit:
	ret
endp

;##############################################################################

;===========================================================
;###########################################################
;===========================================================
START:
;===========================================================
;###########################################################
;===========================================================

	push cs					;ds=cs
	pop ds
	mov oldcs,ds				;salva o cs
	mov seg_psp,es				;salva o psp

        mov oldfs,fs
        mov oldgs,gs

	call echo pascal,offset copyright	;exibe copyright      
        mov ah,30h
	int 21h
	mov word ptr dosver,ax			;salva vers„o do DOS
	mov si,offset dos_error			;msg de erro de vers„o de dos
	cmp al,MINDOS
	jng quit_error				;se for antiga demais, aborta com erro

        call dos_resizemem pascal,es,progsize   ;redimensiona o PSP

	call get_cpu				;obtÈm o cpu
	mov bx,cpu_type
	mov si, offset cpu_error		;msg de erro de cpu
	cmp bx, 3				;ser· que È 386?
	jng quit_error

;===========================================================
	sub bx,3
	add bx,bx
	push word ptr [cpu_table+bx]
	call echo				;exibe msg de tipo de cpu
	cmp mmx_flag,true
	jne @l_001
	call echo pascal, offset mmx_technology	;mensagem de MMX

;===========================================================
 @l_001:
	call dos_getmem pascal,256		;aloca espaÁo para a tabela de ints real
	mov si,offset memory_error		;erro de memÛria
        or dl,dl
        jz quit_error				;se erro aborta

        mov ivtseg,ax				;salva o segmento
        call copyw16 pascal,0,0,ax,0,512	;copia a tabela de interrupÁıes
	movzx eax,ax				;CWD
	shl eax,4
	mov inttable,eax			;linear adx da inttable

;===========================================================	
	xor eax,eax
	mov ax,ss				;salva regs de stack
	mov oldss,ax
	mov oldesp,esp
	mov oldebp,ebp

	shl eax,4				;calcula o endereÁo linear da stack
	mov start_stackbase,eax			;e guarda-o no sÌtio apropriado
	add eax,esp
	mov start_stacktop,eax

;===========================================================

;------------------------------------------ a refazer
        mov ax,_dos_bufsize                     ;aloca o buffer de dos
        shr ax,4                                ;converte bytes em paragr
        call dos_getmem pascal,ax
	mov si,offset memory_error		;erro de memÛria
        or dl,dl
        jz quit_error				;se erro aborta
        mov _dos_bufseg,ax
        movzx eax,ax
        shl eax,4
        mov _dos_buflin,eax

	jmp common_start




;------------------------------------------ atÈ aqui
oldfs dw 0
oldgs dw 0
;===========================================================

 quit_error:					;sai com msg de erro
	push si					;cujo offset vem em si
						;atenÁ„o ! DS tem que ser _TEXT16
	call echo

 _quit:						;saÌda com restauro de stack
						;pode ser chamado em RM ou PM

        mov fs,oldfs
        mov gs,oldgs
	mov al,_exitcode			;cÛdigo de erro
	mov ah,4ch				;E sai para o DOS
	int 21h					;e c'est Áa!!!
					
;##############################################################################
;##############################################################################
comment %
;===========================================================
;| void Get_picmappings()	                           |
;| 							   |
;|                      				   |
;===========================================================

Get_picmappings proc near

	in al,21h
	mov _pic_master,al
	in al,0A1h
	mov _pic_slave,al
	ret
endp

;===========================================================
;| void Set_picmappings()	                           |
;| 							   |
;|                      				   |
;===========================================================

Set_picmappings proc near

	mov al,_pic_master
	out 21h,al
	mov al,_pic_slave
        out 0A1h,al
	ret
endp

%
;===========================================================
;| void Set_masterpic(int base)	                           |
;| 							   |
;|                      				   |
;===========================================================

Set_masterPIC proc near
        arg @loz:word

        cli
        in al,21h                       ;Master PIC State
        mov dl,al
        call poll

        mov al,0ffh                     ;Irqs desactivados
        out 21h,al
        call poll

        mov al,011h                     ;configuraá∆o
        out 20h,al
        call poll

        mov ax,@loz                     ;seta nova base
        out 21h,al
        call poll

        mov al,4                        ;cascade mode
        out 21h,al
        call poll

        mov al,1
        out 21h,al
        call poll

        mov al,0ffh                     ;de novo IRQ's desactivados
        out 21h,al
        call poll

	mov al,dl                      ;restaura config antigo
	out 21h,al
        call poll
        sti
        ret
endp

;===========================================================
Poll proc near
        in al,80
	ret
endp

;##############################################################################################
;Redireccionadores das ints de Hardware (int 8-0F)

_Pic_handlers_ptr  dd 8 dup(?)                ;Far ptr para Pic_Handlers

Pic_handlers:

count = 0
REPT 8
   push ebx
   push ds
   movzx ebx,byte ptr cs:_pic_vecs
   shl ebx,2
   push 0
   pop ds
   db 66h,67h,8bh,5bh
   db count                                  ;mov ebx,ds:[ebx+count]
   xchg ebx,[esp+2]
   pop ds
   retf
   Count = count+4
ENDM

Pic_handler_size EQU ($-pic_handlers)/8

;##############################################################################################
;##############################################################################################

_irq_vec_table	dd 16 dup(0)                            ;tabela de irqs de rm
comment %
set_a20		dw ?
reset_a20	dw ?
setmasterpic	dw ?
resetmasterpic	dw ?
exit		dw ?
%
free_vect_table  db 50h, 58h, 68h, 78h, 88h, 90h, 98h	;tabela dos slots
		 db 0A0h, 0A8h, 0B0h, 0B8h,0F8h -1	;onde procurar ints livres
_New_Master      db 8					;nova base para PIC master
_Pic_vecs	 db 08h,070h				;vectores por defeito para os pics
_reloc_vecs	 db TRUE				;flag para relocaÁ„o do master PIC
_forced_reloc    db false				;se foram salvos os vectores de relocaÁ„o

;===========================================================
;| void Find_PIC_slots()	                           |
;| 							   |
;| Procura 8 vectores para alojar os IRQ0-IRQ7		   |
;===========================================================

Find_PIC_slots proc near

	xor bx,bx			;agora vamos procurar 8 slots de int seguidos
	xor dx,dx
        mov es,bx
	mov si,offset free_vect_table	;offset da tabela

 @chek_slots_loop:
	mov dl,ds:[si]			;lÍ o int slot
	cmp dl,-1			;È o ˙ltimo?
	je @slot_not_found		;se sim, n„o h· nada a mudar

	mov cx,8			;slots a verificar
	inc si
	mov di,dx			;em bx o contador tempor·rio
        shl di,2                        ;offset do 1ß int
 @chek_that_suckers:
	mov eax,es:[di]	        	;em eax, o intvec
        or eax,eax                      ;se for nil, continua
	jnz @chek_slots_loop		;se n„o, procura o prÛximo slot
	add di,4			;e avanÁa para o prÛximo
	loop @chek_that_suckers		;sempre atÈ acabar
					;se chegou aqui, encontrou 8 vectores seguidos livres
	mov _new_master,dl		;guarda ent„o a nova base
	jmp @slots_done

 @slot_not_found:
	mov al,_pic_vecs		;vector por defeito
	mov _new_master,al		;n„o existem slots livres, n„o muda nada

 @slots_done:
	ret
endp

;==============================================================================

Irq_reloc proc near

        mov dx,offset pic_handlers      ;preenche a tabela
        mov si,offset _Pic_handlers_ptr ;de redireccionamento dos PIC'S
        mov cx,8
        cld

    @pic_red_loop:                      ;loop para os 8 IRQ's
        mov [si],dx                     ;escreve offs
        mov [si+2],cs                   ;agora seg
        add si,4
        add dx,Pic_handler_size         ;avanáa para o segundo
        dec cx
        jnz @pic_red_loop

	movzx esi,_pic_vecs		;Vector do Master Pic por defeito
	shl esi,2			;Intno*4
	call copyw16 pascal,0,si,cs,offset _irq_vec_table,16
	movzx esi,byte ptr _pic_vecs+1	;ok, agora o Slave Pic
	shl esi,2
	call copyw16 pascal,0,si,cs,(offset _irq_vec_table)+32,16
					;tudo bem, j· salvou os vectores dos 15
					;irqs. Agora, procura novo vector para o Master Pic,
					;se for permitido
        mov bl,_pic_vecs                ;Master VEC
        mov _new_master,bl              ;e p‰e-se em new_master
	cmp _reloc_vecs,false		;… permitido?
	je @irq_reloc_done		;N„o, sai. (VCPI com clientes)

	call find_pic_slots		;procura 8 vectores livres
	cmp bl,_new_master    		;foi encontrado algum slot livre?
	jne @use_new_slots		;sim, usa-os

	mov bl,50h
	mov _new_master,bl		;n„o, usam-se os vecs 50h-57h
	mov _forced_reloc,true		;e seta-se a flag a indicar que haviam outros vectores

 @use_new_slots:
	movzx eax,_new_master  	        ;agora copiam-se os 8 vectores
	push ax                         ;param para Set_masterpic
	shl ax,2			;acha-se o offset
        cli
                                        ;e setam-se os redireccionadores
	call copyw16 pascal,cs,offset _Pic_handlers_ptr,0,ax,16
					;agora informa-se o pic que se mudou a base
        call [_pic_notify]
;	call set_masterpic		;o argumento na stack
					;daqui pr· frente, INTS DESACTIVADOS!!

 @irq_reloc_done:
        movzx edi,_new_master           ;offset dentro da IDT
        shl edi,3
        mov edx,dword ptr _IDTR+2       ;actualizar a IDT para irq handlers
        shr edx,4
        mov es,dx
        mov ecx,8                       ;Nß de vectores
        mov ax,code16_selector          ;primeira Dword da Int Gate
        shl eax,16
        mov ax,offset common_irq
        cld

   @irq_cb_loop_1:                      ;preenche os 8 vectores
        mov dword ptr es:[edi],eax      ;na IDT
        add edi,8                       ;seta offset e selector de cada
        add ax,6                        ;Gate
        loop @irq_cb_loop_1

        movzx edi,byte ptr _pic_vecs+1  ;offset dentro da IDT do Slave Pic
        shl edi,3
        mov ecx,8                       ;os vecs do Slave Pic
        cld

   @irq_cb_loop_2:                      ;continua em EAX a 1¶ Dword
        mov dword ptr es:[edi],eax
        add edi,8
        add ax,6
        loop @irq_cb_loop_2             ;atÇ terminar os IRQ8-IRQ15

	ret
endp

irq_restore proc near   			;· saÌda, restaura tudo
	cli
	push _text16				;ds=dataseg
	pop ds
	call copyw16 pascal,ivtseg,0,0,0,512	;restaura tabela de ints
	cmp _reloc_vecs,True			;È preciso restaurar o vector por defeito?
	jne @@no_undo_reloc

	movzx ax,_pic_vecs
	call set_masterpic pascal,ax		;ent„o seta o vector por defeito

  @@no_undo_reloc:
	sti					;IRQS de novo!
        ret
endp

;##############################################################################
;##############################################################################

align 16

;##############################################################################

JMP16_32 MACRO sel,_tgt
        db 066h,0eah
        dd offset _tgt
        dw sel
ENDM

JMP32_16 MACRO sel,_tgt
        db 066h,0eah
        dw offset _tgt
        dw sel
ENDM

JMPFAR16 MACRO sel, _tgt
          db 0eah
          dw offset _tgt
          dw sel
ENDM

;##############################################################################
;##############################################################################
;=============================================================================

common_start:

	call pm_init				;detecta o tipo de host
	mov al,forbid_startup			;e verifica os inibidos
	mov bl,start_type
	not al
	mov si,offset prot_error
	and bl,al
	jz quit_error				;se j· estiver em v86, aborta

	test bl,1				;È dpmi?
;	jnz dpmi_init

;===========================================================
;	jmp _quit

;----------------------------------------------------------------------------
;	test start_type,2			;È vcpi?
;        jnz vcpi_init

;       a continuar...

        jmp @nice_start


;===============================================================
; Preenche os campos que faltam nos descriptors da GDT
; Inicia as stacks e seta tambÇm os seus descriptors
;===============================================================

Startup_Descriptors proc near

        mov eax,_TEXT16                		;Preenche os descriptors relativos a _TEXT16
	shl eax,4
        mov code16_DESCR.base_15,ax		;EndereÁo Base de SCODE16
        mov data16_DESCR.base_15,ax		;EndereÁo Base de SDATA16
	shr eax,16
        mov code16_DESCR.base_23,al
        mov DATA16_DESCR.base_23,al

        mov eax,_TEXT                   	;Preenche os descriptors relativos
        shl eax,4                       	;a _TEXT
        mov code32_DESCR.base_15,ax
        mov data32_DESCR.base_15,ax
	shr eax,16
        mov code32_DESCR.base_23,al
        mov data32_DESCR.base_23,al

;------------------------------------------------
;alocar as stacks
;ATENÁ„O: Os descriptors das stacks tÍm limite de 64k na GDT
;         para usar o topo, usar _xxSTACK_top

	mov dx,rmstacksz			;tamanho da exception stack
	add dx,pmstacksz			;tamanho da pm stack
	add dx,excpstacksz			;tamanho da rm stack
	call dos_getmem pascal,dx		;aloca espaÁo para as vari·veis internas
	mov si,offset memory_error		;erro de memÛria
        or dl,dl
        jz quit_error				;se erro aborta

	movzx eax,ax				;cwd
	shl eax,4				;converte seg em linear

	mov _rmstack_base,eax
	mov edx,eax				;em eax e edx o linaddx da base
	mov stack16_descr.base_15,ax		;seta a base na gdt
	shr edx,16
	mov stack16_descr.base_23,dl

	movzx ebx,rmstacksz			;agora calcula o limite
	shl ebx,4
	add eax,ebx				;e o inÌcio da stack seguinte
	dec ebx
	mov _rmstack_top,ebx
	mov _pmstack_base,eax
	mov edx,eax				;em eax e edx o linaddx da base
	mov stack32_descr.base_15,ax		;seta a base na gdt
	shr edx,16
	mov stack32_descr.base_23,dl
	movzx ebx,pmstacksz			;agora calcula o limite
	shl ebx,4
	add eax,ebx				;e o inÌcio da stack seguinte
	dec ebx
	mov _pmstack_top,ebx

	mov _fstack_base,eax
	mov edx,eax				;em eax e edx o linaddx da base
	mov stack_e_descr.base_15,ax		;seta a base na gdt
	shr edx,16
	mov stack_e_descr.base_23,dl
	movzx ebx,excpstacksz
	shl ebx,4
	add eax,ebx				;e o inÌcio da stack seguinte
	dec ebx
	mov _fstack_top,ebx
        ret
endp


;=====================================================================
; Aloca o espaáo para a GDT+IDT
; Copia os descriptors da GDT para o bloco de mem¢ria
; Seta os registos GDTR e IDTR de acordo com os blocos de mem¢ria
;=====================================================================

Startup_GDT_IDT proc near

	mov bx,GDT_SIZE				;calcular o espaÁo ocupado pela GDT+IDT
	add bx,IDT_SIZE
	call dos_getmem pascal,bx		;e alocar a memÛria
	mov si,offset memory_error
	or dl,dl
	jz quit_error				;ou abortar se n„o for suficiente

	movzx ebx,ax
	call fillchar pascal,ax,0,(gdt_size+idt_size)*16,0
        call copyw16 pascal,cs,offset GDT,bx,0,GDT_LIMIT/2 ;copiar a gdt para o bloco

	mov eax,ebx				;de memÛria alocado
	mov edx,GDT_SIZE
	add ebx,edx
	shl eax,4
	shl ebx,4
	shl edx,4
	mov dword ptr _GDTR+2,eax		;base da GDT
	mov dword ptr _IDTR+2,ebx		;base da idt
	dec edx
	mov word ptr _GDTR,dx			;limite da GDT
	mov word ptr _IDTR,(IDT_size*16)-1	;limite da idt
        ret
endp

;=====================================================================
; Preenche a IDT
; Os vectores 0-EXCEption_count para os TSS
; Os vecores Exception_count-0ffh reflectem para modo real
;=====================================================================

Fill_IDT proc near

       mov eax,dword ptr _IDTR+2                ;calc IDT SEG
       shr eax,4
       mov es,ax
       xor edi,edi
       cld
       mov ecx,0ffh				;256 interrupÁıes
       mov bx,code16_selector                   ;em ebx cs:ofs
       shl ebx,16
       mov bx,offset common_ints		;offset do 1∫ handler
       mov edx,00008E00h                        ;segunda dword

  @int_cycle:
       mov es:[edi],ebx
       mov es:[edi+4],edx
       add bx,6
       add edi,8
       loop @int_cycle				;preenche os 256 vectores

       xor edi,edi				;agora os Exception TSS
       mov ecx,exception_count
       mov edx,08500h
       mov bx,_APP_SELECTOR+8			;Exception 0 tss
       cld
  @excp_cycle:
       mov dword ptr es:[edi],0			;limpa dword
       mov word ptr es:[edi+2],bx			;selector
       mov dword ptr es:[edi+4],edx		;tipo
       add edi,8
       add bx,8         			;prÛximo selector
       loop @excp_cycle

       mov ebx,offset common_ints               ;preenche a tabela _LOW_INTS
       mov edi,offset _Low_ints
       mov edx,CODE16_SELECTOR
       mov ecx,exception_count
       push ecx
       cld

  @low_int_cycle:
       mov ds:[edi],edx                         ;escreve o selector
       mov ds:[edi+4],ebx                       ;e o offset
       add bx,6
       add edi,8
       loop @low_int_cycle

;-----------------------------------------------
       pop ecx
       mov edi,offset _User_exceptions          ;preenche a tabela
       mov ebx,offset excp_Table                ;_user_exceptions
       cld

   @exception_cycle:
       mov ds:[edi],edx
       mov ds:[edi+4],ebx
       add edi,8
       add ebx,4
       loop @exception_cycle
       ret
endp

;========================================================================
; aloca os tss para as exception's e para o tss da aplicaÁ„o
; e preenche-os de acordo com a sua funáÑo
; seta as entradas na GDT.
;========================================================================

Startup_Tss proc near


	mov bx,size tss			;calcula o espaÁo a alocar
	mov ax,Exception_count+1	;para albergar os tss necess·rios
	mul bx				;
	mov dx,ax
	mov di,ax			;guarda pra + tarde
	shr ax,4			;converte bytes em par·grafos
	and dx,0fh			;o resto È 0?
	jz @no_adj_zz
	inc ax				;n„o? acrescenta +1 par·grafo

  @no_adj_zz:
	call dos_getmem pascal,ax	;aloca ent„o o bloco necess·rio
	mov si,offset memory_error	;
	or dl,dl			;Por acaso tem memÛria?
	jz quit_error			;ah n„o tem? Ent„o j· te tiramos a tosse!
	mov tss_seg,ax
	mov es,ax				;es vai ser ˙til
	movzx eax,ax				;cwd
	shl eax,4				;converte o segmento em addx linear
	mov _tss_linaddx,eax			;e guardamo-lo no sÌtio devido

	call fillchar pascal,tss_seg,0,di,0	;limpa o bloco

	mov edx,_pmstack_top
	mov ecx,_rmstack_top
	mov es:[tss._esp0],edx			;preenche o _APP_TSS
	mov es:[tss._esp1],edx
	mov es:[tss._esp2],ecx
	mov es:[tss._ss0],stack32_selector	;seta TSS stack
	mov es:[tss._ss1],stack32_selector
	mov es:[tss._ss2],stack16_selector
        mov edx,vcpi_cr3
        mov es:[tss._creg3],edx
        mov dword ptr es:[tss._eflags],0202h
	mov es:[tss._iomapbase],size TSS	;trunca o IO Bitmap

;------------------------------------------------
;ok, agora preenche os tss com os valores apropriados para as exceptions

	mov di,size tss				;inÌcio do 1∫ TSS de Exception
	xor ebx,ebx				;ebx vai ter o n∫ da exception
	mov edx,_fstack_top			;e edx o valor de ESP(_fstack_top)
	mov ecx,-1				;futuramente, HW Int no
        mov eax,vcpi_cr3                        ;suporte paging

 @TSS_exception_loop:				;preenche ent„o os TSS
        mov es:[edi][tss._creg3],eax
	mov es:[edi][tss._esp0],edx		;n„o ser„o necess·rios tantos rings
	mov es:[edi][tss._esp1],edx		;visto que o selector È pra ring 0
	mov es:[edi][tss._esp],edx       	;
	mov es:[edi][tss._ss0],stack_e_selector	;Stack selector
	mov es:[edi][tss._ss1],stack_e_selector  ;
	mov es:[edi][tss._ss],stack_e_selector   ;
	mov es:[edi][tss._ecx],ecx		;Hw int no
	mov es:[edi][tss._edx],ebx		;N∫ da exception
        push edi
	pop dword ptr es:[di][tss._edi]		;offset do tss no segmento
	mov es:[edi][tss._cs],CODE16_SELECTOR	;e seta os selectors
	mov es:[edi][tss._ds],DATA16_SELECTOR
	mov es:[edi][tss._es],LOMEM_SELECTOR
	mov es:[edi][tss._fs],DATA0_SELECTOR
	mov es:[edi][tss._EIP],offset exception_init ;ponto de entrada
        mov dword ptr es:[di][tss._eflags],0202h
	mov es:[di][tss._iomapbase],size TSS	;trunca o IO BITMAP
	add di,size TSS				;funciona bem se <64K
	inc bx
	cmp bx,Exception_count
	jb @TSS_exception_loop

;------------------------------------------------
;Ok, agora preenche na GDT os selectors correspondentes aos TSS

_APP_SELECTOR = GDT_LIMIT

	mov eax,_tss_linaddx			;endereÁo linear dos tss
	mov ebx,08900h				;e a 2™ dword do selector
	mov ecx,Exception_count+1		;comeÁa com APP_TSS
	mov edx,DWORD PTR _GDTR+2		;ok, agora setar os descriptors
	shr edx,4				;na GDT
	mov es,dx				;semento da GDT
	mov di,GDT_LIMIT			;e offset do APP_TSS descriptor

  @@tss_descr_loop:
	push eax
	mov dx,ax				;15 bits de base
	shl edx,16
	mov dx,(size tss)-1			;e 15 do limite
	shr eax,16
	mov bl,al				;16-23 bits de base en bl
	pop eax					;restaura eax
	mov es:[di],EDX				;escreve o descriptor
	mov es:[di+4],EBX			;idem
	add di,8
	add eax,size tss			;avanÁa para o prÛximo tss
	loop @@tss_descr_loop			;e preenche todos
        ret
endp

;-----------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------
@nice_start:

        call Startup_Descriptors
        call Startup_GDT_IDT
        call Fill_IDT

	test start_type,2			;È vcpi?
        jmp vcpi_init

        call startup_tss

        cli
        mov _Pic_notify,offset set_masterpic
	call irq_reloc

        sti

        mov _switch2pm,offset _raw_switx2pm
        mov _switch2rm,offset _raw_switx2rm

;---------------------------------------------------------------------
        mov ax,DATA16_SELECTOR
        mov cx,DATA16_SELECTOR
        mov si,CODE16_SELECTOR
        mov dx,STACK32_SELECTOR
        mov EBX,_pmstack_top
        mov edi,offset @@gainz

;------------------------------------------------------------
;|   AX - Novo DS   DX - Novo SS
;|   CX - Novo ES   EBX - Novo ESP
;|   SI - Novo CS   EDI - Novo EIP

        cli
        jmp _raw_switx2pm
        jmp [_switch2pm]

 @@gainz:
       sti
       xor ax,ax
       int 16h
;        mov ax,_APP_SELECTOR
;        ltr ax
        push lomem_selector
        pop es
        mov edi,0b8000h
        mov eax,0BABAh
        mov ecx,700
        cld
        db 67h
        rep stosw

@rgo_rm:
        mov ax,_text16
        mov cx,ax
        mov si,_text16
        mov dx,oldss
        mov ebx,oldesp
        mov edi,offset _rr_xx
        cli
        jmp _raw_switx2rm
        jmp [_switch2rm]
;------------------------------------------------------------

_rr_xx:
      	call irq_restore
;        call set_picmappings
        sti

  @no_apic:
        jmp _raw_exit


;##############################################################################################
;##############################################################################################

_raw_extmem dd 0             ;quantidade de mem¢ria livre

;=============================================================================

int15 proc far

      push ds es
      cmp ah,088h
      jne @execute_old
      push _text16
      pop ds
      mov eax,_raw_extmem
      shr eax,10
      jmp @int15_quit

   @execute_old:
      push 0
      pop es
      pushf
      push _text16                             ;Iret frame
      push offset @int15_quit                  ;retf frame
      push dword ptr es:[15h*4]
      retf

   @int15_quit:
     pop es ds
     iret
endp


;##############################################################################################
;##############################################################################################

raw_init:
	mov ah,88h			;get bios MEM
	int 15h
	mov si,offset memory_error
	jc quit_error
        movzx eax,ax
        shl eax,10                      ;converte EAX em bytes
        mov _raw_extmem,eax             ;salva os bytes onde devem ficar
        cmp eax,_heap_min               ;Ç mem¢ria acima de 1Mb
        jb quit_error                   ;se nÑo for suficiente, aborta
        cmp eax,_heap_max
        jbe @no_heap_switx              ;se avail_mem>heap_max,
        mov eax,_heap_max               ;usa apenas heap_max

 @no_heap_switx:                        ;rotinas a usar
        sub _raw_extmem,eax             ;retira a mem a usar
        mov _set_a20,offset c_set_a20
        mov _reset_a20,offset c_reset_a20
        mov _exitproc,offset _raw_exit
        mov _switch2pm,offset _raw_switx2pm
        mov _switch2rm,offset _raw_switx2rm

        add eax,0FFFFFh                 ;n∆o esquecer o 1ß MB
        call paging_init pascal,eax     ;inicializa o mecanismo de paging


	jmp [_exitproc]




;=============================================================================
;| Switx to PM
;|   AX - Novo DS   DX - Novo SS
;|   CX - Novo ES   EBX - Novo ESP
;|   SI - Novo CS   EDI - Novo EIP
;|   ATEN«√O: INTS DESACTIVADOS!!!
;=============================================================================

_raw_switx2pm:
        push _text16                        ;para carregar bem as tabelas
        pop ds                              ;assegurar o segmento de dados
        pushfd                              ;preserva EFLAGS
        pop switx_var
        shl ecx,16
        mov cx,ax                           ;liberta EAx
        lgdt fword ptr _GDTR                ;carregar registos de modo protegido
        lidt fword ptr _IDTR
        mov eax,cr0
        or al,1
        mov cr0,eax
        JMPFAR16 CODE16_SELECTOR,@flush_pq  ;limpa o prefetch queue

 @flush_pq:
        mov ss,dx                           ;muda de stack
        mov esp,ebx
        push cs:switx_var                   ;aceder ao valor salvo
        and dword ptr [esp],not 4000h       ;limpa NT
        mov dx,stack16_selector
        mov fs,dx                           ;fs = NULL
        mov gs,dx                           ;gs = NULL
        mov ds,cx                           ;restaura regs
        shr ecx,16
        mov es,cx                           ;restaura es
        popfd                               ;restaura flags
        push esi                            ;32 bit retf para SI:EDI
        push edi                            ;em pm
        db 66h
        retf

;=============================================================================
;| Switx back to RM
;|   AX - Novo DS   DX - Novo SS
;|   CX - Novo ES   EBX - Novo ESP
;|   SI - Novo CS   EDI - Novo EIP
;|   ATEN«√O: INTS DESACTIVADOS!!!
;=============================================================================

_raw_switx2rm:
        push data16_selector              ;ds para segmento de dados
        pop ds
        pushfd                            ;preserva EFLAGS
        pop switx_var
        shl ecx,16                        ;guarda em ECX o DS
        mov cx,ax                         ;e assim eax fica livre
        mov eax,cr0
        and al,0feh                       ;reset bit 0
        mov cr0,eax
        JMPFAR16 _TEXT16,flush_it_again   ;limpa o prefetch queue

  flush_it_again:
        push _text16                      ;o segmento de dados limpinho
        pop ds
        lidt fword ptr REALIDT            ;para carregar a IDT
        push word ptr switx_var+2         ;restaura flags
        mov ds,cx
        shr ecx,16
        mov es,cx
        mov ss,dx                         ;stack switx
        mov esp,ebx
        xor dx,dx
        mov fs,dx                         ;fs = NULL
        mov gs,dx                         ;gs = NULL
        popf
        push si
        push di
        ret

;--------------------------------------------------------------

_raw_exit:
        call irq_restore
;        call set_picmappings
                                            ;n„o esquecer ainda a treta
                                            ;dos irqs
        sti                                 ;garantir ints activadas!!
        mov ss,oldss                        ;Restaura o ss e esp de dos
        mov esp,oldesp                      ;(parece que se n„ for assim, d·
	jmp _quit                           ;molho no DOS 7.10)

;=========================================================================

int_23 proc far

	iret
       endp


int_32 proc far

	iret
       endp



;=========================================================================
;Handlers das 256 ints, para as reflectir para rm
;=========================================================================

common_ints:
 handler_count = 0
      REPT 256
      db 68h
      dw handler_count                  ;push word handler_count
      call int_routine
      handler_count=handler_count+1
      ENDM

;-------------------------------------------------------------------
;rotina geral para as ints

int_routine:
      pushad                            ;salva registos
      mov edx,esp
      push ds es fs gs
      push data16_selector
      push stack16_selector
      push lomem_selector
      pushf
      and word ptr [esp],not 300h       ;cli e CTF
      popf
      pop gs es ds

      mov ebx,_rmstack_top                ;o topo de stack16 em EBX
      mov esi,ebx                         ;e esi para actualizaÁ„o
      sub esi,INT_STACKGAP	          ;retira 100h bytes ao topo (para reentrancia)
      cmp esi,0
      jbe @stack_error		          ;se for 0 erro
      mov _rmstack_top,esi                ;e prepara-se o topo para reentrancia

      sub ebx,50                          ;espaÁo a reservar na rm stack
      mov word ptr es:[ebx+48],ss         ;e guardam-se os valores no topo
      mov dword ptr es:[ebx+44],esp       ;dessa stack (para assegurar a reentrada)
      mov ax,[esp+52]
      and ax,not 300h                     ;limpa IF e TF
      mov word ptr es:[ebx+42],ax         ;flags (para iret)
      mov word ptr es:[ebx+32],ax         ;flags (para popf)
      mov si,word ptr [esp+42]            ;int no
      movzx esi,si                        ;calc int addx
      shl si,2
      mov edi,dword ptr gs:[esi]          ;em edi, o addx
      mov dword ptr es:[ebx+34],edi       ;e salva-o na rm stack
      mov word ptr es:[ebx+38],offset @@rm_back      ;addx de retorno da int
      mov word ptr es:[ebx+40],_TEXT16
      push ds ss                          ;copia registos para a rm stack
      mov edi,ebx                         ;o reg ES È mantido
      mov esi,edx
      pop ds
      mov ecx,8
      cld
      db 67h                              ;e usam-se ESI e EDI
      rep movsd
      pop ds
                                          ;copia registos para a rm stack
      mov ax,_TEXT16                      ;valores para RM
      mov cx,_TEXT16                      ;valor de ES

      mov si,ax                           ;valor de cs
      mov edx,_rmstack_base
      shr edx,4                           ;valor de ss em DX
      mov edi,offset @@rm_int
      jmp [_switch2rm]

  @@rm_int:                               ;-------------> MODO REAL
      popad                               ;restaura regs
      popf                                ;e flags tambÈm
      retf                                ;executa a int

  @@rm_back:
      pop ebx                             ;antigo esp (de PM)
      pop dx                              ;antigo ss (de PM)
      mov ax,DATA16_SELECTOR
      mov cx,DATA16_SELECTOR
      mov si,CODE16_SELECTOR
      mov edi,offset @@rm_go_pm
      jmp cs:[_switch2pm]

  @@rm_go_pm:                             ;-------->MODO PROTEGIDO
      pushf
      pop ax
      add _rmstack_top,int_stackgap       ;restaura a pilha
      and word ptr [esp+52],0200h
      or word ptr [esp+52],ax             ;actualiza as flags
      pop gs fs es ds                     ;e os registos bonitos
      popad
      add esp,4
      iretd

;-------------------------------------------------------------------
; quando h† problemas na reentrÉncia

stack_f00 db 'Erro: Stack Overflow.',0ah,0dh,'7','$'

@stack_error:
      cli
      mov ax,_TEXT16                      ;valores para RM
      mov cx,_TEXT16                      ;valor de ES
      mov si,ax                           ;valor de cs
      mov edx,_rmstack_base
      shr edx,4                           ;valor de ss em DX
      mov edi,offset @@stack_again
      jmp cs:[_switch2rm]

@@stack_again:
      mov si,offset stack_f00
      jmp [_exitproc]

;=========================================================================
;Handler para os IRQ (+ r·pido) que o de cima

common_irq:
  handler_count = 0
      REPT 16
      db 68h
      dw handler_count                  ;push word handler_count
      call handle_irq
      handler_count=handler_count+1
      ENDM

;-------------------------------------------------------------------

handle_irq:
      pushad
      push ds es fs gs
      pushf
      and word ptr [esp],not 300h        ;CLI e CLTF
      popf

      push data16_selector
      pop ds
      push stack16_selector
      pop es

      mov EBX,_rmstack_top                ;o topo de stack16 em EBX
      mov esi,ebx                         ;e esi para actualizaÁ„o
      sub esi,INT_STACKGAP	          ;retira 100h bytes ao topo (para reentrancia)
      cmp esi,0
      jbe  @stack_error		          ;se for 0 erro
      mov _rmstack_top,esi                ;e prepara-se o topo para reentrancia
      sub ebx,16
      mov word ptr es:[ebx+14],ss         ;e guardam-se os valores no topo
      mov dword ptr es:[ebx+10],esp       ;dessa stack (para assegurar a reentrada)
      mov ax,word ptr [esp+52]            ;flags
      and ax,not 300h
      mov word ptr es:[ebx+8],ax          ;flags (para iret)
      mov si,word ptr [esp+42]            ;int no
      shl si,2
      add si,offset _irq_vec_table        ;tabela de IRQS
      mov edi,dword ptr [si]              ;em edi, o addx
      mov dword ptr es:[ebx],edi          ;e salva-o na rm stack
      mov word ptr es:[ebx+4],offset @@irq_rm_back      ;addx de retorno da int
      mov word ptr es:[ebx+6],_TEXT16
                                          ;copia registos para a rm stack
      mov ax,_TEXT16                      ;valores para RM
      mov cx,ax
      mov si,ax                           ;valor de cs
      mov edx,_rmstack_base
      shr edx,4                           ;valor de ss em DX
      mov edi,offset @@irq_rm_int
      jmp [_switch2rm]                    ;a mudar

  @@irq_rm_int:                           ;-------------> MODO REAL
      retf                                ;executa a int

  @@irq_rm_back:
      call trace
      pop ebx                             ;antigo esp (de PM)
      pop dx                              ;antigo ss (de PM)
      mov ax,DATA16_SELECTOR
      mov cx,DATA16_SELECTOR
      mov si,CODE16_SELECTOR
      mov edi,offset @@irq_rm_go_pm
      jmp cs:[_switch2pm]

  @@irq_rm_go_pm:                         ;-------->MODO PROTEGIDO
      add _rmstack_top,int_stackgap       ;restaura a pilha
      pop gs fs es ds
      popad
      add esp,4
      iretd
endp

;=========================================================================

_User_exceptions  dq Exception_count dup(0)  ;vectores de excepáÑo
_Low_ints         dq Exception_count dup(0)  ;ints nos exception vectors

;=========================================================================
;Handler para as exceptions
;-------------------------------------------------------------------------
; Para processar a exception È usado o TSS
;  DX - Nß da exception
;  DS - DATA16_SELECTOR
;  ES - LOMEM_SELECTOR
;  DI - Offset do tss no segmento (linaddx(TSS)=DI+_TSS_LINADDX
;  FS - DATA0_SELECTOR


exception_init:
        push 202h                               ;limpa NT, TF
	popf
        cmp esp,_fstack_top                     ;existe c¢digo de erro
        jne @is_exception_with_ec               ;se sim,Ç uma exception

  @test_sw_int:
        mov si,di                               ;em esi o excp_tss addx
        add esi,_tss_linaddx
        mov bp,word ptr es:[esi]                ;là backlink_selector
        add ebp,dword ptr _GDTR+2
        mov bl,byte ptr es:[ebp+4]
        mov bh,byte ptr es:[ebp+7]
        shl ebx,16
        mov bx,word ptr es:[ebp+2]
        mov edi,ebx                             ;em edi, user_tss

        mov ebx,dword ptr fs:[edi][tss._eip]    ;agora verifica se Ç int xx
        mov gs,word ptr fs:[edi][tss._cs]
        cmp ebx,2
        jb @is_exception_no_ec
        mov ax,word ptr gs:[ebx-2]              ;CS:EIP-2
        cmp al,0cdh                             ;Ç int?
        jne @is_exception_no_ec
        cmp ah,dl                               ;em dl int no
        jne @is_exception_no_ec                 ;Ç int xx?

        mov byte ptr es:[ebp+5],89h             ;USER available tss
        movzx ecx,word ptr es:[esi]             ;USER_TSS_SELECTOR
        ltr cx                                  ;estamos na user Task
        mov ebp,edx                             ;agora procura EXCP_TSS_DESCR
        add bp,_APP_SELECTOR+8
        add ebp,dword ptr _GDTR+2
        mov byte ptr es:[ebp+5],89h             ;EXCEPTION - Avail TSS
        mov ss,word ptr fs:[edi][tss._ss]       ;là user SS
        mov esp,dword ptr fs:[edi][tss._esp]    ;là user ESP

        push dword ptr fs:[edi][tss._eflags]
        push dword ptr fs:[edi][tss._cs]        ;return CS:EIP
        push dword ptr fs:[edi][tss._eip]

        shl dx,3                                ;achar o offset da INT
        add dx,offset _Low_ints                 ;na tabela _Low_ints
        push dword ptr ds:[edx]                 ;INT_CS
        push dword ptr ds:[edx+4]               ;INT_EIP

        mov eax,dword ptr fs:[edi+tss._eax]     ;restaura registos
        mov ebx,dword ptr fs:[edi+tss._ebx]
        mov ecx,dword ptr fs:[edi+tss._ecx]
        mov edx,dword ptr fs:[edi+tss._edx]
        mov esi,dword ptr fs:[edi+tss._esi]
        mov ebp,dword ptr fs:[edi+tss._ebp]
        mov ds,word ptr fs:[edi+tss._ds]
        mov es,word ptr fs:[edi+tss._es]
        mov gs,word ptr fs:[edi+tss._gs]
        push word ptr fs:[edi+tss._fs]
        mov edi,dword ptr fs:[edi+tss._edi]
        pop fs
        clts                                    ;clear task switx
        db 66h
        retf                                    ;executa SW INT

;-------------------------------------
  @is_exception_with_ec:
        pop eax                                 ;codigo de erro
        jmp @go_to_what_matters

  @is_exception_no_ec:
        xor eax,eax

  @go_to_what_matters:
        mov byte ptr es:[ebp+5],89h             ;USER available tss
        movzx ecx,word ptr es:[esi]             ;USER_TSS_SELECTOR
        ltr cx                                  ;estamos na user Task
        mov ebp,edx                             ;agora procura EXCP_TSS_DESCR
        add bp,_APP_SELECTOR+8
        add ebp,dword ptr _GDTR+2
        mov byte ptr es:[ebp+5],89h             ;EXCEPTION - Avail TSS
        push dword ptr fs:[edi][tss._ss]        ;Cria um stack frame
        push dword ptr fs:[edi][tss._esp]       ;compat°vel DPMI

        push dword ptr fs:[edi][tss._eflags]
        push dword ptr fs:[edi][tss._cs]
        push dword ptr fs:[edi][tss._eip]

        push eax                                ;error code
        push large CODE16_SELECTOR
        push large offset @return_to_caller

        shl edx,3
        add dx,offset _User_exceptions
        push dword ptr ds:[edx]                 ;handler CS
        push dword ptr ds:[edx+4]               ;handler EIP

        mov eax,dword ptr fs:[edi+tss._eax]     ;restaura registos
        mov ebx,dword ptr fs:[edi+tss._ebx]
        mov ecx,dword ptr fs:[edi+tss._ecx]
        mov edx,dword ptr fs:[edi+tss._edx]
        mov esi,dword ptr fs:[edi+tss._esi]
        mov ebp,dword ptr fs:[edi+tss._ebp]
        mov ds,word ptr fs:[edi+tss._ds]
        mov es,word ptr fs:[edi+tss._es]
        mov gs,word ptr fs:[edi+tss._gs]
        push word ptr fs:[edi+tss._fs]
        mov edi,dword ptr fs:[edi+tss._edi]
        pop fs

	clts
        db 66h
        retf                                    ;chama entÑo a rotina

 @return_to_caller:

        push edi
        push es
        mov es,word ptr [esp+26]                ;user ss
        sub dword ptr [esp+22],18
        mov edi,dword ptr [esp+22]              ;user esp
        pop word ptr es:[edi]                   ;es
        pop dword ptr es:[edi+2]                ;EDI
        add esp,4                               ;c¢digo de erro
        pop dword ptr es:[edi+6]                ;return EIP
        pop dword ptr es:[edi+10]               ;return CS
        pop dword ptr es:[edi+14]               ;return Eflags
        lss esp,fword ptr [esp]                 ;switx to user stack

        pop es
        pop edi

        iretd

;=========================================================================

callback_struc struc
        db 16h          ;push ss
        db 54h          ;push sp
        db 0eh          ;push cs
        db 50h          ;push ax (vaga pra IP)
        db 0fh,0a8h,0fh,0a0h ;push gs fs
        db 1eh,06h          ;push ds es
        db 09ch             ;pushf
        db 66h,60h          ;pushad
        db 0b8h             ;mov ax,tocall.selector
 P_sel  dw 0
        db 66h,0beh         ;mov esi,tocall.offset
 P_ofs  dd 0
        db 0bbh             ;mov bx,struct.selector
 S_sel  dw 0
        db 66h,0bfh         ;mov edi,struct.offset
 S_ofs  dd 0
        db 0eah             ;jmp
 h_ofs  dw 0                ;addx mecanismo de processamento dos callbacks
 h_seg  dw 0
ends
;=============================================================================
Callback_count EQU 16

callbacks callback_struc Callback_count dup (<>)

;-------------------------------------------------------
;aloca uma Callback
;assume DS - TEXT16/DATA16_SELECTOR

alloc_callback proc near
           arg @tocall:Pointer32, @estr:Pointer32

           mov si,offset callbacks
           mov bx,Callback_count                ;nß de callbacks existentes

   @ac_cykle:
           cmp word ptr [si][callback_struc.h_seg],0
           je @found_avail_callback
           add si,size Callback_struc
           dec bx
           jnz @ac_cykle
           stc
           jmp @alloc_callback_quit

  @found_avail_callback:
           mov dx,@tocall.selector
           mov ebx,@tocall.ofset
           mov cx,@estr.selector
           mov eax,@estr.ofset
           mov word ptr [si][callback_struc.P_sel],dx
           mov dword ptr [si][callback_struc.P_ofs],ebx
           mov word ptr [si][callback_struc.s_sel],cx
           mov dword ptr [si][callback_struc.s_ofs],eax
           mov word ptr [si][callback_struc.h_seg],_TEXT16
           mov word ptr [si][callback_struc.h_ofs],offset common_callback
           clc

  @alloc_callback_quit:                         ;em SI o offset em _TEXT16
           ret
endp

;-------------------------------------------------------
;Liberta uma callback alocada
;assume DS - TEXT16/DATA16_SELECTOR

free_callback proc near
              arg @cofs:word

              mov si,@cofs
              cmp word ptr [si],1654h
              jnz @fail_free_callback
              clc
              mov dword ptr [si][callback_struc.h_ofs],0
              jmp @free_callback_quit

  @fail_free_callback:
              stc

  @free_callback_quit:
              ret
endp
;=============================================================================

common_callback:                          ;entra em Modo real
        cli
        movzx eax,ax                      ;CWD
        push _text16                      ;ds para seg de dados
        pop ds

        mov ecx,_pmstack_top              ;topo da stack em PM
        mov edx,ecx
        sub edx,INT_STACKGAP              ;tira valor de reentrÉncia
        cmp edx,0                         ;se final=0 then erro
        jbe @stack_error                  ;deve pendurar o PC!!
        mov _pmstack_top,edx              ;e actualiza o valor
                                          ;em ecx o ESP
                                          ;criar o stack frame
        mov edx,_pmstack_base
        shr edx,4
        mov fs,dx
        sub ecx,12+4
        mov dword ptr fs:[ecx],edi        ;struc.ofset
        mov word ptr fs:[ecx+4],bx        ;struc.selector
        mov dword ptr fs:[ecx+6],esi      ;tocall.eip
        mov word ptr fs:[ecx+10],ax       ;tocall.cs
        mov ax,ss                         ;achar rm ESP
        shl eax,4
        add eax,esp                       ;ESP da Stack de RM mas em PM
        mov dword ptr fs:[ecx+12],eax

        mov ebx,ecx                       ;PM ESP
        mov cx,lomem_selector             ;
        mov ax,lomem_selector             ;Selector da pilha de RM (Q Ç Lomem)
        mov si,code16_selector
        mov dx,stack32_selector
        mov edi,offset @callback_pm
        jmp [_switch2pm]

  @callback_pm:
        xor ebx,ebx
        mov edi,dword ptr [esp]          ;Struc PTR
        mov es,word ptr [esp+4]
        mov esi,dword ptr [esp+12]       ;stack PTR
        mov ecx,(size Registers)/2
        cld
        db 67h
        rep movsw                        ;copia do stack frame para a Struc
        pop edi                          ;struc Pointer
        pop es
        pop edx                          ;tocall pointer
        pop bx
        pop esi                          ;stack ESP
        mov eax,dword ptr ds:[esi+50]    ;rm cs:ip
        mov dword ptr es:[edi][Registers._vip],eax
        add word ptr es:[edi][Registers._vsp],8
        add esi,52
        pushfd
        db 66h
        push cs                          ;para ITETD
        db 66h,68h                       ;push32 off @to_return
        dw @to_return,0
        push ebx
        push edx
        db 66h
        retf                            ;proc a chamar

  @to_return:
        movzx esi,word ptr es:[edi][registers._vss]     ;Rm stack ptr
        movzx ebx,word ptr es:[edi][registers._vsp]
        push esi
        shl esi,4
        add esi,ebx                                     ;calcula rm stack linaddx
        sub ebx,46                                      ;tira quanto vamos precisar
        sub esi,46
        push es                                         ;es:edi - Stack
        xchg esi,edi                                    ;ds:esi - struc
        push lomem_selector
        pop es
        pop ds
        mov ecx,23
        cld
        db 67h                                          ;copia Registers
        rep movsw                                       ;para a stack RM

        mov ax,_text16
        mov cx,ax
        mov si,ax
        pop edx
        mov edi,offset @to_return_real
        cli
        jmp cs:[_switch2rm]

  @to_return_real:
        popad                                ;p‰e tudo como estava
        popf
        pop es ds fs gs
        retf

;=============================================================================
;=============================================================================
Call_COUNT = 0

IRQ_CALLBACKS:
REPT 17
     db 68h
     dw call_count
     db 0eah
     dw offset irq_callback_handler
     dw _text16
     call_count = call_count+1
ENDM
;----------------------------------------------------------------

irq_callback_buf dd 17*2 dup(0)                 ;ptrs para os IRQ Handlers

;----------------------------------------------------------------
irq_callback_handler:
    push gs fs ds es
    pushad
    push _text16                      ;ds para seg de dados
    pop ds

    mov ebx,_pmstack_top              ;topo da stack em PM
    mov edx,ebx
    sub edx,INT_STACKGAP              ;tira valor de reentrÉncia
    cmp edx,0                         ;se final=0 then erro
    jbe @stack_error                  ;
    mov _pmstack_top,edx              ;e actualiza o valor de topo

    movzx edx,word ptr [esp+40]       ;Nß do callback
    shl dx,3                          ;x8 = ofs no buffer
    add dx,offset irq_callback_buf    ;e j† temos o addx do ptr
    mov ecx,_pmstack_base
    shr ecx,4
    mov fs,cx                         ;stack seg
    sub ebx,26
    mov eax,ds:[edx]                  ;là proc.eip
    mov ecx,ds:[edx+4]                ;là proc.cs
    mov fs:[ebx],eax                  ;stack frame para retf
    mov fs:[ebx+4],ecx
    mov dword ptr fs:[ebx+8],offset @@irq_cb_donecall ;stack frame para iretd
    mov esi,code16_selector
    mov dword ptr fs:[ebx+12],esi
    pushfd
    pop edx
    mov fs:[ebx+16],edx                 ;Eflags
    mov fs:[ebx+20],esp                 ;Stack, adorada stack
    mov fs:[ebx+24],ss                  ;guardada para n∆o haver merda
    mov ax,data16_selector              ;passa para PM
    mov cx,ax
    mov dx,stack32_selector
    mov edi,offset @@irq_cb_go2pm
    jmp [_switch2pm]

  @@irq_cb_go2pm:
    db 66h                              ;executa INT
    retf

  @@irq_cb_donecall:                    ;e retorna ao querido procedimento
    pop ebx                             ;para saltar para modo real
    pop dx
    mov ax,_text16
    mov si,ax
    mov cx,ax
    mov edi,offset @@irq_cd_go2rm
    jmp [_switch2rm]

  @@irq_cd_go2rm:
    popad
    pop es ds fs gs
    add esp,2                           ;int no
    iret


;##############################################################################

;seta user exception vec DL em CX:EBX

set_excp_vec:
     movzx edx,dl
     shl dx,3
     add dx,offset _user_exceptions
     mov ds:[edx+2],cx                          ;selector do novo vector
     mov ds:[edx+4],ebx                         ;offset do novo vector
     ret

;seta user low int vec DL em CX:EBX

set_lowint_vec:
     movzx edx,dl
     shl dx,3
     add dx,offset _low_ints
     mov ds:[edx+2],cx                          ;selector do novo vector
     mov ds:[edx+4],ebx                         ;offset do novo vector
     ret



;##############################################################################
;ROTINAS DE PAGING
;##############################################################################

_Pagetable_base   dd 0                  ;base da page Table
_Pagetable_limit  dd 0                  ;limite da page Table
_Pagedir_limit    dd 0                  ;limite da page directory
_Pagedir_base     dd 0                  ;base da page directory
_Pagedir_seg      dw 0                  ;segmento da page directory
_Table_count      dw 0                  ;nß de tabelas, sem contar c/ Pagedir
_Last_table_count dw 0                  ;nß de p†ginas na £ltima tabela

;==========================================================
; void count_pages(bytes:dword)
; RETORNA COM
;   EAX - N∫ de P·ginas
;   EBX - Bytes na ˙ltima p·gina
;==========================================================

count_pages proc near
	arg @how_much_bytes:dword

	mov eax,@how_much_bytes
	mov ebx,eax
	shr eax,12			;eax=hmb div 4096
	and ebx,0fffh			;ebx=hmb mod 4096
	jz @no_inc_page
	inc eax
					;ok, em eax o n∫ de p·ginas
 @no_inc_page:				;e em ebx o n∫ de bytes na ˙ltima p·gina (Ò interessa)
	ret
endp

;==========================================================
; void count_tables(pages:dword)
; RETORNA COM
;   EAX - N∫ de page tables
;   EBX - N∫ de p·ginas na ˙ltima page table
;==========================================================

count_tables proc near
	arg @how_much_pages:dword

	mov eax,@how_much_pages
	mov ebx,eax
	shr eax,10			;eax=pagecount div 1024
	and ebx,03ffh			;ebx=pagecount mod 1024
	jz @no_inc_table
	inc eax
					;ok, em eax o n∫ de tables
 @no_inc_table:				;e em ebx o n∫ de p·ginas na ˙ltima table
	ret
endp

;==========================================================
;void Paging_init(max_addx)
;
; Aloca a estrutura de p†ginas para endereáar max_addx.
; ê alocada sempre uma tabela a mais, no caso de ser necess†ria
;==========================================================

Paging_init proc near
	call count_pages pascal,EAX     ;quantas p†ginas sÑo
        call count_tables pascal,eax    ;e quantas tabelas sÑo
        inc ax                          ;adiciona mais uma tabela
        mov _table_count,ax             ;guarda o nß de tabelas
        mov _Last_table_count,ax        ;e guarda o nß de p†ginas na £ltima tabela
        inc ax                          ;adiciona a page directory
        shl eax,12-4                    ;calcula o nß de par†grafos a alocar
        push eax                        ;guarda-se o tamanho
        add eax,(4096*2)/16		;n„o esquecer o alinhamento (+8k)
	call dos_getmem pascal,ax	;e tenta alocar o bloco
	mov si,offset memory_error	;no caso de ter corrido mal, previne-se em si
	or dl,dl
        jz quit_error			;se erro aborta
					;em ax o segmento
	movzx eax,ax			;cwd
        shl eax,4			;e alinha-se as page tables a 4k
        add eax,4096                    ;slack
        and eax,not 0fffh               ;o que significa os ˙ltimos 12 bit do linear addx=0
	mov _Pagedir_base,eax		;guarda-se o endereÁo linear da PD
        mov ecx,eax
        shr ecx,4                       ;o segmento da page dir j† alinhado
        mov _pagedir_seg,cx             ;guarda-se pra + † frente
        add eax,1000h                   ;e da page table
	mov _Pagetable_base,eax		;guarda-se o endereÁo linear
        pop ebx                         ;em ebx o tamanho da page table
        shl ebx,4                       ;converte paragr em bytes
        mov ecx,ebx                     ;que tb vai ser usado + Ö frente
        dec ebx                         ;limite Ç sempre tamanho-1
        mov _pagedir_limit,ebx          ;e guarda-se o limite
        sub ebx,1000h                   ;tira-se a page directory
        mov _pagetable_limit,ebx        ;e guarda-se o limite

        mov es,_pagedir_seg             ;agora limpa-se tudo
        xor eax,eax
        shr ecx,12                      ;em ecx o nß de p†ginas

    @page_loop:
        push ecx                        ;limpa uma p†gina
        xor edi,edi
        mov ecx,4096/2
        cld
        rep stosw
        push es
        add word ptr [esp],256          ;avanáa para a p†g seguinte
        pop es
        pop ecx
        loop @page_loop
        ret
endp

;##############################################################################
;ROTINAS VCPI
;##############################################################################

VCPI_f00   db 'Erro VCPI: N„o existem p·ginas suficientes livres.',0ah,0dh,7,'$'
VCPI_f01   db 'Erro VCPI: Falha ao inicializar a interface de modo protegido.',0ah,0dh,7,'$'
VCPI_f02   db 'Erro: ConfiguraÁ„îes do PIC n„o suportadas.',0ah,0dh,7,'$'

;-------------------------------------------------------------------

_vcpi_pagecount dd 0                   ;nß de p†ginas alocadas para a heap
v_pt_free	dd ?                   ;linear addx da 1™ page table livre
v_1pt_free      dw 0                   ;primeira entrada livre a 1ß page table
switx_var       dw 0                   ;vari†vel tempor†ria
vcpi_entry	dd 0                   ;ptr para VCPI entry point
		dw VCPI_01_SELECTOR    ;a ser chamado para switx V86
                dw 0

vcpi_struc_addx dd 0                   ;endereáo linear desta struc
vcpi_cr3	dd 0	               ;valor para cr3
vcpi_gdtr	dd 0                   ;linear addx do valor para gdtr
vcpi_idtr	dd 0                   ;linear addx do valor para idtr
vcpi_ldtr	dw 0                   ;valor para ldtr
vcpi_tss	dw _APP_SELECTOR       ;valor para tr
vcpi_eip	dw offset @vcpi_entry_pm ;eip do ponto de entrada em pm
                dw 0
vcpi_cs		dw CODE16_SELECTOR     ;cs do ponto de entrada em pm
                dw 0

;-------------------------------------------------------------------

VCPI_STACKSZ  EQU 128                  ;Tamanho da stack tempor†ria

vcpi_stack_area db vcpi_stacksz dup(0) ;stack tempor†ria
vcpi_stack_top label word              ;offset do topo da stack

vcpi_stackptr   dw offset vcpi_stack_top ;ptr32 para a stack tempor†ria
                dw 0
                dw data16_selector,0

;=========================================================================
vcpi_init:
	and start_type,2		;seta VCPI flag
        mov _switch2pm,offset _vcpi_switx2pm
        mov _switch2rm,offset _vcpi_switx2rm
        mov _exitproc,offset _vcpi_exit
        mov _failproc,offset _vcpi_exit_error
        mov _Pic_notify, offset _vchange_Pic

        mov ax,0de02h                   ;get max phis addx
        int 67h
        mov eax,edx
        add eax,1000h                   ;adiciona £ltima p†gina

        call paging_init                ;e inicializa as page tables

        mov bx,_pagedir_seg             ;segmento da 1¶ page table
        add bx,100h
        mov es,bx
        xor di,di

        push ds
        mov eax,dword ptr _GDTR+2       ;segmento da GDT
        shr eax,4                       ;porque j† foi alocada na mem¢ria
        mov ds,ax                       ;e Ç a° que deve estar!

	mov ax,0DE01h			;obter VCPI PM interface
	mov si,VCPI_01_Selector  	;offset dos VCPI_SEL na GDT
        int 67h
        pop ds
	mov si,offset vcpi_f01
	or ah,ah
	jnz quit_error

	mov vcpi_entry,ebx              ;vcpi entry point
        mov v_1pt_free,di               ;1¶ entrada livre
	movzx eax,di			;alinha pela prÛxima pagetable
	add eax,4096
	and eax,not 0fffh		;na mesma pelos 12 bits
	add eax,_pagetable_base		;adiciona-lhe a base
	mov v_pt_free,eax		;e guarda ent„o o addx da 1™ page table livre

  @clear_os:
	and byte ptr es:[di+1],0f1h	;limpa os bits "OS Reserved"
	sub di,4			;recua para a page entry anterior
	jnc @clear_os			;sempre assim, atÈ acabar
					;agora que est· tudo bem, criar a page directory
	xor edi,edi
	movzx ebx,_Table_count	        ;n∫ de tabelas
	mov ecx,_pagetable_base		;em cx, o endereÁo linear das tabelas
	shr ecx,12			;que se converte no n∫ de p·gina
        mov es,_pagedir_seg             ;segmento da page directory

  @table_loop:
	mov ax,0de06h			;obtÈm endereÁo fÌsico no 1∫ MB
	int 67h
        or ah,ah
        jnz quit_error                  ;no caso de erro, vai-se embora
	and dh,0f1h			;limpa bits "OS RESERVED"
	or dl,07h                       ;seta bits USER, R/W e Present
        mov eax,edx
        stosd                           ;e guarda na page directory
	inc ecx				;avanáa no index tambÈm
	dec bx				;decrementa o contador
        jnz @table_loop                 ;atÈ que seja <0

	xor ecx,ecx
	mov ax,0de06h			;agora, o endereÁo fÌsico da page directory
	mov cx,_pagedir_seg
        shr cx,8                        ;em cx o page nß
	int 67h
	mov vcpi_cr3,edx		;que se guarda no sÌtio apropriado
        xor edx,edx
        mov dx,cs
        shl edx,4
	mov ecx,offset _Gdtr		;calcula o addx linear da GDT
	mov ebx,offset _idtr		;e da IDT tambÈm!
        add ecx,edx
        add ebx,edx
	mov vcpi_idtr,ebx		;e guarda-os na estrutura VCPI
	mov vcpi_gdtr,ecx

;--------------------------------------------------------------------------
        mov ax,0de0ah                   ;get pic mappings
        int 67h
        mov si,offset VCPI_f02          ;no caso de haver erro
        cmp bl,_Pic_vecs                ;master normal?
        jne quit_error                  ;nÑo,sai c/erro
        cmp cl,_Pic_vecs+1              ;slave normal?
        jne quit_error                  ;nÑo,sai c/erro

        call _vcpi_heap_init            ;aloca as paginas da heap
        call find_pic_slots
        movzx bx,_new_master            ;novo master
        movzx cx,_pic_vecs+1            ;novo slave
        mov ax,0DE0Bh                   ;informa o server das mudanáas
        int 67h
        or ah,ah
        jz @vcpi_pic_accepted           ;e continua se nÑo houve merda

        mov si, offset Vcpi_f02
        jmp [_failproc]

   @vcpi_pic_accepted:
        sti
comment %
        mov eax,_text16
        shl eax,4
        add ax,offset vcpi_cr3
        mov vcpi_struc_addx,eax
%
        xor eax,eax
        mov ax,_text16                 ;calcular addx linear
        shl eax,4                       ;da VCPI switx struc
        xor edx,edx
        mov dx,offset vcpi_cr3
        add eax,edx
        mov vcpi_struc_addx,eax

;----------------------------------------------------
	call irq_reloc                  ;reloca o master PIC
;        call _vchange_Pic

        call startup_tss

;----------------------------------------------------

comment %
        mov eax,offset ctest
        mov ebx,offset regs
        mov ecx,code16_selector
        mov edx,data16_selector
        call alloc_callback pascal,ecx,eax,edx,ebx
        mov cptr,si
        mov word ptr cptr+2,_text16
%
        mov si,code16_selector
        mov dx,stack32_selector
        mov ebx,_pmstack_top
        xor edi,edi
        cli
        mov di,offset @vcpi_pmentry
        mov ax,data16_selector
        mov cx,ax
        jmp [_switch2pm]


  @vcpi_pmentry:
         sti
         mov eax,50
    @aloop:
         mov ecx,655350
    @dloop:
         mov edx,ecx
         shr edx,16
         add edx,256
         mov ebx,edx
         sub ebx,3
         dec ecx
         jnz @dloop
         dec eax
         jnz @aloop

         xor edx,edx
         xor ecx,ecx
         mov eax,code16_selector
         mov dx,offset @vgo_rm
         mov cx,offset common_irq
         add cx,6

         pushfd
         push eax
         push edx
         push ax
         push cx
         retf

       cli
        mov ax,ds
        mov es,ax


 @vgo_rm:
        push data16_selector
        pop ds
        xor edi,edi
        mov ax,_text16
        mov cx,ax
        mov si,ax
        mov dx,oldss
        mov ebx,oldesp
        mov di,offset @vcpi_rmentry
        cli
        jmp [_switch2rm]

     @vcpi_rmentry:
        sti
        mov ax,_text16
        mov ds,ax
        mov es,ax
        mov fs,ax
        mov gs,ax
        mov eax,g1
        mov ebx,g2
        mov ecx,g3
        mov edx,g4
        call trace

        sti
        jmp [_exitproc]
;----------------------------------------------------
g1 dd 0
g2 dd 0
g3 dd 0
g4 dd 0
Cptr dw ?,?
regs Registers <>


Ctest:
        push es
        push edi
        sub word ptr es:[edi][registers._vsp],4
        mov word ptr ds:[esi],06FEEh
        mov word ptr ds:[esi+2],03333h

        mov ax,lomem_selector
        mov es,ax
        mov edi,0b8000h
        mov eax,0fc65h
        mov ecx,500
        cld
        db 67h
        rep stosw
        pop edi
        pop es
        mov es:[edi][registers._VAX],0fffeh
;        jmp @fgh

        iretd
@fgh:
      push data16_selector
      pop ds
      mov ebx,oldesp
      mov ax,_TEXT16                      ;valores para RM
      mov cx,ax
      mov si,ax                           ;valor de cs
      mov dx,oldss
      mov edi,offset @@tracing
      jmp [_switch2rm]                    ;a mudar

    @@tracing:
      mov eax,dword ptr regs._vip
      mov bx,word ptr regs._vss
      mov cx,word ptr regs._vsp
      mov si,word ptr regs._vax
      call trace
      sti
      jmp [_exitproc]

Ctest2:
        mov ax,lomem_selector
        mov es,ax
        mov edi,0b8100h
        mov eax,0ddddh
        mov ecx,500
        cld
        db 67h
        rep stosw

        iretd

;=============================================================================
;|  Muda a Base do Pic
;=============================================================================

_vchange_Pic proc near
             arg @nbase:word

        mov bx,@nbase                   ;novo master
        movzx cx,_pic_vecs+1            ;novo slave
        mov ax,0DE0Bh                   ;informa o server das mudanáas
        int 67h
	mov si,offset vcpi_f02
	or ah,ah
	jnz quit_error

   @v_pic_accepted:
        call set_Masterpic pascal,@nbase
        ret
endp

;=============================================================================
;| Switx to PM
;|   AX - Novo DS   DX - Novo SS
;|   CX - Novo ES   EBX - Novo ESP
;|   SI - Novo CS   EDI - Novo EIP
;=============================================================================
_vcpi_switx2pm:
       cli
       pushf                            ;preserva flags no salto
       push _text16                     ;ds ao que deve ser
       pop ds
       pop switx_var                    ;temp EFLAGS
       shl edx,16                       ;guarda em EDX ss
       mov dx,ax                        ;e tambÇm ds
       shl ecx,16                       ;guarda em ECX es
       mov cx,si                        ;e tambÇm cs
       mov esi,vcpi_struc_addx          ;e entra em PM
       mov ax,0de0ch
       int 67h

  @vcpi_entry_pm:
       movzx eax,cx                     ;cs em EAX
       mov ds,dx                        ;restura o ds
       shr ecx,16                       ;restaura o es
       mov es,cx
       shr edx,16                       ;e o ss tambÇm
       mov ss,dx                        ;stack switx
       mov esp,ebx
       xor dx,dx
       mov gs,dx
       mov fs,dx
       push cs:switx_var                ;eflags na stack
       and  word ptr [esp],not 4000h    ;limpa NT flag
       popf                             ;restaura eflags
       push eax                         ;retf stack frame
       push edi
       db 66h
       retf                             ;salta para cs:EIP

;=============================================================================
;| Switx to RM
;|   AX - Novo DS   DX - Novo SS
;|   CX - Novo ES   EBX - Novo ESP
;|   SI - Novo CS   EDI - Novo EIP
;=============================================================================
_vcpi_switx2rm:
        cli
        push data16_selector            ;ds certo para a execuáÑo
        push data16_selector            ;es tambÇm
        pop ds es
        pushf                           ;preservar EFLAGS
        pop switx_var                   ;na vari†vel do costume
        lss esp,fword ptr vcpi_stackptr ;stack switx
        pushad                          ;valores pra restaurar
        mov eax,esp                     ;esp de entrada em RM
        xor ecx,ecx
        xor edx,edx
        mov ebx,_text16
        mov cx,offset @vcpi_entry_rm
        sub esp,8                       ;FS e GS
        push ebx                        ;DS = TEXT16
        push ebx                        ;ES = TEXT16
        push ebx                        ;SS = TEXT16
        push eax                        ;ESP
        pushfd                          ;Eflags
        push ebx                        ;CS = TEXT16
        push ecx                        ;EIP
        push data0_selector             ;all mapped mem
        pop ds
        mov ax,0de0ch                   ;switx para V86
        call fword ptr es:vcpi_entry

  @vcpi_entry_rm:
        popad                           ;restaura os regs
        mov ss,dx                       ;switx stack
        mov esp,ebx
        xor dx,dx
        mov fs,dx
        mov gs,dx
        push switx_var                  ;flags
        push si                         ;CS
        push di                         ;EIP
        mov ds,ax
        mov es,cx
        iret                            ;jmp SI:DI restore flags


;====================================================================
; Inicializa a Heap em VCPI

_vcpi_heap_init proc near

       mov ax,0de03h                    ;get free page count
       int 67h
       push edx                         ;free_pagecount na stack
       call count_pages pascal,_heap_min;calc paginas para heap_min
       mov si,offset vcpi_f00           ;prepara-se para abortar
       cmp eax,dword ptr [esp]          ;existem suficientes?
       jge quit_error
       push eax                         ;guarda p†ginas para heap_min
       call count_pages pascal,_heap_max ;agora conta para _heap_max
       pop edx                          ;heap_min count
       pop ecx                          ;free page count
       cmp ecx,eax                      ;usa heap_max
       jge @vcpi_fnsw
       mov eax,edx                      ;usa Heap_min

  @vcpi_fnsw:
       mov bx,_pagedir_seg              ;procura a primeira page table livre
       add bx,512                       ;salta a page dir+1¶ pagetable
       mov es,bx                        ;Ç o seg a usar
       xor cx,cx
       xor edi,edi
       mov esi,eax                      ;em esi o nß de pages
       mov _vcpi_pagecount,eax          ;e guarda para info posterior

  @vcpi_alloc_pages:
       mov ax,0de04h                    ;VCPI alloc page
       int 67h
       or edx,07h                       ;seta P,R/W e U/S bits
       mov es:[edi],edx                 ;escreve na page table
       add edi,4                        ;avanáa para a pr¢xima
       inc cx                           ;table_count
       cmp cx,1024                      ;foi a £ltima da page table?
       jne @vcpi_alloc_pages_01
       add bx,256                       ;pr¢xima pagetable
       mov es,bx
       xor edi,edi

  @vcpi_alloc_pages_01:
       dec esi                          ;page_count
       jnz @vcpi_alloc_pages
       ret
endp

;====================================================================
; Liberta a Heap em VCPI

_vcpi_heap_free proc near

       mov bx,_pagedir_seg              ;procura a primeira page table livre
       add bx,512                       ;salta a page dir+1¶ pagetable
       mov es,bx                        ;Ç o seg a usar
       xor cx,cx
       xor edi,edi
       mov eax,_vcpi_pagecount
       mov esi,eax                      ;em esi o nß de pages

  @vcpi_free_pages:
       mov edx,es:[edi]                 ;là endereáo f°sico
       and edx,not 0fffh                ;e limpa as flags da pt entry
       mov ax,0de05h                    ;VCPI free page
       int 67h
       add edi,4                        ;avanáa para a pr¢xima
       inc cx                           ;table_count
       cmp cx,1024                      ;foi a £ltima da page table?
       jne @vcpi_free_pages_01
       add bx,256                       ;pr¢xima pagetable
       mov es,bx
       xor edi,edi

  @vcpi_free_pages_01:
       dec esi                          ;page_count
       jnz @vcpi_free_pages
       ret
endp
;====================================================================
zzmsg_1 db 'Entrada PM VCPI ZZ-z',0ah,0dh,'$'
zzmsg_2 db 'Entrada RM VCPI ZZ-z',0ah,0dh,'$'
zzmsg_3 db 'Sa°da VCPI ZZ-z',0ah,0dh,'$'
zzmsg_4 db 'VCPI_HEAP_FREE ZZ-z',0ah,0dh,'$'
zzmsg_5 db 'IRQ_RESTORE',0ah,0dh,'$'
zzmsg_6 db 'VCPI IRQ REMAP',0ah,0dh,'$'
zzmsg_7 db 'SET_PICMAPPINGS',0ah,0dh,'$'
zzmsg_8 db 'JMP QUIT',0ah,0dh,'$'
;====================================================================
; Procedimentos de sa°da

_vcpi_exit_error:                       ;sa°da com msg de erro
        call echo pascal,si

_vcpi_exit:
        push _text16
        pop ds
        call _vcpi_heap_free
        cli
        movzx bx,_pic_vecs              ;antigo master
        movzx cx,_pic_vecs+1            ;antigo slave
        mov ax,0DE0Bh                   ;informa o server das mudanáas
        int 67h
        call irq_restore
        sti                             ;garantir ints activadas!!
        mov ss,oldss                    ;Restaura o ss e esp de dos
        mov esp,oldesp                  ;(parece que se n„ for assim, d·

        sti
	jmp _quit                       ;molho no DOS 7.10)


;==============================================================================
;##############################################################################################

xms_f00	  db 'Erro XMS: N„o existe memÛria expandida suficiente ou est· demasiado fragmentada'
	  db 7,0ah,0dh,'$'
xms_f01	  db 'Erro XMS: Problemas ao alocar memÛria expandida.',7,0ah,0dh,'$'


_xms_heap_handle  dw -1		;handle do bloco XMS
_xms_heap_base    dd ?		;endereÁo linear da base da heap
_xms_heap_top     dd ?		;endereÁo linear do topo da heap

;=========================================================================
extrn xms_getfreemem:near
extrn xms_malloc:near
extrn xms_lockblock:near
extrn xms_dalloc:near

xms_init:

	call xms_getfreemem		;quanto È que existe livre?
	mov si,offset xms_f00		;msg de erro
	xor ebx,ebx
	mov bx,ax			;converte kb em bytes
	shl ebx,10			;em EBX o tamanho do maior bloco disponÌvel
	cmp _heap_min,ebx		;ser· que È suficiente?
	jg quit_error			;n„o? ent„o sai

	mov edx,ebx			;em edx os bytes a alocar
	mov esi,_heap_max		;se _heap_max=-1
	cmp esi,-1			;alocar toda a mem livre
	je @x_just_alloc_it		; se n„o...

	cmp ebx,esi			;ser· que xms_mem>=_heap_max?
	jb @x_just_alloc_it		;n„o? aloca o que pode

	mov edx,esi			;sim? ent„o aloca _heap_max

 @x_just_alloc_it:
	shr edx,10			;aloca, mas em Kb
	mov edi,edx			;salva os preciosos kb para + tarde
	call xms_malloc pascal,dx
	mov si,offset xms_f01		;msg de erro
	or ax,ax			;correu bem?
	jz quit_error			;parece que n„o!

	mov _xms_heap_handle,dx		;guarda o handle do bloco
	call xms_lockblock pascal,dx	;e fixa-o na memÛria
	or ax,ax			;ser· que est· tudo bem?
	jz _xms_quit			;se n„o, sai pelo sÌtio devido

	mov _xms_heap_base,edx		;guarda o addx linear de base
	shl edi,10			;e cacula ent„o o topo
	add edx,edi
	mov _xms_heap_top,edx		;guarda o addx linear de topo


;====================================================================================

_xms_exit_error:
        call echo pascal,si

_xms_quit:
	call xms_dalloc pascal,_xms_heap_handle
	jmp _quit

;##############################################################################################
;##############################################################################################

dpmi_f32   db 'Erro DPMI: N„o s„o suportados clientes de 32 bit',0ah,0dh,7,'$'
dpmi_f00   db 'Erro DPMI: N„o foi possÌvel entrar em Modo Protegido',0ah,0dh,7,7,7,'$'
dpmi_f01   db 'Erro DPMI: N„o existem selectores suficientes disponÌveis.',0ah,0dh,7,7,7,'$'
dpmi_f02   db 'Erro DPMI: Problemas ao inicializar os Descriptors',0ah,0dh,7,7,7,'$'
dpmi_f03   db 'Erro DPMI: Problemas ao libertar Descriptor(s)',0ah,0dh,7,7,7,'$'
dpmi_f04   db 'Erro DPMI: N„o existe memÛria suficiente para executar a aplicaÁ„o',0ah,0dh,7,'$'


dpmi_mem_info dd 32 dup(0)	;buffer para info de memÛria em DPMI (DPMI 1.0+ requere 128b)
_dpmi_v1      db False		;flag de DPMI host V1.0+
_sel_incval   dw 0		;selector inc value


dpmi09_memstruc Struc
	largest_avail	dd ?
	max_unlocked	dd ?
	max_locked	dd ?
	linear_space	dd ?	;linear addx space in pages
	unlocked_pages  dd ?
	free_pages	dd ?	;free pages
	real_pages	dd ?	;total physical pages
	free_linear_space dd ?  ;free linear addx space in pages
	pagefile_size   dd ?    ;size of paging file in pages
dpmi09_memstruc ends

dpmi10_memstruc struc
	all_f_allocated     dd ?   ;total allocated bytes of physical memory controlled by host
	all_v_allocated     dd ?   ;total allocated bytes of virtual memory controlled by host
	free_v	            dd ?   ;total available bytes of virtual memory controlled by host
        allocated_v_mach    dd ?   ;total allocated bytes of virtual memory for curr virtual mach
	free_v_mach	    dd ?   ;total available bytes of virtual memory for curr virtual mach
	allocated_v_client  dd ?   ;total allocated bytes of virtual memory for current client
	free_v_client	    dd ?   ;total available bytes of virtual memory for current client
	locked_client       dd ?   ;total locked bytes for current client
	max_locked_client   dd ?   ;maximum locked bytes for current client
	high_linaddx	    dd ?   ;highest linear address available to current client
	bigblock_avail	    dd ?   ;largest available memory block in bytes
	alloc_unit	    dd ?   ;minimum allocation unit in bytes
	alloc_alignsize	    dd ?   ;allocation alignment unit size in bytes
dpmi10_memstruc ends

;#########################################################################
GDT_System	= 6

Dpmi_init:					;Inicia em DPMI
	mov si,offset dpmi_f32
	test dpmi_flags,1			;suporta 32 bit?
	jz quit_error			

	mov bx,dpmi_pdatasz			;bloco de mem necess·ria
	or bx,bx				;se for 0, ignora
	jz @dpmi_start
	call dos_getmem pascal,bx
	mov si,offset memory_error		;erro de memÛria
	or dl,dl
        jz quit_error				;se houver tripp, volta ao dos
	mov es,ax				;Seg de dados vem em ax

 @dpmi_start:
	mov ax,1				;App de 32 bit
 	call dword ptr dpmi_entry		;e aqui vamos nÛs!!
	mov edx,offset dpmi_f00			;msg de erro
	jc dpmi_error_exit			;se n„o correu bem, sai para o DOZZ

	mov spsp,es				;guarda o Psp
	mov sdata16,ds				;e os selectors de dados
	mov scode16,cs				;e cÛdigo

	lar cx,scode16				;ajusta o dpl
	and ch,060h
	or data0_descr.acessr,ch		;em todos os descriptors
	or data32_descr.acessr,ch	
	or code32_descr.acessr,ch		
	or lomem_descr.acessr,ch
	or stack16_descr.acessr,ch
	or stack32_descr.acessr,ch

	push ds
	pop es					;es=ds
	mov edi,offset dpmi_mem_info		;comum ·s duas versıes
	mov ax,050Bh
	cmp word ptr dpmi_version,005Ah		;ser· que È dpmi V09?
	cmove ax,0500h				;PII opcode - emulado!!
	setne _dpmi_v1				;e seta uma flag jeitosa
						;Ax=500h se DPMI V0.9
	int 31h
	mov edx,offset dpmi_f04			;msg de erro de memÛria
	jc dpmi_error_exit			;se n„o correu bem, vai prÛ dos

	mov ax,40
	mul _dpmi_v1				;ax ser· 0 se Dpmi 0.9, 40 se DPMI 1.0
	mov bx,ax				;o offset na struc de memÛria
	mov eax,ds:[bx]				;largest avail block, em bytes
	mov edx,offset dpmi_f04			;msg de erro de memÛria
	cmp eax,_heap_min			;ser· que È suficiente?
	jb dpmi_error_exit			;n„o? ent„o sai com msg de erro
						;ok, a memÛria È suficiente!!
	mov ax,03h				;get selector incval
	int 31h
	mov _sel_incval,ax
	mov ax,00h				;aloca os descr de sistema
	mov cx,GDT_system			;N∫ de descriptors a alocar para o sistema
	int 31h
	mov edx,offset dpmi_f01			;msg de erro
	jc dpmi_error_exit			;se n„o houver suficientes, aborta

	mov dx,ax				;em dx o selector
	mov cx,GDT_System			;count do loop
	mov edi,offset data32_descr		;de forma a que se possa modificar a partir da GDT
	mov esi,offset Sel_table		;a tabela de selectors (Que bonito, todos juntos!!!)
	cld
	push offset dpmi_f02			;offset do erro na stack
						;para no caso de haver molho
  @set_descr:
	mov ax,00ch				;Set descriptor
	mov bx,dx
	int 31h
	jc dpmi_error_exit_nc			;se erro, salta para fora
	mov ds:[esi],dx				;guarda o selector
	add esi,2				;e avanÁa para o seguinte
	add edi,8				;e na GDT tambÈm
	add dx,_sel_incval			;sempre assim
	loop @set_descr

	pop si					;tira algo da stack, para parecer bonito
						;visto que n„o houve trip
						;Agora os descriptors j· est„o todos bem
						;e j· podemos aceder a _TEXT

	mov bx,ss				;para libertar o selector da stack
	cli					;agora cuidadinho
	mov ss,sstack32       			;MudanÁa para stack32
	mov esp,_pmstack_top			;com topo e tudo
	xor ebp,ebp				;e cum stack frame!!!
	sti
	and start_type,1			;Seta o bit de modo
						;para garantir que È mesmo DPMI

	mov ax,001h				;liberta o descriptor do ss inicial
	int 31h
	mov edx,offset dpmi_f03
	jc dpmi_error_exit


;---------------------------------------------- LIXO
	mov ecx,2000
 @llz:
	push ecx
	mov es,slomem
	mov edi,0B8000h
	mov eax,0Fa7bCh
	xor eax,ecx
	mov ecx,500
	cld
	db 67h
	rep stosw
	pop ecx
	loop @llz
;--------------------------------------------- AT… AQUI
	jmp dpmi_exit

;#########################################################################

dpmi_error_exit_nc:				;saÌda de dpmi com msg de erro na stack
	xor edx,edx				;Zero extend EDX
	pop dx					;altamente!!!!

dpmi_error_exit:				;saÌda de dpmi com msg de erro em DX
	mov ah,09h
	int 21h					;exibe mensagem de erro c/offset em edx

dpmi_exit:

	jmp _quit				;e sai mesmo!!!

;##############################################################################################
;##############################################################################################
; SUPORTE DE DEBUG
;##############################################################################################
;#############################################################################

 _EXCP_MSG db 'FATAL EXCEPTION ',0ah,0dh,'$'
 _EXCP_M0  db '#00: Divide Error',0ah,0dh,'$'
 _EXCP_M1  db '#01: Debug Interrupt',0ah,0dh,'$'
 _EXCP_M2  db '#02: NMI',0ah,0dh,'$'
 _EXCP_M3  db '#03: Breakpoint',0ah,0dh,'$'
 _EXCP_M4  db '#04: Overflow',0ah,0dh,'$'
 _EXCP_M5  db '#05: BOUND range exceeded',0ah,0dh,'$'
 _EXCP_M6  db '#06: Invalid Opcode',0ah,0dh,'$'
 _EXCP_M7  db '#07: FPU not available',0ah,0dh,'$'
 _EXCP_M8  db '#08: Double Fault',0ah,0dh,'$'
 _EXCP_M9  db '#09: FPU seg overrun',0ah,0dh,'$'
 _EXCP_M10 db '#10: Invalid TSS',0ah,0dh,'$'
 _EXCP_M11 db '#11: Segment not present',0ah,0dh,'$'
 _EXCP_M12 db '#12: Stack Fault',0ah,0dh,'$'
 _EXCP_M13 db '#13: General Protection Fault',0ah,0dh,'$'
 _EXCP_M14 db '#14: Page Fault',0ah,0dh,'$'
 _EXCP_M16 db '#16: FPU Error',0ah,0dh,'$'
 _EXCP_M15 db '#15: Reserved',0ah,0dh,'$'
 _EXCP_M17 db '#UD: Reserved',0ah,0dh,'$'

 EXCP_MSG_TBL dw _EXCP_M0, _EXCP_M1, _EXCP_M2, _EXCP_M3, _EXCP_M4, _EXCP_M5, _EXCP_M6, _EXCP_M7
       	      dw _EXCP_M8, _EXCP_M9, _EXCP_M10, _EXCP_M11, _EXCP_M12, _EXCP_M13, _EXCP_M14
	      dw _EXCP_M15, _EXCP_M16
	      dw 15 dup(_EXCP_M17)
;###########################################################################

_excp_no   dd 0
_excp_err  dd 0

;===========================================================================
excp_table:                                     ;in°cio da tabela de excepáîes
e_cnt = 0

REPT 32                                         ;suporte para as 32 exception's
   mov al,e_cnt
   jmp short Common_Excp_Handler
   e_cnt =e_cnt+1
endm

;===========================================================================
Common_Excp_Handler:                            ;esta rotina ser† chamada por
        cli                                     ;um exception handler
        clts                                    ;numa task Gate
        push ds
        push data16_selector
        pop ds
        mov _excp_no,eax
        add esp,8                               ;elimina ret addx
        pop _saved._ds
        pop _excp_err                           ;dpmi stack frame
        pop _saved._eip
        pop _saved._Cs
        pop _saved._eflags
        pop _saved._ESP
        pop _saved._SS
        mov _saved._eax,eax
        mov _saved._ebx,ebx
        mov _saved._ecx,ecx
        mov _saved._edx,edx
        mov _saved._esi,esi
        mov _saved._edi,edi
        mov _saved._ebp,ebp
        mov word ptr _saved._fs,fs
        mov word ptr _saved._gs,gs
        mov word ptr _saved._es,es
        mov ax,_text16
        mov cx,ax
        mov si,_text16
        mov dx,oldss
        mov ebx,oldesp
        mov edi,offset rm_excp
        jmp [_switch2rm]
;------------------------------------------------------------

 rm_excp:

         call excp_trace
         jmp [_exitproc]
         call irq_restore

         mov ebx,_excp_no
         add bx,bx
         add bx,offset EXCP_MSG_TBL
         mov si,ds:[bx]
         sti
         jmp quit_error

;########################################################################

excp_trace proc near

           call echo pascal,offset _divider2
           call echo pascal,offset _excp_msg
           call echo pascal,offset _divider2
           call dump_all
           ret
endp


;########################################################################
;===========================================================================

SCOUNT = 00h	;N∫ de Selectors

Data_descr EQU 92h
Code_descr EQU 9Ah
TSS_AVAIL  EQU 89h
TSS_BUSY   EQU 8Bh

Usepage	   = 01h        ;bit de granularidade
Usebyte	   = 00h
OP16	   = 00h        ;bit de default opcode
OP32	   = 01h

LDT	   = 02h        ;tipo de descriptor
TASK_GATE  = 05h
;CALL_GATE = 0Ch
INT_GATE   = 0Eh
TRAP_GATE  = 0Fh

Descr Macro nome, limite1, limite2, base1, base2, base3, tipo, gran, ops

	&NOME&_DESCR LABEL DESCRIPTOR
        descriptor <limite1, base1, base2, tipo, (((gran shl 7) OR (ops shl 6)) OR limite2), base3>
	&NOME&_SELECTOR = SCOUNT*8
	scount = scount+1
endm

DGATE MACRO nome, _sel, _ofset1, _ofset2, tipo,word_count
      &NOME&_GATE LABEL GATE
        Gate <_ofset1,_sel, word_count, (80h+tipo), _ofset2>
ENDM

;###########################################################
ALIGN 16
;###########################################################
GDT:
	DESCR GDT_NULL, 0, 0, 0, 0, 0, 0, 0, 0
	DESCR DATA32,  0FFFFh, 0, 0, 0, 0, DATA_DESCR, USEBYTE, OP32
	DESCR CODE32,  0FFFFh, 0, 0, 0, 0, CODE_DESCR, USEBYTE, OP32
	DESCR DATA0,   0FFFFh, 0Fh, 0, 0, 0, DATA_DESCR, USEPAGE, OP16
	DESCR LOMEM,   0FFFFh, 0Fh, 0, 0, 0, DATA_DESCR, USEBYTE, OP16
	DESCR STACK32, 0FFFFh, 0, 0, 0, 0, DATA_DESCR, USEBYTE, OP16
	DESCR STACK_E, 0FFFFh, 0, 0, 0, 0, DATA_DESCR, USEBYTE, OP16
        DESCR BIOS,    0FFFFh, 0,0400h,0,0,DATA_DESCR, USEBYTE, OP16
	DESCR CODE16,  0FFFFh, 0, 0, 0, 0, CODE_DESCR, USEBYTE, OP16
	DESCR DATA16,  0FFFFh, 0, 0, 0, 0, DATA_DESCR, USEBYTE, OP16
	DESCR STACK16, 0FFFFh, 0, 0, 0, 0, DATA_DESCR, USEBYTE, OP16
        DESCR VCPI_01, 0, 0, 0, 0, 0, 0, 0, 0
        DESCR VCPI_02, 0, 0, 0, 0, 0, 0, 0, 0
        DESCR VCPI_03, 0, 0, 0, 0, 0, 0, 0, 0

GDT_LIMIT EQU $-GDT

;##############################################################################
;##############################################################################################
digits  db '0123456789ABCDEF'
tempbuf db 256 dup(0)		;buffer tempor·rio

tempseg dw _TEXT16		;valor a pÙr em ES (se dpmi, mudar para selector)
_base	dd 16			;base numÈrica de convers„o
_flen   dw  8			;tamanho mÌnimo do campo
_pchar	db '0'			;caractere de pad
_sufix	db 'h'			;sufixo
_finale db '$'			;terminador da string

;---------------------------------------------------------------------------------------------

;converte @num em string, que È guardada em tempbuf

_str proc near
	arg @num:dword

	uses es
	mov es,tempseg			;ok, obtÈm segmento ou selector
	mov di,offset tempbuf		;e o offset, que È o mesmo
	mov eax,@num			;EDX:EAX ter„o o n∫ a ser convertido
	mov esi,@num			;e ESI tambÈm vam para a festa!
	xor cx,cx			;claro que cX=0
	or eax,eax			;vamos l· ver se @num È positivo
	jns @@no_sign
	neg eax				;n„o? ent„o nega-se (vai ser usado o opcode div)
					;o n∫ tem que ser positivo
 @@no_sign:
	xor edx,edx			;high dword=0
	div _base			;faz ent„o a convers„o
	push dx
	inc cx				;mais uma ficha, mais uma viagem...
	or ax,ax			;e ser· que a volta terminou?
	jnz @@no_sign			;n„o, mais uma divis„o
	or esi,esi 			;ser· que È negativo?
	jns @@no_minus			;se n„o, n„o precisa do '-'
	mov al,'-'
	stosb				;o primeiro caractere!

 @@no_minus:
	cld
	mov ax,_flen			;em ax, o tamanho mÌnimo
	cmp ax,cx			;ser· que existem dÌgitos suficientes
	jbe @@digit_copy1		;se sim, vamos ent„o ‡ string
	mov bx,cx			;se n„o, vamos preencher
	sub ax,cx			;os dÌgitos em falta com _pchar
	mov cx,ax
	mov al,_pchar
	rep stosb
	mov cx,bx			;e restauramos cx no final
	cld

 @@digit_copy1:
	mov si,offset digits

 @@digit_copy2:
	pop bx
	mov al,ds:[si+bx]
	stosb
	loop @@digit_copy2

	mov al,_sufix
	or al,al
	jz @@put_term
	stosb

 @@put_term:
	mov al,_finale
	stosb
	ret
endp

;##############################################################################################

_cr   	  db 0ah,0dh,'$'		;carry return
_divider  db 65 dup (196)		;divisor
	  db 0ah,0dh,'$'
_divider2 db 65 dup (35)		;divisor
	  db 0ah,0dh,'$'
_slack	  db 10 dup(32)			;espaÁo de slack
	  db '$'
_regstr   db 'EAX: $', 'EBX: $', 'ECX: $', 'EDX: $', 'ESI: $', 'EDI: $', 'ESP: $'
	  db 'EBP: $', ' CS: $', ' DS: $', ' ES: $', ' FS: $', ' GS: $', ' SS: $'
	  db 'EFLAGS: $','MSW: $'

_saved	  tss <>		;estrutura para salvar os dados
;##############################################################################################


dumpreg proc near
	arg @num:dword, @msg:word

	call echo  pascal,@msg
	call _str pascal,@num
	call echo pascal,offset tempbuf	
	call echo pascal,offset _slack
	ret
endp


dump_all proc near

	call echo pascal, offset _cr
	call echo pascal, offset _divider

	call dumpreg pascal, _saved._eax, offset _regstr
	call dumpreg pascal, _saved._ebx, offset _regstr+6
	call dumpreg pascal, _saved._ecx, offset _regstr+12
	call echo pascal, offset _cr
	call dumpreg pascal, _saved._edx, offset _regstr+18
	call dumpreg pascal, _saved._esi, offset _regstr+24
	call dumpreg pascal, _saved._edi, offset _regstr+30
	call echo pascal, offset _cr
	call dumpreg pascal, _saved._esp, offset _regstr+36
	call dumpreg pascal, _saved._ebp, offset _regstr+42

	call echo pascal, offset _cr
	call echo pascal, offset _divider
	mov eax,dword ptr _saved._cs
	call dumpreg pascal,eax, offset _regstr+48
	mov eax,dword ptr _saved._ds
	call dumpreg pascal,eax, offset _regstr+54
	mov eax,dword ptr _saved._es
	call dumpreg pascal,eax, offset _regstr+60

	call echo pascal, offset _cr
	mov eax,dword ptr _saved._fs
	call dumpreg pascal,eax, offset _regstr+66
	mov eax,dword ptr _saved._gs
	call dumpreg pascal,eax, offset _regstr+72
	mov eax,dword ptr _saved._ss
	call dumpreg pascal,eax, offset _regstr+78

	call echo pascal, offset _cr
	call echo pascal, offset _divider
	
	call dumpreg pascal,_saved._eflags, offset _regstr+84
	xor eax,eax
	smsw ax
	call dumpreg pascal, eax, offset _regstr+93
	call echo pascal, offset _cr
	call echo pascal, offset _divider

	ret
endp

dump_paragr proc near
	arg @from:dword, @count:word

	uses es,ax,bx,cx,dx,di
	xor eax,eax
	mov al,_sufix
	push ax
	mov _sufix,32
	push _flen
	mov _flen,2

	les di,@from
	movzx ecx,@count

  @para_loop:
	push ecx
	mov ecx,16

  @dump_loop:
	mov al,es:[di]
	inc di
	push es di ecx eax
	call _str pascal,eax
	call echo pascal,offset tempbuf
	pop eax ecx di es
	loop @dump_loop
	push eax 
	call echo pascal, offset _cr
	pop eax ecx
	loop @para_loop

	pop _flen
	pop ax
	mov _sufix,al
	ret
endp

trace proc far
	
	push ds
	push _text16
	pop ds
	mov _saved._eax,eax
	mov _saved._ebx,ebx
	mov _saved._ecx,ecx
	mov _saved._edx,edx	
	mov _saved._esi,esi 
	mov _saved._edi,edi	
	mov _saved._ebp,ebp	
	mov word ptr _saved._cs,cs
	pushfd
	pop dword ptr _saved._eflags
	pop ax
	mov word ptr _saved._ds,ax
	mov word ptr _saved._es,es
	mov word ptr _saved._fs,fs
	mov word ptr _saved._gs,gs
	mov word ptr _saved._ss,ss
	mov _saved._esp,esp
	push dword ptr _saved._eax		;o ax foi mudado 
	push ebx ecx edx esi edi		;salva tudo
	push word ptr _saved._ds		;e o ds tambÈm foi mexido
	push es fs gs
	call dump_all
	pop gs fs es ds
	pop edi esi edx ecx ebx eax		;restaura tudo
	ret
endp

CODE16ENDS
;###########################################################################
CODESTART
;###########################################################################

ASSUME DS:_TEXT

align 16

;=========================================================
CALL32  MACRO sel
        db 09Ah
        dd 0
        dd sel
       ENDM



;##########################################################################################

copyd proc near
	arg @src:pointer32, @dest:pointer32, @count:dword

	uses ds,es,esi,edi,ecx
	lds esi,fword ptr @src
	les edi,fword ptr @dest
	mov ecx,@count
	cld
	rep movsd
	ret
endp


;##########################################################################################
;      						 D E B U G G I N G    S U P P O R T
;##########################################################################################

vcpi_debugregs:				;necess·rio para suporte VCPI/DPMI
	_dr0	dd 0			;debug breakpoint addx
	_dr1	dd 0			;debug breakpoint addx
	_dr2	dd 0			;debug breakpoint addx
	_dr3	dd 0			;debug breakpoint addx
	_dr4	dd 0
	_dr5	dd 0
	_dr6	dd 0			;debug control register
	_dr7	dd 0			;debug control register

old_screen 	dd 0		;ecr‚ antigo



;===========================================================================

BASE_NUMBER DD 10         ;Base do N∫ a ser convertido para string
FIELD_LEN   DW 1          ;Tamanho mÕnimo do Campo
PAD_char    Db ' '        ;Caracter de PAD
SUFFIX      DB 0          ;Sufixo
Terminator  DB 0          ;Terminador da string

;===========================================================================
ALIGN 4

;---------------------------------------------
; LONG STRLEN(S:POINTER)
;---------------------------------------------
PUBLIC STRLEN

STRLEN PROC NEAR
	ARG @S:POINTER32
	
	USES EAX,ECX,EDI,ES
	LES EDI,fword ptr @S
	MOV ECX,-1
	XOR EAX,EAX
	CLD
	REPNE SCASB
	MOV EDX,-2	
	SUB EDX,ECX
	RET
	ENDP

;---------------------------------------------
; BOOL STRCOMP(S1:POINTER, S2:POINTER)
;---------------------------------------------
PUBLIC STRCOMP

STRCOMP PROC NEAR
	ARG @S1:POINTER32, @S2:POINTER32
	
	USES EAX,ECX,ESI,EDI,ES,DS
	LES EDI,fword ptr @S1
	LDS ESI,fword ptr @S2
	CALL STRLEN PASCAL,ES,EDI
	MOV EAX,EDX
	CALL STRLEN PASCAL,DS,ESI
	CMP EAX,EDX
	JBE @@NO_SWITCH
	XCHG EAX,EDX
 @@NO_SWITCH:
	XOR EDX,EDX
	MOV ECX,EAX
	CLD
 	REPE CMPSB	
	SETE DL
	RET
	ENDP
	

;-----------------------------------------------------
;VOID INT2STR(LONG @N, POINTER @S)
;-----------------------------------------------------
PUBLIC INT2STR

INT2STR PROC NEAR
	ARG @N:DWORD, @S:POINTER32

	USES EAX,EBX,ECX,EDX,EDI,ESI,ES
	LES EDI,fword ptr @S
	MOV EAX,@N
    	MOV ESI,EAX
    	XOR ECX,ECX
	OR ESI,ESI
	JNS @@ST_1
	NEG EAX

 @@ST_1:
	XOR EDX,EDX
	DIV CS:[BASE_NUMBER]
	PUSH EDX
	INC CX
	CMP AX,0
	JNE @@ST_1
	OR ESI,ESI
	JNS @@ST_2
	MOV AL,'-'
	STOSB
 @@ST_2:
	PUSH ECX
	MOV AX,CS:FIELD_LEN
	CMP AX,CX
	JBE @@ST_STORE
	MOV BX,CX
	SUB AX,CX
	MOV CX,AX
	MOV AL,CS:PAD_CHAR
	CLD
	REP STOSB
	MOV CX,BX
	
 @@ST_STORE:
 	POP ECX
	CLD

 @@ST_STORE1:
	POP EAX
	ADD AL,30H
	STOSB
	LOOP @@ST_STORE1

	
	MOV AL,CS:SUFFIX
	CMP AL,0
	JE @@ST_TERMINATOR
	STOSB

 @@ST_TERMINATOR:
 	MOV AL,CS:TERMINATOR
	STOSB 	

 	RET
 	ENDP


;----------------------------------------------------------
; VOID UPCASE(POINTER STR)
;----------------------------------------------------------
PUBLIC UPCASE

UPCASE PROC NEAR
	ARG @S:POINTER32
	
	USES EAX,ECX,EDX,ESI,EDI,DS,ES
	LDS ESI,fword ptr @S
	LES EDI,fword ptr @S
	CALL STRLEN PASCAL,ES,EDI
	MOV ECX,EDX
	CMP ECX,0
	JE @@Q_UCASE
	CLD

 @@UCASE_LL1:	
	LODSB
	CMP AL,97
	JB @@UCASE_L3
	CMP AL,122
	JA @@UCASE_L3
	SUB AL,20H

 @@UCASE_L3:
	STOSB
 	LOOP @@UCASE_LL1

 @@Q_UCASE:
	RET
	ENDP


;###########################################################
_text ENDS
;###########################################################


end start
