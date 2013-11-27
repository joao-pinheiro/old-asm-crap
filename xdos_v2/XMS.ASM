;###########################################################
;#                                                         #
;#  Ficheiro: XMS.ASM                                      #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################

Model large, pascal
.386

include globals.inc


;###########################################################
CODE16START
;###########################################################


;###########################################################
public xms_version

xms_entry	dw ?,?	;far ptr para o driver xms
xms_version	db ?,?	;versão do driver xms
hma_flag	dw ?    ;True se hma (1M a 1M + 64K) existe
old_a20		db ?,?	;Estado da linha A20

;###########################################################
public xms_detect

xms_detect proc near

	mov ax,4300h		;int 2f, fn 4300h - detectar xms
	int 2fh
	cmp al,80h		;se al=80h, driver instalado
	jne @no_xms

	mov ax,4310h		;serviço 4310h, obter entry point
	int 2fh
	mov xms_entry,bx	;salvar o pointer
	mov xms_entry+2,es
	
	mov ah,00h		;serviço 00h - obter versão
	call dword ptr xms_entry
	mov xms_version,ah	;versão
	mov xms_version+1,al	;release
	mov hma_flag,dx

	mov ax,true		;xms encontrado
	jmp @xms_quit

@no_xms:
	mov ax,false

@xms_quit:
	ret
endp

;===========================================================
public xms_setA20

xms_setA20 proc near

	mov ah,07h		;Obtém o estado de A20
	call dword ptr xms_entry
	mov old_a20,al		;salva o estado

	mov ah,03h		;seta A20
	call dword ptr xms_entry
	ret
endp


;===========================================================
public xms_resetA20

xms_resetA20 proc near

	mov ah,old_a20		;restaura o estado da linha
	xor ah,1
	add ah,3
	call dword ptr xms_entry
	ret
endp

;===========================================================
public xms_getfreemem

xms_getfreemem proc near


	cmp xms_version,3	;suporta SXMS?
	jb @fn_08		;se não, modo antigo
        mov ah,88h
        call dword ptr xms_entry
                                ;EAX - Tamanho do maior bloco disponível em Kb (ax=0 se erro)
                                ;EDX - Quantidade de memória livre em kb
	or bl,bl
	jz @fn_08_quit

  @fn_08:
	mov ah,08h
	call dword ptr xms_entry
	movzx eax,ax		;eax - Tamanho do maior bloco disponível em Kb (ax=0 se erro)
	movzx edx,dx		;edx - Quantidade de memória livre em kb

  @fn_08_quit:
	ret
endp

;===========================================================
public xms_malloc

xms_malloc proc near
	arg @kilobytes:dword

	mov edx,@kilobytes
	cmp xms_version,3		;suporta SXMS?
	jb @fn_09			;se não, modo antigo
        mov ah,89h
        call dword ptr xms_entry
	or bl,bl			;se não houve erro, sai
	jz @fn_09_quit			;handle em DX

  @fn_09:
	xor ax,ax			;se o bloco >64m
	mov bl,80h			;tem que correr mal!
	cmp edx,$FFFF
	jg @fn_09_quit

	mov ah,09h
	call dword ptr xms_entry	;AX=0 se erro, =1 se bem sucedido
	
  @fn_09_quit:				;em dx, o handle do bloco
	ret
endp

;===========================================================
public xms_dalloc

xms_dalloc proc near
	arg @handle:word

	mov ah,0ah
	mov dx,@handle
	call dword ptr xms_entry	;Ax=0 se erro,=1 se sucedido
	ret
endp

;===========================================================
public xms_lockblock

xms_lockblock proc near
	arg @handle:word

	mov ah,0ch
	mov dx,@handle
	call dword ptr xms_entry	;Ax=0 se erro,=1 se sucedido
	shl edx,16
	mov dx,bx			;em edx, o endereço linear do bloco
	ret
endp

;===========================================================
public xms_unlockblock

xms_unlockblock proc near
	arg @handle:word

	mov ah,0dh
	mov dx,@handle
	call dword ptr xms_entry	;Ax=0 se erro,=1 se sucedido
	ret
endp


;###########################################################
CODE16ENDS
;###########################################################
END
