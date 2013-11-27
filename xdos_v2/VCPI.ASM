;###########################################################
;#                                                         #
;#  Ficheiro: VCPI.ASM                                     #
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

public ems_page, vcpi_version, vcpi_detect, vcpi_pagesfree
public vcpi_Master_pic, vcpi_Slave_pic

;###########################################################

emm_name 	db "EMMXXXX0",0
emm_name_dis 	db "EMMQXXX0",0
emm_name_dis2 	db "$MMXXXX0",0

ems_page	dw -1

vcpi_version 	db ?,?		;versão vcpi (maior-menor)
vcpi_pagesfree  dd ?		;páginas de 4k livres

vcpi_Master_pic	dw ?		;primeiro vector do PIC master
vcpi_Slave_pic	dw ?		;primeiro vector do PIC slave

;###########################################################

;===========================================================
;| bool vcpi_detect()                                      | 
;===========================================================
public vcpi_detect

vcpi_detect proc near

    mov dx,offset emm_name		;open file or device
    mov ax,3D00h
    int 21h
    jnc @emm_found			;encontrado á primeira!!

    mov dx,offset emm_name_dis		;emm desactivado?
    mov ax,3D00h
    int 21h
    jnc @emm_found_d

    mov dx,offset emm_name_dis2		;emm desactivado?
    mov ax,3D00h
    int 21h
    jc @no_vcpi				;não foi encontrado!!

 @emm_found_d:
    push ax				;preserva o handle de dispositivo
    xor ax,ax				;verifica se tem um int válido
    mov es,ax
    mov ax,es:[67h*4]			;se intvec está vazio
    or ax,es:[67h*4+2]
    pop ax
    je @no_vcpi				;não há emm para ninguém

 @emm_ok:
    mov bx,ax
    mov ax,4400h			;get IOCTL data
    int 21h
    jc @no_vcpi				;aborta se erro
    test dx,80h				; testa bit de device
    jz @no_vcpi				; se 0, é ficheiro

    mov ax,4407h			;get output IOCTL status
    int 21h
    push ax				;preservar status
    mov ah,03Eh				;close handle
    int 21h
    pop ax
    cmp al,-1				;o device está pronto?
    jne @no_vcpi

 @emm_found:
    mov ax,0DE00h	    		;int 67, VCPI installation check
    int 67h
    test ah,ah              		;se ch=0, vcpi presente
    jz @vcpi_found

    mov ah,43h				;ems alloc pages
    mov bx,1
    mov ems_page,dx			;salva o handle
    int 67h 
    test ah,ah				;se erro, não há vcpi
    jnz @no_vcpi

    mov ax,0DE00h
    int 67h
    test ah,ah              		;se ah=0, vcpi presente
    jz @vcpi_found

    mov ah,45h				;ems dalloc pages
    int 67h
    jmp @no_vcpi
	
 @vcpi_found:
    mov vcpi_version,bh
    mov vcpi_version+1,bl

    mov ax,0DE03h	    		;obter nº de páginas de 4kb livres
    int 67h
    mov vcpi_pagesfree, edx 

    mov ax,true		    		;vcpi encontrado
    jmp @vcpi_quit

 @no_vcpi:
    xor ax,ax

 @vcpi_quit:
    ret
endp 


;===========================================================
;| long vcpi_queryfreepages()                              | 
;===========================================================
public vcpi_queryfreepages

vcpi_queryfreepages proc near

	mov ax,0DE03h		;serviço 03h - Nº de páginas de 4k livres
	int 67h
	mov bx,ax
	xor ax,ax
	test bh,bh
	jnz @vcpi_03quit
	inc ax			;edx=nº de páginas

 @vcpi_03quit:
	ret
endp

;===========================================================
;| bool vcpi_getpicmaps()                                  | 
;===========================================================
public vcpi_getpicmaps

vcpi_getpicmaps proc near

	mov ax,0DE0Ah		;serviço 04h - Obter vectores dos PICS
	int 67h
	mov dx,ax
	xor ax,ax
	test dh,dh
	jnz @vcpi_04quit

	inc ax		
	mov vcpi_master_pic,bx	;salva os valores nas variáveis
	mov vcpi_slave_pic,cx	;idem ibidem

 @vcpi_04quit:
	ret
endp


;===========================================================
;| bool vcpi_setpicmaps(int master, int slave)             | 
;===========================================================
public vcpi_setpicmaps

vcpi_setpicmaps proc near
	arg @master:word, @slave:word

	mov ax,0DE0Bh		;serviço 0Bh - setar vectores dos PICS
	mov bx,@master
	mov cx,@slave
	cli
	int 67h
	sti
	mov dx,ax
	xor ax,ax
	test dh,dh
	jnz @vcpi_0bquit
	inc ax		

 @vcpi_0Bquit:
	ret
endp


;===========================================================
;| long vcpi_readcr0()   			           | 
;===========================================================
public vcpi_readcr0

vcpi_readcr0 proc near

	mov ax,0DE07h	;função 07h - read CR0
	int 67h		;Valor de CR0 em EBX
	ret
endp

;===========================================================
;| long vcpi_allocpage()   			           | 
;===========================================================
public vcpi_allocpage

vcpi_allocpage proc near

	mov ax,0de04h
	int 67h			
	test al,al
	sete al			;AL=1 se sucesso
				;EDX - Endereço físico da página
	ret
endp
	
;===========================================================
;| long vcpi_freepage()   			           |
;===========================================================
public vcpi_freepage

vcpi_freepage proc near
	arg @page_addx:dword

	mov edx, @page_addx
	mov ax,0de05h
	int 67h			
	test al,al
	sete al			;AX=1 se sucesso
	ret
endp


CODE16ENDS
end
;###########################################################
