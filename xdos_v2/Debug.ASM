;###########################################################
;#                                                         #
;#  Ficheiro: DEBUG.ASM                                    #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################

Model large, pascal
.386p

include globals.inc

;###########################################################
; Equates						  ||
;===========================================================
CODESTART

;###########################################################

Extrn echo:near

Public Base_number, Field_len,Pad_char, Suffix, Terminator
Public int2Str, showval
;###########################################################
BASE_NUMBER DD 10         ;Base do Nº a ser convertido para string
FIELD_LEN   DW 1          ;Tamanho mInimo do Campo
PAD_char    Db ' '        ;Caracter de PAD
SUFFIX      DB 0          ;Sufixo
Terminator  DB 0          ;Terminador da string


INTERN_BUF DB 256 DUP(0)  ;BUFFER INTERNO DA STRING
TERM	   DB 0AH,0DH,'$' ;TERMINADOR DA STRING

ATribute   DB ?
Vseg	   DW 0B800h


;============================================================
;CONVERTE UM Nº INTEIRO EM STRING
;ASSUME DS-SEGMENT32

PUBLIC INT2STR

int2str proc near
	arg @n:dword, @o:word

	uses eax,ebx,ecx,edx,edi,esi,es
	push cs
	pop es
	movzx edi,@o
	mov eax,@n
    	mov esi,eax
    	xor ecx,ecx
	or esi,esi
	jns @@st_1
	neg eax
	
 @@st_1:
	xor edx,edx
	div cs:base_number
	push edx
	inc cx
	cmp ax,0
	jne @@st_1
	or esi,esi
	jns @@st_2
	mov al,'-'
	stosb
 @@st_2:
	push ecx
	mov ax,cs:field_len
	cmp ax,cx
	jbe @@st_store
	mov bx,cx
	sub ax,cx
	mov cx,ax	
	mov al,cs:pad_char
	cld
	rep stosb
	mov cx,bx
	
 @@st_store:
 	pop ecx
	cld

 @@st_store1:
	pop eax
	cmp al,9
	jbe @dec_1
	sub al,10
	add al,'a'
    	jmp @go_2
    @dec_1:
	add al,30h
    @go_2:
	stosb
	loop @@st_store1

	
	mov al,cs:suffix
	cmp al,0
	je @@st_terminator
	stosb
 
 @@st_terminator:
 	mov al,cs:terminator
	stosb 	

 	ret
     endp






;===========================================================================
;exibe um inteiro no ecrâ usando int 21 fn 09
;

public showval 

showval proc near
	arg @no:dword,@suf:word,@b:word

	uses ds,eax, edx
	mov ax,@suf
	mov suffix,al
	push @b
	pop base_number
	mov terminator,'$'
	call int2str pascal,@no,offset intern_buf
	call echo pascal,offset intern_buf
	call echo pascal,offset term
	ret
       endp

;###########################################################
CODEENDS
;###########################################################
END
