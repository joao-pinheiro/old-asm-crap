;###########################################################
;#                                                         #
;#  Ficheiro:  DOS.ASM                                     #
;#                                                         #
;#  (c) Jo�o Pinheiro, 1998                                #
;#                                                         #
;###########################################################

Model large, pascal
.386p

include globals.inc


;###########################################################
CODE16START
;###########################################################

Public dos_getmem, dos_resizemem, dos_freemem, echo

;===========================================================
;| bool dos_getmem(int size)                               | 
;| 						           |
;| Aloca um bloco de (size) par�grafos		           |
;| AX->segmento do bloco                                   |
;| Dl->c�digo de sucesso                                   |
;| BX->maior bloco dispon�vel			           |
;===========================================================

dos_getmem proc near
	arg @blocksize:word
     
	mov ah,48h	;alloc dos memory
	mov bx,@blocksize
	int 21h
        setnc dl

 @getmem_quit:	
	ret
endp

;===========================================================
;| void dos_freemem(int segm)                              | 
;| 						           |
;| Aten��o: Destr�i ES!!			           |
;===========================================================

dos_freemem proc near
        arg @block:word

        mov ah,49h
        mov bx,@block
        mov es,bx
	int 21h
	ret
endp


;===========================================================
;| bool dos_resizemem(int segm, int newsize)               | 
;| 						           |
;| Aten��o: Destr�i ES!!			           |
;===========================================================

dos_resizemem proc near
	arg @segm:word, @newsz:word

	mov ah,04Ah
	mov bx,@newsz
	mov es,@segm
	int 21h
	setnc al
	cbw
	ret
endp

;===========================================================
;| void echo (int ofs_msg)                                 | 
;| aten��o! DS->_Text16					   |
;===========================================================

echo proc near
	arg @ofs_msg:word

        uses ds
 	mov dx,@ofs_msg			
	mov ah,09h
	int 21h
	ret
endp
;###########################################################
CODE16ENDS
;###########################################################
end
