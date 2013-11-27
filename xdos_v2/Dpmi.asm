;###########################################################
;#                                                         #
;#  Ficheiro: DPMI.ASM                                     #
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

public dpmi_version, dpmi_flags, dpmi_pdsize, dpmi_entry
public dpmi_detect

;###########################################################


dpmi_version    db ?,?  ;versão do servidor dpmi (maior-menor)
dpmi_flags      dw 0    ;flags
dpmi_pdsize     dw 0    ;parágrafos a reservar
dpmi_entry      dw 0,0  ;fptr de entrada

master_vpic     db 0  ;virtual master pic settings
slave_vpic      db 0  ;virtual slave pic settings
sel_incval      dw 0  ;selectors increment value


;###########################################################

;===========================================================
;| bool dpmi_detect()                                      | 
;===========================================================

dpmi_detect proc near

	mov ax,1687h		;ax=1687h, detect DPMI host
	int 2fh
	mov cx,ax
	mov ax,false		;prepara para abortar
	test cx,cx		;se cx<>0, dpmi não instalado
	jnz @dpmi_quit

	mov dpmi_flags,bx
	mov dpmi_version,dh
	mov dpmi_version+1,dl
	mov dpmi_pdsize,si
	mov dpmi_entry,di
	mov dpmi_entry+2,es

	inc ax			;dpmi encontrado!!

 @dpmi_quit:
	ret
endp




CODE16ENDS
end



