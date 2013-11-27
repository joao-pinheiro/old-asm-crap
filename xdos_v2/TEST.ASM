model large,pascal
.386p

include globals.inc

CODE16START

movseg macro dest,src
	push src
	pop dest
endm

extrn vcpi_detect:near

start:
    xor ax,ax
    call vcpi_detect
    add ax,48
    movseg es,0b800h
    mov es:[0],al
   	MOV AX,4C00H
	INT 21H

CODE16ENDS

end start
