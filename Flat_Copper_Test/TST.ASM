.model large
.stack 1024

.code
    start:
       xor ax,ax
       int 16h
       mov ax,4c00h
       int 21h
end start       