Ideal
p486
Model Flat,pascal
 DataSeg
 Hello db 'Hello!$'
 CodeSeg
   Start:
     mov edx,offset hello
     mov ah,09h
     int 21h
     mov edx,offset Start
     mov ax,4c00h
     retf
End Start
