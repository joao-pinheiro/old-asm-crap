iNCLUDE COMMON.INC

Libmodel

include mydevice.inc

Declare File_init
Declare File_assign
Declare File_create
Declare File_open
Declare File_close
Declare file_truncate
Declare File_read
Declare File_write
Declare File_GetSize
Declare File_getpos
Declare File_setpos
Declare File_SetFilemode
Declare File_done

.data

INCLUDE MYDEVICE.ASO
INCLUDE MYFILE.ASO

.code

;-----------------------------------------------------------------------------
PASCALPROC FILE_INIT

           USES es,di,ds,si,ax
           tblinit es:di

           Mov ax,20h
           mov es:di.fileatr,al
           mov ax,02h
           mov es:di.filemode,al
           mov es:di.opened,ah
           mov ax,80h
           mov es:di.devicetype,80h
           xor ax,ax
           mov es:di.handle,ax
           mov es:di.errorcode,al

           pascalEnd

;-----------------------------------------------------------------------------

PascalProc File_assign
           Arg @nome:dword

           Uses ES,DI,DS,SI,AX,CX

           Les di,@nome
           cld
           mov cx,0ffffh
           xor al,al
           repne scasb
           mov ax,0fffeh
           sub ax,cx
           push @data
           pop es
           mov di,Offset FNAME
           LDS SI,@nome
           mov cx,ax
           inc cx
           cld
           rep movsb

           PascalEnd

;-----------------------------------------------------------------------------

PascalProc File_create

           Uses ES,DI,DS,DX,AX,BX,CX,DX
           TBLINIT ES:DI
           cmp es:di.opened,1
           jne @Fnew_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fnew_ok:
           push @data
           pop ds
           mov dx,offset fname
           xor cx,cx
           mov cl,ES:di.Fileatr
           mov ah,3ch
           int 21h
           jc @Fnew_error
           mov es:di.handle,ax
           Xor ax,ax
           mov es:di.errorcode,al
           inc al
           mov es:di.opened,al
           ret

           @Fnew_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------


PascalProc File_Open

           Uses ES,DI,DS,DX,AX,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           jne @Fopen_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fopen_ok:

           Push @Data
           pop ds
           mov dx,offset FName
           xor cx,cx
           mov cl,ES:di.FileMode
           mov ah,3dh
           int 21h
           jc @Fopen_error
           mov es:di.handle,ax
           Xor ax,ax
           mov es:di.errorcode,al
           inc al
           mov es:di.opened,al
           ret

           @Fopen_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc File_Close

           Uses ES,DI,AX,BX
           TBLINIT ES:DI

           cmp es:di.opened,0
           jne @Fclose_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fclose_ok:

           mov bx,es:di.handle
           mov ah,3Eh
           int 21h
           jc @Fclose_error
           Xor ax,ax
           mov es:di.errorcode,al
           mov es:di.opened,al
           ret

           @FClose_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc  File_Truncate

            PascalEnd

;-----------------------------------------------------------------------------

PascalProc File_READ
           ARG @BUF:Dword, @count:word

           Uses ES,DI,DS,DX,AX,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           je @Fread_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fread_ok:

           mov ah,3fh
           mov bx,ES:Di.handle
           mov cx,@Count
           LDS DX,@BUf
           int 21h

           jc @Fread_error
           xor ax,ax
           mov es:di.errorcode,al
           ret

           @Fread_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc File_Write
           ARG @BUF:Dword, @count:word

           Uses ES,DI,DS,DX,AX,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           je @Fwrite_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fwrite_ok:

           mov ah,40h
           mov bx,ES:Di.handle
           mov cx,@Count
           LDS DX,@BUf
           int 21h

           jc @Fwrite_error
           xor ax,ax
           mov es:di.errorcode,al
           ret

           @Fwrite_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc File_GetSize

           Uses ES,DI,DS,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           je @Fgetsize_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fgetsize_ok:
           mov bx,es:di.Handle
           mov ax,4202h
           xor cx,cx
           xor dx,dx
           int 21h
           jc @Fgetsize_error
           xor bx,bx
           mov es:di.errorcode,bl
           ret

           @Fgetsize_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc File_GetPos

           Uses ES,DI,DS,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           je @Fgetpos_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fgetpos_ok:

           mov bx,es:di.handle
           mov aX,4201h
           xor cx,cx
           xor dx,dx
           int 21h

           jc @Fgetpos_error
           xor bx,bx
           mov es:di.errorcode,bl
           ret

           @Fgetpos_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------

PascalProc File_SetPos
           ARG @newPOS:dword

           Uses ES,DI,DS,DX,AX,BX,CX,DX
           TBLINIT ES:DI

           cmp es:di.opened,1
           je @Fsetpos_OK
           mov al,05h
           mov es:di.errorcode,al
           ret

           @Fsetpos_ok:

           mov bx,es:di.handle
           Mov dx,word ptr @newpos[0]
           mov cx,word ptr @newpos[2]
           mov ax,4200h
           int 21h

           jc @Fsetpos_error
           xor ax,ax
           mov es:di.errorcode,al
           ret

           @Fsetpos_error:
           Mov ES:DI.Errorcode,al
           ret

           Pascalend

;-----------------------------------------------------------------------------


PascalProc File_setFilemode
           Arg @fm, @fa:word

           Uses es,di,ax,bx
           tblinit es:di
           mov ax,@fm
           mov bx,@fa
           Mov es:di.filemode,al
           Mov ES:di.fileatr,bl

           pascalEnd

;-----------------------------------------------------------------------------

PascalProc File_done

           Uses ES,di,ax
           tblinit es:di
           cmp es:di.opened,1
           je @LetsClose
           ret

           @LetsClose:
           Call file_close
           ret

           pascalend

END
