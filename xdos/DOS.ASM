.386p
.model large,pascal

Include DPMi.inc
INCLUDE EXTERNal.INC


CODE32 segment para public use32

ORG 0
Align 4

;============================================================

        ASSUME DS:CODE32, CS:code32,FS:CODE16,SS:STACK32

;SYSTEM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Global IORESULT:DWORD
GLOBAL IO_errorHandler:DWORD

GLOBAL DEF_DOSERROR_HANDLER:NEAR
GLOBAL SETDOSERRORHANDLER:NEAR

;DIRECTORIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GLOBAL MKDIR:NEAR
GLOBAL RMDIR:NEAR
GLOBAL CHDIR:NEAR
GLOBAL GETCURRDIR:NEAR

;FILE HANDLING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Global Fnew:Near
Global Fopen:near
Global Fclose:Near
Global Blockread:Near
Global Blockwrite:near
GLOBAL KILL:NEAR
Global TRUNCATE:near
Global FSeek:near
Global Getpos:Near
GlOBAL SETPOS:NEAR

;DISK SERVICES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GLOBAL DISKRESET:NEAR
GLOBAL SETDRIVE:NEAr
GLOBAL GetDriveNo:near
GLOBAL GETCURDrive:Near
GLOBAL SETDTA:Near
GLOBAL GETDTA:Near

;MISC SERVICES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GLOBAL DosVersion:Near

;DATE/TIME  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GLObal GETDATE:NEAR
GLOBAL SETDATE:NEAR
GLOBAL GETTIME:NEAR
GLOBAL SETTIME:NEAR

;==================================================================
;Equates


;==== Filemode <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

fmClosed = 0D7B0h
fmInput  = 0D7B1h
fmOutput = 0D7B2h
fmInOut  = 0D7B3h

;=== File attribute <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

faReadOnly  = 01h
faHidden    = 02h
faSysFile   = 04h
faVolumeID  = 08h
faDirectory = 10h
faArchive   = 20h
faAnyFile   = 3Fh

;Search record

Searchrec Struc global
	    rFill db 21 dup(0)
	    rATTR db ?
	    rTime DD ?
	    rSize DD ?
	    rName db 13 dup(0)
Ends

;--------------------------------------------------------------------------
;--------------------------------------------------------------------------


;##########################################################################
; DATA32
;##########################################################################



IORESULT            DD ?  ;C¢digo de Erro
IO_ERRORHANDLER     DD ?  ;Handler do erro

ERROR105 DB 'ERRO #105 : Erro na chamada aos servi‡os do MSDOS.',0ah,0dh,7,'$'


;##########################################################################
; SYSTEM ROUTINES
;##########################################################################


;®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®®
;Default DOS error handler
;¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

DEF_DOSERROR_Handler Proc near
                 Arg @C:Dword

                 USES EAX,DS
                 MOV DS,SDATA32
                 Mov eAx,@C
                 Mov ioresult,eAX

                 mov ax,iocheck
                 CMP AX,false
                 je @DOSERROR_QUIT

                 MOV EAX,offset Error105
                 CALL DOSPRINT Pascal,DS,EAX
                 Mov ax,3
                 CALL QUIT Pascal,ax

 @DOSERROR_quit:
                Ret
Endp


;-----------------------------------------------------
;Procedure SetDosERRorHandler(OFS:DWORD);
;-----------------------------------------------------

SetDOSERRORHANDLER Proc Near
                   ARG @O:DWord

                   USES EAX
                   MOV EAX,@O
                   MOV IO_Errorhandler,EAX
                   ret
ENdp

;##########################################################################
; MACROS
;##########################################################################


SETDOSERR Macro CD

          PUSH EAX
          Push CD
          MOV EAX,IO_errorHandler
          CALL EAX
          POP EAX
          ENDM

CLRDOSERR MACRO

          PUSH 0
          POP IORESULT
         ENDM

;##########################################################################
; FILE ROUTINES
;##########################################################################

;---------------------------------------------------------
;Function FNEW(Nome:pointer;tipo:DWORD):WORD;
;---------------------------------------------------------

Fnew Proc Near
         Arg @S:Pointer, @T:dword

         USES ECX,EDX
         CLRDOSERR
         Call CopyReal pascal,@s.selector,@s.ofset,80
         MOV AX,3c00h
         Mov Ecx,@T
         MOV dx,RMBUF_seg
         MOV RDS,dx
         XOR DX,dx
         IntR 21h
         jnc @@FN_QUIT

         SETDOSERR EAX
 @@FN_QUIT:

        ret
ENDP

;---------------------------------------------------------
; Procedure Fclose(Handle:DWord);
;---------------------------------------------------------

FClose Proc Near
         Arg @h:Dword

         USES EAX,EBX,ECX,EDX
         CLRDOSERR
         MOV AX,3e00h
         Mov ebx,@h
         IntR 21h
         jnc @@FC_QUIT

         SETDOSERR EAX
 @@FC_QUIT:

        ret
ENDP

;---------------------------------------------------------
;Function FOPEN(Nome:pointer;tipo:dWORD):WORD;
;---------------------------------------------------------

FOPEN Proc Near
         Arg @S:Pointer, @T:dword

         USES ECX,EDX
         CLRDOSERR
         Call CopyReal pascal,@s.selector,@s.ofset,80
         MOV eAX,@T
         MOV aH,3Dh
         MOV dx,RMBUF_seg
         MOV RDS,dx
         XOR DX,dx
         IntR 21h
         jnc @@FO_QUIT

         SETDOSERR EAX
 @@FO_QUIT:
        ret
ENDP

;---------------------------------------------------------
;Function FGETPOS(handle:dword):DWORD(EDX);
;---------------------------------------------------------

FGETPOS PROC near
        ARG @H:dWord

        USES EAX,EBX,ECX
        CLRDOSERR
        MOV AX,4201h
        Mov ebx,@H
        Xor CX,CX
        XOR DX,DX
        IntR 21h
        jnc @@FGP_QUIT

        SETDOSERR EAX

 @@FGP_QUIT:
        SHL EDX,16
        Mov dx,ax
        ret
ENDP

;---------------------------------------------------------
;Procedure FSETPOS(handle:Dword;P:Dword;Mode:Dword);
;---------------------------------------------------------

FSETPOS PROC near
        ARG @H:DWord,@P:DWORD,@M:DWORD

        USES EAX,EBX,ECX,EDX
        CLRDOSERR
        MOV eAX,@M
        MOV Ah,42h
        Mov ebx,@H
        MOV DX,Word ptr @P[0]
        MOV CX,Word ptr @P[2]
        IntR 21h
        jnc @@FsP_QUIT

        SETDOSERR EAX

 @@FsP_QUIT:
        ret
ENDP


;---------------------------------------------------------
;Procedure Truncate(Handle:Dword);
;---------------------------------------------------------

Truncate proc near
         ARG @H:DWORD

         USES EAX,EBX,ECX,EDX
         CLRDOSERR
         Mov ebx,@H
         Xor Cx,cx
         Mov Ah,40h
         INTR 21h
         jnc @TRUNC_QUIT

         SETDOSERR EAX

 @TRUNC_quit:
         ret
ENDP

;---------------------------------------------------------
;Procedure Blockread(Handle:DWORD;Count:DWORD);
;---------------------------------------------------------

BlockRead Proc near
          Arg @H:dword, @C:dword

          USES EAX,EBX,ECX,EDX
          CLRDOSERR
          Mov Ebx,@H
          MOV ECx,@C
          MOV DX,RMBUF_seg
          Mov rds,dx
          Xor dx,dx
          MOV AX,3f00h
          INTR 21h
          jc @BR_ERR
          CMP ax,cx
          je @BR_quit
          mov eax,0f0h
 @BR_err:
          SETdoserr EAX

 @BR_QUIT:
          ret
ENDP

;-------------------------------------------------------------
;Procedure BlockWrite(BUF:POINTER;Count:DWORD;Handle:DWORD);
;-------------------------------------------------------------

BlockWRITE Proc near
          Arg @P:pointer,@C:dword,@A:DWORD

          USES EAX,EBX,ECX,EDX
          CLRDOSERR
          MOV ECX,@C
          MOV EDX,@P.ofset
          MOV AX,@P.selector
          Call copyreal Pascal,Ax,edx,ecx
          Mov Ebx,@A
          MOV DX,RMBUF_seg
          Mov rds,dx
          Xor dx,dx
          MOV Ax,4000h
          INTR 21h
          jc @Bw_ERR
          CMP ax,cx
          je @Bw_quit
          mov eax,0f0h
 @Bw_err:
          SETdoserr EAX

 @Bw_QUIT:
          ret
ENDP

;---------------------------------------------------------
; Procedure FSEEK(Handle:Dword;P:Dword);
;---------------------------------------------------------

Fseek proc near
      ARG @H:DWord,@P:dword

      Uses EAX
      MOV ax,0
      Call FSETPOS Pascal,@H,@P,EAX
      ret

Endp


;---------------------------------------------------------
;Procedure RENAME(OLDNAME:POINTER;NEWNAME:POINTER);
;---------------------------------------------------------

RENAME Proc near
       ARG @O:pointer, @N:pointer

       USES EAX,EBX,ECX,EDX,ESI,EDI

       Call CopyReal pascal,@O.selector,@o.ofset,80
       MOV DX,RMBUF_SEG
       MOV RDS,DX
       xor dx,dx

       MOV EAX,RMBUF_OFFSET
       ADD EAX,128
       Call MOVE Pascal,@N.Selector,@N.ofset,SDATA0,EAX,80
       MOV DI,RMBUF_SEG
       MOV RES,DI
       Mov dx,8

       MOV AX,5600h
       INTR 21h
       jnc @REN_QUIT

       SETdosERR EAX

  @Ren_quit:
       ret
ENDP

;---------------------------------------------------------
;Procedure KILL(Nome:POINTER);
;---------------------------------------------------------

KILL Proc Near
     Arg @n:Pointer

     USES EAX,ECX,EDX
     CLRDOSERR
     Call CopyReal pascal,@n.selector,@n.ofset,80
     Mov ah,41h
     MOV dx,RMBUF_seg
     MOv RDS,DX
     xor dx,dx
     IntR 21h
     jnc @@kill_QUIT

      SETDOSERR EAX

 @@kill_QUIT:
        ret
ENDP


;##########################################################################
; DIRECTORY ROUTINES
;##########################################################################

;---------------------------------------------------------
; Procedure Mkdir(Nome:Pointer);
;---------------------------------------------------------

MKDIR proc Near
	Arg @n:pointer

	Uses EAX,EBX,ECX,EDX
	clrdoserr
        Call CopyReal pascal,@n.selector,@n.ofset,80
        MOV DX,RMBUf_seg
        MOV RDS,dx
        Xor dx,dx
	MOV ah,39h			;Servi‡o 39h - Create Dir
	INTr 21h
	jnc @MD_quit

	Setdoserr EAX

@MD_quit:

	ret
Endp

;---------------------------------------------------------
; Procedure RMdir(Nome:Pointer);
;---------------------------------------------------------

RMDIR proc Near
	Arg @n:pointer

	Uses EAX,EBX,ECX,EDX
	clrdoserr
        Call CopyReal pascal,@n.selector,@n.ofset,80
        MOV DX,RMBUf_seg
        MOV RDS,dx
        Xor dx,dx
	MOV ah,3ah
	INTr 21h
	jnc @rD_quit

	Setdoserr EAX

@rD_quit:
	ret
Endp

;---------------------------------------------------------
; Procedure CHdir(Nome:Pointer);
;---------------------------------------------------------

CHDIR proc Near
	Arg @n:pointer

	Uses EAX,EBX,ECX,EDX
	clrdoserr
        Call CopyReal pascal,@n.selector,@n.ofset,80
        MOV DX,RMBUf_seg
        MOV RDS,dx
        Xor dx,dx
	MOV ah,3bh
	INTr 21h
	jnc @cD_quit

	Setdoserr EAX

@cD_quit:

	ret
Endp

;---------------------------------------------------------
; FUNCTION GETCurrdir(DRIVE:WORD):STRING;
;---------------------------------------------------------

GETCURRDIR proc Near
	Arg @d:Word

	Uses EAX,EBX,ECX,EDX,ESI
	clrdoserr

        MOV DX,RMBUf_seg
        MOV RDS,dx
        Mov dx,@D
        Xor si,si
        MOV di,si
	MOV ah,47h
	INTr 21h
	jnc @gD_quit

	Setdoserr EAX

@gD_quit:

	ret
Endp

;##########################################################################
; DIsk Functions
;##########################################################################


;------------------------------------------------------------
;Procedure DiskReset;
;------------------------------------------------------------

DISKRESET PROC NEAR

          USES Eax
          mov ah,0Dh
          intr 21h
          ret
Endp

;------------------------------------------------------------
;Procedure SetDrive(D:word);   (Use Char da drive)
;------------------------------------------------------------

SETDRIVE Proc NEAr
         Arg @D:word

         Uses EAX,EDX
         clrdoserr
         Mov DX,@D
         Sub Dl,'A'
         Mov ah,0eh
         Intr 21h
         jnc @@SETD_quit
         Setdoserr EAX

    @@SETD_quit:
         RET
Endp


;------------------------------------------------------------
;Function GetDriveNo:Word(AX)
;------------------------------------------------------------

GetDriveNo Proc near

           USES EDX
           Mov ah,19h
           intr 21h
           mov dl,al
           mov ah,0eh
           intr 21h
           xor ah,ah
           ret
Endp


;------------------------------------------------------------
;Function getcurdrive:WORD;
;------------------------------------------------------------

GETCURDrive  proc Near


	     clrdoserr
	     MOV ah,19h
	     INTr 21h
	     jnc @getcurdrive_quit
	     Setdoserr EAX

  @getcurdrive_quit:
             ret
Endp


;------------------------------------------------------------
;Procedure SETDTA(DTA_SEG:WOrd,DTA_OFS:WORD);
;------------------------------------------------------------

SETDTA  proc Near
	Arg @DS:WORd,@Do:Word

	Uses EAX,EDX
	clrdoserr
        MOV ax,@DS
        Mov rds,ax
        Mov dx,@DO
	MOV ah,1ah
	INTr 21h
	jnc @SETDTA_quit
	Setdoserr EAX

  @SETDTA_quit:

	ret
Endp

;------------------------------------------------------------
;Function GETDTA:RMpointer(AX:DX);
;------------------------------------------------------------

GETDTA  proc Near

	Uses EBX
	clrdoserr
	MOV ah,2fh
	INTr 21h
	MOV AX,RES        ;AX - DTA RM segment
	MOV DX,BX	  ;DX - DTA RM OFFSET
	jnc @GETDTA_quit
	Setdoserr EAX

@GETDTA_quit:

	ret
Endp


;##########################################################################
; MISC Functions
;##########################################################################

;------------------------------------------------------------
;
;------------------------------------------------------------

;------------------------------------------------------------
; Function Dosversion:Word(AX);
;------------------------------------------------------------

DosVersion proc Near

	Mov ah,30h
	INtR 21h
	ret
Endp





;##########################################################################
; DATE/TIME
;##########################################################################


;------------------------------------------------------------
;Function GETDATE;
; RETS AL - Dia da semana  (Domingo=0)
;      CX - ANO
;      DH - Mes / DL - DIA
;------------------------------------------------------------

GETDATE  proc Near

	clrdoserr
	MOV ah,2Ah
	INTr 21h
	jnc @GETDate_quit
	Setdoserr eAX

  @GETDate_quit:

	ret
Endp

;------------------------------------------------------------
;Procedure SetDate(Ano:Word;MES,DIA:WORD);
;------------------------------------------------------------

SETDATE  proc Near
	ARG ANO:WORD, MES:word, DIA:WORD

	USES EAX,ECX,EDX
	clrdoserr
	MOV Cx,ANO
	MOV dx,DIA
	MOV ax,mes
	mov dh,al
	MOV ah,2Bh
	INTr 21h
	jnc @SETDate_quit

	Setdoserr EAX

   @sETDate_quit:

	ret
Endp

;------------------------------------------------------------
;Function GetTime;
;  RETS CH- HORAS /CL - Minutos /DH-Segundos /DL-Cent‚simos
;------------------------------------------------------------

GETTIME  proc Near

	clrdoserr
	MOV ah,2Ch
	INTr 21h
	jnc @gettime_quit
	Setdoserr EAX

   @gettime_quit:

	ret
Endp

;------------------------------------------------------------
;Procedure SetDate(Horas,Minutos,segundos,cent‚simos:WORD);
;------------------------------------------------------------

SETTime  proc Near
	ARG Horas:WORD, Mins:Word, Segs:word, Cent:Word

	USES EAX,ECX,EDX
	clrdoserr
	MOV Cx,mins
	mov dx,horas
	mov ch,dl
	mov dx,cent
	Mov ax,segs
	Mov dh,al
	MOV ah,2Dh
	INTr 21h
	jnc @SETTime_quit
	Setdoserr EAX

  @SETTime_quit:

	ret
Endp


;--------------------------------------------------------------------------

code32 ends

end

