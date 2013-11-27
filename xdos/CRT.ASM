.386p
.model large,pascal


INCLUDE DPMI.INC
INCLUDE EXTERNAL.inc
INCLUDE CRT.ASO

ASSUME DS:CODE32, CS:CODE32, ES:NOTHING, FS:CODE16


;=========================================================================
;=========================================================================

CODE32 Segment Para Public use32

org 0
ALIGN 4
;=========================================================================

Global Simples:Byte
Global Dupla:Byte
Global Duplahor:Byte
Global Duplavert:Byte
Global Classica:Byte
Global Dotted:byte

Global INITiaL_mode:byte
Global textbuf:Dword
Global TXT_ROWS:Byte
Global TXT_colS:Byte

;=========================================================================


;Tipos de caixa ---------------------------------------------------------

Simples		db 'ÚÄ¿³ÙÄÀ³'
Dupla		db 'ÉÍ»º¼ÍÈº'
DuplaHor	db 'ÕÍ¸³¾ÍÔ³'
DuplaVert	db 'ÖÄ·º½ÄÓº'
classica	db 'ÛßÛÞÛÜÛÝ'
Dotted		db '+-+|+-+|'

;==========================================================================

gLOBAL vIDEO_INIT:NEAR
global gOTOXY:NEAR
global getXY:NEAR
global sETCURSOR:NEAR

global scrn_init:near
Global Scrn_gotoxy:near
Global Scrn_setrect:near
Global Scrn_setsegment:near
Global Scrn_calcoffset:near
Global Scrn_color:near
Global Scrn_cls:near
Global Scrn_fill:near
Global Scrn_printxy:near
Global Scrn_print:near
Global Scrn_box:near
Global Scrn_Scrollup:near
Global Scrn_scrollDown:near
Global Scrn_getchar:near
Global Scrn_getatr:near
Global Scrn_setchar:near
Global Scrn_setatr:near
Global Scrn_Muda:near
GLObal Scrn_Limpa:near
Global Scrn_Getarea:near
Global Scrn_PUtarea:near


;##########################################################################
; CRT INITIALIZATION
;##########################################################################
;VIDEO VARS

INITIAL_MODE  DB ?	;Initial Video Mode
TEXTBUF	      DD ?    	;Phisical Text buffer

TXT_ROWS      DB ?      ;N§ of rows
TXT_COLS      DB ?      ;N§ of cols


;----------------------------------------------------------------
; Inicializa as rotinas modo texto, e salva o modo de entrada
; Procedure Video_init;
;----------------------------------------------------------------

VIdeo_init proc near

        Uses EAX,EDX
        MOV eax,0449h
        Mov dl,ES:[EAX]
        Mov Initial_mode,dl
        cmp dl,7
        je @@Is_mono

        Push 0B8000h
        Pop Textbuf
        Mov al,24
        Mov txt_cols,al
        Mov al,79
        Mov TXT_rows,al
        ret

@@IS_Mono:
        Push 0B0000h
        POp textbuf
        Mov al,24
        Mov txt_cols,al
        Mov al,79
        Mov TXT_rows,al
        ret
Endp

;-------------------------------------------------------------
; PROCeDURE GOTOXY(x,Y:WORD)
;-------------------------------------------------------------

gOTOXY PROC NEAR
       ARG @x:Word,@y:WORD

       uSES EAX,EBX,ECX,EDX
       mOV AH,0FH

       mOV BX,CODE16
       mOV REGS._cS,BX
       mOV BX,OFFSET RM_INT10
       mOV REGS._ip,bx
       cALL Call_RM_far

       MOV AH,02H
       MOV DX,@x
       MOV CX,@Y
       MOV DH,CL

       mOV CX,CODE16
       mOV REGS._cS,CX
       mOV CX,OFFSET RM_INT10
       mOV REGS._ip,Cx
       cALL Call_RM_far

       RET
ENDP

;-------------------------------------------------------------
; fUNCTION getXY:WORD(DX(DH-LINHA,DL-COLUNA));
;-------------------------------------------------------------

getXY PROC NEAR

       uSES EAX,EBX,ECX
       mOV AH,0FH

       mOV BX,CODE16
       mOV REGS._cS,BX
       mOV BX,OFFSET RM_INT10
       mOV REGS._ip,bx
       cALL Call_RM_far

       MOV AH,03H

       mOV CX,CODE16
       mOV REGS._cS,CX
       mOV CX,OFFSET RM_INT10
       mOV REGS._ip,Cx
       cALL Call_RM_far

       RET
ENDP


;-------------------------------------------------------------
; PROCeDURE sETCURSOR(B:BOOL);
;-------------------------------------------------------------

sETCURSOR PROC NEAR
          ARG @B:WORD

          uSES EAX,EBX,ECX,EDX
          mOV AH,0FH

          mOV BX,CODE16
          mOV REGS._cS,BX
          mOV BX,OFFSET RM_INT10
          mOV REGS._ip,bx
          cALL Call_RM_far

          mOV AH,01H
          CMP @B,1
          JE @setc_on

          mOV CH,00
          MOV CL,100
          JMP @SETC_COMMON

   @sETC_ON:
          mOV ch,0
          MOV CL,2

   @SETC_COMMON:
          mOV DX,CODE16
          mOV REGS._cS,DX
          mOV dX,OFFSET RM_INT10
          mOV REGS._ip,Dx
          cALL Call_RM_far

          RET
ENDP




;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

align 4

;----------------------------------------------------------
;Procedure screen.init(Self:DWORD);
;----------------------------------------------------------

SCRN_INIT PROC Near
	ARG @s:dword

	Uses EAX,EDI
        Mov EDi,@S
	mov ax,80
	Mov ES:[EDI][Scrn.colunas],ax
	Mov ES:[EDI][Scrn.cols],ax
	Xor ax,ax
        MOV ES:[EDI][SCRN.Tofset],ax
	MOV ES:[EDI][Scrn.window.a.x],AX
	MOV ES:[EDI][Scrn.window.a.y],AX
	Mov ES:[EDI][Scrn.AT.X],ax
	Mov ES:[EDI][Scrn.AT.Y],ax

	Mov aX,79
	MOV ES:[EDI][Scrn.window.b.x],AX
	Mov ax,24
	MOV ES:[EDI][Scrn.window.b.y],AX

        mov al,7
	Mov ES:[EDI][Scrn.atr],al

	Mov EAX,TEXTBUF
	Mov ES:[EDI][Scrn.displ],EAX

 @Init_Quit:
	ret
Endp

;----------------------------------------------------------
;Procedure  screen.gotoxy(Self:DWORD;x,y:word);
;----------------------------------------------------------

Scrn_gotoxy proc NEAR
	ARG @S:DWORD,@X:Word,@y:word

	Uses EAX,EDI
	MOv Edi,@S
	Mov ax,@X
	Mov ES:[EDI][scrn.at.X],ax
	Mov ax,@y
	Mov ES:[EDI][scrn.at.y],ax
	ret
Endp

;-----------------------------------------------------
;Procedure screen.SetRect(SELF:DWORD;x,y,x1,y1:word);
;------------------------------------------------------

Scrn_setrect proc Near
	ARG @S:DWORD,@X:Word,@y:word,@X1:Word,@Y1:word

	Uses EBX,EAX,EDI,EDX
	MOv Edi,@s
	Mov ax,@X
	Mov ES:[EDI][scrn.Window.a.x],ax
	shl ax,1
	push ax
	Mov ax,@x1
	Mov ES:[EDI][scrn.Window.b.x],ax
	Mov ax,@y
	Mov ES:[EDI][scrn.Window.a.y],ax
	push ax
	Mov ax,@y1
	Mov ES:[EDI][scrn.Window.b.y],ax
	Mov ax,Colunas
	SHL ax,1
	pop bx
	mul bx
	Pop bx
	add ax,bx
	Mov ES:[EDI][scrn.tofset],AX
	ret
Endp

;-----------------------------------------------------
;Procedure screen.SetSegment(Self:Dword;s:dword);
;------------------------------------------------------

Scrn_setsegment proc near
	ARG @s:Dword,@sg:dword

	USES EDI,EAX
	Mov EDI,@s
	Mov EAX,@sg
	Mov ES:[edi][scrn.displ],eax
	ret
endp


;-------------------------------------------------------
;Function screen.CalcOffset(Self:Dword;X,y:word):word;
;-------------------------------------------------------

Scrn_Calcoffset Proc Near
	Arg @S:dword,@X:word,@y:word

	Uses EDI,EDX,EBX
	Mov EDi,@S
	MOV ax,ES:[EDI][Scrn.Colunas]
	Shl ax,1
	Mov bx,@y
	Mul bx
	ADD Ax,es:[EDI][scrn.Tofset]
	ADD ax,@X
	ADD ax,@X
	ret
Endp


;-------------------------------------------------
;Procedure screen.color(Self:DWORD;fg,bg:Word);
;-------------------------------------------------

Scrn_color Proc Near
	Arg @S:Dword,@FG:word,@BG:word

	uses edi,eax
	MOV EDI,@s
	Mov ax,@bg
	shl ax,4
	add ax,@fg
	Mov ES:[EDI][scrn.atr],al
	ret
Endp


;---------------------------------------------
;Procedure Pscreen.cls(self:DWORD);
;---------------------------------------------

Scrn_CLS Proc NEAR
	Arg @S:DWord

	USES EAX,EBX,ECX,EDX,ESI,EDI
	MOV EDI,@s

	MOVzx edx,ES:[EDI][scrn.b.x]
	Sub dx,ES:[EDI][scrn.a.x]

	MOVzx ecx,ES:[EDI][scrn.b.y]
	Sub cx,ES:[EDI][scrn.a.y]

	inc dx
	inc cx
	MOvzx ebx,ES:[EDI][Scrn.Colunas]
	Sub bx,dx
	shl bx,1

	MOv ESI,ES:[EDI][scrn.displ]
 	ADD SI,ES:[EDI][Scrn.TOFSET]
	Mov ah,ES:[EDI][scrn.atr]
	MOv Al,20h
	MOv EDI,ESI

	CLD
    @@CLS_LOOP: Push ECX
                MOV ECX,EDX
		cld
		Rep Stosw
		ADD EDi,Ebx
		POP ECX
		loop @@CLS_loop
    @CLS_quit:
	 ret
endp


;-----------------------------------------------------------------
;Procedure screen.Fill(Self:DWORD;X,y,x1,y1:word;Car:char);
;-----------------------------------------------------------------

Scrn_fill proc Near
	ARG @S:DWORD,@X:Word,@Y:word,@X1:word,@Y1:word,@C:byte

	USES EAX,EBX,ECX,EDX,ESI,EDI
	Mov EDI,@S

	mov ax,ES:[EdI][scrn.window.b.x]
	sub ax,es:[EDI][scrn.window.a.x]
	cmp @X,ax
	jg @Fill_exit

	cmp @x1,ax
	jbe @Fill_c1

	mov @x1,ax

   @Fill_c1:
	mov ax,ES:[EDI][scrn.window.b.y]
	sub ax,es:[EDI][scrn.window.a.y]
	cmp @y,ax
	jg @Fill_exit

	cmp @y1,ax
	jbe @Fill_c2

	mov @y1,ax

   @Fill_c2:
	mov ax,@y
	movzx Ebx,ES:[EDi][scrn.colunas]
	shl bx,1
	push Ebx
	mul bx
	add ax,@X
	add ax,@X
	movzx Esi,ax
	add si,es:[EDI][scrn.tofset]

	movzx Ecx,@y1
	sub cx,@y
	movzx Edx,@x1
	sub dx,@X
	inc cx
	inc dx

	mov al,@c
	mov ah,es:[EDI][scrn.atr]
	add ESI,ES:[EDI][scrn.displ]
	Mov EDi,ESI
	pop Ebx
	cld
   @Fill_loop: Push Ecx
               Push Edi
               mov cx,dx
               rep stosw
               pop Edi
               pop Ecx
               add Edi,Ebx
               loop @fill_loop

@fill_exit:
	ret
Endp



;-------------------------------------------------------------------------
;Procedure screen.PrintXY(Self:Dword;X,y:word;S:string);
;-------------------------------------------------------------------------

Scrn_PrintXY Proc NEAR
	ARG @S:DWORD,@X:word,@Y:word,@SP:WORD,@so:DWORD

	USES ESI,EAX,EBX,ECX,EDX,EDI,DS
	Mov EDI,@S

	MOV ESI,ES:[EDI][scrn.displ]
	Add si,ES:[edi][scrn.tofset]

	Movzx eax,es:[EDI][scrn.window.b.x]	;TX:=Win.b.x-win.a.x
	sub ax,es:[EDI][scrn.window.a.x]
	cmp @x,ax                         	;IF x>TX then exit
	push ax
	jg @Printxy_exit

	Movzx eax,es:[EDI][scrn.window.b.y]	;TY:=Win.b.x-win.a.x
	sub ax,es:[EDI][scrn.window.a.y]
	cmp @y,ax
	jg @PrintXY_exit

	pop bx
        Movzx EAX,@SP
        Call StringLen Pascal,eax,@SO

	cmp al,0                       ;        {if s[0]=#0 then exit;}
	jz @Printxy_exit
	xor ah,ah
	mov cx,@x
	add cx,ax
	cmp bx,cx
	jg @Printxy_Cont

	movzx edx,ES:[EDI][scrn.window.a.x]
	add dx,@x
	mov ax,ES:[EDI][scrn.window.b.x]
	sub ax,dx
	inc ax

  @Printxy_Cont:
	Push ax

	mov bx,ES:[EDI][SCRN.Colunas]
	shl bx,1

	mov ax,@y
	mul bx           ;{BX:=Y*BX}
	add ax,@x
	add ax,@x        ;{BX:=BX+(x*2)}
	add Si,ax

	pop cx
	cld
	mov ah,ES:[EDI][scrn.atr]
	MOV DS,@SP
	PUSH esi
	Mov ESI,@SO
	POP EDI

  @Printxy_Looper: Lodsb
                   Stosw
                   loop @PrintXY_looper
  @Printxy_exit:
	ret
endp


;----------------------------------------------------------------
;Procedure screen.Print(Self:Dword;S:string);
;----------------------------------------------------------------

SCrn_Print Proc Near
	ARG @S:Dword, @ST:pointer

	USES ESI,EAX,EBX,ECX,EDX,EDI,DS
	Mov EDI,@S

	MOV ESI,ES:[EDI][scrn.displ]
	Add si,ES:[edi][scrn.tofset]

	Movzx eax,es:[EDI][scrn.window.b.x]	;TX:=Win.b.x-win.a.x
	sub ax,es:[EDI][scrn.window.a.x]
	cmp ES:[EDI][scrn.at.x],ax            	;IF x>TX then exit
	push ax
	jg @Print_exit

	Movzx eax,es:[EDI][scrn.window.b.y]	;TY:=Win.b.x-win.a.x
	sub ax,es:[EDI][scrn.window.a.y]
	cmp ES:[EDI][scrn.at.y],ax
	jg @Print_exit

	pop bx
        Movzx EAX,@ST.selector
        Call StringLen Pascal,eax,@St.Ofset

	cmp al,0                       ;        {if s[0]=#0 then exit;}
	jz @Print_exit
	xor ah,ah
	mov cx,ES:[EDI][scrn.at.x]
	add cx,ax
	cmp bx,cx
	jg @Print_Cont

	movzx edx,ES:[EDI][scrn.window.a.x]
	add dx,ES:[EDI][scrn.at.x]
	mov ax,ES:[EDI][scrn.window.b.x]
	sub ax,dx
	inc ax

  @Print_Cont:
	Push ax

	mov bx,ES:[EDI][SCRN.Colunas]
	shl bx,1

	mov ax,ES:[EDI][scrn.at.y]
	mul bx           ;{BX:=Y*BX}
	add ax,ES:[EDI][scrn.at.x]
	add ax,ES:[EDI][scrn.at.x]       ;{BX:=BX+(x*2)}
	add Si,ax

	pop cx
	cld
	mov ah,ES:[EDI][scrn.atr]
	MOV DS,@ST.selector
	PUSH esi
	Mov ESI,@ST.ofset
	POP EDI

  @Print_Looper: Lodsb
                 Stosw
                 loop @Print_looper
  @Print_exit:
	ret
endp


;-------------------------------------------------------------------------
;Procedure screen.box(Self:Dword;X,y,x1,y1:word;Tipo:string8);
;-------------------------------------------------------------------------
B1 dw ?
B2 dw ?
EX dw ?

Scrn_Box Proc NEAR
	Arg @S:Dword,@X:Word,@Y:word,@X1:word,@Y1:Word,@Ts:word,@TO:dword

	USES EDI,ESI,DS,EAX,EBX,ECX,EDX
	MOV EDI,@S
	xor Eax,Eax
	mov b1,ax
	mov b2,ax

	mov ax,ES:[EDI][scrn.window.b.x]
	sub ax,ES:[EDI][scrn.window.a.x]

	cmp @x,ax
	jg @Box_exit          ; IF X>(win.screen.b.screen.x-win.screen.a.screen.x) then exit

	inc ax
	cmp @x1,ax
	jbe @Box_c1           ; IF X1>(win.screen.b.screen.x-win.screen.a.screen.x) then
                              ; B1=true, X1:=(win.screen.b.screen.x-win.screen.a.screen.x)
	mov @x1,ax
	mov ax,1
	mov b1,ax


  @Box_C1:
	mov ax,ES:[EDI][scrn.window.b.y]
	sub ax,ES:[EDI][scrn.window.a.y]
	cmp @y,ax
	jg @Box_exit          ;  IF Y>(win.screen.b.screen.y-win.screen.a.screen.y) then exit

	inc ax
	cmp @y1,ax
	jbe @Box_c2           ;  IF Y1>(win.screen.b.screen.Y-win.screen.a.screen.Y) then B1=true, y1:=(win.screen.b.screen.y-win.screen.a.screen.y)

	mov @y1,ax
	mov ax,1
	mov b2,ax

  @BOX_C2:

	mov ax,@y               ; É
	movzx Ebx,ES:[EDI][scrn.COLUNAS]
	add bx,bx
	push bx
	mul bx
	add ax,@x
	add ax,@x
	mov ex,ax

	MOv ds,@Ts
	Mov ESI,@to
	lodsb
	mov ah,ES:[EDI][scrn.atr]
	Mov bx,ES:[EDI][scrn.tofset]
	Push EDI
	push ES:[EDI][scrn.displ]
	pop EDI
	add di,ex
	add di,bx
	stosw
	mov ex,di
	POP EDI

	Mov ax,@y1       ;-
	sub ax,@y
	pop bx
	mul bx
	mov dx,ax
	sub dx,2

	lodsb
	mov bl,al
	push si
	add si,3
	lodsb
	mov bh,al
	pop si

	mov cx,@x1
	sub cx,@x
	dec cx
	Mov al,bl
	mov ah,ES:[EDI][scrn.atr]
	cld
	Push EDI ESI
	MOV ESi,ES:[EDI][scrn.displ]
	Xchg EDI,ESI
	POP ESI
	push di
	add di,ex

  @Box_loopX: Stosw
              cmp b1,1
              je @Box_NOX1
              Push di
              add di,dx
              mov al,bh
              stosw
              mov al,bl
              pop di

      @BOX_Nox1:
              loop @box_loopX

	mov cx,di
	pop di

	cmp b2,1
	je @BOX_c3

	push di si        ;¿
	mov di,cx
	lodsb
	stosw
	pop si di

  @BOX_C3:
	cmp b1,1
	je @Box_c4
                	;À
	push di
	add di,ex
	add di,dx
	push si
	add si,4
	lodsb
	pop si
	stosw
	pop di

	Mov bx,@x1       ;Ù
	sub bx,@x
	add bx,bx
	add bx,dx
	add di,ex
	add di,bx
	push si
	add si,2
	lodsb
	pop si
	stosw

  @Box_c4:

	pop Edi          ;³

	mov ax,ES:[EDI][scrn.cols]
	add ax,ax
	add ex,ax
	mov cx,ax

	Mov dx,@x1
	sub dx,@x
	dec dx
	shl dx,1


	add si,1
	lodsb
	mov bh,aL
	add si,3
	lodsb
	mov bl,al
	mov al,bh
	mov ah,ES:[EDI][scrn.atr]
	PUSH EDI
	PUSH ES:[EDI][scrn.displ]
	POP EDI
	add di,ex
	sub di,2

	mov ex,cx
	sub ex,2

	mov cx,@y1
	sub cx,@y
	dec cx

	cld

  @Box_loopY: Stosw
              cmp b2,1
              je @Box_Noy1
              push di
              add di,dx
              mov al,bl
              stosw
              mov al,bh
              pop di

        @Box_noY1:
            add di,ex
            Loop @Box_loopY

	Pop EDI

  @Box_exit:
	ret
ENDp


;---------------------------------------------------------------
;Procedure screen.ScrollUp(Self:Dword;X,y,x1,y1,l:word);
;---------------------------------------------------------------

Scrn_Scrollup Proc near
	Arg @S:Dword, @X:Word,@Y:word,@x1:word,@y1:word,@l:word

	Uses EDI,EAX,EBX,ECX,EDX,ESI,DS
	MOV EDI,@s

	mov ax,ES:[EDI][scrn.window.a.x]
	add @x,ax
	add @x1,ax

	mov ax,ES:[EDI][scrn.window.b.x]
	Cmp @X,ax
	jg @Sup_Exit

	cmp @x1,ax
	jbe  @SUP_checkY

	mov @x1,ax

  @Sup_checky:
	mov ax,ES:[EDI][scrn.window.a.y]
	add @y,ax
	add @y1,ax

	mov ax,ES:[EDI][scrn.window.b.y]
	Cmp @y,ax
	jg @Sup_Exit

	cmp @y1,ax
	jbe  @Sup_cont

	mov @y1,ax

  @Sup_cont:

	Movzx ecx,@y1
	movzx eax,@y
	sub cx,ax
	sub cx,@l

	Mov ESI,es:[edi][scrn.displ]

	Movzx ebx,ES:[EDI][scrn.Colunas]       ;Endere‡o de destino
	Mov EDI,ESI

	Shl bx,1
	push bx
	mul bx
	add ax,@x
	add ax,@x
	add Di,ax
	add si,ax


	POP bx                          ;endere‡o de origem
	Mov ax,@l
	mul bx
	add si,ax

	Movzx eAx,@X1                       ;words por linha
	sub ax,@x
	inc eax
	push eax
	Mov edx,eax

	sub bx,dx
	sub bx,dx

        Mov ds,sdata0

	cld

@SUP_l1:	Push Ecx
     		Mov cx,dx
     		cld
     		rep movsw
     		add di,bx
     		add si,bx
     		pop Ecx
     		loop @SUP_l1

	pop edx
        Mov ESI,@S
	Mov al,20h
	MOV ah,es:[ESI][scrn.atr]
	movzx ecx,@l
        cld

@SUP_l2:	Push Ecx
     		Mov Ecx,Edx
     		cld
     		rep stosw
     		add di,bx
     		pop Ecx
    		loop @SUP_l2

@Sup_exit:
	ret
endp


;---------------------------------------------------------------
;Procedure screen.ScrollDOWN(Self:Dword;X,y,x1,y1,l:word);
;---------------------------------------------------------------

Scrn_ScrollDOWN Proc near
	Arg @S:Dword, @X:Word,@Y:word,@x1:word,@y1:word,@l:word

	Uses EDI,EAX,EBX,ECX,EDX,DS,ESI
	MOV EDI,@s

	mov ax,ES:[EDI][scrn.window.a.x]
	add @x,ax
	add @x1,ax

	mov ax,ES:[EDI][scrn.window.b.x]
	Cmp @X,ax
	jg @Sdn_Exit

	cmp @x1,ax
	jbe  @Sdn_checkY

	mov @x1,ax

  @Sdn_checky:
	mov ax,ES:[EDI][scrn.window.a.y]
	add @y,ax
	add @y1,ax

	mov ax,ES:[EDI][scrn.window.b.y]
	Cmp @y,ax
	jg @Sdn_Exit

	cmp @y1,ax
	jbe  @Sdn_cont

	mov @y1,ax

  @Sdn_cont:
	MOV ESI,ES:[eDI][scrn.displ]

	MOVzx eax,@y1               ;Destino
	movzx ebx,ES:[EDI][scrn.colunas]
	shl bx,1
	mul bx
	add ax,@x
	add ax,@x
	push edi
	Mov EDI,ESI
	add DI,AX

	mov ax,@y1               ;Origem
	sub ax,@l
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	Mov DX,@X1               ;{Words por linha}
	sub dx,@x
	inc dx

	add bx,dx
	add bx,dx

	push es
	pop ds

	Movzx ecx,@y1
	sub cx,@y
	sub cx,@l
	inc cx
	cld

  @SDN_L1: 	Push ECX
     		Mov cx,dx
     		cld
     		rep movsw
     		sub si,bx
     		sub di,bx
     		pop Ecx
     		loop @SDN_l1

	Movzx Ecx,@l
	POP ESI
	mov ah,ES:[ESI][scrn.atr]
	mov al,20h

	cld

  @SDN_l2:	push Ecx
     		Mov cx,dx
     		cld
     		rep stosw
     		sub di,bx
     		pop ecx
     		loop @SDN_l2

  @Sdn_exit:
	ret
endp


;------------------------------------------------------------------
;Function screen.getchar(self:dword;x,y:word):(DL:CHar);
;------------------------------------------------------------------

Scrn_getchar Proc near
	Arg @S:DWord,@X:word,@y:word


	USES EDI,EAX,EBX,ESI
	Mov Edi,@S

	Mov ESi,ES:[EDI][scrn.displ]
	Add si,eS:[edi][scrn.tofset]

	Movzx eax,eS:[edI][scrn.window.b.x]
	sub ax,eS:[edI][scrn.window.a.x]
	cmp @x,ax
	jg @GC_exitErr

	Mov ax,eS:[edI][scrn.window.b.y]
	sub ax,eS:[edI][scrn.window.a.y]
	cmp @y,ax
	jg @GC_exitErr

	push EDX
	movzx ebx,es:[edi][scrn.colunas]
	shl bx,1
	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	pop edx
	mov dl,ES:[ESI]

	jmp @gc_exit

  @gc_ExitErr:
	xor dl,dl

  @gc_Exit:
	ret
Endp

;---------------------------------------------------------
;Function screen.getatr(SELF:DWORD;x,y:word):(DH:BYTE);
;---------------------------------------------------------

Scrn_GetATR Proc near
	ARG @s:Dword, @X:WORD,@Y:word

	Uses EDI,ESI,EAX,EBX,ECX
	Mov EDI,@S
	Mov ESi,ES:[EDI][scrn.displ]
	add si,es:[EDI][scrn.tofset]

	MOvzx eax,ES:[EDi][scrn.window.b.x]
	sub ax,ES:[EDi][scrn.window.a.x]
	cmp @x,ax
	jg @GA_exitErr

	MOvzx eax,ES:[EDi][scrn.window.b.y]
	sub ax,ES:[EDi][scrn.window.a.y]
	cmp @y,ax
	jg @GA_exitErr

	push EDX
	movzx ebx,ES:[EDI][scrn.colunas]
	shl bx,1
	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax
	INC si
	POP EDX
	MOV DH,ES:[ESI]

        jmp @GA_exit

  @GA_ExitErr:
	xor dh,dh

  @GA_Exit:
	ret
endp


;-------------------------------------------------------------------
;Procedure screen.setchar(Self:Dword;x,y:word;c:char);
;-------------------------------------------------------------------

Scrn_setchar proc near
	Arg @S:Dword, @X:word,@Y:word,@c:word

	USES EDI,ESI,EAX,EBX,ECX,EDX
	MOV EDI,@s

	MOv Esi,ES:[edi][scrn.displ]
	Add si,ES:[edi][scrn.tofset]


	MOvzx eax,ES:[EDi][scrn.window.b.x]
	sub ax,ES:[EDi][scrn.window.a.x]
	cmp @x,ax
	jg @sc_exit

	MOvzx eax,ES:[EDi][scrn.window.b.y]
	sub ax,ES:[EDi][scrn.window.a.y]
	cmp @y,ax
	jg @sc_exit

	movzx ebx,ES:[EDI][scrn.Colunas]
	shl bx,1
	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	mov ax,es:[esi]
	MOv cx,@c
	mov al,cl
	Mov ES:[ESI],ax

  @SC_Exit:
	ret
endp


;--------------------------------------------------------------
;Procedure screen.setatr(Self:dword;x,y:word;a:byte);
;--------------------------------------------------------------

Scrn_setatr proc near
	Arg @S:Dword, @X:word,@Y:word,@a:word

	USES EDI,ESI,EAX,EBX,ECX,EDX
	MOV EDI,@s

	MOv Esi,ES:[edi][scrn.displ]
	Add si,ES:[edi][scrn.tofset]


	MOvzx eax,ES:[EDi][scrn.window.b.x]
	sub ax,ES:[EDi][scrn.window.a.x]
	cmp @x,ax
	jg @sa_exit

	MOvzx eax,ES:[EDi][scrn.window.b.y]
	sub ax,ES:[EDi][scrn.window.a.y]
	cmp @y,ax
	jg @sa_exit

	movzx ebx,ES:[EDI][scrn.colunas]
	add bx,bx
	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	mov ax,es:[esi]
	MOv cx,@a
	mov ah,cl
	Mov ES:[ESI],ax

 @Sa_Exit:
	ret
endp


;---------------------------------------------------------------
;Procedure screen.muda(Self:Dword;X,y,x1:word;a:byte);
;---------------------------------------------------------------

Scrn_muda Proc near
	ARG @S:Dword, @X:word, @Y:word,@x1:word,@A:word

	USES EDI,ESI,EAX,EBX,ECX,EDX,DS
	MOV EDI,@S

	MOv esi,eS:[edI][scrn.Displ]
	add si,eS:[edI][scrn.tofset]

	MOvzx eax,ES:[EDi][scrn.window.b.x]
	sub ax,ES:[EDi][scrn.window.a.x]
	cmp @x,ax
	jg @sm_exit
	MOv cx,ax

	MOvzx eax,ES:[EDi][scrn.window.b.y]
	sub ax,ES:[EDi][scrn.window.a.y]
	cmp @y,ax
	jg @sm_exit

	mov ax,@X1
	sub ax,@x
	cmp al,0
	jz @SM_exit
	push cx
	cmp ax,CX

	jbe @SM_cont1
	sub cx,ax
	sub @X1,cx
			
  @SM_cont1:
	xor ah,ah
	mov cx,@x
	add cx,ax
	pop bx
	cmp bx,cx
	jg @SM_Cont

	mov dx,ES:[EDI][scrn.window.a.x]
	add dx,@x
	mov ax,ES:[EDI][scrn.window.b.x]
	sub ax,dx

  @SM_Cont:
	Push ax

	mov bx,ES:[EDI][scrn.colunas]
	add bx,bx

	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	pop cx
	inc cx
	cld
	MOv dx,@a
	push es
	pop ds
	MOV EDI,ESI

  @SM_Looper: Lodsw
	      mov ah,dl
              Stosw
              loop @SM_looper
  @SM_exit:
	ret
endp


;-----------------------------------------------------------------
;Procedure screen.limpa(Self:dword;X,y,x1:word;c:char);
;-----------------------------------------------------------------

Scrn_limpa Proc near
	ARG @S:Dword, @X:word, @Y:word,@x1:word,@c:word

	USES EDI,ESI,EAX,EBX,ECX,EDX,DS
	MOV EDI,@S

	MOv esi,eS:[edI][scrn.Displ]
	add si,eS:[edI][scrn.tofset]

	MOvzx eax,ES:[EDi][scrn.window.b.x]
	sub ax,ES:[EDi][scrn.window.a.x]
	cmp @x,ax
	jg @sl_exit
	MOv cx,ax

	MOvzx eax,ES:[EDi][scrn.window.b.y]
	sub ax,ES:[EDi][scrn.window.a.y]
	cmp @y,ax
	jg @sl_exit

	mov ax,@X1
	sub ax,@x
	cmp al,0
	jz @Sl_exit
	push cx
	cmp ax,CX

	jbe @Sl_cont1
	sub cx,ax
	sub @X1,cx

  @Sl_cont1:
	xor ah,ah
	mov cx,@x
	add cx,ax
	pop bx
	cmp bx,cx
	jg @Sl_Cont

	mov dx,ES:[EDI][scrn.window.a.x]
	add dx,@x
	mov ax,ES:[EDI][scrn.window.b.x]
	sub ax,dx

  @Sl_Cont:
	Push ax

	mov bx,ES:[EDI][scrn.colunas]
	add bx,bx

	mov ax,@y
	mul bx
	add ax,@x
	add ax,@x
	add si,ax

	pop cx
	inc cx
	cld
	MOv dx,@c
	push es
	pop ds
	MOV EDI,ESI

  @Sl_Looper: Lodsw
	    mov al,dl
            Stosw
            loop @Sl_looper
  @Sl_exit:
	ret
endp


;-----------------------------------------------------------------------
;Procedure screen.Getarea(@Self:dword;X,y,x1,y1:word;dest:pointer);
;-----------------------------------------------------------------------

Scrn_GetAREA proc neAR
	ARG @S:DWORD,@X:Word,@Y:word,@X1:word,@Y1:word,@ss:word,@so:dword

	USES EAX,EBX,ECX,EDX,ESI,EDI,DS,es
	Mov EDI,@s

	mov ax,ES:[EdI][scrn.window.b.x]
	sub ax,es:[EDI][scrn.window.a.x]
	cmp @X,ax
	jg @get_exit

	cmp @x1,ax
	jbe @get_c1

	mov @x1,ax

  @get_c1:
	mov ax,ES:[EDI][scrn.window.b.y]
	sub ax,es:[EDI][scrn.window.a.y]
	cmp @y,ax
	jg @get_exit

	cmp @y1,ax
	jbe @get_c2

	mov @y1,ax

  @get_c2:

	movzx eax,@y
	movzx ebx,ES:[EDI][scrn.colunas]

	shl bx,1
	push Ebx
	mul bx
	add ax,@X
	add ax,@X
	movzx Esi,ax
	add si,es:[EDI][scrn.tofset]
	add ESI,ES:[EDI][scrn.displ]

	movzx Ecx,@y1
	sub cx,@y
	movzx Edx,@x1
	sub dx,@X
	inc cx
	inc dx

	pop Ebx
	MOV EDI,@so
	mov ds,sdata0
	mov es,@ss
	Mov ax,dx
	stosw
	mov ax,cx
	stosw
	MOV ax,bx
	STOSW
	cld

  @get_loop: Push Ecx
             Push Esi
             mov ecx,edx
             cld
             rep movsw
             pop Esi
             pop Ecx
             add esi,ebx
             loop @get_loop


  @get_exit:
	ret
Endp




;-----------------------------------------------------------------
;Procedure screen.PUTarea(Self:dword;X,y:word;Src:pointer);
;-----------------------------------------------------------------

Scrn_PUTarea Proc near
	ARG @S:Dword, @X:word, @Y:word, @Ps:word,@po:dword

	USES EDI,ESI,EAX,EBX,ECX,EDX,DS
	Mov EDI,@s

	mov ax,ES:[EdI][scrn.window.b.x]
	sub ax,es:[EDI][scrn.window.a.x]
	cmp @X,ax
	jg @put_exit

	mov ax,ES:[EDI][scrn.window.b.y]
	sub ax,es:[EDI][scrn.window.a.y]
	cmp @y,ax
	jg @put_exit

	mov ax,@y
	movzx Ebx,ES:[EDI][scrn.colunas]

	shl bx,1
	mul bx
	add ax,@X
	add ax,@X
	movzx Esi,ax
	add si,es:[EDI][scrn.tofset]
	add ESI,ES:[EDI][scrn.displ]
	MOV EDI,@Po
	xchg esi,edi

	MOv DS,@Ps
	LODSW
	Movzx edx,ax
	LODSW
	movzx ecx,ax
	Lodsw
	Movzx ebx,ax
	cld

@put_loop: Push Ecx
            Push Edi
            mov cx,dx
            cld
            rep movsw
            pop Edi
            pop Ecx
            add di,bx
            loop @put_loop


@put_exit:
	ret
Endp


;------------------------------------------------------------------------
;
;------------------------------------------------------------------------

code32 ends
end


