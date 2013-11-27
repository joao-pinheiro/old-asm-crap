	Ideal
	P486

	LargeStack
	Model	NearStack Flat,C

	Assume	Cs:FLAT, Ds:FLAT, Es:FLAT, Fs:FLAT, Gs:FLAT, Ss:FLAT

	NoSmart		; Smart code generation
	Locals		; Allow local labels
	NoJumps		; No smart jumps
	MultErrs	; Warn with multiple errors

			; Warnings:
	Warn	BRK	; brackets needed []
	Warn	GTP	; type declared must match
	Warn	ICG	; inefficient code generation
	Warn	LCO	; location counter overflow
	Warn	OVF	; overflow
	Warn	PRO	; code segment override

        dataSeg

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

CopyrightMsg    db 'DPMI Stub v1.0b',0ah,0dh
		db 'Programmed by Joao Pinheiro (c) 2000'
                db 0ah,0dh,'$' 

InitErro01      db 'Nao existe memoria suficiente!',7,0ah,0dh,'$'

DpmiErro01	db 'Dpmi Host not found!',7,0ah,0dh,'$'
DpmiErro02	db 'Dpmi Host doesnt support 32 bit clients!'
		db 7,0ah,0dh,'$'
DpmiErro04      db 'Error on Switch to Dpmi!',7,0ah,0dh,'$'
DpmiErro05      db 'Error Allocating Descriptors!',7,0ah,0dh,'$'

DpmiVersionMsg  db 'Found Dpmi host v'
DpmiVersionH    db 0,'.'
DpmiVersionL    db 0,0,0ah,0dh,'$'

;--------------------------------------------------------------------
PspSegment      Dw 0               ;Segmento do Psp do programa
Dpmi_PdSize     Dw 0               ;Tamanho da PdSize em paragr
Dpmi_Entry      Dw 0,0             ;Far Ptr DpmiEntry
RmRegs          DpmiRegisters <>   ;Registos para mudanca de modo

StructStart     = $
Stack16Seg      Dw 0               ;Segmento da Stack16
Stack32Seg      Dw 0               ;Segmento da Stack32

InternalBuffer  Dw 0               ;Segmento do buffer interno
DosHeapBlock    Dw 0               ;Segmento do Bloco a reservar

SelPsp          Dw 0               ;Selector do Psp
SelCode16       Dw 0               ;Selector do Code16
SelData16       Dw 0               ;Selector do Data16
SelStack16      Dw 0               ;Selector do Stack16
SelData32       Dw 0               ;Selector do Data32 
SelCode32       Dw 0               ;Selector de code32
SelStack32      Dw 0               ;Selector de stack32
SelData0        Dw 0               ;Selector do Data0
SelCode0        Dw 0               ;Selector de code0

Dpmi_Version    Dw 0               ;Versao do dpmi
Dpmi_Flags      Dw 0               ;Flags de dpmi
Dpmi_Cpu        Db 0               ;Tipo de cpu
Dpmi_MasterPic  Db 0               ;virtual master interrupt controller base interrupt
Dpmi_SlavePic   Db 0               ;virtual slave interrupt controller base interrupt
Dpmi_SelIncVal  Dw 0               ;Valor de incremento aos selectors

;--------------------------------------------------------------------

StructSize = $ - StructStart



        codeseg
;####################################################################

PrintMsg Macro msg
         
         push dx ax
         mov dx,offset Msg
	 mov ah,09h
	 int 21h
	 pop ax dx

         EndM 

Quit	Macro Code

	Mov ah,4ch
	Mov al,Code
	Int 21h

        EndM

;####################################################################

;--------------------------------------------------------------------
;Início do programa
;--------------------------------------------------------------------

Start:
        Push cs                         ;Assegura q DS = Cs
        Pop Ds
        Push es
        pop PspSegment			;Salva Psp
        PrintMsg CopyrightMSG           ;Exibe msg de copyright

        mov ah,4ah
        int 21h

        mov ah,48h
        int 21h
        Mov InternalBuffer,ax
        Jnc @StartLabel_a2

  @StartLabel_a1:
        PrintMsg InitErro01
        Quit 1   

  @StartLabel_a2:
        cli                             ;Muda as stacks
        mov ss,stack16seg
        mov sp,dx
        xor bp,bp
        sti

;--------------------------------------------------------------------

  @StartLabel_a3:
        Mov ax,1687h                    ;verifica se existe DPMI
	int 2fh
	or ax,ax
        jz @StartLabel_a4

	PrintMsg DpmiErro01
        Quit 2

  @StartLabel_a4:
        Mov Dpmi_Version,Dx             ;Salva as vars necessarias
        Push Bx
        Mov Dpmi_pdSize,si
	Mov Dpmi_entry,Di
	Mov Dpmi_entry+2,Es

        add dh,48                       ;Exibe a msg de versao
        Mov DpmiVersionH,dh
        xor ax,ax
        mov al,dl
        mov bl,10
        div bl
        add al,48
        add ah,48
        Mov DpmiVersionL,al
        Mov DpmiVersionL+1,ah
        PrintMsg DpmiVersionMsg

        Pop Bx
        or bx,1                         ;ve se suporta 32 bit
        Jnz @StartLabel_a5

        PrintMsg DpmiErro02
        Quit 2

;--------------------------------------------------------------------
  @StartLabel_a5:
        .386P

        Mov bx,Dpmi_PdSize              ;aloca a mem para Dpmi
        cmp Bx,0
        Je @StartLabel_a6
        Mov ah,48h
        int 21h
        jc @StartLabel_a1               ;se falhou, aborta

  @StartLabel_a6:
        Mov es,ax                       ;prepara-se pra iniciar DPMI
        mov ax,1                        ;em 32 bit
        call dword ptr cs:dpmi_Entry    ;inicia
        jnc @StartLabel_a7

        push cs                         ;Restaura DS
        pop ds
        PrintMsg DpmiErro04
        Quit 3

;--------------------------------------------------------------------
  @StartLabel_a7:                       ;codigo em pm
        Mov SelPsp,Es                   ;Salva os novos selectors
        Mov Selcode16,cs
        Mov SelData16,Ds
        Mov SelStack16,ss

        Mov ax,400h                     ;Obtem dpmi flags
        int 31h
        Mov Dpmi_Flags,Bx
        Mov Dpmi_Cpu,Cl
        Mov Dpmi_MasterPic,Dh
        Mov Dpmi_SlavePic,Dl

        Mov ax,3                        ;Obtem Selector IncVal
        Int 31h
        mov Dpmi_SelIncVal, Ax

        Xor Ax,ax                       ;Aloca os descriptors de sistema
        Mov cx,3                        ;3 Descriptors
        Int 31h
        jnc @StartLabel_a9

        mov dx,offset DpmiErro05
        Call quiterror

  @StartLabel_a9:
        mov SelCode32,ax                ;Seta o selector de Code32
        mov dx,ax
        add ax,Dpmi_SelIncVal           ;Avanca para o proximo
        mov SelStack32,ax               ;Seta o selector de Stack32
        add ax,Dpmi_SelIncVal           ;Avanca para o proximo
        mov SelCode0,ax                 ;Seta o selector de Code0

;-------------------------------------------------------Code32/Data32
        Mov bx,cs                       ;Obtem os cpl bits de code16
        Lar cx,bx
        And ch,01100000b                ;isola os CPL bits        
        mov cl,ch
        push cx cx                      ;salva CPL
        or cl,10011011b                 ;Descr de code, present
        mov ch,40h                      ;Descr Use32

        mov bx,dx                       ;selector code32
        mov ax,09h                      ;Seta access rights
        int 31h

        mov ecx,text32                  ;calcula o limite
        shl ecx,4
        push ecx
        add ecx,offset Final32
        mov dx,cx
        shr ecx,16                      ;em cx:dx
        mov ax,08h                      ;seta o limite
        int 31h

        pop ecx
        mov ax,07h
        mov dx,cx
        shr ecx,16
        int 31h                         ;seta a base do descriptor

        mov ax,0ah                      ;cria alias de code32
        int 31h
        mov SelData32,ax                ;Salva Data32

;---------------------------------------------------------Code0/Data0
        pop cx                          ;obtem iopl
        or cl,10011011b                 ;Descr de code, present
        mov ch,0c0h                     ;Descr Use32,page aligned
        mov bx,SelCode0                 ;selector code0
        mov ax,09h                      ;Seta access rights
        int 31h

        mov cx,0ffffh
        mov dx,0ffffh
        mov ax,08h                      ;seta o limite
        int 31h                         ;a base sera 0

        mov ax,0ah                      ;cria alias de code0
        int 31h
        mov SelData0,ax                 ;salva data0

;-------------------------------------------------------------Stack32

;--------------------------------------------------------------------
        Movzx Edx,SelCode32
        Mov Ebx, offset Start32
        push edx
        push ebx
        push SelData32
        push ds
        pop fs ds
        db 66h                          ;32 bit retf
        retf
        quit 0

;-------------------------------------------------------------------
;-------------------------------------------------------------------

QuitError:
        Mov ax,stack16Seg
        mov bx,RmStackSize
        Mov RmRegs._dx,dx
        Mov RmRegs._ss,ax
        mov rmregs._sp,bx
        mov rmRegs._bp,0
        lahf
        shr ax,8
        mov rmRegs._flags,ax
        mov cx,text16
        mov dx,offset @QuitMsgErro
        mov rmregs._Cs,cx
        mov rmregs._ds,cx
        mov rmregs._es,cx
        mov rmregs._ip,dx
        mov ax,0301h
        xor bx,bx
        xor cx,cx
        push ds
        pop es
        mov edi,offset rmregs
        int 31h

        quit 10

 @QuitMsgErro:
        Mov ah,09h
        int 21h

        Quit 4
;--------------------------------------------------------------------

End Start
