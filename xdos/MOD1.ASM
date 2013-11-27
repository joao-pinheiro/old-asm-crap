.386
.MODEL Large,pascal

INCLUDE DPMI.INC
INCLUDE EXTERNAL.INC


SEG_Start

ORG 0
ALIGN 4

;==================================================== DATA32


; STRUC DEFINITION

;------ Procedure list node -------------------


PPROCNODE Struc
           PHandle   DW ?
           LINK      Dd ?
           NEXT      OBJPTR <>
ENDS

;-------------- Public Data ------------------


ProcnodeLIST OBJPTR <0>

;==================================================== DECLARES

DeclareMethod LinksList_init
DECLAREMETHOD ADDNODE
DECLAREMETHOD RemoveNODE
DeclareMethod CallLinks
DeclareMethod DisposeLinksList

GLOBAL Procnodelist:OBJPTR

;==================================================== CODE32

;-----------------------------------------
;procedure LinksList_init;assembler;
;-----------------------------------------

LINKSLIST_init Proc far

        Uses EAX
        Mov eax,0
        mov word ptr procnodelist.selector,ax
        mov dword ptr procnodelist.ofset,eax
        mov dword ptr procnodelist.Ohandle,eax
        ret
Endp




;----------------------------------------------------------
;Function AddNode(Link:dWord):word;Assembler;
;----------------------------------------------------------

ADDNODE PRoc FAR
        Arg @L:dWord

        USES EDI,ESI,EAX,EBX,ECX,EDX,DS,ES

        Mov EAX,Size Pprocnode
        Call Getmem pascal,EAX
        Mov EDI,EAX
        MOV EBX,EDX

        Mov eax,0
        Mov dword ptr ES:[EDI.pprocnode.next.ofset],eax
        mov word ptr es:[EDI.pprocnode.next.selector],ax
        Mov dword ptr es:[edi.pprocnode.next.ohandle],eax
        MOV eax,@l
        Mov dword ptr ES:[EDI.pprocnode.link],EAX

        Mov DX,word ptr procnodelist.selector
        MOV EAX,dword ptr procnodelist.ofset
        cmp dx,ax
        jne @ADD_Seek0
        cmp eax,0
        jne @ADD_Seek0

        Mov Word ptr procnodelist.selector,es
        Mov dword ptr procnodelist.ofset,EDI
        Mov dword ptr procnodelist.ohandle,EBX
        mov ax,1
        Mov word ptr ES:[EDI.pprocnode.Phandle],ax
        jmp @ADDNode_quit

@ADD_SeeK0:
         Mov DS,dx
         mov ESI,EAX
         MOV EAX, dword ptr DS:[Esi.pprocnode.next.ofset]
         MOV DX,word ptr DS:[ESI.pprocnode.next.selector]
         cmp ax,dx
         jne @ADD_Go1
         cmp eax,0
         jne @ADD_Go1

         Mov word ptr DS:[ESI.pprocnode.next.selector],es
         Mov dword ptr DS:[ESI.pprocnode.Next.ofset],EDI
         Mov dword ptr DS:[ESI.pprocnode.next.ohandle],EBX
         Mov ax, word ptr DS:[ESI.pprocnode.phandle]
         inc ax
         Mov word ptr ES:[EDi.pprocnode.phandle],ax
         jmp @ADDnode_quit

@ADD_Go1:
         Mov Ds,dx
         mov ESI,EAX
         jmp @ADD_SEEK0

@ADDNODE_Quit:
          ret
Endp

;---------------------------------------------------
;Procedure Freenode(N:Procnode);
;---------------------------------------------------

FreeNode Proc Far
         ARG @N:OBJPTR

         USES ES,DS,eax
         MOV ES,SData0
         MOV DS,SDATA32

         MOv EAX,@N.ohandle
         Call Freemem pascal,eax
         ret
Endp

;---------------------------------------------------
;Procedure RemoveNode(H:word);assembler;
;---------------------------------------------------

RemoVenode Proc far
           Arg @H:word

           USES EDI,ESI,EAX,EBX,ECX,EDX,DS,ES

           Mov Eax,dword ptr procnodelist.ofset
           Mov Dx,word ptr procnodelist.selector
           cmp dx,ax
           jne @Removenode_Go1
           cmp eax,0
           je @Removenode_quit

@Removenode_go1:
           Mov ES,dx
           MOV EDI,EAX
           Mov bx,@H
           CMP Word ptr ES:[EDI.pprocnode.phandle],bx
           jne @Removenode_go2
           MOv EAX, dword ptr ES:[EDI.pprocnode.next.ofset]
           MOV dx, word ptr ES:[EDI.pprocnode.next.selector]
           Mov ECX,dword ptr ES:[EDI.pprocnode.next.Ohandle]

           MOv word ptr procnodelist.selector,dx
           Mov EDX,dword ptr procnodelist.Ohandle
           MOv dword ptr procnodelist.Ofset,EAX
           MOv dword ptr procnodelist.Ohandle,ECx

           Call Freenode Pascal,ES,EDI,EDX
           jmp @reMoveNode_quit

@RemoveNode_go2:
           push es
           pop ds
           MOV ESI,EDI

@REmoveNode_L1:
           Cmp bx,word ptr ES:[EDI.pprocnode.phandle]
           jne @Removenode_go3

           Mov EAX,dword ptr ES:[EDI.pprocnode.next.ofset]
           Mov dx,word ptr ES:[EDI.pprocnode.next.selector]
           Mov ECX,dword ptr ES:[EDI.pprocnode.next.Ohandle]

           Mov dword ptr DS:[ESI.pprocnode.next.ofset],EAX
           Mov word ptr DS:[Esi.pprocnode.next.selector],dx
           Mov EDX,dword ptr DS:[ESi.pprocnode.next.ohandle]
           Mov dword ptr DS:[ESI.pprocnode.next.ohandle],ECX

           Call FreeNode Pascal,ES,eDI,EDX
           jmp @Removenode_Quit

@Removenode_Go3:
           Push ES
           pop ds
           mov esi,edi
           MOV dx,word ptr DS:[ESi.pprocnode.next.selector]
           MOV EAX,dword ptr DS:[ESI.pprocnode.next.ofset]

           Cmp dx,ax
           jne @Removenode_go4
           cmp eax,0
           je @Removenode_quit

@removenode_go4:
           Mov Es,dx
           mOv edi,eax
           jmp @Removenode_l1

@Removenode_quit:
            ret
Endp

;---------------------------------------------------------
;Procedure CallLinks;assembler;
;---------------------------------------------------------

CallLinks Proc far

         Uses EDI,ESI,EAX,EBX,ECX,EDX,ES,DS

         MOv dx,word ptr procnodelist.selector
         mov EAX,dword ptr procnodelist.ofset
         cmp dx,aX
         jne @CallLinks_go0
         cmp eax,0
         je @CalLinks_quit

@CallLinks_go0:
         mov es,dx
         Mov EDI,EAX

@CallInks_Go1:

         PUSH ES EDI
         MOV ebx,dword ptr eS:[EdI.pprocnode.link]
         MOV ES,SDATA0
         Call EBX

         POP EDI ES

         Mov DX,word ptr ES:[EDI.pprocnode.next.Selector]
         MOV EAX,dword ptr ES:[EDI.pprocnode.next.Ofset]
         Cmp dx,ax
         jne @Callinks_go2
         Cmp EAX,0
         je @Callinks_quit

@Callinks_Go2:
         Mov es,dx
         mov Edi,Eax
         jmp @Callinks_Go1

@Callinks_Quit:
         ret
Endp

;-----------------------------------------------------
;Procedure DisposeLinksList;assembler;
;-----------------------------------------------------

Disposelinkslist Proc far

         Uses ESI,EDI,EAX,EBX,ECX,EDX,ES,DS

         Mov dx,word ptr procnodelist.selector
         MOV EAX,dword ptr procnodelist.Ofset
         MOV ECX,dword ptr procnodelist.Ohandle

         cmp dx,ax
         jne @Dispose_go1
         cmp eax,0
         je @Dispose_quit

@Dispose_go1:
         Mov ES,dx
         mov EDI,EAX
         Mov DX,Word ptr ES:[EDI.pprocnode.next.selector]
         MOV EAX,dWord ptr ES:[EDI.pprocnode.next.OFset]
         MOV EBX,dWord ptr ES:[EDI.pprocnode.next.ohandle]

         PUSH ES DS
         Call Freemem pascal,ECX
         POP DS ES
         Mov ECX,EBX

         Cmp ax,dx
         jne @Dispose_go1
         cmp eax,0
         jne @Dispose_go1

@Dispose_finish:
         Mov Eax,0
         Mov word ptr procnodelist.selector,ax
         mov dword ptr procnodelist.ofset,eax
         mov dword ptr procnodelist.ohandle,eax

@Dispose_quit:
         ret
Endp




SEG_END
END