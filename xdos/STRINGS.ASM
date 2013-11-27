.386p
Model large,pascal

Global STRLEN:Near
Global STREND:NEAR
GLOBAL STRMOVE:NEAR

INCLUDE DPMI.INC
Include External.inc

ASSUME DS:DATa32, CS:CODE32

CODe32 Segment Para public use32
ORG 0
Align 4
;----------------------------------------------------------------
; Retorna len de uma string. AX - Len


STRLEN     PROC Near
        ARG @S:Pointer

        Uses cx,es,edi
        MOV es, @S.selector
        MOV EDI, @S.ofset
        Mov cx,0ffffh
        xor al,al
        cld
        Repne scasb
        mov ax,0fffeh
        sub ax,cx
        ret
endp


;----------------------------------------------------------------
; Retorna offset do Null char. AX - OFS


STRENd   PROC Near
        ARG @S:Pointer

        Uses cx,es,edi
        MOV es, @S.selector
        MOV EDI, @S.ofset
        Mov cx,0ffffh
        xor al,al
        cld
        Repne scasb
        mov ax,di
	dec ax
        ret
endp


;----------------------------------------------------------------
;COPY count chars from SRC to dest


STRmove PROC Near
        ARG @S:Pointer,@D:pointer,@CNT:Word

        Uses ax,cx,es,edi,DS,ESI
        MOV es, @d.selector
        MOV EDI, @d.ofset
	Mov DS,@s.Selector
	Mov ESI,@S.ofset
        Mov cx,@Cnt
        cld
        Rep Movsb
        ret
endp


;----------------------------------------------------------------
;COPY STR From Source do Dest


STRcopy PROC Near
        ARG @S:Pointer,@D:pointer

        Uses ax,cx,es,edi,DS,ESI
	MOV ES,@S.Selector
	Mov EDI,@S.Ofset
	Mov cx,0FFFFh
	Xor ax,ax
	cld
	repne scasb
	not CX
        MOV es, @d.selector
        MOV EDI, @d.ofset
	Mov DS,@s.Selector
	Mov ESI,@S.ofset
        Rep Movsb
        ret
endp

;----------------------------------------------------------------
;ADDS SOURCe string at End of dest


STRcat PROC Near
        ARG @D:Pointer,@S:pointer

        Uses ax,cx,es,edi,DS,ESI
        ret
endp


COde32 Ends

end