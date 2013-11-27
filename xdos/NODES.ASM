.386p
.model large,pascal


INCLUDE DPMI.INC
INCLUDE EXTERNAL.inc
INCLUDE NODES.ASO

ASSUME DS:CODE32, CS:CODE32, ES:NOTHING, FS:CODE16, SS:STACK32


;=========================================================================
;=========================================================================

CODE32 Segment Para Public use32

org 0
ALIGN 4

;=========================================================================

;BASENODE STRUC

;          NEXT   OBJPTR <>
;          INDEX  LONGINT ?

;         ENDS

;=========================================================================
DECLARE NODE_GETNEXT
DECLARE NODE_SETNEXT
DECLARE NODE_INSERT
DECLARE NODE_SETIDX
DECLARE NODE_GETIDX
DECLARE NODE_CLEAR
DECLARE NODE_REMOVENEXT
DECLARE NODE_SEEK
;=========================================================================
;----------------------------------------------------------------
;Function Node_getNEXT(OFSET:DWORD):Objptr (EDX:EBX) EDX-OFSET
;----------------------------------------------------------------

Node_GETNEXT Proc Near
             ARG @O:DWORD

             USES EBX,ES
             MOV ES,SDATA0
             MOV Ebx,@O
             Mov EDX,ES:[EBX][BASEnode.Next.ofset]
             MOV EBX,ES:[EBX][BASEnode.next.handle]
             ret

ENDP

;----------------------------------------------------------------
;Procedure Node_SETNEXT(OFSET:DWORD;Next:OBJPTR);
;----------------------------------------------------------------

NODE_SetNext Proc Near
             ARG @O:DWORD, @N:OBJPTR

             USES EDI,EAX,ES
             MOV ES,SDATA0
             MOV EDI,@O
             MOV EAX,@N.ofset
             MOV ES:[EDI][BASENODE.NEXT.OFSET],eax
             MOV EAX,@N.Handle
             MOV ES:[EDI][BASEnode.NEXT.handle],EAX
             RET
ENDP

;----------------------------------------------------------------
;Procedure Node_SETIDX(OFSET:DWORD;IDX:DWORD);
;----------------------------------------------------------------

NODE_SetIDX PROC NEAR
            ARG @O:DWORD,@I:DWORD

            USES EDI,EAX,ES
            MOV ES,SDATA0
            MOV EDI,@O
            MOV EAX,@I
            MOV ES:[EDI][BASEnode.INDEX],EAX
            ret
ENDP

;----------------------------------------------------------------
;Function Node_GETIDX(OFSET:DWORD):Longint(EDX);
;----------------------------------------------------------------

NODE_GetIDX PROC NEAR
            ARG @O:DWORD

            USES EDI,ES
            MOV ES,SDATA0
            MOV EDI,@O
            MOV EDX,ES:[EDI][BASEnode.INDEX]
            ret
ENDP



;----------------------------------------------------------------
;Procedure Node_INSERT(OFSET:DWORD;NEXT:OBJPTR);
;----------------------------------------------------------------

NODe_Insert Proc Near
            ARG @O:DWORD, @N:objptr

            USES EDI,EAX,EBX,ECX,EDX,ES
            MOV ES,sdata0
            Mov edi,@O
            MOv EAX,ES:[EDI][BASENODE.NEXT.OFSET]
            MOV EBX,ES:[EDI][BASENODE.NEXT.HANDLE]
            MOV ECX,@N.OFSET
            MOV EDX,@N.HANDLE
            MOv ES:[EDI][BASENODE.NEXT.OFSET],ecx
            MOV ES:[EDI][BASENODE.NEXT.HANDLE],EDX
            MOV EDI,ECX
            MOv ES:[EDI][BASENODE.NEXT.OFSET],eax
            MOV ES:[EDI][BASENODE.NEXT.HANDLE],EbX
            RET
ENDP


;----------------------------------------------------------------
;Procedure Node_Clear(OFSET:DWORD);
;----------------------------------------------------------------

NODE_clear Proc NEAR
           ARG @O:DWORD

           USES EDI,EAX,ES
           MOV ES,SDATA0
           MOV EDI,@O
           XOR EAX,EAX
           MOV ES:[EDI][BASENODE.NEXT.OFSET],EAX
           MOV ES:[EDI][BASENODE.NEXT.handle],EAX
           RET
ENDp


;-----------------------------------------------------------------
;FUNCTION NODE_REMOVENEXT:OBJPTR;
;-----------------------------------------------------------------

Node_removeNext Proc near
                ARG @O:DWORD

                CALL NODE_GETNEXT Pascal,@O
                PUSH EBX EDX
                CMP EDX,0
                je @REMOVE_1

                CALL NODE_Getnext Pascal,EDX
      @Remove_1:
                Call Node_Setnext Pascal,@O,EDX,EBX
                POP EDX EBX
                RET
Endp


;---------------------------------------------------
;Function Node_SEEK(@O:OBJPTR;I:DWORD):OBJPTR;
;---------------------------------------------------

NODE_Seek Proc NEAR
           ARG @O:OBJPTR, @I:Dword

           USES ES
           MOV EAX,@O.OFSET
           MOV EBX,@O.HANDLE

   @SEEK_L1:
            Call NODE_GetIDX Pascal,EAX
            Cmp EDX,@I

            je @SEEK_FOUND
            CALL NODE_GETNEXT Pascal,EAX
            CMP EDX,0
            je @SEEK_quit
            Mov eax,edx
            JMP @SEEK_l1

   @SEEK_found:
            Mov eDX,EAX

   @SEEK_quit:
              ret
ENDP




CODE32 ENDS
END