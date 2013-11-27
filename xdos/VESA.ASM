.386p
.model large,pascal

Include DPMi.inc
INCLUDE EXTERNal.INC


CODE32 segment para public use32

ORG 0
Align 4

;===========================================================================

        ASSUME DS:CODE32, CS:code32,FS:CODE16,SS:STACK32

;=========³==================================================================

VesaModeInfo Struc
       Attributes    DW ?    ; Mode Attributes
       WinA          DB ?    ; Window A attributes
       WinB          DB ?    ; Window B attributes
       Granularity   DW ?    ; Window granularity in K bytes
       WindowSize    DW ?    ; Size of window in K bytes
       WINA_seg      DW ?    ; Segment address of window A
       WinB_seg      DW ?    ; Segment address of window B
       WinProc       DW ?,?  ; Windows positioning function
       BytesLine     DW ?    ; Number of bytes per line
       Xres          DW ?    ; Number of horizontal pixels
       Yres          DW ?    ; Number of vertical pixels
       CharW         Db ?    ; Width of character cell
       CharH         DB ?    ; Height of character cell
       Planes        DB ?    ; Number of memory planes
       BitsPixel     DB ?    ; Number of bits per pixel
       MemBanks      DB ?    ; Number of banks
       MemModel      DB ?    ; Memory model type
       Banksize      DB ?    ; Size of bank
       Pages         DB ?    ; Number of image pages
       PAD1          DB ?    ; The following are for 15,16,24,32 bit colour modes
       RedMaskSize   DB ?    ; Size of Red mask in bits
       RedFieldPos   DB ?    ; Bit position of LSB of Red mask
       GreenMaskSize DB ?    ; Size of Green mask in bits
       GreenFieldPos DB ?    ; Bit position of LSB of Green mask
       BlueMaskSize  DB ?    ; Size of Blue mask in bits
       BlueFieldPos  DB ?    ; Bit position of LSB of Blue mask
       RsvdMaskSize  DB ?    ; Size of Reserved mask in bits
       RsvdFieldPos  DB ?    ; Bit pos. of LSB of Reserved mask
       DirColModeInf DB ?    ; Direct Colour mode attributes
       PAD2          DB 216 dup(0)
ENDS

VesaDriverInfo Struc
                Signature     DB 4 dup(0)  ; Signature - "VESA"
                Vers          DW ?         ; VESA Version number
                OEMNAME       DW ?,?       ; Pointer to manufacturer name
                Capabilities  DD ?         ; Capabilities
                ModeList      DW ?,?       ; Pointer to list of VESA modes
                TotalMemory   DW ?         ; Number of 64k memory blocks on card
                Padder        DB 238 dup(0)
ENDS


;=========³==================================================================


Driver_Info      VesaDriverInfo  <>  ;Driver info struc
Curr_ModeInfo    VesaModeinfo    <>  ;Specific mode info

CurrMode         DW ?                ;Current Vesa mode
CurrBank         DB ?                ;Current vesa bank

VESA_FLAG        DW False       ;Vesa true/false flag
Vesa_error       DW False       ;Vesa Error flag

;=========³==================================================================

ErrorClear Macro
           mov ax,0
           mov Vesa_error,ax
           Endm

ErrorSet Macro E
         mov Vesa_error,E
         Endm


;----------------------------------------------------------
;Function  DEtectVesa:Boolean;
;----------------------------------------------------------

DetectVesa Proc Near

           Uses EBX
           MOv Ax,04F03h
           Intr 10h
           Cmp Al,04Fh
           jne @@No_vesa
           Mov Ax,true
           jmp @@Detect_quit

      @@No_vesa:
           Mov ax,false

      @@Detect_quit:
           Ret
Endp


;----------------------------------------------------------
;Procedure  GetVesaInfo;
;----------------------------------------------------------

GetVesaInfo Proc near

            Uses EAx,EDI
            Errorclear
            Mov Ax,04F00h
            Mov di,Code32
            Mov Res,Di
            Mov Di,offset Driver_info
            Intr 10h
            cmp ah,0
            je @@Vesainfo_quit
            Mov ax,false
            Errorset ax

       @@VesaInfo_quit:
            Ret
Endp


;--------------------------------------------------------------
;Procedure  GetVesaModeInfo(mode:Dword;Struc(S:Word;O:WORD);
;--------------------------------------------------------------

GetVesaModeInfo Proc near
                Arg @M:Dword, @S:Word, @O:Word

                Uses EAx,ECX,EDI
                Errorclear
                Mov ECX,@M
                Mov Ax,04F01h
                Mov di,@S
                Mov Res,Di
                Mov Di,@O
                Intr 10h
                cmp ah,0
                je @@VesaMinfo_quit

                Mov ax,false
                Errorset ax

       @@VesaMInfo_quit:
                Ret
Endp

;--------------------------------------------------------------
;Procedure SetVesaMode(Mode:dword);
;--------------------------------------------------------------

SetVesaMode Proc near
            Arg @M:Dword

            Uses EAx,EBX
            Errorclear
            MOv Ebx,@M
            Mov Ax,04F02h
            Intr 10h
            cmp ah,0
            je @@SetVesaM_quit

            Mov Eax,@M
            Mov Currmode,AX
            mov ax,offset Curr_modeinfo
            Call GetVesaModeInfo Pascal,@M,Code32,AX
            Mov ax,false
            Errorset ax

       @@SetVesaM_quit:
            Ret
Endp


;--------------------------------------------------------------
;Function SetVesaMode(Mode:dword):word;
;--------------------------------------------------------------

GetVesaMode Proc near

            Uses EBX
            Errorclear
            Mov Ax,04F03h
            Intr 10h
            cmp ah,0
            mov ax,bx
            je @@GetVesaM_quit
            Mov ax,false
            Errorset ax

       @@getVesaM_quit:
            Ret
Endp


;--------------------------------------------------------------
;Function GetVstateSize:word;
;--------------------------------------------------------------

GetVstateSize Proc near

            Uses EDX,CX
            Errorclear
            Mov dl,0
            mov cx,0fh
            Mov Ax,04F04h
            Intr 10h
            cmp ah,0
            mov ax,bx
            je @@GetVss_quit
            Mov ax,false
            Errorset ax

       @@getVss_quit:
            Ret
Endp


;--------------------------------------------------------------
;Procedure SaveVesaState(S:Word;O:Word);
;--------------------------------------------------------------

SaveVesaState Proc near
              Arg @S:Word,@O:Word

              USES EAX,EBX,EDX,ECX
              Errorclear
              Mov ax,04f04h
              Mov dl,1
              mov cx,@S
              Mov RES,CX
              mov cx,0fh
              Mov bx,@O
              Intr 10h
              cmp ah,0
              je @@SaveState_quit
              Mov ax,false
              Errorset ax

       @@Savestate_quit:
              Ret
Endp


;--------------------------------------------------------------
;Procedure RestoreVesaState(S:Word;O:Word);
;--------------------------------------------------------------

RestoreVesaState Proc near
              Arg @S:Word,@O:Word

              USES EAX,EBX,EDX,ECX
              Errorclear
              Mov ax,04f04h
              Mov dl,2
              mov cx,@S
              Mov RES,CX
              mov cx,0fh
              Mov bx,@O
              Intr 10h
              cmp ah,0
              je @@restoreState_quit
              Mov ax,false
              Errorset ax

       @@restorestate_quit:
              Ret
Endp


;-------------------------------------------------------
;Procedure SetBank(B:word);
;-------------------------------------------------------

Setbank Proc near
        Arg @B:word

        Uses EAx,EBX
        Mov ax,@B
        cmp ax,currbank
        je @Sb_quit

        MOv currbank,AX

    @SB_Quit:
        RET
Endp

   mov     CurBank, ax
   mov     ax, 4F05h
   xor     bx, bx
   mov     dx, bank
   call    p
 @end:
end;


Code32 ends
END
