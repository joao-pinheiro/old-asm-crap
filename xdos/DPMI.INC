;----------------------------------------------------------------------------
;Equates

True  = 1
False = 0

NIL = -1

ISERROR =1
NOERROR =0

IOCHECK = 1     ;I/O Runtime error 


;DPMI SERVER FLAG MASK

Is_32_bit   = 01h
NO_v86_ints = 02h
Virtual_mem = 04h


DosMem 	   = 256       ;Dos memory paragraphs to be allocated on startup
RMSTACK    = 48        ;Size of RM stack in paragraphs
PMSTACK    = 2000      ;Size of PM32 stack in bytes
PROGRAM_SZ = 10000     ;Tamanho do programa em par grafos
HEAPMIN    = 1000000   ;M¡nima mem¢ria livre

;----------------------------------------------------------------------------
;TYPEDEFS


LONGINT    TYPEDEF DWORD
SHORTINT   Typedef BYTE
CHAR       TYPEDEF BYTE
INTEGER    TYPEDEF WORD
BOOLEAN    Typedef Byte

;----------------------------------------------------------------------------
;Structures

RMPOINTER STruc
          ROFSET DW ?
          RSEG   DW ?
ENDS

TOFFSET Struc
           OFSET DD ?
         ENDS

THANDLER STRUC
          HANDLE DD ?
         ENDS

Pointer Struc
          TOFFSET <>
          SELECTOR DW ?,?
        ENDS


Descriptor struc
  Limit_15   dw ?
  Base_15    dw ?
  base_23    db ?
  AcessR     db ?
  Limit_19   db ?
  Base_31    db ?
Descriptor ends


DPMI_info Struc
  DFlags   dw ?
  Cpu      db ?
  ver      dw ?
  pic      dw ?
DPMI_info ends


Registers Struc
  _Di dw ?,?
  _Si DW ?,?
  _bp dw ?,?
      dd 0
  _bx dw ?,?
  _dx dw ?,?
  _cx dw ?,?    
  _ax dw ?,?
  Flags dw ?    
  _ES dw ?
  _ds dw ?
  _Fs dw ?    
  _gs dw ?
  _ip dw ?
  _cs dw ?
  _sp dw ?
  _ss dw ?
Registers ends  


DPMI_mem Struc
	 LB   DD ?	;largest available block in bytes
	 MUPA DD ?	;maximum unlocked page allocation
	 MLPA DD ?	;maximum locked page allocation
	 TLAS DD ?	;total linear address space in pages
	 TUP  DD ?	;total unlocked pages
	 FP   DD ?	;free pages
	 TFP  DD ?	;total physical pages
	 FLAS DD ?	;free linear address space in pages
	 SPF  DD ?	;size of paging file/partition in pages
	 PAD  DD 3 dup(?)  ;reserved
ENDS


Memory struc
  sz      dd ?
  ADDress dd ?
  THandler <>
Memory ends

OBJPTR Struc
        Toffset  <>
       ENDS

Dos_memory struc
  Paragr   dw ?
  segm     dw ?
  select   dw ?
dos_memory ends


EXCEP_SF32 struc  ;exception Stack Frame 32 bit
   RET_EIP DD ? 	;return EIP (do not change)
   RET_CS  DW ?		;return CS selector (do not change)
   PAD_1   DW ?         ;reserved (do not change)
   E_Code  DD ?		;error code
   EXCP_EIP DD ?	;EIP of exception
   EXCP_CS  DW ?	;CS selector of exception
   PAD_2    DW ?	;reserved (do not change)
   XEFLAGS  DD ?	;Eflags
   SAVE_ESP DD ?
   Curr_SS  DW ?
   PAD_3    DW ?	;reserved (do not change)
Ends


;-------------------------------------------------------------------
;Macros

;----------------------------------------CONTROLO

Declare Macro Procedure
	
	Global Procedure:Near

Endm

DeclareMethod Macro Procedure

	Global Procedure:FAR

Endm

Module Macro Procedure

	EXTRN Procedure:NEAR

EndM

;----------------------------------------CODIGO

INTR  Macro Intno			;Real mode Int
       CAll RMint pascal,Intno
Endm



OBJinit Macro Obj, objtype		;Aloca memoria para o objecto
	Push EAX EDX
	Mov EAX,size objtype
        ADD EAX,16
	CAll Getmem pascal,eax
	mov obj.handle,edx
	Mov obj.OFSET,eax
	POP EDX EAX
endm

OBJfree Macro Obj	;liberta memoria do objecto
	Push EAX
	Mov EAX,Obj.handle
	CAll freemem pascal,eax
        Xor EAX,EAX
        MOV obj.Ofset,eax
	POP EAX
endm


SEG_Start Macro
          CODE32 SEGMENT Public para Use32

          Assume CS:Code32,DS:Code32,FS:CODE16
          EndM


Seg_END Macro
         CODE32 ENDS
         EndM


VCLI MACRO
      PUSH EAX
      MOV AX,0900h
      INT 31h
      POP EAX
     ENDM

VSTI MACRO
      PUSH EAX
      MOV AX,0901h
      INT 31h
      POP EAX
     ENDM


SPTR MACRO V
      MOV v.ofset,EDX
      mOv v.handle,EBX
     EndM


LPTR MACRO v
      Mov EDX,v.ofset
      MOV EBX,v.handle
     ENDM


