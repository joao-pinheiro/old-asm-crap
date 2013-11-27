
;----------------------------------------------- PROGRAM EQUATES

PROGSIZE = 50000	;Size of program in bytes
HEAPSIZE = 200000	;SIZE OF HEAP IN BYTES
STACKSIZE = 500		;Size of PM stack
RMSTACKSIZE = 200H	;Size of inital RM stack
IOCHECK	   = 1		;FUNCTION ERROR CHECK

;----------------------------------------------- CPU CONSTS

i86	= 0
i286	= 1
i386	= 2
i486	= 4
iP5	= 5
iP6	= 6

;----------------------------------------------- BOOLEAN EQUATES

TRUE  = 1
FALSE = 0
NIL   = 0

;------------------ MASKS for DPMI host flags

IS32_BIT	= 1 ;running under an 80386 (32-bit) implementation
GO_RM2INT	= 2 ;processor returns to real mode for reflected interrupts instead of V86 Mode
VIRTUAL_MEM	= 4 ;virtual memory supported

;------------------ MASKS for DPMI capabilities

PA_AVAIL	= 1 ;paged accessed/dirty supported
ER_AVAIL	= 2 ;exceptions restartability supported
DMAPPING_AVAIL	= 4 ;device mapping supported
CONVMAPPING	= 8 ;conventional memory mapping supported
ZEROFILL_AVAIL	= 10H ;Demand zero-fill supported
WPC_AVAIL	= 20H ;write-protect client capability supported
WPH_AVAIL	= 40H ;write-protect host capability supported


;----------------------------------------------- VIDEO VARS

MDA	= 0
CGA	= 1
MCGA	= 2
EGA	= 3
VGA	= 4

MONO	= 0
COR	= 1

;----------------------------------------------- TYPES

CHAR TYPEDEF BYTE
BOOL TYPEDEF BYTE
LONG TYPEDEF DWORD
INTEGER TYPEDEF WORD
UINT TYPEDEF WORD

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
          SELECTOR DD ?
        ENDS

DPMI_CAPABILITIES STRUC
	
	HOST_VERSION UINT ?
	VENDOR_NAME  CHAR 126 DUP(?)	
	CAPABILITIES UINT ?
ENDS	

Tpoint Struc
	 X UINT ?
	 Y UINT ?
ENDS


TRECT STRUC
	A TPOINT <>
	B TPOINT <>
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


TSS_STRUC STRUC
	TSS_BACKLINK	LONG ?
	TSS_ESP0	LONG ?	
	TSS_SS0		LONG ?
	TSS_ESP1	LONG ?
	TSS_SS1		LONG ?
	TSS_ESP2	LONG ?
	TSS_SS2		LONG ?
	TSS_CR3		LONG ?
	TSS_EIP		LONG ?
	TSS_EFLAGS	LONG ?
	TSS_EAX		LONG ?
	TSS_ECX		LONG ?	
	TSS_EDX		LONG ?
	TSS_EBX		LONG ?
	TSS_ESP		LONG ?
	TSS_EBP		LONG ?
	TSS_ESI		LONG ?
	TSS_EDI		LONG ?
	TSS_ES		LONG ?
	TSS_CS		LONG ?
	TSS_SS		LONG ?
	TSS_DS		LONG ?
	TSS_FS		LONG ?
	TSS_GS		LONG ?
	TSS_LDT		LONG ?
	TSS_TRAP	UINT ?
ENDS

DPMI_MEM Struc
	 LARGEST_BLOCK  DD ?	;largest available block in bytes
	 MAX_UNL_PAGES	DD ?	;maximum unlocked page allocation
	 MAX_LCK_PAGES	DD ?	;maximum locked page allocation
	 TOTAL_LINADX	DD ?	;total linear address space in pages
	 TOTAL_UNL_PAGES DD ?	;total unlocked pages
	 FREE_PAGES      DD ?	;free pages
	 TOTAL_FIS_PAGES DD ?	;total physical pages
	 FREE_LINADX	DD ?	;free linear address space in pages
	 PAG_FILE_SZ	DD ?	;size of paging file/partition in pages
	 PAD  		DD 3 dup(?)  ;reserved
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

;----------------------------------------------------------------- MACROS

CALLBACK_PROC MACRO NOME

	NOME PROC FAR
	USES EDI,ES
	
      ENDM

USTRING MACRO
	 DB 255 DUP(0)
	 ENDM
	 

LPTR MACRO S,O,P
	MOV S,WORD PTR P.SELECTOR[0]
	MOV O,P.OFSET
	ENDM
	 
	 