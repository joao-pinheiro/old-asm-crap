
;------------------------------------------------------------------------------
TRUE  = 1
FALSE = 0

CPUMIN = 5	    ;CPU REQ'D PENTIUM
PROGRAMSIZE = 4096  ; TOTAL SIZE OF PROG, IN PARAGR
STACKSIZE   = 4096  ;SIZE OF STACK, IN PARAGR
HEAPSIZE    = 3048  ;SIZE OF HEAP, IN K	
DMABUFSZ    = 8192  ;SIZE OF DMA BUFFER, IN PARAGR

INT31CHECK  = TRUE  ;TURNS ON OR OFF INT 31H FAIL CHECK 
;------------------------------------------------------------------------------

MDA     = 0
CGA     = 1
MCGA    = 2
EGA     = 3
VGA     = 4

MONO    = 0
COR     = 1

;------------------------------------------------------------------------------

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

DPMI_MEM Struc
         LARGEST_BLOCK  DD ?    ;largest available block in bytes
         MAX_UNL_PAGES  DD ?    ;maximum unlocked page allocation
         MAX_LCK_PAGES  DD ?    ;maximum locked page allocation
         TOTAL_LINADX   DD ?    ;total linear address space in pages
         TOTAL_UNL_PAGES DD ?   ;total unlocked pages
         FREE_PAGES      DD ?   ;free pages
         TOTAL_FIS_PAGES DD ?   ;total physical pages
         FREE_LINADX    DD ?    ;free linear address space in pages
         PAG_FILE_SZ    DD ?    ;size of paging file/partition in pages
         PAD            DD 3 dup(?)  ;reserved
ENDS




EXCEP_SF32 struc  ;exception Stack Frame 32 bit
   RET_EIP DD ?         ;return EIP (do not change)
   RET_CS  DW ?         ;return CS selector (do not change)
   PAD_1   DW ?         ;reserved (do not change)
   E_Code  DD ?         ;error code
   EXCP_EIP DD ?        ;EIP of exception
   EXCP_CS  DW ?        ;CS selector of exception
   PAD_2    DW ?        ;reserved (do not change)
   XEFLAGS  DD ?        ;Eflags
   SAVE_ESP DD ?
   Curr_SS  DW ?
   PAD_3    DW ?        ;reserved (do not change)
Ends

LPTR MACRO S,O,P
        MOV S,WORD PTR P.SELECTOR[0]
        MOV O,P.OFSET
        ENDM
         
;=============================================================================

_SEGMENT16 SEGMENT PARA PUBLIC 'SEGMENT16' USE16
_SEGMENT16 ENDS

_SSEGMENT32 SEGMENT PARA PUBLIC 'SSEGMENT32' USE32
_SSEGMENT32 ENDS

