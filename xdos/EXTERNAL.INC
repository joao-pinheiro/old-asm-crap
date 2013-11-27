;------------------------------------------- CODE16
CODE16 Segment para public use16

Extrn SDATa16:Word
Extrn Scode16:Word
Extrn Scode32:word
Extrn Sdata32:Word
Extrn Sdata0:word
Extrn Sincval:Word
Extrn PSP_sel:Word
Extrn RSS:word
extrn RM_INT10:FAR

CODE16 ENDS

;------------------------------------------- CODE32
Code32 segment para public use32

Extrn Errorflag:Dword
Extrn REGS:Registers
Extrn RES:Word
Extrn RDS:Word
Extrn RCS:WORD


Extrn Sys_seterrorHandler:Near
Extrn Alloc_Descriptor:Near
Extrn Free_Descriptor:Near
Extrn Set_base:Near
Extrn Set_limit:Near
Extrn Set_AccessRights:Near
Extrn GETMEMINFO:Near
Extrn SEG2Descriptor:Near
Extrn GET_BASe:Near
Extrn CreaTe_Alias:Near
Extrn Get_descriptor:Near
Extrn Set_descriptor:Near
Extrn ALLOC_DOSMEM:Near
Extrn Dalloc_DOSMEM:Near
Extrn Resize_DOSMEM:Near
Extrn GET_RMIntvec:Near
Extrn SET_RMintvec:Near
Extrn GET_Except_handler:Near
Extrn SET_Except_handler:Near
Extrn GetintVEC:Near
Extrn SETINTVEC:Near
Extrn GetVerSion:Near
Extrn Getmem:Near
Extrn Freemem:Near
Extrn RESIZE:Near
Extrn DOLock:Near
Extrn UnLock:Near
Extrn MARK_RMPage:Near
Extrn RELOCKRM:Near
Extrn GETPAGESIZE:Near
Extrn Disable_VIS:Near
Extrn Enable_VIS:Near
Extrn Get_VIS:Near
Extrn RMINT:Near
Extrn ErrorPrint:Near
Extrn Quit:Near
Extrn MEMMove:Near
EXTRN Call_RM_Far:Near

EXTRN STRINGLEN:NEAR
EXTRN COPYREAL:NEAR
EXTRN MOVE:NEAR
EXTRN DOSPRINT:NEAR

EXTRN RMBUF_SEG:WORD
EXTRN RMBUF_OFFSET:DWORD


extrn Free_RM_CallBack:Near
extrn ALLOC_RM_CallBACK:NEAR
extrn CallBack_STRUCt:REGISTERS
extrn MOVE:near
extrn DOS2PSTRING:near
extrn Pstring2DOS:near
extrn SET_Conv_info:Near
extrn INT2STR:near

Code32 ENDS

;------------------------------------------- STACK SEGMENT

STACK32 Segment para public use32

Stack32 ENDS



;------------------------------------------- FINISH




