;###########################################################
;#                                                         #
;#  Ficheiro: GETCPU.INC                                   #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################

;###########################################################
;--------------------------------------------------------------------------
;; CPUID - Valores de retorno em AX
;;	Type (bits 13-12), Family (bits 11-8), Model (bits 7-4), Stepping (bits 3-0)
;;	T = 00, F = 0101, M = 0001, for Pentium Processors (60, 66 MHz)
;;	T = 00, F = 0101, M = 0010, for Pentium Processors (75, 90, 100, 120, 133, 150, 166, 200 MHz)
;;	T = 00, F = 0101, M = 0100, for Pentium Processors with MMX technology
;;
;;	T = 00, F = 0110, M = 0001, for Pentium Pro Processor
;;	T = 00, F = 0110, M = 0011, for Pentium II Processor
;;	T = 00 for original OEM processor
;;	T = 01 for Intel OverDrive Processor
;;	T = 10 for dual processor
;;	T = 11 is reservedDWORD 

;###########################################################
;EQUATES PARA cpu_type

i_386	   	    equ 3
i_486	   	    equ 4
i_pentium  	    equ 5
i_p6	    	    equ 6


;###########################################################
;EQUATES PARA cpuid_type

cpu_oem 	    equ 0
cpu_overdrive	    equ 1
cpu_dual	    equ 2


;###########################################################
;EQUATES DE MÁSCARAS DE BIT PARA feature_flags


FPU_ON_CHIP	 EQU 0001h	;The processor contains an FPU that supports the 
				;Intel387 floating-point instruction set
VIRTUAL_MODE_EXT EQU 0002h	;The processor supports extensions to virtual-8086 mode
DEBUG_EXTENSION  EQU 0004h	;The processor supports I/O breakpoints, including the CR4.DE 
				;bit for enabling debug extensions and optional trapping 
				;of access to the DR4 and DR5 regs
PAGE_SIZE_EXT	 EQU 0008h	;The processor supports 4-Mbyte pages
TIME_STAMP	 EQU 0010h	;The RDTSC instruction is supported including the CR4.TSD bit
MSR		 EQU 0020h	;Model Specific Registers are implemented with the 
				;RDMSR, WRMSR instructions
PHYSICAL_ADR_EXT EQU 0040h	;Physical addresses greater than 32 bits are supported
EXCEPTION_18	 EQU 0080h	;Machine Check Exception, Exception 18, and
				;the CR4.MCE enable bit are supported
CX8		 EQU 0100h	;CMPXCHG8 Instruction Supported
LOCAL_APIC	 EQU 0200h	;The processor contains a local APIC
MTRRS		 EQU 1000h	;Memory Type Range Registers
PGE		 EQU 2000h	;The global bit in the PDEs and PTEs and 
				;the CR4.PGE enable bit are supported
MCA		 EQU 4000h	;The Machine Check Architecture is supported,specifically 
				;the MCG_CAP register
CMOVCC		 EQU 8000h	;Conditional Move Instruction Supported


;###########################################################

extrn Get_Cpu:Near
extrn Get_Fpu:Near

extrn fp_status:word
extrn vendor_id:dword
extrn cpu_type:word
extrn modell:word
extrn stepping:word
extrn cpuid_type:word
	
extrn id_flag:word 
extrn fpu_type:word
extrn feature_flags:dword
extrn mmx_flag:word

;###########################################################
