;###########################################################
;#                                                         #
;#  Ficheiro: GETCPU.ASM                                   #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################

Model large, pascal
.386p

include globals.inc

;###########################################################
; Equates						  ||
;===========================================================


FAMILY_MASK     equ     0f00h
FAMILY_SHIFT    equ     8
MODEL_MASK      equ     0f0h
MODEL_SHIFT     equ     4
STEPPING_MASK   equ     0fh
FPU_FLAG        equ     1h
MCE_FLAG        equ     80h
CMPXCHG8B_FLAG  equ     100h



;###########################################################
; Código						  ||
;===========================================================

CODE16START

public fp_status, vendor_id, cpu_type, modell, stepping, cpuid_type
public id_flag, fpu_type, feature_flags, mmx_flag

fp_status       DW 0 
vendor_id       DD 0,0,0 	;ID do Fabricante: "GenuineIntel"
cpu_type        DW 0		;Tipo de cpu
modell          DW 0		;Modelo
stepping        DW 0
cpuid_type      DW 0            ;Tipo de processador retornado por CPUID
				;ver tabela de equates para + info

id_flag         DW 0		;TRUE se suporta CPUID
fpu_type        DW 0
feature_flags   DD 0
mmx_flag	DW 0		;True se MMX


;=========================================================================
;| Rotina de detecção de CPU  						 |
;| (c) Intel Corporation 						 |
;| 						 			 |
;| cpu_type = 0 : 8086 						 	 |
;|          = 2 : 80286 						 |
;|          = 3 : 80386 						 |
;|          = 4 : 80486 						 |
;=========================================================================

public Get_Cpu

Get_Cpu proc near

	check_8086:
		pushf                   ; push original FLAGS
		pop     ax              ; get original FLAGS
		mov     cx, ax          ; save original FLAGS
		and     ax, 0fffh       ; clear bits 12-15 in FLAGS
		push    ax              ; save new FLAGS value on stack
		popf                    ; replace current FLAGS value
		pushf                   ; get new FLAGS
		pop     ax              ; store new FLAGS in AX
		and     ax, 0f000h      ; if bits 12-15 are set, then CPU
		cmp     ax, 0f000h      ;   is an 8086/8088
		mov     cpu_type, 0     ; turn on 8086/8088 flag
		jne     check_80286     ; jump if CPU is not 8086/8088
		jmp     end_get_cpuid

	check_80286:
		or      cx, 0f000h      ; try to set bits 12-15
		push    cx              ; save new FLAGS value on stack
		popf                    ; replace current FLAGS value
		pushf                   ; get new FLAGS
		pop     ax              ; store new FLAGS in AX
		and     ax, 0f000h      ; if bits 12-15 clear, CPU=80286
		mov     cpu_type, 2     ; turn on 80286 flag
		jnz     check_80386     ; if no bits set, CPU is 80286
		jmp     end_get_cpuid

	check_80386:
	          mov bx, sp          ; save current stack pointer to align
	          and sp,  0FFFCh     ; align stack to avoid ac fault
	          pushfd              ; push original eflags
	          pop eax             ; get original eflags 
	          mov ecx, eax        ; save original eflags
	          xor eax, 40000h     ; flip ac bit in eflags
	          push eax
	          popfd               ; replace current eflags value
	          pushfd              ; get new eflags
	          pop eax             ; store new eflags in eax
	          xor eax, ecx        ; can't toggle ac bit, cpu=80386
	          mov cpu_type, 3     ; turn on 80386 cpu flag
	          mov sp, bx          ; restore original stack pointer
	          jz end_get_cpuid    ; jump if 80386 cpu
	          and sp, 0FFFCh      ; align stack to avoid ac fault
	          push ecx
	          popfd               ; restore ac bit in eflags first
	          mov sp, bx          ; restore original stack pointer

		          ;       intel486 dx cpu, intel487 sx ndp, and intel486 sx cpu check
		          ;       checking for ability to set/clear id flag (bit 21) in eflags
		          ;       which indicates the presence of a processor
		          ;       with the ability to use the cpuid instruction.

		  .586	         
		  mov cpu_type, 4     	; turn on 80486 cpu flag
	          mov eax, ecx        		; get original eflags
	          xor eax, 200000h    		; flip id bit in eflags
	          push eax            		; save new eflags value on stack
	          popfd               		; replace current eflags value
	          pushfd              		; get new eflags
	          pop eax             		; store new eflags in eax
	          xor eax,ecx         		; can't toggle id bit,
	          je end_get_cpuid    		;   cpu=80486

		          ;       execute cpuid instruction to determine vendor, family,
		          ;       model and stepping.
        
  
	          mov id_flag,true    		; set flag indicating use of cpuid inst.
	          xor eax,eax         		; set up input for cpuid instruction
	          cpuid          
	          mov [vendor_id],ebx  		; setup to test for vendor id
	          mov [vendor_id+4],edx
	          mov [vendor_id+8],ecx


	          cmp eax, 1          		; make sure 1 is a valid input value for cpuid
	          jl end_get_cpuid    		; if not, jump to end
	          xor eax,eax         		; otherwise, use as input to cpuid
	          inc eax             		; and get stepping, model and family
	          cpuid
		  push ax			; get cpu type (bits 13-12)
		  shr ax,12
		  and ax,03h
	          mov cpuid_type,ax			
		  pop ax
	          mov stepping, ax
	          and stepping, STEPPING_MASK 	; isolate stepping info
	          and al, MODEL_MASK        	; isolate model info
	          shr al, 4
	          mov modell, ax
		  and modell,00FFh
	          and ax, FAMILY_MASK        	; mask everything but family
	          shr ax,8
	          mov cpu_type, ax    		; set cpu_type with family
	          mov feature_flags,edx      	; save feature flag data

	          test edx,00800000h  		; if ia mmx technology, bit 23 in feature flag
	          jz end_get_cpuid
	          mov mmx_flag,true

	end_get_cpuid:
		  ret
endp



;=========================================================================
;| Rotina de detecção do FPU  						 |
;| (c) Intel Corporation 						 |
;| 						 			 |
;| fpu_type = 0 : FPU não existente					 |
;|     	    = 1 : FPU presente						 |
;|          = 2 : 287 Presente (apenas se 386) 				 |
;|          = 3 : 387 Presente (apenas se 386) 				 |
;=========================================================================


Public Get_Fpu

Get_Fpu proc near

          fninit                  ; reset FP status word
          mov fp_status, 5a5ah    ; initialize temp word to non-zero value
          fnstsw fp_status        ; save FP status word
          mov ax,fp_status        ; check FP status word
          cmp al,0                ; see if correct status with written
          mov fpu_type,0          ; no fpu present
          jne end_get_fpuid

   check_control_word:
          fnstcw  fp_status       ; save FP control word
          mov ax,fp_status        ; check FP control word
          and ax,103fh            ; see if selected parts looks OK
          cmp ax,3fh              ; check that 1's & 0's correctly read
          mov fpu_type,0
          jne end_get_fpuid
          mov fpu_type, 1

          ;
          ;   80287/80387 check for the Intel386 CPU
          ;
   
   check_infinity:
         cmp cpu_type, 3
         jne end_get_fpuid
         fld1                    ; must use default control from FNINIT
         fldz                    ; form infinity
         fdiv                    ; 8087 and Intel287 NDP say +inf = -inf
         fld st                  ; form negative infinity
         fchs                    ; Intel387 NDP says +inf <> -inf
         fcompp                  ; see if they are the same and remove them
         fstsw fp_status         ; look at status from FCOMPP
         mov ax, fp_status
         mov fpu_type, 2         ; store Intel287 NDP for fpu type
         sahf                    ; see if infinities matched
         jz end_get_fpuid        ; jump if 8087 or Intel287 is present
         mov fpu_type, 3         ; store Intel387 NDP for fpu type

   end_get_fpuid:
         ret
endp


CODE16ENDS
END
;###########################################################
