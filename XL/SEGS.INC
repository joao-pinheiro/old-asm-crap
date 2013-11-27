;-------------------------------------------------
; Macros dos segmentos
;-------------------------------------------------

SEGMENT16 SEGMENT PARA PUBLIC USE16
SEGMENT16 ENDS

SEGMENT32 SEGMENT PARA PUBLIC USE32
SEGMENT32 ENDS

STACK32 SEGMENT PARA PUBLIC USE32
STACK32 ENDS

;-------------------------------------------------
_TEXT16 MACRO

	ASSUME CS:SEGMENT16, DS:SEGMENT16
	
	SEGMENT16 SEGMENT PARA PUBLIC USE16
	ALIGN 4
	ENDM
	
_TEXT16_END MACRO
	SEGMENT16 ENDS
	ENDM

;-------------------------------------------------

_TEXT32 MACRO

	ASSUME CS:SEGMENT32, DS:SEGMENT32, ES:NOTHING
	ASSUME FS:SEGMENT16, GS:NOTHING,   SS:STACK32

	SEGMENT32 SEGMENT PARA PUBLIC USE32
	ALIGN 4
	ENDM

	
_TEXT32_END MACRO
	SEGMENT32 ENDS
	ENDM

;-------------------------------------------------

_STACK32 MACRO
	STACK32 SEGMENT PARA PUBLIC USE32
	ALIGN 4
	ENDM
	
_STACK32_END MACRO
	STACK32 ENDS
	ENDM

;-------------------------------------------------
