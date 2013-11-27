;=============================================================================
; MACROS


INTR MACRO INTNO
	CALL RM_INT PASCAL,INTNO
     ENDM	

SND MACRO
        MOV AL,33H
        OUT 61H,AL
        MOV ECX,800000H
W_LOOP:
        LOOPD W_LOOP
        MOV AL,032H
        OUT 61H,AL
        MOV ECX,800000H
W_LOOP1:
        LOOP W_LOOP1    
        ENDM

;=============================================================================

CODESTART MACRO

        ASSUME CS:_SEGMENT32, DS:_SEGMENT32, SS:_SSEGMENT32
        ASSUME ES:NOTHING   , FS:_SEGMENT16, GS:NOTHING

        _SEGMENT32 SEGMENT PARA PUBLIC 'SEGMENT32' USE32

            ALIGN 8
      ENDM

CODEENDS MACRO
      _SEGMENT32 ENDS
      ENDM

;=============================================================================

MAINSTART MACRO
      
      PUBLIC _MAIN
      _MAIN PROC NEAR

     ENDM

MAINENDS MACRO
      RET
      ENDP
     ENDM	

;=============================================================================
