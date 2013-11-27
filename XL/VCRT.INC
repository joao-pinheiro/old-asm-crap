;ีออ XDOS DOS EXTENDER อออออออออออออออออออออออ FILE: VCRT.INC อออออออออออออธ
;ณ                                                                         ณ
;ณ  VIDEO TEXT MODE ROUTINES - INCLUDE FILE                                ณ
;ณ                                                                         ณ
;ณ  Copyright (c) 1997  Joo Manuel Pinheiro                               ณ
;ณ  V.N.Gaia - Portugal                                                    ณ
;ณ                                                                         ณ
;ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

;===========================================================================
_TEXT32
;===========================================================================


SCREEN STRUC GLOBAL METHOD{

	T_INIT		:DWORD = SCRN_INIT
	T_GOTOXY	:DWORD = SCRN_GOTOXY
	T_SETRECT	:DWORD = SCRN_SETRECT
	T_SETCOLOR	:DWORD = SCRN_COLOR
	T_CLS		:DWORD = SCRN_CLS
	T_FILL		:DWORD = SCRN_FILL
	T_PRINTXY	:DWORD = SCRN_PRINTXY
	T_PRINT		:DWORD = SCRN_PRINT	
	T_BOX		:DWORD = SCRN_BOX
	T_SCROLLUP	:DWORD = SCRN_SCROLLUP
	T_SCROLLDOWN	:DWORD = SCRN_SCROLLDOWN
	T_GETCHAR	:DWORD = SCRN_GETCHAR
	T_PUTCHAR	:DWORD = SCRN_PUTCHAR	
	T_GETATR	:DWORD = SCRN_GETATR
	T_PUTATR	:DWORD = SCRN_PUTATR
	T_MUDA		:DWORD = SCRN_MUDA
	T_LIMPA		:DWORD = SCRN_LIMPA
	T_GETAREA	:DWORD = SCRN_GETAREA
	T_PUTAREA	:DWORD = SCRN_PUTAREA

	}

   WINDOW	TRECT <>	;COORDS DA JANELA
   AT   	TPOINT <>	;POSI€O DO CURSOR NA JANELA
   ATR		CHAR ?		;ATRIBUTO
		CHAR ?		;DUMMY - PARA ALINHAMENTO!!!
   WOFS		UINT ?		;OFFSET DA JANELA
ENDS


;----------------------------------------- TIPOS DE CAIXA ------------------
EXTRN SIMPLES:BYTE
EXTRN DUPLA:BYTE
EXTRN DUPLAHOR:BYTE
EXTRN DUPLAVERT:BYTE
EXTRN CLASSICA:BYTE
EXTRN DOTTED:BYTE

;----------------------------------------- METHODS -------------------------

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CONTROLO ~~~~~~~~~~~
EXTRN SCRN_INIT:NEAR
EXTRN SCRN_GOTOXY:NEAR
EXTRN SCRN_SETRECT:NEAR
EXTRN SCRN_COLOR:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AREAS ~~~~~~~~~~~~~
EXTRN SCRN_CLS:NEAR
EXTRN SCRN_FILL:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ STRINGS ~~~~~~~~~~~
EXTRN SCRN_PRINTXY:NEAR
EXTRN SCRN_PRINT:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CAIXAS ~~~~~~~~~~~~
EXTRN SCRN_BOX:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SCROLLING ~~~~~~~~~
EXTRN SCRN_SCROLLUP:NEAR
EXTRN SCRN_SCROLLDOWN:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CARACTERE ~~~~~~~~~
EXTRN SCRN_PUTCHAR:NEAR
EXTRN SCRN_GETCHAR:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ATRIBUTO ~~~~~~~~~~
EXTRN SCRN_PUTATR:NEAR
EXTRN SCRN_GETATR:NEAR
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MISCELANEA ~~~~~~~~
EXTRN SCRN_MUDA:NEAR
EXTRN SCRN_LIMPA:NEAR
EXTRN SCRN_GETAREA:NEAR
EXTRN SCRN_PUTAREA:NEAR

;===========================================================================
_TEXT32_END


