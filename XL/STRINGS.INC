;ีออ XDOS DOS EXTENDER อออออออออออออออออออออออ FILE: STRINGS.INC ออออออออออธ
;ณ                                                                         ณ
;ณ  STRING OPERATION ROUTINES - INCLUDE FILE                               ณ
;ณ                                                                         ณ
;ณ  Copyright (c) 1997  Joo Manuel Pinheiro                               ณ
;ณ  V.N.Gaia - Portugal                                                    ณ
;ณ                                                                         ณ
;ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

;===========================================================================
_TEXT32
;===========================================================================

EXTRN BASE_NUMBER:DWORD
EXTRN FIELD_LEN:WORD
EXTRN PAD_CHAR:BYTE
EXTRN SUFFIX:BYTE
EXTRN TERMINATOR:BYTE

;--------------------------------------------- PROCEDIMENTOS

EXTRN STRLEN:NEAR
EXTRN STRCOMP:NEAR
EXTRN INT2STR:NEAR
EXTRN UPCASE:NEAR
EXTRN DELETE:NEAR
EXTRN STRCOPY:NEAR
EXTRN STRAPPEND:NEAR

;===========================================================================
_TEXT32_END
