;ΥΝΝ XDOS DOS EXTENDER ΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ FILE: STRINGS.INC ΝΝΝΝΝΝΝΝΝΝΈ
;³                                                                         ³
;³  STRING OPERATION ROUTINES - INCLUDE FILE                               ³
;³                                                                         ³
;³  Copyright (c) 1997  Jo„o Manuel Pinheiro                               ³
;³  V.N.Gaia - Portugal                                                    ³
;³                                                                         ³
;ΤΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΎ

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
