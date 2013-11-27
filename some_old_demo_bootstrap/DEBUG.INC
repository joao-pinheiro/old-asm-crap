
_SEGMENT32 SEGMENT PARA PUBLIC 'SEGMENT32' USE32
;===========================================================================
EXTRN BASE_NUMBER:DWORD
EXTRN FIELD_LEN:WORD
EXTRN PAD_CHAR:BYTE
EXTRN SUFFIX:BYTE
EXTRN TERMINATOR:BYTE

EXTRN INT2STR:NEAR
EXTRN SHOWVAL:NEAR

;===========================================================================
_SEGMENT32 ENDS
