;###########################################################
;#                                                         #
;#  Ficheiro: XMS.INC                                      #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################


;###########################################################

EXTRN xms_entry:dword
EXTRN xms_version:byte
EXTRN hma_flag:word
EXTRN old_a20:byte

;###########################################################

EXTRN xms_detect:near
EXTRN xms_setA20:near
EXTRN xms_resetA20:near
EXTRN xms_getfreemem:near
EXTRN xms_malloc:near
EXTRN xms_dalloc:near
EXTRN xms_lockblock:near
EXTRN xms_unlockblock:near

;###########################################################
