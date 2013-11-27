;###########################################################
;#                                                         #
;#  Ficheiro: GLOBALS.INC                                  #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################

;###########################################################

TRUE    equ 1
FALSE   equ 0
NIL	equ 0

;###########################################################

MDA	EQU 0
CGA	EQU 1
MCGA	EQU 2
EGA	EQU 3
VGA	EQU 4

MONO	EQU 0
COR	EQU 1

;###########################################################

include inc\Segdefs.inc
include inc\Segdefs.mac
include inc\p2_emu.mac

include strucs.inc

;###########################################################

MOVSEG MACRO dest,src

	push src
	pop dest
ENDM

CALL16 MACRO S		;de um segmento de 32 bit

	db 09Ah
	dd offset S
	dw seg S
ENDM

CALL32 MACRO S		;de um segmento de 16 bit

	db 09Ah
	dw offset S
	dw seg S
ENDM

UD2 Macro
    db 0fh,0bh
ENDM

;###########################################################
