;###########################################################
;#                                                         #
;#  Ficheiro: VCPI.INC                                     #
;#                                                         #
;#  (c) João Pinheiro, 1998                                #
;#                                                         #
;###########################################################


;###########################################################

Vcpi_switchstruc struc
	_cr3		dd ?
	_gdtr_addx    	dd ?
	_idtr_addx	dd ?
	_ldtr		dw ?
	_tr		dw 0
	_entry_eip      dd ?
	_entry_cs	dw ?
ends	


;###########################################################

EXTRN ems_page:Word 
EXTRN vcpi_version:byte
EXTRN vcpi_pagesfree:dword
EXTRN vcpi_Master_pic:word
EXTRN vcpi_Slave_pic:word

;###########################################################

EXTRN vcpi_detect:near
EXTRN vcpi_queryfreepages:near
EXTRN vcpi_getpicmaps:near
EXTRN vcpi_setpicmaps:near
EXTRN vcpi_readcr0:near
EXTRN vcpi_allocpage:near
EXTRN vcpi_freepage:near
EXTRN vcpi_switch2pm:near

;###########################################################
