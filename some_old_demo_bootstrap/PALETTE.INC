
;-------------------------------------------------- DEFINES
RGBSTRUC STRUC		;RGB ENTRY FOR PAL
     	   RED   DB ?
     	   GREEN DB ?
     	   BLUE  DB ?
RGBSTRUC ENDS


PALSTRUC STRUC		;PALETTE STRUCTURE
	 DB 768 DUP(0)
PALSTRUC ENDS

;-------------------------------------------------- DEFINES
EXTRN VRETRACE:NEAR
EXTRN SETRGB:NEAR
EXTRN SETPAL:NEAR
EXTRN GETPAL:NEAR
EXTRN FADEIN:NEAR
EXTRN FADEOUT:NEAR
;-------------------------------------------------- END
