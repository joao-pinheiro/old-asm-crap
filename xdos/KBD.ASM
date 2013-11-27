.386p
.model Large,pascal

Include DPMI.INC
INCLUDE EXTERNAL.inc



CODE32 SEgment Para Use32

;--------------------------------------------------------------
;Function Readkey(DL:Char);
;--------------------------------------------------------------


READKEY Proc Far

	USES EAX
	Xor eax,eax
	INTR 16h
	Mov dl,al
	ret
Endp	





CODE32 ENDS


