;--------------------------------------------------- CONST'S
DOSBUFSIZE = 07FFFh
FREADONLY  = 00h
FWRITEONLY = 01h
FREADWRITE = 02h


FROMSTART = 00h
FROMEND   = 02h
FROMPOS   = 01h

;--------------------------------------------------- VARS

EXTRN DOSBUFFER:DWORD
EXTRN DOSBUFSEL:DWORD
EXTRN DOSBUFSEG:DWORD
;--------------------------------------------------- PROCS
EXTRN INIT_DOSBUFFER:NEAR
EXTRN FREE_DOSBUFFER:NEAR
EXTRN COPY:NEAR
EXTRN FOPEN:NEAR
EXTRN FCLOSE:NEAR
EXTRN FREAD:NEAR
EXTRN FSEEK:NEAR
EXTRN GETFILESIZE:NEAR
;--------------------------------------------------- END
