;--------------------------------------------------- VARS

PUBLIC DPMI_VERSION, DPMI_FLAGS, DPMI_PDSIZE, DPMI_ENTRY
PUBLIC MASTER_VPIC, SLAVE_VPIC, SEL_INCVAL
PUBLIC OLDCS, OLDSS, OLDSP,OLDBP, SCODE16, SDATA16, SSTACK16
PUBLIC SPSP, STACKSEG
PUBLIC CPU_TYPE, FP_STATUS, VENDOR_ID, MODELL, STEPPING
PUBLIC ID_FLAG, FPU_TYPE, FEATURE_FLAGS, MMX_FLAG


PUBLIC SCODE32, SDATA32, SSTACK32, SDATA0
PUBLIC REGS, DPMIMEM, HEAP_ADDX, EXITPROC, EXITCOD
PUBLIC ERRORFLAG, ERRORHANDLER, VERIFY, INIT_MODE

;--------------------------------------------------- PROCS

PUBLIC ERROR_HANDLER
PUBLIC PRINT
PUBLIC FAIL 
PUBLIC ALLOC_DESCR 
PUBLIC FREE_DESCR 
PUBLIC SET_BASE 
PUBLIC SET_LIMIT 
PUBLIC SET_AR 
PUBLIC GET_DESCR 
PUBLIC SET_DESCR 
PUBLIC GETDOSMEM 
PUBLIC FREEDOSMEM 
PUBLIC RESIZEDOSMEM 
PUBLIC GET_EXCEPTION 
PUBLIC SET_EXCEPTION 
PUBLIC GET_INTVEC 
PUBLIC SET_INTVEC 
PUBLIC RM_INT
PUBLIC CALL_RMPROC 
PUBLIC ALLOC_CALLBACK 
PUBLIC FREE_CALLBACK 
PUBLIC DPMI_MEM_INFO 
PUBLIC GETMEM 
PUBLIC FREEMEM 
PUBLIC RESIZEMEM 
PUBLIC LOCK_REGION 
PUBLIC UNLOCK_REGION 
PUBLIC MARK_RM_PAGEABLE 
PUBLIC VCLI 
PUBLIC VSTI 
PUBLIC GET_VI
PUBLIC HEAP_INIT
PUBLIC MALLOC
PUBLIC MDALLOC
PUBLIC MEMAVAIL

PUBLIC Q00003

;--------------------------------------------------- END
