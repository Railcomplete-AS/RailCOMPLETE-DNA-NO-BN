;=========================================================================================================================
;
; 2D Symbol Library bootstrap - THBEN.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY Merged 2D symbol generation into specific DNA repository. LISP code was adapted as needed.
;
;=========================================================================================================================
;
; Usage:  
;
; ==> Adjust 'adm' and 'github' below to personalise the bootstrap process.
;
; 1) Start AutoCAD
; 2) Write _VLIDE
; 3) In the Visual LISP IDE, press Ctrl+Shift+L and select one of the personal bootstrap files, such as 
;    "_2D Symbol Library bootstrap - CLFEY.lsp" which loads and calls the library commands file.
; 4) In VLIDE, write (mk) to start building the 2D library.
; 5) Tip: Use Ctrl+Shift+C in VLIDE to switch between interactive debugging and command mode.
;
(princ "\n================================= 2D Symbol Library Commands.lsp =================================") (prin1)
(setq 
	adm			"NO-BN"
	github		"C:\\Users\\Norctest\\Documents\\GitHub"
)
(vl-load-com) ; Load AutoCAD's Visual LISP environment (if not already loaded)
(setq rootFolder (strcat github "\\RailCOMPLETE-DNA-" adm "\\" adm "\\2D\\_SRC"))
(load (findfile (strcat github "\\RailCOMPLETE-DNA-" adm "\\" adm "\\2D\\_BOOTSTRAPS\\_2D Symbol Library Commands.lsp"))); Garbage collection:
(setq 
	adm nil
	github nil
	commandFile nil
)
