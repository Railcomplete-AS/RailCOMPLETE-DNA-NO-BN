;=========================================================================================================================
;
; 2D Symbol Library Bootstrap
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
;
;=========================================================================================================================
;
; Usage:  
;
; ==> Adjust 'adm' and 'github' below to personalise the bootstrap process.
;
; 1) Start AutoCAD
; 2) Write _VLIDE
; 3) In the Visual LISP IDE, press Ctrl+Shift+L and select this file, which loads and calls the library commands file.
; 4) In VLIDE, write (mk) to start building the 2D library.
; 5) Tip: Use Ctrl+Shift+C in VLIDE to switch between interactive debugging and command mode.
;

(setq _OSLASH_	"\U+00F8") ;	ø	0248	&oslash;	Latin Small Letter O with stroke	0184

(princ "\n================================= 2D Symbol Library Commands.lsp =================================") (prin1)
(setq 
	adm			"NO-BN"
  user-profile (getenv "USERPROFILE")
	github		(strcat user-profile "\\Documents\\GitHub")
)
(vl-load-com) ; Load AutoCAD's Visual LISP environment (if not already loaded)
(setq rootFolder (strcat github "\\RailCOMPLETE-DNA-NO-BN\\" adm "\\2D\\_SRC"))
(load (findfile (strcat github "\\RailCOMPLETE-DNA-NO-BN\\" adm "\\2D\\_BOOTSTRAPS\\_2D Symbol Library Commands.lsp"))); Garbage collection:
(setq 
	adm nil
	github nil
	commandFile nil
)
