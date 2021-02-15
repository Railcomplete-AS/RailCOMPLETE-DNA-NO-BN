;=========================================================================================================================
;
; 20_Common.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Common objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Common"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-COMMON-OBJECTS ( )
	(setCadSystemDefaults)
	(subStep "INSERTION-POINT")	(C:INSERTION-POINT)
	(subStep "CONTROLLER")		(C:CONTROLLER)
	(subStep "LABEL")			(C:LABEL)
	(subStep "MARKER")			(C:MARKER)
	(subStep "SECTION")			(C:SECTION)
	(subStep "WATCH")			(C:WATCH)
)

