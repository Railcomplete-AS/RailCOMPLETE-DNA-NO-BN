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
(LoadFolder f)

(defun C:GENERATE-COMMON-OBJECTS ( )
	(SetCadSystemDefaults)
	(TraceLevel2 "INSERTION-POINT")	(C:INSERTION-POINT)
	(TraceLevel2 "CONTROLLER")		(C:CONTROLLER)
	(TraceLevel2 "LABEL")			(C:LABEL)
	(TraceLevel2 "MARKER")			(C:MARKER)
	(TraceLevel2 "SECTION")			(C:SECTION)
	(TraceLevel2 "WATCH")			(C:WATCH)
)
