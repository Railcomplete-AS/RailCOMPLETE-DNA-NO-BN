;=========================================================================================================================
;
; Common.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Common objects top-level LISP routine

(loadFolder (findfile "Common"))

(defun C:GENERATE-COMMON-OBJECTS ( )
	(setCadSystemDefaults)
	(subStep "INSERTION-POINT")	(C:INSERTION-POINT)
	(subStep "CONTROLLER")		(C:CONTROLLER)
	(subStep "LABEL")			(C:LABEL)
	(subStep "MARKER")			(C:MARKER)
	(subStep "SECTION")			(C:SECTION)
	(subStep "WATCH")			(C:WATCH)
)

