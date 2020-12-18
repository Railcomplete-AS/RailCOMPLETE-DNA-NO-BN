;=========================================================================================================================
;
; Annotations.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Annotation objects top-level LISP routine

(loadFolder (findfile "Annotations"))

(defun C:GENERATE-ANNOTATIONS ( / )
	(setCadSystemDefaults)
	(subStep "ANNOTATION-CHAINBREAK")	(C:ANNOTATION-CHAINBREAK)
)
