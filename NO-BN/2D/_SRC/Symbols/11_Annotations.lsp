;=========================================================================================================================
;
; Annotations.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Annotation objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Annotations"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)


(defun C:GENERATE-ANNOTATIONS ( / )
	(setCadSystemDefaults)
	(subStep "ANNOTATION-CHAINBREAK")	(C:ANNOTATION-CHAINBREAK)
)
