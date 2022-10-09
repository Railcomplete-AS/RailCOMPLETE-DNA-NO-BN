;=========================================================================================================================
;
; 11_Annotations.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Annotation objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\Annotations"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)


(defun 11_GENERATE-ANNOTATIONS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "ANYADM-ANNOTATION-CHAINBREAK")	(ANYADM-ANNOTATION-CHAINBREAK)

	; Specific to this administration:
)
