;=========================================================================================================================
;
; 27_Telecom.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Telecom objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\Telecom"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 27_GENERATE-TELECOM-OBJECTS ( / )
	(SetCadSystemDefaults)
	(TraceLevel2 "WAYSIDE-TELEPHONE") 		(WAYSIDE-TELEPHONE) 
	(TraceLevel2 "TELECOM-RACK			")			(TELECOM-RACK)
)
