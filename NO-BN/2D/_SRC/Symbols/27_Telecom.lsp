;=========================================================================================================================
;
; Telecom.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Telecom objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Telecom"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-TELECOM-OBJECTS ( / )
	(setCadSystemDefaults)
	(subStep "TELEPHONE") 		(C:TELEPHONE) 
	(subStep "TELECOM-RACK")	(C:TELECOM-RACK)
)