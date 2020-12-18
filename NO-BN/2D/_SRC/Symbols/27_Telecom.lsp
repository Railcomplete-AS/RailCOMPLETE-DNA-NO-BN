;=========================================================================================================================
;
; Telecom.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Telecom objects top-level LISP routine

(loadFolder (findfile "Telecom")) 

(defun C:GENERATE-TELECOM-OBJECTS ( / )
	
	(setCadSystemDefaults)

	(subStep "TELEPHONE") 		(C:TELEPHONE) 
	(subStep "TELECOM-RACK")	(C:TELECOM-RACK)
)