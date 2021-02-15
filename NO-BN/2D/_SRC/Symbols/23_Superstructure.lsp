;=========================================================================================================================
;
; 23_Superstructure.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Superstructure objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Superstructure"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-SUPERSTRUCTURE-OBJECTS ( / )
	(setCadSystemDefaults)  
	(subStep "CONNECTOR-SPLICE")				(C:CONNECTOR-SPLICE)
	(subStep "CONNECTOR-CROSSING")				(C:CONNECTOR-CROSSING)
	(subStep "ISOLATED-JOINT") 					(C:ISOLATED-JOINT) ; TRACK object when built, but designed by Signaling or Return Current specialists.
	(subStep "TRACK-AXIS-CRITICAL-LOCATION")	(C:TRACK-AXIS-CRITICAL-LOCATION)
	(subStep "DEFLECTION-BAR")					(C:DEFLECTION-BAR)
	(subStep "BUFFER-STOP")						(C:BUFFER-STOP)
	(subStep "CONNECTOR-SWITCH")				(C:CONNECTOR-SWITCH)
	(subStep "SWITCH-TONGUE")					(C:SWITCH-TONGUE)
)
