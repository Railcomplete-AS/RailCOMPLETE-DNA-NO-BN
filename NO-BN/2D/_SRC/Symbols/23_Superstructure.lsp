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
(LoadFolder f)

(defun C:GENERATE-SUPERSTRUCTURE-OBJECTS ( / )
	(SetCadSystemDefaults)  
	(TraceLevel2 "ISOLATED-JOINT") 					(C:ISOLATED-JOINT) ; TRACK object when built, but designed by Signaling or Return Current specialists.
	(TraceLevel2 "DEFLECTION-BAR")					(C:DEFLECTION-BAR)
	(TraceLevel2 "BUFFER-STOP")						(C:BUFFER-STOP)
	(TraceLevel2 "TRACK-AXIS-CRITICAL-LOCATION")	(C:TRACK-AXIS-CRITICAL-LOCATION)
	(TraceLevel2 "CONNECTOR-SPLICE")				(C:CONNECTOR-SPLICE)
	(TraceLevel2 "CONNECTOR-CROSSING")				(C:CONNECTOR-CROSSING)
	(TraceLevel2 "CONNECTOR-SWITCH")				(C:CONNECTOR-SWITCH)
	(TraceLevel2 "SWITCH-TONGUE")					(C:SWITCH-TONGUE)
)
