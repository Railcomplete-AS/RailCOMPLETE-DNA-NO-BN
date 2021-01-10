;=========================================================================================================================
;
; Superstructure.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Superstructure objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Superstructure"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-SUPERSTRUCTURE-SCALED-OBJECTS ( / )
	(setCadSystemDefaults)  
	(subStep "CONNECTOR-SPLICE")				(C:CONNECTOR-SPLICE)
	(subStep "CONNECTOR-CROSSING")				(C:CONNECTOR-CROSSING)
	(subStep "ISOLATED-JOINT") 					(C:ISOLATED-JOINT) ; Track object when built, but their placement mainly planned by Signaling engineers, subsidiary by return current specialists.
	(subStep "TRACK-AXIS-CRITICAL-LOCATION")	(C:TRACK-AXIS-CRITICAL-LOCATION)
	(subStep "DEFLECTION-BAR")					(C:DEFLECTION-BAR)
	(subStep "BUFFER-STOP")						(C:BUFFER-STOP)
)



(defun C:GENERATE-SUPERSTRUCTURE-FIXED-SCALE-OBJECTS ( / )
	(setCadSystemDefaults)  
	(subStep "CONNECTOR-SWITCH")			(C:CONNECTOR-SWITCH)	; Geographical 1:1 symbols for connecting the topological track network (switches, tongues). See also 'Schematic connections.lsp'.
	(subStep "SWITCH-TONGUE")						(C:SWITCH-TONGUE)
)