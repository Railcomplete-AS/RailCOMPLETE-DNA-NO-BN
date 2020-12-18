;=========================================================================================================================
;
; Superstructure.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Superstructure objects top-level LISP routine

(loadFolder (findfile "Superstructure"))

(defun C:GENERATE-SUPERSTRUCTURE-SCALED-OBJECTS ( / )
	(setCadSystemDefaults)  
	
	(subStep "CONNECTOR-ALIGNMENT-EXTENSION")		(C:CONNECTOR-ALIGNMENT-EXTENSION)
	(subStep "CONNECTOR-ALIGNMENT-CROSSING")		(C:CONNECTOR-ALIGNMENT-CROSSING)
	(subStep "TRACK-AXIS-CRITICAL-LOCATION")		(C:TRACK-AXIS-CRITICAL-LOCATION)
	(subStep "DEFLECTION-BAR")						(C:DEFLECTION-BAR)
	(subStep "BUFFER-STOP")							(C:BUFFER-STOP)
)



(defun C:GENERATE-SUPERSTRUCTURE-FIXED-SCALE-OBJECTS ( / )
	(setCadSystemDefaults)  
	(subStep "CONNECTOR-TRACK-BIFURCATION")			(C:CONNECTOR-TRACK-BIFURCATION)	; Geographical 1:1 symbols for connecting the topological track network (switches, tongues). See also 'Schematic connections.lsp'.
	(subStep "SWITCH-TONGUE")						(C:SWITCH-TONGUE)
)