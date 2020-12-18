;=========================================================================================================================
;
; Track Axis Critical Location.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Characteristic locations in track (gradient, tangent/curve/spiral, cant ramp)

(defun C:TRACK-AXIS-CRITICAL-LOCATION ( / )
	(drawTrasepunkt)
)



; Symmetrical short line transversal to track, to be centered on alignment axis (whereas insulated joints are located on one of the rails).
 (defun drawTrasepunkt ( / blockName sideLength )
	(setq 
		blockName "NO-BN-2D-JBTOB-TRACK-AXIS-CRITICAL-LOCATION"
		sideLength 1.5
	)
	(command "._LINE" (list 0 (- sideLength)) (list 0 sideLength) "" )
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
