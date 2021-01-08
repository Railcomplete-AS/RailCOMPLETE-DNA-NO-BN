;=========================================================================================================================
;
; Connector Alignment Crossing.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

; Concerns schematic symbols that scale with paperspace drawing scale.

(defun C:CONNECTOR-ALIGNMENT-CROSSING ( / )
	
	(setCadSystemDefaults)  
	(CONNECTOR-ALIGNMENT-CROSSING)
)

(defun CONNECTOR-ALIGNMENT-CROSSING ( / blockName radius )
	(setq
		blockName "NO-BN-2D-JBTOB-CONNECTOR-ALIGNMENT-CROSSING" 
		radius 0.5 ; Radius of 180 deg half-circle arc, to be shown at the ends of a long horizontal and short vertical line meeting at the crossing
	)
	(setLayer layer_Zero)
	(command
		"._LINE" (list (* -4 radius) 0) (list (* 4 radius) 0) "" ; long horizontal line
		"._LINE" (list 0 (* -2 radius)) (list 0 (* 2 radius)) "" ; short vertical line
		"_ARC" "_CE" (list (* -5 radius) 0) (list (* -5 radius) (* -1 radius)) (list (* radius -5) (* 1 radius)) ; Left arc
		"_ARC" "_CE" (list (* 5 radius) 0) (list (* 5 radius) (* 1 radius)) (list (* radius 5) (* -1 radius)) ; Right arc
		"_ARC" "_CE" (list 0 (* -3 radius)) (list (* 1 radius) (* -3 radius)) (list (* -1 radius) (* -3 radius)) ; Bottom arc
		"_ARC" "_CE" (list 0 (* 3 radius)) (list (* -1 radius) (* 3 radius)) (list (* 1 radius) (* 3 radius)) ; Top arc
	)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
