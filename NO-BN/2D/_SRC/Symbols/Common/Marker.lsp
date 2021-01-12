;=========================================================================================================================
;
; Marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Marker for positions and other stuff which needs a pin-needle symbol and a text in a drawing.

(defun C:MARKER ( / blockName description )
	(setq
		blockName "NO-BN-2D-JBTFE-MARKOER-KNAPPENAAL"
	)
	(drawMarker)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun drawMarker ( / )
	(command "._CIRCLE" "1,1" "0.4")
	(command "._LINE" "0,0" "0.7563,0.6828" "")
	(command "._LINE" "0,0" "0.6828,0.7563" "")
	(addAtt "FARGE" "Farge" "" "1,2" 0.9 0 "iso" "_MC" _lockPosition_)
)
