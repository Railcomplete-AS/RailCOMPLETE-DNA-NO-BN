;=========================================================================================================================
;
; Marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Marker for positions and other stuff which needs a pin-needle symbol and a text in a drawing.

(defun C:MARKER ( / blockName description )
	(setq
		blockName "NO-BN-2D-JBTFE-MARKOER-KNAPPENAAL"
	)
	(drawMarker)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun drawMarker ( / )
	(command _CIRCLE_ "1,1" "0.4")
	(command _LINE_ _origo_ "0.7563,0.6828" _ENTER_)
	(command _LINE_ _origo_ "0.6828,0.7563" _ENTER_)
	(addAtt "FARGE" "Farge" _ENTER_ "1,2" _th100_ _angleZero_ _rcTextStyle_ _middleCenter_ _lockPosition_)
)
