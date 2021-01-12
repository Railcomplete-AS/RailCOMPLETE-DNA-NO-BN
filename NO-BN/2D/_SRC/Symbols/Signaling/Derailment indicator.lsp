;=========================================================================================================================
;
; Derailment indicator.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Derailment indicator (a bar across one sleeper, triggered by derailed axle)

(defun C:DERAILMENT-INDICATOR ( )
	(DERAILMENT-INDICATOR)
)

(defun DERAILMENT-INDICATOR ( / blockName h dh paperScale )
	(setq
		blockName "NO-BN-2D-JBTSI-DIVERSE-DERAILMENT-INDICATOR"
		side 3.0
		h		(/ side 2.0) ; half
        dh 		0.2	; +/- Proportion of box to be hatched (a vertical bar - the derailment indicator device - across the track)
	)
	; Schematic symbol:
	(command "._RECTANGLE" (list (- h) (- h)) (list h h)) ; 3 x 3 box
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(addScaledGraphicsFromBlock blockName 1.0) 								; Retrieve, explode and scale S symbol
	(command "._RECTANGLE" (list (* h (- dh)) (- h)) (list (* h dh) h)) 	; Add hatched 'bar' across the track
	(drawHatch _denseHatch_)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 1.0 blockName)
)
