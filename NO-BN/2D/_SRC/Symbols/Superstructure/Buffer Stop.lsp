;=========================================================================================================================
;
; Buffer Stop.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Buffer stop

(defun C:BUFFER-STOP ( / )
	(sporstopper "FAST")
	(sporstopper "GLIDBAR")
	(sporstopper "HYDRAULISK")
)

(defun sporstopper ( variation / blockName x y )
	(setq	
		blockName (strcat "NO-BN-2D-JBTOB-SPOROBJEKT-SPORSTOPPER-" variation)
		description (strcat "SPORSTOPPER, " variation)
		x 10.0
		y 8.5
	)
	(drawBox layer_Zero x y _noWipeout_)
	(if (= variation "GLIDBAR")
		(drawHatch _mediumHatch_)
	)
	(if (= variation "HYDRAULISK")
		(drawHatch _filledHatch_)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
	(addDescriptionBelowOrigo description 0.0)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
