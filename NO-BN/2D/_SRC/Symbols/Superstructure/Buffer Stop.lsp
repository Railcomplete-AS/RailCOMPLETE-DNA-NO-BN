;=========================================================================================================================
;
; Buffer Stop.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Buffer stop

(defun C:BUFFER-STOP ( / )
	(sporstopper "FAST")
	(sporstopper "GLIDBAR")
	(sporstopper "HYDRAULISK")
)

(defun sporstopper ( variation / blockName description x y )
	(setq	
		blockName (strcat "NO-BN-2D-JBTOB-TRACK-OBJECT-SPORSTOPPER-" variation)
		description (strcat "SPORSTOPPER, " variation)
		x 10.0
		y 8.5
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(if (= variation "GLIDBAR")
		(DrawHatch _mediumHatch_)
	)
	(if (= variation "HYDRAULISK")
		(DrawHatch _solidHatch_)
	)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)
	(AddDescriptionBelowOrigo description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
