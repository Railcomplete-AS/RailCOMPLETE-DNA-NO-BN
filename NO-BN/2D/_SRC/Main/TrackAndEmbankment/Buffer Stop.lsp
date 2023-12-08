;=========================================================================================================================
;
; Buffer Stop.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Buffer stop



(defun BUFFER-STOPS ( / )
	(BUFFER-STOP "F") 	; fixed
	(BUFFER-STOP "S")	; sliding
	(BUFFER-STOP "H")	; hydraulic
)



(defun BUFFER-STOP ( variation / blockName description x y )
	;
	; +-----+
	; |     | ; Fixed     : no hatch
	; |     | ; Sliding   : medium hatch
	; |     | ; Hydraulic : solid hatch
	; +--.--+
	;
	(setq vv (cond ((= variation "F") "FAST")		((= variation "S") "GLIDBAR")		((= variation "H") "HYDRAULISK"		)))
	(setq blockName (strcat _TRK_ "SST-" "SPORSTOPPER-"	vv	))
	(setq description (strcat "SPORSTOPPER, "	vv	))
	(setq	
		x 8.5
		y 10.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	; (No hatch if "F")
	(if (= variation "S") ; sliding
		(DrawHatch _sparseHatch_)
	)
	(if (= variation "H")
		(DrawHatch _mediumHatch_) ; hydraulic
	)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
