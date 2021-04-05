;=========================================================================================================================
;
; Suspension Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Suspension mast - from yoke, under bridge or from tunnel ceiling

(defun C:SUSPENSION-MAST ( / )
	(HENGEMAST-FOR-BRU-OG-TUNNEL)
	(HENGEMAST-CARIBONI)
	(HENGEMAST-FOR-AAK)
)



(defun HENGEMAST-FOR-BRU-OG-TUNNEL ( / blockName description x y )
  (setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-FOR-TUNNEL-OG-BRU"
		description	"HENGEMAST FOR TUNNEL OG BRU"
		x 1.5
		y 2.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun HENGEMAST-CARIBONI ( / blockName description x y offset )
	(setq
		blockName "NO-BN-2D-JBTKL-HENGEMAST-CARIBONI"
		description	"HENGEMAST I TUNNELHVELV FOR CARIBONI UTLIGGER"
		x 1.0
		y 2.0
		offset 0.075 ; offset pga kort utligger og masten må plasseres nær sporet
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(MoveUp (- y offset))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun HENGEMAST-FOR-AAK ( / blockName description r )
	(setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-FOR-AAK"
		description (strcat "HENGEMAST I " _uARING_ "K")
		r 0.525
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigo description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

