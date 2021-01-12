;=========================================================================================================================
;
; Suspension Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Suspension mast - from yoke, under bridge or from tunnel ceiling

(defun C:SUSPENSION-MAST ( / )
	(HENGEMAST-FOR-BRU-OG-TUNNEL)
	(HENGEMAST-CARIBONI)
	(HENGEMAST-FOR-AAK)
)



(defun HENGEMAST-FOR-BRU-OG-TUNNEL ( / blockName x y )
  (setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-FOR-TUNNEL-OG-BRU"
		description	"HENGEMAST FOR TUNNEL OG BRU"
		x 1.5
		y 2.0
	)
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun HENGEMAST-CARIBONI ( / blockName x y offset )
	(setq
		blockName "NO-BN-2D-JBTKL-HENGEMAST-CARIBONI"
		description	"HENGEMAST I TUNNELHVELV FOR CARIBONI UTLIGGER"
		x 1.0
		y 2.0
		offset 0.075 ; offset pga kort utligger og masten må plasseres nær sporet
	)
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description (halfOf y))
	(moveUp (- y offset))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun HENGEMAST-FOR-AAK ( / blockName r )
	(setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-FOR-AAK"
		description (strcat "HENGEMAST I " _uAA_ "K")
		r 0.525
	)
	(drawCircle layer_Zero r _noWipeout_)
	(drawHatch _denseHatch_)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)

