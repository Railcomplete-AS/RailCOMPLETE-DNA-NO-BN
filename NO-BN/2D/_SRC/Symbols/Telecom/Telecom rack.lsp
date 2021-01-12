;=========================================================================================================================
;
; Telecom rack.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Rack for telecomms cable distribution or equipment (?)

(defun C:TELECOM-RACK ( / )
	(TELERACK)
)



(defun TELERACK ( / blockName x y r gx gy gr )
	; Box with circle, centered on origo (large cabinet to be placed on a suitable foundation)
	(setq
		blockName "NO-BN-2D-JBTTE-TELERACK"
		x 9.0
		y 4.5
		r (* 0.1 y)
		gx 1.2 ; Large outdoor cabinet ??
		gy 0.5
		gr (* 0.1 gy)
	)
	; Schematic symbol
	(drawBox layer_Zero x y layer_Cabinet_Wipeout)
	(drawBottomEmphasis layer_Zero x y)
	(drawCircle layer_Zero r _noWipeout_)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) "Telerack")
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols
	(drawBox layer_Zero gx gy layer_Cabinet_Wipeout)
	(drawBottomEmphasis layer_Zero gx gy)
	(drawCircle layer_Zero gr _noWipeout_)
	(addTextAtPos layer_Zero (* 0.8 gy) _origo_ "TR")
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) "Telerack")
	(createGeoBlockInAllPaperScalesFromCurrentGraphics	1.0 blockName)
)
