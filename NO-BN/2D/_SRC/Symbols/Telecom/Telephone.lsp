;=========================================================================================================================
;
; Telephone.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Telephone

(defun C:TELEPHONE ( / )
	(TELEFON-LANGS-SPOR)
)



(defun TELEFON-LANGS-SPOR ( / blockName x y p )
	; Box (geo with text "T"), centre top is insertion point.
	;
	;  TL--.--TR
	;  |  "T"  |
	;  BL-----BR
	;
	(setq 
		blockName	"NO-BN-2D-JBTTE-TELEFON-LANGS-SPOR"
		description	"Tlf"
		x 3.0
		y 3.0
		gx 0.3
		gy 0.2
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	(addDescriptionBelowOrigo description y)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	(addTextAtPos layer_Zero (* 0.8 gy) "0,0" "T")
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) "Telefon")
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
