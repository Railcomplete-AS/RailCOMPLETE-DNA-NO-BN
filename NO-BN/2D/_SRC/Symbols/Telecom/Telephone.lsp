;=========================================================================================================================
;
; Telephone.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Telephone

(defun C:TELEPHONE ( / )
	(TELEFON-LANGS-SPOR)
)



(defun TELEFON-LANGS-SPOR ( / blockName description x y gx gy )
	; Box (geo with text "T"), centre top is insertion point.
	;
	;  TL--.--TR
	;  |  "T"  |
	;  BL-----BR
	;
	(setq 
		blockName	"NO-BN-2D-JBTTE-TELEFON-LANGS-SPOR"
		description	"TELEFON"
		x 3.0
		y 3.0
		gx 0.3
		gy 0.2
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigo description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	(AddTextAtPos layDef_Zero (* 0.8 gy) _origo_ "T")
	(MoveDown (HalfOf gy))
	(AddDescriptionBelowOrigo description gy)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
