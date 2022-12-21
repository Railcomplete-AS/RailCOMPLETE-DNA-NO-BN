;=========================================================================================================================
;
; Wayside telephone.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Telephone

(defun WAYSIDE-TELEPHONE ( / blockName description x y gx gy )
	; Box (geo with text "T"), centre top is insertion point.
	;
	;  TL--.--TR
	;  |  "T"  |
	;  BL-----BR
	;
	(setq blockName (strcat _TEL_ "TLT-" "TOGLEDERTELEFON"	))
	(setq description (strcat "TOGLEDERTELEFON"				))
	(setq 
		x 3.0
		y 3.0
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddTextAtPoint layDef_Zero (* 0.8 y) _origin_ "T")
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
