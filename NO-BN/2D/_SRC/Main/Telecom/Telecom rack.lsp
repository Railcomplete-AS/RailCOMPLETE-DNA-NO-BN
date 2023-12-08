;=========================================================================================================================
;
; Telecom rack.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Rack for telecomms cable distribution or equipment (?)

(defun TELECOM-RACK ( / )
	(TELERACK)
)



(defun TELERACK ( / blockName description x y r gx gy gr )
	;
	; +-----------------5
	; |       ___       |
	; |      (_._)      |  Box with circle, centered on origin (large cabinet, to be placed on a suitable foundation)
	; |                 |
	; +-----------------+
	; +-----------------+
	;
	(setq
		blockName	(strcat _TEL_ "RAC-" "TELERACK")
		description "TELERACK"
		x 9.0
		y 4.5
		r (* 0.1 y)
		gx 1.2 ; Large outdoor cabinet ??
		gy 0.5
		gr (* 0.1 gy)
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_Zero x y)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(DrawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_Zero gx gy)
	(DrawCircle layDef_Zero gr _noWipeout_)
	(AddTextAtPoint layDef_Zero (* 0.8 gy) _origin_ "TR")
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (HalfOf gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
