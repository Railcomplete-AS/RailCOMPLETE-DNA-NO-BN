;=========================================================================================================================
;
; Telecom rack.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Rack for telecomms cable distribution or equipment (?)

(defun C:TELECOM-RACK ( / )
	(TELERACK)
)



(defun TELERACK ( / blockName description x y r gx gy gr )
	; Box with circle, centered on origo (large cabinet to be placed on a suitable foundation)
	(setq
		blockName "NO-BN-2D-JBTTE-TELERACK"
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
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(DrawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_Zero gx gy)
	(DrawCircle layDef_Zero gr _noWipeout_)
	(AddTextAtPos layDef_Zero (* 0.8 gy) _origo_ "TR")
	(AddDescriptionBelowOrigo description gy)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(DrawBox layDef_MetricDetails gx gy layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_MetricDetails gx gy)
	(DrawCircle layDef_MetricDetails gr _noWipeout_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)
