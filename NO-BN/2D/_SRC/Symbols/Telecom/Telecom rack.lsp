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
	(drawBox layDef_Zero x y layDef_Cabinet_Wipeout)
	(drawBottomEmphasis layDef_Zero x y)
	(drawCircle layDef_Zero r _noWipeout_)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(drawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(drawBottomEmphasis layDef_Zero gx gy)
	(drawCircle layDef_Zero gr _noWipeout_)
	(addTextAtPos layDef_Zero (* 0.8 gy) _origo_ "TR")
	(addDescriptionBelowOrigo description gy)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(drawBox layDef_MetricDetails gx gy layDef_Cabinet_Wipeout)
	(drawBottomEmphasis layDef_MetricDetails gx gy)
	(drawCircle layDef_MetricDetails gr _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
)
