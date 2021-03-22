;=========================================================================================================================
;
; Derailment indicator.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Derailment indicator (a bar across one sleeper, triggered by derailed axle)

(defun C:DERAILMENT-INDICATOR ( )
	(DERAILMENT-INDICATOR)
)

(defun DERAILMENT-INDICATOR ( / blockName h dh paperScale )
;
;  TL--1-2--TR
;  |   |*|   |
;  |   |.|   |
;  |   |*|   |
;  BL--3-4--BR
;
;
	(setq
		blockName "NO-BN-2D-JBTSI-DIVERSE-DERAILMENT-INDICATOR"
		x		3.0 ; Box
		y		3.0
        x2		0.6	; Width of vertical hatched bar (1234) in geo symbol
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	; Add hatched vertical bar to geo symbol:
	(DrawBox layDef_Zero x2 y _noWipeout_)
	(DrawHatch _denseHatch_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
