;=========================================================================================================================
;
; NO-BN Avsporingsindikator.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Derailment indicator (a bar across one sleeper, triggered by derailed axle)

(defun NOBN-AVSPORINGSINDIKATOR ( )
	(AVSPORINGSINDIKATOR)
)

(defun AVSPORINGSINDIKATOR ( / blockName h dh paperScale )
;
;  TL--1-2--TR
;  |   |*|   |
;  |   |.|   |
;  |   |*|   |
;  BL--3-4--BR
;
;
	(setq
		blockName (strcat _SIG_ "AVI-" "AVSPORINGSINDIKATOR")
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
