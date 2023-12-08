;=========================================================================================================================
;
; 201.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Level transition


(defun E201 ( /	blockName description x y )
	;
	; +---------+
	; |         |
	; |   LT    |
	; |  ETCS   |
	; |         |
	; +----.----+ 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E201-LEVEL-TRANSITION-ETCS"
		description "SKILT SIGNAL E201 LEVEL TRANSITION ETCS"
		x 4.0
		y 4.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th0736_ '(0 0.55) "LT")
	(AddTextAtPoint layDef_Zero _th0736_ '(0 -0.55) "ETCS")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
