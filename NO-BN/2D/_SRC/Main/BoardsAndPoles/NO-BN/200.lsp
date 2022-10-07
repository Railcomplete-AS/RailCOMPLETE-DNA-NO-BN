;=========================================================================================================================
;
; 200.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Level zero


(defun E200 ( /	blockName description x y )
	;
	; TL-------TR
	; |         |
	; |  Nivå 0 |
	; |         |
	; BL---.---BR 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E200-LEVEL-ZERO"
		description "SKILT SIGNAL E200 LEVEL ZERO"
		x 4.0
		y 4.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th100_ _origin_ "Nivå 0")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
