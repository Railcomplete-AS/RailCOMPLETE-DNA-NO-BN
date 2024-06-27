;=========================================================================================================================
;
; 200.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
; 2024-04-16 SVNOE renamed
;
;=========================================================================================================================

; Level zero


(defun E37B ( /	blockName description x y )
	;
	; TL-------TR
	; |         |
	; |  Nivå 0 |
	; |         |
	; BL---.---BR 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-ERTMS-LEVEL-ZERO"
		description "SKILT ERTMS LEVEL ZERO"
		x 4.0
		y 4.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th100_ _origin_ "Nivå 0")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
