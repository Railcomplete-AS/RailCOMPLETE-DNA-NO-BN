;=========================================================================================================================
;
; 66.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; End of train route

; For debugging:
; (66A)
; (The board's real name is '66', but '66' is not a valid LISP identifier)

(defun 66A ( /	blockName description x y posCircle radius1 radius2 description )
	; An 'S' shape
	;
	; TL-------TR
	; |   ___   |  
	; |  /   \  |
	; | (       | 
	; |  \___   |
	; |      \  |
	; |       ) |
	; |  \___/  |
	; |         |
	; BL---.---BR 
	;
	(setq	
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-66-TOGVEI-SLUTT"
		description "SKILT SIGNAL 66 TOGVEI SLUTT"
		x 3.0
		y 4.5
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th250_ _origin_ "S")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
