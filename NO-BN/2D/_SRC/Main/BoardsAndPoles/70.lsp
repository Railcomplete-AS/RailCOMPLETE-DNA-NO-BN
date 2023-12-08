;=========================================================================================================================
;
; 70.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Level crossing announcement

; For debugging:
; (70A)

(defun 70A ( / blockName description x y )
	; Level crossing announcement
	;
	; TL---TR
	; |     |
	; |  V  | ; Letter 'V'
	; |     |
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-70-PLANOVERGANGSSKILT"
		description "SKILT 70 PLANOVERGANG"
		x 3.0
		y 4.5
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th250_ _origin_ "V")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
