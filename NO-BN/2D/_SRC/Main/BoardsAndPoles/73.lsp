;=========================================================================================================================
;
; 73.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Avalanche warning

; For debugging:
; (73A)

(defun 73A ( / blockName description x y r1 r2 ) 
	; Avalanche warning
	;
	; TL-----TR
	; |       |
	; | ( R ) | ; Letter 'R' in a circle
	; |       |
	; BL--.--BR
	;
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-73-RASVARSEL"
		description "SKILT 73 RASVARSLINGSSKILT"
		x 4.5
		y 4.5
		r 1.5
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPoint layDef_Zero (* 0.5 x) _origin_ "R") ; Letter size is 45% of side
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
