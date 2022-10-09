;=========================================================================================================================
;
; Skilt Spornummer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Track number

; For debugging:
; (SKILT-SPORNUMMER)

(defun SKILT-SPORNUMMER ( / blockName description x y p1 p2 attTrackNumber )
	; Siding (e.g., industry access track on the line between two railway stations)
	;
	; TL-------TR
	; |         |
	; |  Spor   |
	; |         |
	; |   NR    |
	; |         |
	; BL---.---BR
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SPORNUMMER"
		description "SKILT SPORNUMMER"
		x 4.875
		y 6.0
		p1 (list 0 4.8)
		p2 (list 0 1.8)
		attTrackNumber	'("SPORNUMMER" "Spornummer" "38")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAtPos          layDef_Zero _th180_ p1 "Spor")
	(AddTextAttributeAtPos layDef_Zero _th250_ p2 attTrackNumber)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
