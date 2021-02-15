;=========================================================================================================================
;
; Skilt Spornummer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Track number

; For debugging:
; (SKILT_SPORNUMMER)

(defun Skilt_Spornummer ( / blockName description x y )
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SPORNUMMER"
		description "SKILT SPORNUMMER"
		x 4.875
		y 6.0
		p1 (list 0 4.8)
		p2 (list 0 1.8)
		attTrackNumber	'("SPORNUMMER" "Spornummer" "38")
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(addTextAtPos          layDef_Zero _th180_ p1 "Spor")
	(addTextAttributeAtPos layDef_Zero _th250_ p2 attTrackNumber)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
