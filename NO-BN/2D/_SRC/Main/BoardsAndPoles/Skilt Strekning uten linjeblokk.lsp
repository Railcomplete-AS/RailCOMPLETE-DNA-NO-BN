;=========================================================================================================================
;
; Skilt Strekning uten linjeblokk.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Start of line without automatic blocking system

; For debugging:
; (SKILT-STREKNING-UTEN-LINJEBLOKK)

(defun SKILT-STREKNING-UTEN-LINJEBLOKK ( / blockName description x y p1 p2 p3 p4 )
	; Segment of line without line blocking system
	;
	; TL---------TR
	; | STREKNING |
	; |    UTEN   |
	; | LINJEBLOKK|
	; |  BEGYNNER |
	; BL----.----BR
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-STREKNING-UTEN-LINJEBLOKK"
		description "SKILT STREKNING UTEN LINJEBLOKK"
		x 9.75
		y 8.25
		p1 (list 0 6.82)
		p2 (list 0 4.95)
		p3 (list 0 3.07)
		p4 (list 0 1.20)
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAtPoint layDef_Zero _th150_ p1 "STREKNING")
	(AddTextAtPoint layDef_Zero _th150_ p2    "UTEN")
	(AddTextAtPoint layDef_Zero _th150_ p3 "LINJEBLOKK")
	(AddTextAtPoint layDef_Zero _th150_ p4  "BEGYNNER")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
