;=========================================================================================================================
;
; 74.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train length

; For debugging:
; (74-1) (74-2)

(defun 74-1 ( / blockname description x y attDef )
	; Train length
	;
	; TL---------TR
	; |  1 1 0 m  |
	; BL----.----BR
	;
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-74-1-TOGLENGDE1"
		description "SKILT 74-1 TOGLENGDESKILT ALT 1"
		x 7.5
		y 3.0
		attDef '("TOGLENGDE" "Toglengde" "110m")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th180_ _origin_ attDef)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 74-2 ( / blockName description x y p1 p2 p3 p4 attHundred attTen attOne )
	; Train length (old type, narrow stacked)
	;
	; TL---TR
	; |  1  |  ; p1
	; |  1  |  ; p2
	; |  0  |  ; p3
	; |  m  |  ; p4
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-74-2-TOGLENGDE2"
		description "SKILT 74-2 TOGLENGDESKILT ALT 2"
		x 3.0
		y 7.5
		p1 (list 0 6.50)
		p2 (list 0 4.55)
		p3 (list 0 2.65)
		p4 (list 0 1.00)
		attHundred	'("HUNDRE_M" "100 m" "1")
		attTen		'("TI_M" 	"10 m" "1")
		attOne		'("EN_M" 	 "1 m" "0")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAttributeAtPoint layDef_Zero _th150_ p1 attHundred)
	(AddTextAttributeAtPoint layDef_Zero _th150_ p2 attTen)
	(AddTextAttributeAtPoint layDef_Zero _th150_ p3 attOne)
	(AddTextAtPoint layDef_Zero _th150_ p4 "m")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
