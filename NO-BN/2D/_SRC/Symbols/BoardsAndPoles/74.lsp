;=========================================================================================================================
;
; 74.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
	; |  7 4 2 m  |
	; BL----.----BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-74-1-TOGLENGDE1"
		description "SKILT 74-1 TOGLENGDESKILT ALT 1"
		x 7.5
		y 3.0
		attDef '("TOGLENGDE" "Toglengde" "220m")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPos layDef_Zero _th250_ _origo_ attDef)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 74-2 ( / blockName description x y p1 p2 p3 p4 attHundred attTen attOne )
	; Train length (old type, narrow stacked)
	;
	; TL---TR
	; |  7  |  ; p1
	; |  4  |  ; p2
	; |  2  |  ; p3
	; |  m  |  ; p4
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-74-2-TOGLENGDE2"
		description "SKILT 74-2 TOGLENGDESKILT ALT 2"
		x 3.0
		y 7.5
		p1 (list 0 6.50)
		p2 (list 0 4.55)
		p3 (list 0 2.65)
		p4 (list 0 1.00)
		attHundred	'("HUNDRE_M" "100 m" "2")
		attTen		'("TI_M" 	"10 m" "2")
		attOne		'("EN_M" 	 "1 m" "0")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAttributeAtPos layDef_Zero _th180_ p1 attHundred)
	(AddTextAttributeAtPos layDef_Zero _th180_ p2 attTen)
	(AddTextAttributeAtPos layDef_Zero _th180_ p3 attOne)
	(AddTextAtPos layDef_Zero _th180_ p4 "m")
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
