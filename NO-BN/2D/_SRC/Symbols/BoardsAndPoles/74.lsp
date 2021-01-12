;=========================================================================================================================
;
; 74.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train length

; For debugging:
; (74-1) (74-2)

(defun 74-1 ( / blockname description dim1 dim2 )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th250_ _origo_ attDef)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 74-2 ( / blockName description dim1 dim2 )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(addTextAttributeAtPos layer_Zero _th180_ p1 attHundred)
	(addTextAttributeAtPos layer_Zero _th180_ p2 attTen)
	(addTextAttributeAtPos layer_Zero _th180_ p3 attOne)
	(addTextAtPos layer_Zero _th180_ p4 "m")
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
