;=========================================================================================================================
;
; Skilt Strekning uten linjeblokk.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Start of line without automatic blocking system

; For debugging:
; (SKILT_STREKNING_UTEN_LINJEBLOKK)

(defun Skilt_Strekning_uten_linjeblokk ( / blockName description x y )
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-STREKNING-UTEN-LINJEBLOKK"
		description "SKILT STREKNING UTEN LINJEBLOKK"
		x 9.75
		y 8.25
		p1 (list 0 6.82)
		p2 (list 0 4.95)
		p3 (list 0 3.07)
		p4 (list 0 1.20)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(addTextAtPos layer_Zero _th150_ p1 "STREKNING")
	(addTextAtPos layer_Zero _th150_ p2    "UTEN")
	(addTextAtPos layer_Zero _th150_ p3 "LINJEBLOKK")
	(addTextAtPos layer_Zero _th150_ p4  "BEGYNNER")
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
