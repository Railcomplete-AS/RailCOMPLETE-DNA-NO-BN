;=========================================================================================================================
;
; Skilt Anleggsomraade.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Possession (civil works area / railway operational rules temporarily suspended)

; For debugging:
; (SKILT_ANLEGGSOMRAADE_BEGYNNER (SKILT_ANLEGGSOMRAADE_SLUTTER)

(defun Skilt_Anleggsomraade_begynner ( / blockName description x y )
	; Start of possession
	;
	; TL-------TR
	; | Anleggs-|   p9
	; |  område |   p10
	; | jernbane|   p11
	; BL---.---BR
	; 
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-ANLEGGSOMRAADE-BEGYNNER"
		description (Strcat "SKILT ANLEGGSOMR" _uAA_ "DE BEGYNNER")
		x 6.0
		y 4.5
		p9  (list 0 (*  0.322 y))
		p10	_origo_
		p11 (list 0 (* -0.267 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th100_ p9 "Anleggs-")
	(addTextAtPos layer_Zero _th100_ p10 (strcat "omr" _aa_ "de"))
	(addTextAtPos layer_Zero _th100_ p11 "jernbane")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun Skilt_Anleggsomraade_slutter ( / blockName description x y p1 p2 p3 p4 p5 p6 )
	; End of possession
	;
	; +------2-4+
	; |     / / 6
	; | Anl/g/s/8   p9
	; |/o/r/d/  |   p10
	; 1j/r/bane |   p11
	; 3 / /     |
	; 5-7--.----+
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-ANLEGGSOMRAADE-SLUTTER"
		description (strcat "SKILT ANLEGGSOMR" _uAA_ "DE SLUTTER")
		x 6.0
		y 4.5
		p1 (list (* -0.50 x) (* -0.30 y))
		p2 (list (*  0.35 x) (*  0.50 y))
		p3 (list (* -0.50 x) (* -0.40 y))
		p4 (list (*  0.45 x) (*  0.50 y))
		p5 (list (* -0.50 x) (* -0.50 y))
		p6 (list (*  0.50 x) (*  0.45 y))
		p7 (list (* -0.40 x) (* -0.50 y))
		p8 (list (*  0.50 x) (*  0.35 y))
		p9  (list 0 (*  0.322 y))
		p10	_origo_
		p11 (list 0 (* -0.267 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th100_ p9 "Anleggs-")
	(addTextAtPos layer_Zero _th100_ p10 (strcat "omr" _aa_ "de"))
	(addTextAtPos layer_Zero _th100_ p11 "jernbane")
	(drawLine layer_Zero p1 p2)
	(drawLine layer_Zero p3 p4)
	(drawLine layer_Zero p5 p6)
	(drawLine layer_Zero p7 p8)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
