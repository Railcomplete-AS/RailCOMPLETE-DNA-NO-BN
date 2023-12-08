;=========================================================================================================================
;
; Skilt Anleggsomraade.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Possession (civil works area / railway operational rules temporarily suspended)

; For debugging:
; (SKILT-ANLEGGSOMRAADE-BEGYNNER (SKILT-ANLEGGSOMRAADE-SLUTTER)

(defun SKILT-ANLEGGSOMRAADE-BEGYNNER ( / blockName description x y p9 p10 p11 )
	; Start of possession
	;
	; TL-------TR
	; | Anleggs-|   p9
	; |  område |   p10
	; | jernbane|   p11
	; BL---.---BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-ANLEGGSOMRAADE-BEGYNNER"
		description (strcat "SKILT ANLEGGSOMR" _uARING_ "DE BEGYNNER")
		x 6.0
		y 4.5
		p9  (list 0 (*  0.322 y))
		p10	_origin_
		p11 (list 0 (* -0.267 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th100_ p9 "Anleggs-")
	(AddTextAtPoint layDef_Zero _th100_ p10 (strcat "omr" _ARING_ "de"))
	(AddTextAtPoint layDef_Zero _th100_ p11 "jernbane")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun SKILT-ANLEGGSOMRAADE-SLUTTER ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 )
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
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-ANLEGGSOMRAADE-SLUTTER"
		description (strcat "SKILT ANLEGGSOMR" _uARING_ "DE SLUTTER")
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
		p10	_origin_
		p11 (list 0 (* -0.267 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th100_ p9 "Anleggs-")
	(AddTextAtPoint layDef_Zero _th100_ p10 (strcat "omr" _ARING_ "de"))
	(AddTextAtPoint layDef_Zero _th100_ p11 "jernbane")
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(DrawLine layDef_Zero p7 p8)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
