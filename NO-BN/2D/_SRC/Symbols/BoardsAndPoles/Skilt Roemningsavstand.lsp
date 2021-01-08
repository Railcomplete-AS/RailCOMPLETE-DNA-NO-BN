;=========================================================================================================================
;
; Skilt Roemningsavstand.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distance to nearest escape route

; For debugging:
; (ROEMNINGSAVSTAND_V) (ROEMNINGSAVSTAND_H) (ROEMNINGSAVSTAND_VH)

(defun Skilt_Roemningsavstand_V ( / blockName description x y p0 p1 p3 attMetersLeft )
	; Distance to nearest escape towards the left
	;
	; TL-----------TR
	; | RØMNINGSVEI |
	; |     <==     | ; p1 = tip
	; |     275     | ; p3
	; BL-----.-----BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-TREDJEPERSON-ROEMNINGSVEI-V"
		description (strcat "R" _uOE_ "MNINGSAVSTAND MOT VENSTRE")
		x 12.0
		y 9.0
		p0 (list (*  0.00 x) (*  0.3 y))
		p1 (list (* -0.15 x) (*  0.0 y))
		p3 (list (*  0.00 x) (* -0.3 y))
		attMetersLeft	'("VDIST" (strcat "Avstand til r" _oe_ "mning mot venstre") "275")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)

	(addTextAtPos layer_Zero _th125_ p0 (strcat "R" _uOE_ "MNINGSVEI"))
	(drawHollowArrowAtpoint x y p1 _west_)
	(addTextAttributeAtPos layer_Zero _th180_ p3 attMetersLeft)

	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun Skilt_Roemningsavstand_H ( / blockName description x y p0 p2 p4 attMetersRight )
	; Distance to nearest escape towards the right
	;
	; TL-----------TR
	; | RØMNINGSVEI |
	; |     ==>     | ; p2 = tip
	; |     625     | ; p4
	; BL-----.-----BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-TREDJEPERSON-ROEMNINGSVEI-H"
		description (strcat "R" _uOE_ "MNINGSAVSTAND MOT H" _uOE_ "YRE")
		x 12.0
		y 9.0
		p0 (list (*  0.00 x) (*  0.3 y))
		p2 (list (*  0.15 x) (*  0.0 y))
		p4 (list (*  0.00 x) (* -0.3 y))
		attMetersRight	'("HDIST" (strcat "Avstand til r" _oe_ "mning mot h" _oe_ "yre") "625")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)

	(addTextAtPos layer_Zero _th125_ p0 (strcat "R" _uOE_ "MNINGSVEI"))
	(drawHollowArrowAtpoint x y p2 _east_)
	(addTextAttributeAtPos layer_Zero _th180_ p4 attMetersRight)

	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun Skilt_Roemningsavstand_VH ( / blockName description x y p0 p1 p2 p3 p4 attMetersLeft attMetersRight )
	; Distance to nearest escape towards the right
	;
	; TL-----------TR
	; | RØMNINGSVEI |
	; |  <==   ==>  | ; p1 p2
	; |  275   625  | ; p3 p4
	; BL-----.-----BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-TREDJEPERSON-ROEMNINGSVEI-VH"
		description (strcat "R" _uOE_ "MNINGSAVSTAND MOT H" _uOE_ "YRE OV VENSTRE")
		x 12.0
		y 9.0
		p0 (list (*  0.0  x) (*  0.3 y))
		p1 (list (* -0.4  x) (*  0.0 y))
		p3 (list (* -0.25 x) (* -0.3 y))
		p2 (list (*  0.4  x) (*  0.0 y))
		p4 (list (*  0.25 x) (* -0.3 y))
		attMetersLeft	'("VDIST" (strcat "Avstand til r" _oe_ "mning mot venstre") "275")
		attMetersRight	'("HDIST" (strcat "Avstand til r" _oe_ "mning mot h" _oe_ "yre") "625")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)

	(addTextAtPos layer_Zero _th125_ p0 (strcat "R" _uOE_ "MNINGSVEI"))
	(drawHollowArrowAtpoint x y p1 _west_)
	(addTextAttributeAtPos layer_Zero _th180_ p3 attMetersLeft)

	(addTextAtPos layer_Zero _th125_ p0 (strcat "R" _uOE_ "MNINGSVEI"))
	(drawHollowArrowAtpoint x y p2 _east_)
	(addTextAttributeAtPos layer_Zero _th180_ p4 attMetersRight)

	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
