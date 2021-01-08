;=========================================================================================================================
;
; Skilt Fritekst.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; 'Free text' board / End of 'Free text'

; For debugging:
; (SKILT_FRITEKST) (SKILT_FRITEKST_SLUTTER)

(defun Skilt_Fritekst ( / blockName description x y )
	; Start of 'free text' area
	;
	; TL---TR
	; | FT  | ; Text Attribute 'FREE_TEXT'
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-FRITEKST"
		description "SKILT FRITEKST"
		x 9.0
		y 6.0
		attFreeText '("FREE_TEXT" "Enter free text: " "XYZZY")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th250_ _origo_ attFreeText)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun Skilt_Fritekst_Slutter ( / blockName description x y p1 p2 p3 p4 p5 p6 )
	; End of 'free text' area
	;
	; TL-----------p4--p5
	; |          / / / p6  
	; |     /F/T/       | ; Text Attribute 'FREE_TEXT'
	; p1/ / /           |
	; p2--p3-----------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-FRITEKST-SLUTTER"
		description "SKILT FRITEKST SLUTTER"
		x	9.0
		y	6.0
		attFreeText '("FREE_TEXT" "Enter free text: " "XYZZY")
		p1 (list (* -0.5 x) (* -0.4 y))
		p2 (list (* -0.5 x) (* -0.5 y))
		p3 (list (* -0.4 x) (* -0.5 y))
		p4 (list (*  0.4 x) (*  0.5 y))
		p5 (list (*  0.5 x) (*  0.5 y))
		p6 (list (*  0.5 x) (*  0.4 y))
  	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th250_ _origo_ attFreeText)
	(setLayer layer_Zero)
	(command
		"._LINE" p1 p4 ""
		"._LINE" p2 p5 ""
		"._LINE" p3 p6 ""
	)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
