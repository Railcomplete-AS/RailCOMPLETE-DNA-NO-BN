;=========================================================================================================================
;
; Skilt Fritekst.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; 'Free text' board / End of 'Free text'

; For debugging:
; (SKILT-FRITEKST) (SKILT-FRITEKST-SLUTTER)

(defun SKILT-FRITEKST ( / blockName description x y attFreeText )
	; Start of 'free text' area
	;
	; TL---TR
	; | FT  | ; Text Attribute 'FREE_TEXT'
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-FRITEKST"
		description "SKILT FRITEKST"
		x 9.0
		y 6.0
		attFreeText '("FREE_TEXT" "Enter free text: " "XYZZY")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th250_ _origin_ attFreeText)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun SKILT-FRITEKST-SLUTTER ( / blockName description x y attFreeText p1 p2 p3 p4 p5 p6 )
	; End of 'free text' area
	;
	; TL-----------p4--p5
	; |          / / / p6  
	; |     /F/T/       | ; Text Attribute 'FREE_TEXT'
	; p1/ / /           |
	; p2--p3-----------BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-FRITEKST-SLUTTER"
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
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th250_ _origin_ attFreeText)
	(SetLayer layDef_Zero)
	(command
		_LINE_ p1 p4 _ENTER_
		_LINE_ p2 p5 _ENTER_
		_LINE_ p3 p6 _ENTER_
	)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
