;=========================================================================================================================
;
; Skilt Driftsbanegaard.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train depot

; For debugging:
; (SKILT-DRIFTSBANEGAARD) (SKILT-DRIFTSBANEGAARD-SLUTTER)

(defun SKILT-DRIFTSBANEGAARD ( / blockName description x y )
	; Start of depot area
	;
	; TL---TR
	; | DBG | ; Letters 'DBG' (Driftsbanegård = Depot)
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-DRIFTSBANEGAARD"
		description (strcat "SKILT DRIFTSBANEG" _uARING_ "RD")
		x 9.0
		y 6.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th250_ _origin_ "DBG")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(AddDescriptionBelowOrigin description 2)
	(AddDescriptionBelowOrigin description 4)
	(AddDescriptionBelowOrigin description 6)
	(AddDescriptionBelowOrigin description 8)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun SKILT-DRIFTSBANEGAARD-SLUTTER ( / blockName description x y p1 p2 p3 p4 p5 p6 )
	; End of depot area
	;
	; TL-----------p4--p5
	; |          / / / p6  
	; |     /D/B/G      |
	; p1/ / /           |
	; p2--p3-----------BR
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-DRIFTSBANEGAARD-SLUTTER"
		description (strcat "SKILT DRIFTSBANEG" _uARING_ "RD SLUTTER")
		x	9.0
		y	6.0
		p1 (list (* -0.5 x) (* -0.4 y))
		p2 (list (* -0.5 x) (* -0.5 y))
		p3 (list (* -0.4 x) (* -0.5 y))
		p4 (list (*  0.4 x) (*  0.5 y))
		p5 (list (*  0.5 x) (*  0.5 y))
		p6 (list (*  0.5 x) (*  0.4 y))
  	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPoint layDef_Zero _th250_ _origin_ "DBG")
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
