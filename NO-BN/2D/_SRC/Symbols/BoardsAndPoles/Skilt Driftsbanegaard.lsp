;=========================================================================================================================
;
; Skilt Driftsbanegaard.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train depot

; For debugging:
; (SKILT_DRIFTSBANEGAARD) (SKILT_DRIFTSBANEGAARD_SLUTTER)

(defun Skilt_Driftsbanegaard ( / blockName description x y )
	; Start of depot area
	;
	; TL---TR
	; | DBG | ; Letters 'DBG' (Driftsbanegård = Depot)
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-DRIFTSBANEGAARD"
		description (strcat "SKILT DRIFTSBANEG" _uAA_ "RD")
		x 9.0
		y 6.0
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(addTextAtPos layDef_Zero _th250_ _origo_ "DBG")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_Driftsbanegaard_Slutter ( / blockName description x y p1 p2 p3 p4 p5 p6 )
	; End of depot area
	;
	; TL-----------p4--p5
	; |          / / / p6  
	; |     /D/B/G      |
	; p1/ / /           |
	; p2--p3-----------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-DRIFTSBANEGAARD-SLUTTER"
		description (strcat "SKILT DRIFTSBANEG" _uAA_ "RD SLUTTER")
		x	9.0
		y	6.0
		p1 (list (* -0.5 x) (* -0.4 y))
		p2 (list (* -0.5 x) (* -0.5 y))
		p3 (list (* -0.4 x) (* -0.5 y))
		p4 (list (*  0.4 x) (*  0.5 y))
		p5 (list (*  0.5 x) (*  0.5 y))
		p6 (list (*  0.5 x) (*  0.4 y))
  	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(addTextAtPos layDef_Zero _th250_ _origo_ "DBG")
	(setLayer layDef_Zero)
	(command
		_LINE_ p1 p4 _ENTER_
		_LINE_ p2 p5 _ENTER_
		_LINE_ p3 p6 _ENTER_
	)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
