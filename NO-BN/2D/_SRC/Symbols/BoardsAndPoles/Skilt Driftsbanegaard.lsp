;=========================================================================================================================
;
; Skilt Driftsbanegaard.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "DBG")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "DBG")
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
