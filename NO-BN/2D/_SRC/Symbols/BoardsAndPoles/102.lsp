;=========================================================================================================================
;
; 102.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Arrow board

; For debugging:
; (102-1) (102-2) (102-3)

(defun 102-1 ( / blockName description x y )
	; Right arrow board
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-102-1-PIL-H"
		description (strcat "SKILT SIGNAL 102-1 PIL H" _uOE_ "YRE")
		x 6.0
		y 3.0
	)
	; Schematic symbol
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawRightArrow x y)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 102-2 ( / blockName description x y )
	; Left arrow board
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-102-2-PIL-V"
		description "SKILT SIGNAL 102-2 PIL VENSTRE"
		x 6.0
		y 3.0
	)
	; Schematic symbol
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawLeftArrow x y)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)

	description ; Used if table is created
)



(defun 102-3 ( / blockName description x y arrowShaftStartX arrowShaftLength arrowHeadX arrowHeadY )
	; Right and left arrow board
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-102-3-PIL-HV"
		description "SKILT SIGNAL 102-3 PIL BEGGE VEIER"
		x 6.0
		y 3.0
	)
	; Schematic symbol
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawDoubleArrow x y)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)

	description ; Used if table is created
)
