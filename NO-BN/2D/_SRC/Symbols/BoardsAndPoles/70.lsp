;=========================================================================================================================
;
; 70.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Level crossing announcement

; For debugging:
; (70A)

(defun 70A ( / blockName description x y )
	; Level crossing announcement
	;
	; TL---TR
	; |     |
	; |  V  | ; Letter 'V'
	; |     |
	; BL-.-BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-70-PLANOVERGANGSSKILT"
		description "SKILT 70 PLANOVERGANG"
		x 3.0
		y 4.5
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "V")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
