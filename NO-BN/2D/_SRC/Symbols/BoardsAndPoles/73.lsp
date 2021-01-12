;=========================================================================================================================
;
; 73.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Avalanche warning

; For debugging:
; (73A)

(defun 73A ( / blockName description x y r1 r2 ) 
	; Avalanche warning
	;
	; TL-----TR
	; |       |
	; | ( R ) | ; Letter 'R' in a circle
	; |       |
	; BL--.--BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-73-RASVARSEL"
		description "SKILT 73 RASVARSLINGSSKILT"
		x 4.5
		y 4.5
		r 1.5
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawCircle layer_Zero r _noWipeout_)
	(addTextAtPos layer_Zero (* 0.5 x) _origo_ "R") ; Letter size is 45% of side
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
