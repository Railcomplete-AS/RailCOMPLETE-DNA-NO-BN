;=========================================================================================================================
;
; Skilt StoppSeOgLytt.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Stop, listen (before crossing track at an unprotected level crossing)

; For debugging:
; (SKILT_STOPPSEOGLYTT)

(defun Skilt_StoppSeOgLytt ( / blockName description x y )
	; Warning for pedestrians or cars - Stop, look out for trains
	;
	; +----------------------+
	; |        STOPP         |
	; | Se og lytt etter tog |
	; +----------.-----------+
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-TREDJEPERSON-STOPPSEOGLYTT"
		description "SKILT STOPP SE OG LYTT ETTER TOG"
		x 9.0
		y 4.5
		p1 '(0 3.0)
		p2 '(0 1.0)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(addTextAtPos layer_Zero _th180_ p1       "STOPP"         )
	(addTextAtPos layer_Zero _th070_ p2 "Se og lytt etter tog")
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
