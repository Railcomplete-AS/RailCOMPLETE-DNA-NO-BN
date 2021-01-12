;=========================================================================================================================
;
; Skilt Togvarmeanlegg 1000V.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train heating power connector

; For debugging:
; (SKILT_TOGVARMEANLEGG_1000V)

(defun Skilt_Togvarmeanlegg_1000V ( / blockName description x y )
	; 1000V auxuliary power for parked train heating system
	;
	; TL------TR
	; | 1000 V | ; Letters '1000 V'
	; BL--.---BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-1000V"
		description "SKILT TOGVARMEANLEGG 1000V"
		x (* 25 0.280) ; =7.0
		y (* 25 0.080) ; =3.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th180_ _origo_ "1000 V")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
