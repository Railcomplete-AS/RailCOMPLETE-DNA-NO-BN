;=========================================================================================================================
;
; Skilt Underskilt meterangivelse.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distance in meters

; For debugging:
; (SKILT_UNDERSKILT_METERANGIVELSE)

(defun Skilt_Underskilt_meterangivelse ( / blockName description x y attMeters )
	; Distance in meters to target
	;
	; +-------+
	; | 1250m |
	; +---.---+
	; 
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-UNDERSKILT-METERANGIVELSE"
		description "SKILT UNDERSKILT METERANGIVELSE"
		x 9.0
		y 3.0
		attMeters '("AVSTAND" "Avstand (m):" "300m")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th250_ _origo_ attMeters)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
