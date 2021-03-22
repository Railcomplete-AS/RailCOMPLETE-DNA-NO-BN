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
; (SKILT-UNDERSKILT-METERANGIVELSE)

(defun SKILT-UNDERSKILT-METERANGIVELSE ( / blockName description x y attMeters )
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
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPos layDef_Zero _th250_ _origo_ attMeters)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
