;=========================================================================================================================
;
; Skilt Underskilt meterangivelse.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
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
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-UNDERSKILT-METERANGIVELSE"
		description "SKILT UNDERSKILT METERANGIVELSE"
		x 9.0
		y 3.0
		attMeters '("AVSTAND" "Avstand (m):" "300m")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th250_ _origin_ attMeters)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
