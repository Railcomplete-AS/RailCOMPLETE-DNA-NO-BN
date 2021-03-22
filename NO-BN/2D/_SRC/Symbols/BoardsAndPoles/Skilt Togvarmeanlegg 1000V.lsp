;=========================================================================================================================
;
; Skilt Togvarmeanlegg 1000V.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Train heating power connector

; For debugging:
; (SKILT-TOGVARMEANLEGG-1000V)

(defun SKILT-TOGVARMEANLEGG-1000V ( / blockName description x y )
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
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th180_ _origo_ "1000 V")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
