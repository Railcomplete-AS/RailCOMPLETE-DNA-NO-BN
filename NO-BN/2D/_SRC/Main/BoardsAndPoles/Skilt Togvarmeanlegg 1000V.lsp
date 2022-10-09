;=========================================================================================================================
;
; Skilt Togvarmeanlegg 1000V.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
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
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-1000V"
		description "SKILT TOGVARMEANLEGG 1000V"
		x (* 25 0.280) ; =7.0
		y (* 25 0.080) ; =3.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th180_ _origin_ "1000 V")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
