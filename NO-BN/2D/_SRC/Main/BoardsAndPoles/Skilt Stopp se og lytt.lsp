;=========================================================================================================================
;
; Skilt Stopp se og lytt.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Stop, listen (before crossing track at an unprotected level crossing)

; For debugging:
; (SKILT-STOPP-SE-OG-LYTT)

(defun SKILT-STOPP-SE-OG-LYTT ( / blockName description x y p1 p2 )
	; Warning for pedestrians or cars - Stop, look out for trains
	;
	; +----------------------+
	; |        STOPP         |
	; | Se og lytt etter tog |
	; +----------.-----------+
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-STOPPSEOGLYTT"
		description "SKILT STOPP SE OG LYTT ETTER TOG"
		x 9.0
		y 4.5
		p1 '(0 3.0)
		p2 '(0 1.0)
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAtPoint layDef_Zero _th180_ p1       "STOPP"         )
	(AddTextAtPoint layDef_Zero _th070_ p2 "Se og lytt etter tog")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
