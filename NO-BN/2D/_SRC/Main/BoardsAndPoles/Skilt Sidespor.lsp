;=========================================================================================================================
;
; Skilt Sidespor.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Siding

; For debugging:
; (SKILT-SIDESPOR)

(defun SKILT-SIDESPOR ( / blockname description x y attSidingName attKm attBetweenStations )
	; Siding (e.g., industry access track on the line between two railway stations)
	;
	; TL-----------------TR
	; |     SSP_NAVN      |
	; |     sidespor      |
	; |      SSP_KM       |
	; | mellom stasjonene |
	; |   SSP_STASJONER   |
	; BL--------.--------BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIDESPOR"
		description "SKILT SIDESPOR"
		x 12.0
		y 4.5
		attSidingName		'("SSP_NAVN" "Sidespor navn (store bokstaver):" "PEDER AAS INDUSTRIER AS")
		attKm				'("SSP_KM" "Km xxx,xxx:" "Km 132,720")
		attBetweenStations	'("SSP_STASJONER" "Mellom stasjon <xxx> og <yyy>" "Lilleby og Storeby")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th070_ (Point51 y) attSidingName)
	(AddTextAtPoint          layDef_Zero _th050_ (Point52 y) "sidespor")
	(AddTextAttributeAtPoint layDef_Zero _th070_ (Point53 y) attKm)
	(AddTextAtPoint          layDef_Zero _th050_ (Point54 y) "mellom stasjonene")
	(AddTextAttributeAtPoint layDef_Zero _th070_ (Point55 y) attBetweenStations)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
