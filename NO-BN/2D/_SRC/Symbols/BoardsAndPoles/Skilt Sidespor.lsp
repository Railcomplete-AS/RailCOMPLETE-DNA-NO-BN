;=========================================================================================================================
;
; Skilt Sidespor.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Siding

; For debugging:
; (SKILT_SIDESPOR)

(defun Skilt_Sidespor ( / blockname description x y attSidingName attKm attBetweenStations )
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIDESPOR"
		description "SKILT SIDESPOR"
		x 12.0
		y 4.5
		attSidingName		'("SSP_NAVN" "Sidespor navn (store bokstaver):" "PEDER AAS INDUSTRIER AS")
		attKm				'("SSP_KM" "Km xxx,xxx:" "Km 132,720")
		attBetweenStations	'("SSP_STASJONER" "Mellom stasjon <xxx> og <yyy>" "Lilleby og Storeby")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th070_ (pos51 y) attSidingName)
	(addTextAtPos          layer_Zero _th050_ (pos52 y) "sidespor")
	(addTextAttributeAtPos layer_Zero _th070_ (pos53 y) attKm)
	(addTextAtPos          layer_Zero _th050_ (pos54 y) "mellom stasjonene")
	(addTextAttributeAtPos layer_Zero _th070_ (pos55 y) attBetweenStations)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
