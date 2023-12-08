;=========================================================================================================================
;
; 63.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Gradient

; For debugging:
; (63A) (63B)

(defun 63A ( / blockName description p1 p2 p3 p4 attTag attPrompt attDefaultValue )
	; 
	; p1
	; | \ 
	; |  \ 
	; |   \   Semaphore with number inside, at p4
	; .    \
	;  \ p4 \
	;   \   p2
	;    \  /
	;     p3
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-63A-FALLVISER"
		description "SKILT SIGNAL 63A FALLVISER"
		p1	(list 0.00  3.50)
		p2	(list 6.25 -2.50)
		p3	(list 4.25 -4.25)
		p4	(list 2.75 -1.00)
		attTag			"GRADIENT"
		attPrompt		"FAll"
		attDefaultValue	"18"
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(SetLayer layDef_Zero)
	(AddAtt attTag attPrompt attDefaultValue p4 _th180_ _angleMinus45_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description -2)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 63B ( / blockName description p1 p2 p3 p4 attTag attPrompt attDefaultValue )
	; 
	;    p2
	;   /  \
	;  /   p3
	; / p4 /   Semaphore with number inside, at p4
	; p1  /
	; |  /
	; | /
	; ./
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-63B-STIGNINGSVISER"
		description "SKILT SIGNAL 63B STIGNINGSVISER"
		p1	(list 0.00 3.50)
		p2	(list 4.25 7.75)
		p3	(list 6.00 6.00)
		p4	(list 2.75 4.50)
		attTag			"GRADIENT"
		attPrompt		"Stigning"
		attDefaultValue	"11"
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 _closedPolyline_)
	(SetLayer layDef_BoardOrPole_Wipeout)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(SetLayer layDef_Zero)
	(AddAtt attTag attPrompt attDefaultValue p4 _th180_ _angle45_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
