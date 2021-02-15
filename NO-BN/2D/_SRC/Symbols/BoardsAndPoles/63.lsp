;=========================================================================================================================
;
; 63.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-63A-FALLVISER"
		description "SKILT SIGNAL 63A FALLVISER"
		p1	(list 0.00  3.50)
		p2	(list 6.25 -2.50)
		p3	(list 4.25 -4.25)
		p4	(list 2.75 -1.00)
		attTag			"GRADIENT"
		attPrompt		"FAll"
		attDefaultValue	"18"
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ _origo_ p1 p2 p3 _closedPolyline_)
	(addWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)
	(setLayer layDef_Zero)
	(addAtt attTag attPrompt attDefaultValue p4 _th180_ _angle315_ _rcTextStyle_ _middleCenter_ _lockPosition_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-63B-STIGNINGSVISER"
		description "SKILT SIGNAL 63B STIGNINGSVISER"
		p1	(list 0.00 3.50)
		p2	(list 4.25 7.75)
		p3	(list 6.00 6.00)
		p4	(list 2.75 4.50)
		attTag			"GRADIENT"
		attPrompt		"Stigning"
		attDefaultValue	"11"
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ _origo_ p1 p2 p3 _closedPolyline_)
	(setLayer layDef_BoardOrPole_Wipeout)
	(command _WIPEOUT_ _createWipeoutFromPolyline_ _lastSelection_ _ENTER_ _eraseWipeoutSource_)
	(setLayer layDef_Zero)
	(addAtt attTag attPrompt attDefaultValue p4 _th180_ _angle45_ _rcTextStyle_ _middleCenter_ _lockPosition_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
