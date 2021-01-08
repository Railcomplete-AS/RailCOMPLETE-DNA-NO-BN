;=========================================================================================================================
;
; 63.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
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
	(setLayer layer_Zero)
	(command "._PLINE" _origo_ p1 p2 p3 _closed_)
	(addWipeoutToLastClosedPolyline layer_BoardOrPole_Wipeout _erase_)
	(setLayer layer_Zero)
	(addAtt attTag attPrompt attDefaultValue p4 _th180_ 315 "iso" "_MC" _lockPosition_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	(setLayer layer_Zero)
	(command "._PLINE" _origo_ p1 p2 p3 _closed_)
	(setLayer layer_BoardOrPole_Wipeout)
	(command "._WIPEOUT" "_POLYLINE" "_LAST" "" "_YES")
	(setLayer layer_Zero)
	(addAtt attTag attPrompt attDefaultValue p4 _th180_ 45 "iso" "_MC" _lockPosition_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
