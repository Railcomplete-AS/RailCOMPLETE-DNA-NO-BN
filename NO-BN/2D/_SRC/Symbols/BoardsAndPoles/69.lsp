;=========================================================================================================================
;
; 69.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Temporary speed restriction

; For debugging:
; (69A) (69B)

(defun 69A ( / blockName description p1 p2 p3 p4 p5 attTen )
	; Temporary speed restriction warning
	;
	;     1-------4 
	;     |       |
	;     |       |
	;     |       |
	;     |   5   | ; Reduced speed ahead
	;     |       |
	;     2       3
	;      \     /  
	;       \   /   
	;        \ /
	;         .
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-69A-MIDLERTIDIG-NEDSATT"
		description "SKILT SIGNAL 69A MIDLERTIDIG NEDSATT HASTIGHET"
		p1  '(-1.875 7.125)
		p2  '(-1.875 2.250)
		p3  '( 1.875 2.250)
		p4  '( 1.875 7.125)
		p5  '( 0.0 4.5)
		attTen '("HAST_10" "Vent-hastighet (10-ere)" "4")
	)
	(setLayer layer_Zero)
	(command "._PLINE" p1 p2 _origo_ p3 p4 _closed_)
	(addTextAttributeAtPos layer_Zero _th250_ p5 attTen)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 69B ( / blockName description p1 p2 p3 p4 p5 p6 attTen )
	; Temporary speed restriction lifted
	;
	;         3
	;        / \
	;       /   \   
	;      /     \  
	;     2       4
	;     |       |
	;     |   6   | ; Recap ordinary speed
	;     |       |
	;     |       |
	;     |       |
	;     1---.---5 
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-69B-MIDLERTIDIG-OPPHOERER"
		description "SKILT SIGNAL 69B MIDLERTIDIG HASTIGHET OPPHØRER"
		p1  '(-1.875 0.000)
		p2  '(-1.875 4.875)
		p3  '( 0.000 7.125)
		p4  '( 1.875 4.875)
		p5  '( 1.875 0.000)
		p6  '( 0.0 2.5)
		attTen '("HAST_10" (strcat "Gjenoppta kj" _oe_ "r-hastighet (10-ere)") "9")
	)
	(setLayer layer_Zero)
	(command "._PLINE" p1 p2 p3 p4 p5 _closed_)
	(addWipeoutToLastClosedPolyline layer_BoardOrPole_Wipeout _keep_)
	(addTextAttributeAtPos layer_Zero _th250_ p6 attTen)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
