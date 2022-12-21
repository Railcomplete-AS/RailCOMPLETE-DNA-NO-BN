;=========================================================================================================================
;
; 69.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
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
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-69A-MIDLERTIDIG-NEDSATT"
		description "SKILT SIGNAL 69A MIDLERTIDIG NEDSATT HASTIGHET"
		p1  '(-1.875 7.125)
		p2  '(-1.875 2.250)
		p3  '( 1.875 2.250)
		p4  '( 1.875 7.125)
		p5  '( 0.0 4.5)
		attTen '("HAST_10" "Vent-hastighet (10-ere)" "4")
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 _origin_ p3 p4 _closedPolyline_)
	(AddTextAttributeAtPoint layDef_Zero _th250_ p5 attTen)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
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
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-69B-MIDLERTIDIG-OPPHOERER"
		description (strcat "SKILT SIGNAL 69B MIDLERTIDIG HASTIGHET OPPH" _uOSLASH_ "RER")
		p1  '(-1.875 0.000)
		p2  '(-1.875 4.875)
		p3  '( 0.000 7.125)
		p4  '( 1.875 4.875)
		p5  '( 1.875 0.000)
		p6  '( 0.0 2.5)
		attTen '("HAST_10" (strcat "Gjenoppta kj" _OSLASH_ "r-hastighet (10-ere)") "9")
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 p4 p5 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(AddTextAttributeAtPoint layDef_Zero _th250_ p6 attTen)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
