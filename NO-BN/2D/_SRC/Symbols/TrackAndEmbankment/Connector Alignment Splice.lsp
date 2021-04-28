;=========================================================================================================================
;
; Connector Alignment Splice.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

; Concerns schematic symbols that scale with paperspace drawing scale.

(defun C:CONNECTOR-SPLICE ( / )
	; Connects an alignment to another alignment when it has been recognized as a valid extension of the first alignment.
	; The symbols look like this: [>>], [><], [<>], and [<<]. The arrows represent each alignment's direction of increasing mileages.
	(SetCadSystemDefaults)  
	(CONNECTOR-SPLICE 0 0) ; [> >]
	(CONNECTOR-SPLICE 0 1) ; [> <]
	(CONNECTOR-SPLICE 1 0) ; [< >]
	(CONNECTOR-SPLICE 1 1) ; [< <]
)



(defun CONNECTOR-SPLICE ( leftArrow rightArrow / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 )
	;
	; 1                   3
	; |         x         | 
	; TL--5--6-----7--8--TR   ^
	; |    \        \     |   |
	; |     \        \    |   |
	; |   9 10    11 12   |   y
	; |     /        /    |   |
	; |    /        /     |   |
	; BL-13-14----15-16--BR   v
	; |                   |
	; 2                   4
	;
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-CONNECTOR-SPLICE-" (rtos leftArrow 2 0) "-" (rtos rightArrow 2 0))
		description (strcat "SPLICE " (if (= leftArrow 0) "> " "< ") (if (= rightArrow 0) ">" "<"))
		x	3.0
		y	1.0
		p1	(list (* -0.500 x) (*  1.000 y))
		p2	(list (* -0.500 x) (* -1.000 y))
		p3	(list (*  0.500 x) (*  1.000 y))
		p4	(list (*  0.500 x) (* -1.000 y))

		p5	(list (* -0.333 x) (*  0.500 y))
		p6	(list (* -0.167 x) (*  0.500 y))
		p7	(list (*  0.167 x) (*  0.500 y))
		p8	(list (*  0.333 x) (*  0.500 y))
		
		p9	(list (* -0.333 x) (*  0.000 y))
		p10 (list (* -0.167 x) (*  0.000 y))
		p11 (list (*  0.167 x) (*  0.000 y))
		p12	(list (*  0.333 x) (*  0.000 y))

		p13 (list (* -0.333 x) (* -0.500 y))
		p14 (list (* -0.167 x) (* -0.500 y))
		p15 (list (*  0.167 x) (* -0.500 y))
		p16	(list (*  0.333 x) (* -0.500 y))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(if (= leftArrow 0)
		(command _POLYLINE_ p5 p10 p13 _openPolyline_)
		(command _POLYLINE_ p6 p9 p14 _openPolyline_)
	)
	(if (= rightArrow 0)
		(command _POLYLINE_ p7 p12 p15 _openPolyline_)
		(command _POLYLINE_ p8 p11 p16 _openPolyline_)
	)
	(AddDescriptionBelowOrigo description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
