;=========================================================================================================================
;
; Power Distribution Cabinet.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Power distribution xabinet

(defun POWER-DISTRIBUTION-CABINET ( / )
	(FORDELINGSSKAP)	; Fordelingsskap
	(GRUPPESKAP)		; Gruppeskap (=sporvekselvarmeskap)
	(TOGVARMEPOST)		; Skap for togvarme
)


 
(defun FORDELINGSSKAP ( / blockName description x y y2 p1 p2 p3 p4 p5 )
	;
	; +-----------------4
	; |               / |
	; |             /   |
	; |        2  /     |
	; |       /./       |    
	; |     /  3        |
	; |   /             |
	; | /               |
	; 1-----------------+
	; +-----------------+
	;
	(setq 
		blockName	(strcat _POW_ "FSP-" "SKAP-FORDELINGSSKAP")
		description "LAVSPENT FORDELINGSSKAP"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
		p1 (AddVectors (PosBL x y) (list 0 (/ y 9)))
		p2 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		p3 (list (/ x -40) (+ (/ y -6) (/ y 18)))
		p4 (PosTR x y)
		gx 1.2 ; Large outdoor cabinet ??
		gy 0.5
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawBottomEmphasis layDef_Zero x y)
	(command _POLYLINE_ p1 p3 p4 p5 _openPolyline_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_Zero gx gy)
	(AddTextAtPos layDef_Zero (* 0.8 gy) _origin_ "Ford.")
	(ScaleAll _four_)		; Up to 1:1000 size
	(AddDescriptionBelowOrigin description (HalfOf gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun GRUPPESKAP ( / blockName description x y p1 p2 p3 p4 gx gy )
	; Same graphics as FORDELINGSSKAP
	(setq 
		blockName	(strcat _POW_ "SVG-" "SKAP-GRUPPESKAP")
		description "LAVSPENT GRUPPESKAP"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
		p1 (AddVectors (PosBL x y) (list 0 (/ y 9)))
		p2 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		p3 (list (/ x -40) (+ (/ y -6) (/ y 18)))
		p4 (PosTR x y)
		gx 1.2 ; Large outdoor cabinet ??
		gy 0.5
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawBottomEmphasis layDef_Zero x y)
	(command _POLYLINE_ p1 p2 p3 p4 _openPolyline_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(DrawBottomEmphasis layDef_Zero gx gy)
	(AddTextAtPos layDef_Zero (* 0.8 gy) _origin_ "GR")
	(ScaleAll _four_)		; Up to 1:1000 size
	(AddDescriptionBelowOrigin description (HalfOf gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun TOGVARMEPOST ( / blockName description x y )
	;
	;  +---------+
	;  | \     / |
	;  |  \   /  |
 	;  |   \./   |
	;  |   / \   |
	;  |  /   \  |
	;  | /     \ |
	;  +---------+
	;
	(setq 
		blockName	(strcat _POW_ "TVP-" "SKAP-TOGVARMEPOST")
		description "LAVSPENT TOGVARMEPOST"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
		gx 1.2 ; Large outdoor cabinet ??
		gy 0.5
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy layDef_Cabinet_Wipeout)
	(AddTextAtPos layDef_Zero (* 0.8 gy) _origin_ "GR")
	(ScaleAll _four_)		; Up to 1:1000 size
	(AddDescriptionBelowOrigin description gy)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
