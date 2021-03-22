;=========================================================================================================================
;
; Power Distribution Cabinet.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Power distribution xabinet

(defun C:POWER-DISTRIBUTION-CABINET ( / )
	(FORDELINGSSKAP)	; Fordelingsskap
	(GRUPPESKAP)		; Gruppeskap (=sporvekselvarmeskap)
	(TOGVARMEPOST)		; Skap for togvarme
)


 
(defun FORDELINGSSKAP ( / blockName description x y y2 p1 p2 p3 p4 p5 )
	;
	; +-----------------5
	; |               / |
	; |             /   |
	; |        3  /     |
	; |       /./       |    
	; |     /  4        |
	; |   /             |
	; | /               |
	; 1-----------------2
	; |                 |  y2
	; +-----------------+
	;
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-FORDELINGSSKAP"
		description "LAVSPENT FORDELINGSSKAP"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
		y2 (/ y 9) 
		p1 (AddVectors (PosBL x y) (list 0 y2))
		p2 (AddVectors (PosBR x y) (list 0 y2))
		p3 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		p4 (list (/ x -40) (+ (/ y -6) (/ y 18)))
		p5 (PosTR x y)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(command _POLYLINE_ p1 p3 p4 p5 _openPolyline_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun GRUPPESKAP ( / blockName description x y y2 p1 p2 p3 p4 p5 )
	; Same graphics as FORDELINGSSKAP
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-GRUPPESKAP"
		description "LAVSPENT GRUPPESKAP"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
		y2 (/ y 9) 
		p1 (AddVectors (PosBL x y) (list 0 y2))
		p2 (AddVectors (PosBR x y) (list 0 y2))
		p3 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		p4 (list (/ x -40) (+ (/ y -6) (/ y 18)))
		p5 (PosTR x y)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(command _POLYLINE_ p1 p3 p4 p5 _openPolyline_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun TOGVARMEPOST ( / blockName description x y )
	;
	;  TL-------TR
	;  | \     / |
	;  |  \   /  |
 	;  |   \./   |
	;  |   / \   |
	;  |  /   \  |
	;  | /     \ |
	;  BL-------BR
	;
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-TOGVARMEPOST"
		description "LAVSPENT TOGVARMEPOST"
		; TRV:  4.5 x 2.25
		x 4.5 	
		y 2.25  
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
