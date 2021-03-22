;=========================================================================================================================
;
; OCS Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Overhead Catenary System mast

(defun C:OCS-MAST ( / )

	(TraceLevel3 "BJELKEMAST")		(BJELKEMAST)
	(TraceLevel3 "GMBMAST")			(GMBMAST)
	(TraceLevel3 "GITTERMAST-B")	(GITTERMAST-B)
	(TraceLevel3 "GITTERMAST-H")	(GITTERMAST-H)
	(TraceLevel3 "BETONGMAST")		(BETONGMAST)
	(TraceLevel3 "TREMAST")			(TREMAST)
)



(defun BJELKEMAST ( / blockName description x y w1 w2 r p1 p2 p3 p4 p5 p6 p7 )
	;
	; 7-----------------6 = (x,y)
	; :           w2    |
	; :    /-4----------5
	; :   3    
	; :  2   r       Quarter beam - mirror twice to get complete HEB beam profile
	; :  |           x wide, y high 
	; :w1|           Waist = 2 * w1 
	; :  |           Footplate / top-plate thickness: w2 
	; .  1           Rounding radius = r
	;
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BJELKEMAST"
		description "KL BJELKEMAST TYPE HEB"
		x (/ 3.0 2) ; Halvbredde
		y (/ 3.0 2) ; Halvhøyde
		w1 0.09 	; Halvbredde "livet"
		w2 0.20     ; hel tykkelse "tverr-beina"
		r 0.15		; bøyradius i "innerhjørner" på HEB bjelken
		p1	(list w1 0)
		p2	(list w1 (- y (+ w2 r)))
		p3	(list (+ w1 (* 0.293 r)) (- y (+ w2 (* 0.293 r))))
		p4	(list (+ w1 r) (- y w2))
		p5	(list x (- y w2))
		p6	(list x y)
		p7	(list 0 y)
	)
	; Draw the first quadrant of an "I" shaped beam, then mirror+mirror:
	(DrawLine layDef_Zero p1 p2)
	(DrawArc layDef_Zero p2 p3 p4)
	(command _POLYLINE_ p4 p5 p6 p7 _openPolyline_)
	(MirrorAboutYaxis _keepMirrorSource_)
	(MirrorAboutXaxis _keepMirrorSource_)
	(DrawHatchFromPoint _denseHatch_ _origo_ _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigo description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun GMBMAST ( / blockName description x y p1 p2 p3 p4 p5 )
	;
	;    -4
	;  5  |
	; |   |
	; 1   |
	; | . |
	; |   |
	; |   |
	; 2---3
	;
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GMBMAST"
		description "KL BJELKEMAST TYPE HEB/GMB"
		x 0.85 ;bredde
		y 3.00 ;høyde
		p1	(list 0 (/ y 2))
		p2	_origo_
		p3	(list x 0)
		p4	(list x y)
		p5	(list 0.222 2.365)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 p4 _openPolyline_)
	(DrawArc layDef_Zero p4 p5 p1)
	(MoveLeft (HalfOf x))
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun GITTERMAST-B ( / blockName description x y )
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-B"
		description "KL GITTERMAST TYPE B2,B3,B4,B5,B6"
		x 1.05
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun GITTERMAST-H ( / blockName description x y )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-H"
		description "KL GITTERMAST TYPE H3,H5,H6"
		x 3.0
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun BETONGMAST ( / blockName description x y )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BETONGMAST"
		description "KL BETONGMAST"
		x 3.0
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawHatch _mediumHatch_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun TREMAST ( / blockName description r )
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-TREMAST"
		description "KL TREMAST"
		r	1.5
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddDescriptionBelowOrigo description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
