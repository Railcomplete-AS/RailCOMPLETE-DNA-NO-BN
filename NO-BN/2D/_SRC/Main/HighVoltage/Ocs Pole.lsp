;=========================================================================================================================
;
; Ocs Pole.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Overhead Catenary System pole

(defun ANYADM-OCS-UPRIGHT-POLE ( / )

	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "NOBN-OCS-H-BEAM-POLE")		(NOBN-OCS-H-BEAM-POLE)
	(TraceLevel3 "NOBN-OCS-POLE-TYPE-GMB")		(NOBN-OCS-POLE-TYPE-GMB)
	(TraceLevel3 "NOBN-OCS-LATTICE-MAST-B")		(NOBN-OCS-LATTICE-MAST-B)
	(TraceLevel3 "NOBN-OCS-LATTICE-MAST-H")		(NOBN-OCS-LATTICE-MAST-H)
	(TraceLevel3 "NOBN-OCS-CONCRETE-POLE")		(NOBN-OCS-CONCRETE-POLE)
	(TraceLevel3 "NOBN-OCS-WOODEN-POLE")		(NOBN-OCS-WOODEN-POLE)
)                                           



(defun NOBN-OCS-H-BEAM-POLE ( / blockName description x y w1 w2 r p1 p2 p3 p4 p5 p6 p7 )
	;  _____________
	; |_____   _____|
	;       | |
	;       | |    
	;       |.|        Hatched
	;       | |    
	;  _____| |_____
	; |_____________|
	;
	; The first quadrant of the H-beam is drawn, then mirrored twice:
	;
	; 7-----------------6 = (x,y)
	; :           w2    |
	; :    /-4----------5
	; :   3  : 
	; :  2   r       Quarter beam - mirror twice to get complete HEB beam profile
	; :  |¨¨¨        x wide, y high 
	; :w1|           Waist = 2 * w1 
	; :  |           Footplate / top-plate thickness: w2 
	; .  1           Rounding radius = r
	;
	(setq blockName (strcat _OCS_ "MAS-" "BJELKEMAST"		))
	(setq description (strcat "KL-MAST, HEB BJELKEMAST"		))
	(setq
		x (/ 3.0 2) ; Halfwidth
		y (/ 3.0 2) ; Halfheight
		w1 0.09 	; Halwidth "waist"
		w2 0.20     ; Thickness "parallel parts"
		r 0.15		; Inner corner radius
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
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-POLE-TYPE-GMB ( / blockName description x y p1 p2 p3 p4 p5 )
	;
	;    /4
	;  5  |
	; |   |
	; 1   |
	; | . |
	; |   |
	; |   |
	; 2---3
	;
	(setq blockName (strcat _OCS_ "MAS-" "GMBMAST"		))
	(setq description (strcat "KL-MAST, GMB-MAST"		))
	(setq
		x 0.85 ;width
		y 3.00 ;height
		p1	(list 0 (/ y 2))
		p2	_origin_
		p3	(list x 0)
		p4	(list x y)
		p5	(list 0.222 2.365)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 p4 _openPolyline_)
	(DrawArc layDef_Zero p4 p5 p1)
	(MoveLeft (HalfOf x))
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-LATTICE-MAST-B ( / blockName description x y )
	;     _
	;    | |
	;    | |
	;    |.|
	;    | |
	;    |_|
	;      
	(setq blockName (strcat _OCS_ "MAS-" "GITTERMAST-B"				))
	(setq description (strcat "KL GITTERMAST TYPE B2,B3,B4,B5,B6"	))
	(setq 
		x 1.05
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-LATTICE-MAST-H ( / blockName description x y )
	;     _____
	;    |     |
	;    |  .  |
	;    |_____|
	;      
	(setq blockName (strcat _OCS_ "MAS-" "GITTERMAST-H"		))
	(setq description (strcat "KL GITTERMAST TYPE H3,H5,H6"	))
	(setq
		x 3.0
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-CONCRETE-POLE ( / blockName description x y )
	;     _____
	;    |     |
	;    |  . *|	Hatched
	;    |_____|
	;      
	(setq blockName (strcat _OCS_ "MAS-" "BETONGMAST"	))
	(setq description (strcat "KL-MAST, BETONGMAST"		))
	(setq
		x 3.0
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawHatch _mediumHatch_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-WOODEN-POLE ( / blockName description r )
	;      ___ 
	;     /   \    
	;    (  .  )
	;     \___/  
	;      
	(setq blockName (strcat _OCS_ "MAS-" "TREMAST"		))
	(setq description (strcat "KL-MAST, TREMAST"		))
	(setq 
		r	1.5
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
