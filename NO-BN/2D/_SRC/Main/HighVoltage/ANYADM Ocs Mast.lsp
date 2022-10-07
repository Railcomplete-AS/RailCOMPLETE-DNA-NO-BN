;=========================================================================================================================
;
; ANYADM Ocs Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Overhead Catenary System mast

(defun ANYADM-OCS-UPRIGHT-MAST ( / )

	; Implemented for all administrations:

	; Implemented only for some administrations:
	(cond 
		((= _ADM_ _XXGL_) 
		)
		((= _ADM_ _NOBN_) 
			(TraceLevel3 "NOBN-OCS-H-BEAM-MAST")		(NOBN-OCS-H-BEAM-MAST)
			(TraceLevel3 "NOBN-OCS-MAST-TYPE-GMB")		(NOBN-OCS-MAST-TYPE-GMB)
			(TraceLevel3 "NOBN-OCS-LATTICE-MAST-B")		(NOBN-OCS-LATTICE-MAST-B)
			(TraceLevel3 "NOBN-OCS-LATTICE-MAST-H")		(NOBN-OCS-LATTICE-MAST-H)
			(TraceLevel3 "NOBN-OCS-CONCRETE-MAST")		(NOBN-OCS-CONCRETE-MAST)
			(TraceLevel3 "NOBN-OCS-WOODEN-MAST")		(NOBN-OCS-WOODEN-MAST)
		)
		((= _ADM_ _FRSR_) 
			; TODO - model FRSR objects, do not use NOBN objects:
			(TraceLevel3 "NOBN-OCS-H-BEAM-MAST")		(NOBN-OCS-H-BEAM-MAST)
		)
		((= _ADM_ _DEDB_) 
		)
		((= _ADM_ _JPTX_) 
			(TraceLevel3 "JPTX-OCS-MAST")				(JPTX-OCS-MAST)
		)
	)
)                                           



(defun NOBN-OCS-H-BEAM-MAST ( / blockName description x y w1 w2 r p1 p2 p3 p4 p5 p6 p7 )
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
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-MAST-HEB-NOBN-OCS-H-BEAM-MAST"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "BJELKEMAST"								)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-POUTRE-HEB-EN-ACIER"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-HEB-STAHLTRAEGER"		)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY MAST, HEB STEEL BEAM"						)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-MAST, HEB BJELKEMAST"							)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, POUTRE HEB EN ACIER"			)))
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, HEB STAHLTRAEGER"				)))
	)
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



(defun NOBN-OCS-MAST-TYPE-GMB ( / blockName description x y p1 p2 p3 p4 p5 )
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
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-MAST-NOBN-OCS-H-BEAM-MAST-TYPE-GMB"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "GMBMAST"								)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-POUTRE-TYPE-GMB"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-STAHLTRAEGER-TYP-GMB"	)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY MAST, TYPE GMB"							)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-MAST, GMB-MAST"									)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, POUTRE TYPE GMB"				)))
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, STAHLTRAEGER TYP GMB"			)))
	)
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
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-NOBN-OCS-LATTICE-MAST-TYPE-B"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "GITTERMAST-B"						)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-TREILLIS-TYPE-B"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-GITTERMAST-TYP-B"	)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY LATTICE MAST TYPE B"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL GITTERMAST TYPE B2,B3,B4,B5,B6"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, TREILLIS TYPE B"	))) ; Trouble with E _uEACUTE_
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, GITTERMASTTYP B"		)))
	)
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
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-NOBN-OCS-LATTICE-MAST-TYPE-H"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "GITTERMAST-H"						)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-TREILLIS-TYPE-H"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-GITTERMAST-TYP-H"	)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY LATTICE MAST TYPE H"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL GITTERMAST TYPE H3,H5,H6"			)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, TREILLIS TYPE H"	))) ; Trouble with E _uEACUTE_
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, GITTERMASTTYP H"		)))
	)
	(setq
		x 3.0
		y 3.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-CONCRETE-MAST ( / blockName description x y )
	;     _____
	;    |     |
	;    |  . *|	Hatched
	;    |_____|
	;      
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-MAST-CONCRETE-BEAM"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "BETONGMAST"						)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-POUTRE-EN-BETON"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-BETONTRAEGER"		)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY MAST, CONCRETE BEAM"				)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-MAST, BETONGMAST"						)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, POUTRE EN BETON"		))) ; Trouble with E _uEACUTE_
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, BETONTR" _uAUML_ "GER"	)))
	)
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



(defun NOBN-OCS-WOODEN-MAST ( / blockName description r )
	;      ___ 
	;     /   \    
	;    (  .  )
	;     \___/  
	;      
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "NOBN-OCS-WOODEN-MAST"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "TREMAST"						)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-MAT-EN-BOIS"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-HOLZMAST"		)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY MAST, WOODEN MAST"				)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-MAST, TREMAST"							)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, M" _uACIRC_ "T EN BOIS"	))) ; Trouble with E _uEACUTE_
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, HOLZMAST"				)))
	)
	(setq 
		r	1.5
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun JPTX-OCS-MAST ( / blockName description r )
	;      ___ 
	;     /   \    
	;    (  .  )
	;     \___/  
	;      
	(setq
		blockName	(strcat _OCS_ "MAS-" "STEEL-POLE"			)
		description	(strcat "CATENARY STEEL POLE"				)
		r	1.5
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
