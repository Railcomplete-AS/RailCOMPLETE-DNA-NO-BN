;=========================================================================================================================
;
; Earthing.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Earthing (general symbol, earthing busbar)

(defun ANYADM-EARTHING ( / )
  	(TraceLevel3 "ANYADM-EARTH-POTENTIAL") 				(ANYADM-EARTH-POTENTIAL)
  	(TraceLevel3 "ANYADM-EARTHING-BUSBAR")				(ANYADM-EARTHING-BUSBAR)
  	(TraceLevel3 "ANYADM-EARTHING-POTENTIAL-AT-RAIL")	(ANYADM-EARTHING-POTENTIAL-AT-RAIL)
)



(defun ANYADM-EARTH-POTENTIAL ( / blockName description p0 p1 p2 p3 p4 p5 p6 p7 )
	; General earth symbol - can be used as an	 earth potential marker, or as a crow's foot symbol, etc.
	; NO-BN: Ref. TRV "KL skjemasymboler".
	;
	;      .
	;      |
	;      |
	; 1----0----2
	;   3-----4
	;     5-6
	;     (7)
	;
	(setq blockName (strcat _OCS_ "JEL-" "JORDING-JORDPOTENSIAL"	))
	(setq description (strcat "JORDPOTENSIAL"						))
	(setq
		p0 (list  0.0 -3.0)
		p1 (list -2.1 -3.0)
		p2 (list  2.1 -3.0)
		p3 (list -1.4 -3.7)
		p4 (list  1.4 -3.7)
		p5 (list -0.5 -4.4)
		p6 (list  0.5 -4.4)
		p7 (list  0.0 -5.0)
	)
	; Schematic symbol
	(DrawLine layDef_Zero _origin_ p0)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(AddTextAtPos layDef_Description _descriptionTextHeight_ p7 description)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Geo symbol
	; Scale down 1/4:
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)


(defun ANYADM-EARTHING-BUSBAR ( / blockName description x y d r p1 p2 p3 p4 )
	; For installation under computer floors in buildings, on walls, in manholes and more. 
	; Used at Bane NOR in 50x10x290 (9xØ13 pluss a Ø5 hole for marking) or 50x10x530 (17xØ13 pluss 1xØ5). Galvanized (GMB version) or pure copper (most common).
	; Mounted 40mm from the substrate.
	; NO-BN: See also "magebelte" around HEB masts, ref ECT drawing / JEMTLAND (Gjøvik) drawing for HEB260 mast (in RC_3D catalogue for EH-JSK).
	;
	;  TL------------------------------TR
	;  |  (1)   (2)   (.)   (3)   (4)   |  
	;  BL------------------------------BR
	;
	(setq blockName (strcat _OCS_ "JSK-" "JORDING-JORDSKINNE"	))
	(setq description (strcat "JORDSKINNE"						))
	(setq
		x 6.0	; Schematic / annotative symbol measures. Geo size is 1:1000 drawings (1:1) is 1/4 of this. Real size (4:1 = 1:250) is 0.375 m.
		y 2.0
		d 1.0
		r 0.35
		p1 (list (* -2 d) 0)
		p2 (list (* -1 d) 0)
		p3 (list (*  1 d) 0)
		p4 (list (*  2 d) 0)
	)
	
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p2 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero _origin_ r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p3 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Geo symbol
	; Scale down 1/4:
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun ANYADM-EARTHING-POTENTIAL-AT-RAIL ( / blockName description )
	(setq blockName (strcat _OCS_ "JOF-" "JORDING-SKINNEJORD"	))
	(setq description (strcat "SKINNEJORDPOTENSIAL"				))
	; Schematic symbol
	(DrawRailEarthPotential nil)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Geo symbol
	; Scale down 1/4:
	(AddGraphicsFromScaledSchematicBlock blockName _quarter_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)