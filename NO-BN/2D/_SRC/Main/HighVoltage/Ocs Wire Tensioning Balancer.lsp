;=========================================================================================================================
;
; Ocs Wire Tensioning Balancer.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Midpoint Anchor (fixpoint) and Wire Tensioning Balancer
;
; See Bane NOR Elkraftportalen: EH.707487 - Contact wire tensioning device (2xLodd)

 (defun OCS-WIRE-TENSIONING-BALANCER ( / )
 	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "NOBN-OCS-TENSIONING-MIDPOINT-ANCHOR")		(NOBN-OCS-TENSIONING-MIDPOINT-ANCHOR)
	(TraceLevel3 "NOBN-OCS-TENSIONING-ANCHOR-SINGLE")		(NOBN-OCS-TENSIONING-ANCHOR-SINGLE)
	(TraceLevel3 "NOBN-OCS-TENSIONING-ANCHOR-DOUBLE")		(NOBN-OCS-TENSIONING-ANCHOR-DOUBLE)
	(TraceLevel3 "NOBN-OCS-TENSIONING-WEIGHT-SINGLE")		(NOBN-OCS-TENSIONING-WEIGHT-SINGLE)
	(TraceLevel3 "NOBN-OCS-TENSIONING-WEIGHT-DOUBLE")		(NOBN-OCS-TENSIONING-WEIGHT-DOUBLE)
	(TraceLevel3 "NOBN-OCS-TENSIONING-SPRING-SINGLE")		(NOBN-OCS-TENSIONING-SPRING-SINGLE)
	(TraceLevel3 "NOBN-OCS-TENSIONING-SPRING-DOUBLE")		(NOBN-OCS-TENSIONING-SPRING-DOUBLE)
)



(defun NOBN-OCS-TENSIONING-MIDPOINT-ANCHOR ( / blockName description x y r )
	; A circular hatched blob with a St Andrew cross
	;
	;   \   /
	;    (.)    ; Hatched circle
	;   /   \
	;
	(setq 
		blockName 	"NO-BN-2D-JBTEH_AEH-AVSPENNING-FIXPUNKT"
		description	"KL AVSPENNING, FIXPUNKT"
		x	(* 5 (sqrt 0.5)) ; StAndrew, diagonal = 5
		y	x
		r	1.0 ; The hatched circle's radius
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawHatch _denseHatch_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (* 1.5 r))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-TENSIONING-ANCHOR-SINGLE ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; ENKEL hatched arrow, single bar
	;
	;     10
	;     |
	;   6-+-7
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName 	"NO-BN-2D-JBTEH_AEH-AVSPENNING-FAST-ENKEL"
		description	"KL AVSPENNING, 1 x FAST"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4)
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10	'(  0.0  6.5)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p6 p7) ; bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-TENSIONING-ANCHOR-DOUBLE ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; ENKEL hatched arrow, double bar
	;
	;     10
	;     |
	;   8-+-9
	;   4-+-5
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName	"NO-BN-2D-JBTEH_AEH-AVSPENNING-FAST-DOBBEL"
		description	"KL AVSPENNING, 2 x FAST"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4)
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10	'(  0.0  6.5)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p4 p5) ; lower bar
	(DrawLine layDef_Zero p8 p9) ; upper bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-TENSIONING-WEIGHT-SINGLE ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; Double hatched arrow, single bar
	;
	;     13
	;    /*\
	; 11\10 /12
	;     |
	;   6-+-7
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName	"NO-BN-2D-JBTEH_AEH-AVSPENNING-LODD-ENKEL"
		description	"KL AVSPENNING, 1 x LODD"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10 '(  0.0  6.6)
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(command _POLYLINE_ p11 p12 p13 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p6 p7) ; bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)


(defun NOBN-OCS-TENSIONING-WEIGHT-DOUBLE ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; Double hatched arrow, double bar
	;
	;     13
	;    /*\
	; 11\10 /12
	;     |
	;   8-+-9
	;   4-+-5
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName	"NO-BN-2D-JBTEH_AEH-AVSPENNING-LODD-DOBBEL"
		description	"KL AVSPENNING, 2 x LODD"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10 '(  0.0  6.6)
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(command _POLYLINE_ p11 p12 p13 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p4 p5) ; bar
	(DrawLine layDef_Zero p8 p9) ; bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-TENSIONING-SPRING-SINGLE ( /  blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; Double hatched arrow, single bar
	;
	;     13
	;    /*\
	; 11\10 /12
	;     |
	;   6-+-7
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName	"NO-BN-2D-JBTEH_AEH-AVSPENNING-FJAER-ENKEL"
		description	(strcat "KL AVSPENNING, 1 x FJ" _uAELIG_ "R")
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10 '(  0.0  6.6)
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(command _POLYLINE_ p11 p12 p13 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p6 p7) ; bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-TENSIONING-SPRING-DOUBLE ( /  blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	; Double hatched arrow, double bar
	;
	;     13
	;    /*\
	; 11\10 /12
	;     |
	;   8-+-9
	;   4-+-5
	;     |
	;  1\ 3 /2
	;    \*/
	;     .
	;
	(setq
		blockName	"NO-BN-2D-JBTEH_AEH-AVSPENNING-FJAER-DOBBEL"
		description	(strcat "KL AVSPENNING, 2 x FJ" _uAELIG_ "R")
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10 '(  0.0  6.6)
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command _POLYLINE_ p1 _origin_ p2 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(command _POLYLINE_ p11 p12 p13 _closedPolyline_)	; arrowhead
	(DrawHatch _denseHatch_)
	(DrawLine layDef_Zero p3 p10) ; shaft
	(DrawLine layDef_Zero p4 p5) ; bar
	(DrawLine layDef_Zero p8 p9) ; bar
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
