;=========================================================================================================================
;
; Tensioning Device.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Tensioning device
;
; See Bane NOR Elkraftportalen: EH.707487 - Contact wire tensioning device (2xLodd)

 (defun C:TENSIONING-DEVICE ( / )
 
	(AVSPENNING-FIXPUNKT)
	(AVSPENNING-FAST-ENKEL)
	(AVSPENNING-FAST-DOBBEL)
	(AVSPENNING-LODD-ENKEL)
	(AVSPENNING-LODD-DOBBEL)
	(AVSPENNING-FJAER-ENKEL)
	(AVSPENNING-FJAER-DOBBEL)
)



(defun AVSPENNING-FIXPUNKT ( / blockName description x y r )
	; A circular hatched blob with a St Andrew cross
	;
	;   \   /
	;    (.)
	;   /   \
	;
	(setq blockName 	"NO-BN-2D-JBTKL-AVSPENNING-FIXPUNKT"
		description	"FIX"
		x	(* 5 (sqrt 0.5)) ; StAndrew, diagonal = 5
		y	x
		r	1.0 ; The hatched circle's radius
	)
	(drawCircle layer_Zero r _noWipeout_)
	(drawHatch _denseHatch_)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun AVSPENNING-FAST-ENKEL ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName 		"NO-BN-2D-JBTKL-AVSPENNING-FAST-ENKEL"
		description	"1 x FAST"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4)
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10	'(  0.0  6.5)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p6 p7) ; bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun AVSPENNING-FAST-DOBBEL ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName 		"NO-BN-2D-JBTKL-AVSPENNING-FAST-DOBBEL"
		description	"2 x FAST"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4)
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10	'(  0.0  6.5)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p4 p5) ; lower bar
	(drawLine layer_Zero p8 p9) ; upper bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun AVSPENNING-LODD-ENKEL ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName 		"NO-BN-2D-JBTKL-AVSPENNING-LODD-ENKEL"
		description	"1 x LODD"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10 '(  0.0  6.6)
		p10b '( 0.0  6.61) ; (addVectors (p10 _slightlyAbove_)) ; for hatching
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(command "._PLINE" p11 p12 p13 _open_)	; arrowhead
	(drawHatchFromPoint _denseHatch_ p10b 0 0) ; sc pt ang offs
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p6 p7) ; bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)


(defun AVSPENNING-LODD-DOBBEL ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName 		"NO-BN-2D-JBTKL-AVSPENNING-LODD-DOBBEL"
		description	"2 x LODD"
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10 '(  0.0  6.6)
		p10b '( 0.0  6.61) ; (addVectors (p10 _slightlyAbove_)) ; for hatching
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(command "._PLINE" p11 p12 p13 _open_)	; arrowhead
	(drawHatchFromPoint _denseHatch_ p10b 0 0) ; sc pt ang offs
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p4 p5) ; bar
	(drawLine layer_Zero p8 p9) ; bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun AVSPENNING-FJAER-ENKEL ( /  blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FJAER-ENKEL"
		description	(strcat "1 x FJ" _uAE_ "R")
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p6	'( -1.0  4.5)
		p7	'(  1.0  4.5)
		p10 '(  0.0  6.6)
		p10b '( 0.0  6.61) ; (addVectors (p10 _slightlyAbove_)) ; for hatching
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(command "._PLINE" p11 p12 p13 _open_)	; arrowhead
	(drawHatchFromPoint _denseHatch_ p10b 0 0) ; sc pt ang offs
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p6 p7) ; bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun AVSPENNING-FJAER-DOBBEL ( /  blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p10b )
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
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FJAER-DOBBEL"
		description	(strcat "2 x FJ" _uAE_ "R")
		p1	'( -1.0  2.4)
		p2	'(  1.0  2.4)
		p3	'(  0.0  2.4) 
		p4	'( -1.0  4.0)
		p5	'(  1.0  4.0)
		p8	'( -1.0  5.0)
		p9	'(  1.0  5.0)
		p10 '(  0.0  6.6)
		p10b '( 0.0  6.61) ; (addVectors (p10 _slightlyAbove_)) ; for hatching
		p11	'( -1.0  6.6)
		p12	'(  1.0  6.6)
		p13	'(  0.0  9.0)
	)
	(command "._PLINE" p1 _origo_ p2 _open_)	; arrowhead
	(drawHatch _denseHatch_)
	(command "._PLINE" p11 p12 p13 _open_)	; arrowhead
	(drawHatchFromPoint 0.05 p10b 0 0) ; sc pt ang offs
	(drawLine layer_Zero p3 p10) ; shaft
	(drawLine layer_Zero p4 p5) ; bar
	(drawLine layer_Zero p8 p9) ; bar
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
