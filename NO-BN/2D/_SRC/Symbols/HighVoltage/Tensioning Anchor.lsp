;=========================================================================================================================
;
; Tensioning Anchor.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Force uptake (stretchers and spanners)

(defun C:TENSIONING-DEVICE-ANCHOR ( / )
	(TUNNELFESTE)			; Bolts to tunnel wall, holding a bracket which holds a cantilever, or a rod terminated in a ring where the OCS wire stretcher is to be fastened. See EH-702679.
	(BARDUNFESTE-ENKEL)		; Bracket which is screwed onto a foundation for a single OCS wire strecher (e.g., bardunfundament boret 2500mm)
	(BARDUNFESTE-DOBBEL)	; Bracket which is screwed onto a foundation for two OCS wire strechers (going to the same OCS mast) (e.g., bardunfundament boret 2500mm)
	(STREVERFESTE)			; Bracket which is screwed onto a foundation for a OCS wire spanners (e.g., bardunfundament boret 2500mm)
	(STREVERFESTE-TUNNEL-120)	; Bracket which is fastened to tunnel roof / under bridge which clamps with two arms to an Ø120 bridge / tunnel mast
)



(defun TUNNELFESTE ( / blockName description x y  )
	; Just an "X", a bracket used for fixing stretchers (for fixed contact wire, catenary wire, fix line) 
	; or cantilever to a bolt group set in a tunnel ceiling or wall.
	; Ref TRV Strekningsplan symboler
	; Default insertion direction is "both" in wire / catenary / contact wire alignment,  i.e. pointing towards 
	; foundation, no matter which wire end it as attached to.
	;
	;     TL   TR
	;       \ /
	;        .
	;       / \
	;     BL   BR
	;
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-TUNNELFESTE"
		description "KL KRAFTAVLASTING, ANKERPUNKT I TUNNELHVELV"
		x	1.5
		y	2.0
	)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description y)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		


(defun BARDUNFESTE-ENKEL ( / blockName description p1 p2 p3 p10 )
	; Single arrow - stretcher fastening
	; Default insertion direction is "down" in strecher wire alignment, i.e. pointing towards foundation.
	; Ref TRV Strekningsplan symboler
	;
	;    10     (schem.)
	;     |
	;  1\ 3 /2  3 = geo
	;    \|/
	;     .
	;
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-BARDUN-ENKEL"
		description "KL KRAFTAVLASTING, ANKERPUNKT FOR 1 x BARDUN"
		p1	(list -0.5  2.0)
		p2	(list  0.5  2.0)
		p3	(list  0.0  2.0)
		p10	(list  0.0  7.875)
	)
	; Schematic symbol
	(command _POLYLINE_ p1 _origo_ p2 _openPolyline_)	; arrow
	(drawLine layer_Zero _origo_ p10)	; shaft
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command _POLYLINE_ p1 _origo_ p2 _openPolyline_)	; arrow
	(drawLine layer_Zero _origo_ p3) 	; SHORT shaft
	(addDescriptionBelowOrigo description 0)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)		


	
(defun BARDUNFESTE-DOBBEL ( / blockName description p1 p2 p3 p4 p5 p6 p10 )
	; Double arrow - double stretcher fastening
	; Ref TRV Strekningsplan symboler
	; Default insertion direction is "down" in strecher wire alignment, i.e. pointing towards foundation.
	;
	;    10     (schem.)
	;     |
	;  4\ 6 /5  (6 = geo)
	;    \|/
	;  1\ 3 /2
	;    \|/
	;     .
	;
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-BARDUN-DOBBEL"
		description "KL KRAFTAVLASTING, ANKERPUNKT FOR 2 x BARDUN"
		p1	(list -0.5  2.0)
		p2	(list  0.5  2.0)
		p3	(list  0.0  2.0)
		p4	(list -0.5  4.0)
		p5	(list  0.5  4.0)
		p6	(list  0.0  4.0) ; short
		p10	(list  0.0  7.875)
	)
	; Schematic symbol
	(command _POLYLINE_ p1 _origo_ p2 _openPolyline_)	; lower arrow
	(command _POLYLINE_ p4 p3 p5 _openPolyline_)			; upper arrow
	(drawLine layer_Zero _origo_ p10) 	; shaft
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command _POLYLINE_ p1 _origo_ p2 _openPolyline_)	; lower arrow
	(command _POLYLINE_ p4 p3 p5 _openPolyline_)			; upper arrow
	(drawLine layer_Zero _origo_ p6) 	; SHORT shaft
	(addDescriptionBelowOrigo description 0)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)

	

(defun STREVERFESTE ( / blockName shaftLength arrowHeight arrowHalfWidth baseHalfWidth )
	;Default insertion direction is "up" in spanner rod alignment, i.e. pointing towards top of OCS mast.
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-STREVER"
		description "KL KRAFTAVLASTING, ANKERPUNKT FOR STREVER"
		shaftLength	4	; Bane NOR length 2D symbol is basically 7.875 and can be changed, RC is fixed and will usually extend past the start of its spanner alignment.
		arrowHeight 2.0
		arrowHalfWidth 0.5
		baseHalfWidth 0.875
	)
	(command
		_LINE_ _origo_ (list (- arrowHalfWidth) arrowHeight) _ENTER_
		_LINE_ _origo_ (list (+ arrowHalfWidth) arrowHeight) _ENTER_
		_LINE_ _origo_ (list 0 shaftLength) _ENTER_
		_LINE_ (list (- baseHalfWidth) shaftLength) (list (+ baseHalfWidth) shaftLength) _ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_
	)
	(addDescriptionBelowOrigo description arrowHeight)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun STREVERFESTE-TUNNEL-120 ( / blockName shaftLength arrowHeight arrowHalfWidth baseHalfWidth )
	;2020-07-26 Added. See EH.705007 (tunnel mast Ø120) and EH-70355 (suspension mast Ø120 with spanner)
	;Default insertion direction is "both", i.e. spanner is located at the track side of its bridge / tunnel suspension mast.
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-STREVER-MED-FESTEPLATE-FOR-BRU-OG-TUNNEL-120"
		description (strcat "KL KRAFTAVLASTING, STREVER MED FESTEPLATE FOR BRU OG TUNNEL, " _uOE_ "120")
	)
	(command
		; Base plate with holes for roof fastening (draw right side, then mirror left side):
		_POLYLINE_ "0.125,-0.650" "0.125,-0.950" "0.225,-0.950" "0.225,-0.650" _closedPolyline_ ; right bolt hole plate
		_CIRCLE_ "0.175,-0.685" 0.012 ; upper bolt hole
		_CIRCLE_ "0.175,-0.915" 0.012 ; lower bolt hole
		_POLYLINE_ "0.225,-0.749" "0.225,-0.851" "0.425,-0.851" "0.375,-0.749" _closedPolyline_ ; right part of base plate
		_POLYLINE_ "0.369,-0.737" "0.431,-0.863" "0.441,-0.858" "0.380,-0.732" _closedPolyline_ ; right side-plate of base
		_POLYLINE_ "0.050,-0.153" "0.366,-0.779" "0.415,-0.754" "0.102,-0.128" _closedPolyline_ ; right side spanner
		_POLYLINE_ "0.028,-0.071" "0.066,-0.146" "0.086,-0.135" "0.049,-0.060" _closedPolyline_ ; right side spanner's fastening to clamp ring
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_ ; mirror to make left side
		_POLYLINE_ "-0.125,-0.749_ENTER_-0.125,-0.851_ENTER_0.125,-0.851" "0.125,-0.749" _closedPolyline_ ; baseplate center part
		_CIRCLE_ _origo_ 0.060 ; Inside of Ø16 U iron clamp around Ø120 suspension mast
		_CIRCLE_ _origo_ 0.076 ; Outside of clamp
	)
	(scaleAll _four_)
	(addDescriptionBelowOrigo description 4)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
