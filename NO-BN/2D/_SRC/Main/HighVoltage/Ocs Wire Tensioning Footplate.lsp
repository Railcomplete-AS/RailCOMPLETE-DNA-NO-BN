;=========================================================================================================================
;
; Ocs Wire Tensioning Anchor.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
; 2022-09-08 CLFEY Updated symbols.
; 2026-02-19 CLFEY Updated and added symbols. File fissioned from Ocs Wire Tensioning Console.lsp
;
;=========================================================================================================================

; Footplate for anchoring of guywire(s) or spanner

(defun OCS-WIRE-TENSIONING-FOOTPLATE ( / )
	(TraceLevel3 "NOBN-TUNNELFESTE")						(NOBN-TUNNELFESTE)					; Bolts to tunnel wall, holding a bracket which holds a cantilever, or a rod terminated in a ring where the OCS wire guywire is to be fastened. See EH-702679.
	(TraceLevel3 "NOBN-BARDUN-FOTPLATE-ENKEL")				(NOBN-BARDUN-FOTPLATE-ENKEL)		; Bracket which is screwed onto a foundation for a single guywire
	(TraceLevel3 "NOBN-BARDUN-FOTPLATE-DOBBEL")				(NOBN-BARDUN-FOTPLATE-DOBBEL)		; Bracket which is screwed onto a foundation for two guywires
	(TraceLevel3 "NOBN-STREVER-FOTPLATE")					(NOBN-STREVER-FOTPLATE)				; Bracket which is screwed onto a foundation for a spanner
	(TraceLevel3 "NOBN-STREVER-FOTPLATE-TUNNEL-120")		(NOBN-STREVER-FOTPLATE-TUNNEL-120)	; Bracket which is fastened to tunnel ceiling / under bridge
)



(defun NOBN-TUNNELFESTE ( / blockName description x y  )
	; Just an "X", a bracket used for fixing a WTB or a guywire or a spanner (for fixed contact wire, catenary wire, midpoint anchor line)
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
		blockName (strcat _OCS_ "TUF-" "TUNNELFESTE")
		description "KL KRAFTAVLASTING, TUNNELFESTE, FORANKRINGSPUNKT FOTPLATE I TUNNELHVELV"
		x	1.5
		y	2.0
	)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun NOBN-BARDUN-FOTPLATE-ENKEL ( / blockName description p1 p2 p3 p10 )
	; Single arrow - guywire fastening
	; Default insertion direction is "down" in guywire wire alignment, i.e. pointing towards foundation.
	; Ref TRV Strekningsplan symboler
	;
	;    10     (schem.)
	;     |
	;  1\ 3 /2  3 = geo
	;    \|/
	;     .
	;
	(setq
		blockName (strcat _OCS_ "BAF-" "BARDUN-FOTPLATE-ENKEL")
		description "KL KRAFTAVLASTING, BARDUN-FOTPLATE FOR 1 x BARDUN"
		p1	(list -0.5  2.0)
		p2	(list  0.5  2.0)
		p3	(list  0.0  2.0)
		p10	(list  0.0  7.875)
	)
	; Schematic symbol
	(command _POLYLINE_ p1 _origin_ p2 _openPolyline_)	; arrow
	(DrawLine layDef_Zero _origin_ p10)	; shaft
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command _POLYLINE_ p1 _origin_ p2 _openPolyline_)	; arrow
	(DrawLine layDef_Zero _origin_ p3) 	; SHORT shaft
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)		


	
(defun NOBN-BARDUN-FOTPLATE-DOBBEL ( / blockName description p1 p2 p3 p4 p5 p6 p10 )
	; Double arrow - double guywire fastening
	; Ref TRV Strekningsplan symboler
	; Default insertion direction is "down" in guywire wire alignment, i.e. pointing towards foundation.
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
		blockName (strcat _OCS_ "BAF-" "BARDUN-FOTPLATE-DOBBEL")
		description "KL KRAFTAVLASTING, BARDUN-FOTPLATE FOR 2 x BARDUN"
		p1	(list -0.5  2.0)
		p2	(list  0.5  2.0)
		p3	(list  0.0  2.0)
		p4	(list -0.5  4.0)
		p5	(list  0.5  4.0)
		p6	(list  0.0  4.0) ; short
		p10	(list  0.0  7.875)
	)
	; Schematic symbol
	(command _POLYLINE_ p1 _origin_ p2 _openPolyline_)	; lower arrow
	(command _POLYLINE_ p4 p3 p5 _openPolyline_)			; upper arrow
	(DrawLine layDef_Zero _origin_ p10) 	; shaft
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(command _POLYLINE_ p1 _origin_ p2 _openPolyline_)	; lower arrow
	(command _POLYLINE_ p4 p3 p5 _openPolyline_)			; upper arrow
	(DrawLine layDef_Zero _origin_ p6) 	; SHORT shaft
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)

	

(defun NOBN-STREVER-FOTPLATE ( / blockName description shaftLength arrowHeight arrowHalfWidth baseHalfWidth )
	; Default insertion direction is "down" in spanner rod alignment, i.e. pointing towards top of OCS mast.
	; 
	; NOTE: Since the symbol is supposed to snap to the far end of an alignment, drawn from OCS mast to anchor point,
	; the tip-of-symbol will point to the OCS mast but will generally NOT hit the OCS mast center. Use AllowSymbolMode="True" in the DNA.
	;
	;     5
	;    /|\
	;   3 | 4
	;     |
	;     |
	;  1--.--2
	;
	(setq
		blockName (strcat _OCS_ "STF-" "STREVER-FOTPLATE")
		description "KL KRAFTAVLASTING, STREVER-FOTPLATE"
		p1	'(-0.875  0.000)
		p2	'( 0.875  0.000)
		p3	'(-0.500  2.000)
		p4	'( 0.500  2.000)
		p5	'( 0.000  4.000)
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p5)
	(DrawLine layDef_Zero p3 p5)
	(DrawLine layDef_Zero p4 p5)
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-STREVER-FOTPLATE-TUNNEL-120 ( / blockName description shaftLength arrowHeight arrowHalfWidth baseHalfWidth )
	; 2020-07-26 Added. See EH.705007 (tunnel mast Ø120) and EH-70355 (suspension mast Ø120 with spanner)
	; Default insertion direction is "down" in spanner rod alignment, i.e. pointing towards bottom (or shaft) of suspended OCS mast.
	; 
	; NOTE: Since the symbol is supposed to snap to the far end of an alignment, drawn from OCS mast to anchor point,
	; the tip-of-symbol will point to the OCS mast but will generally NOT hit the OCS mast center. Use AllowSymbolMode="True" in the DNA.
	;
	;        ( )              <-- Suspended OCS mast
	;        / \
	;      / / \ \
	;     / /   \ \   
	;    / /     \ \  
	;   / /       \ \
	;  / /||     ||\ \
	;  /==||==.==||==\        <-- Base plates attached to tunnel ceiling or bridge underside
	;     ||     ||
	;
	(setq
		blockName (strcat _OCS_ "STF-" "STREVER-FOTPLATE-OG-STREVER-FOR-BRU-OG-TUNNEL-120")
		description (strcat "KL KRAFTAVLASTING, STREVER-FOTPLATE OG STREVER(E) FOR BRU OG TUNNEL, " _uOSLASH_ "120 HENGEMAST")
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
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; mirror to make left side
		_POLYLINE_ "-0.125,-0.749" "-0.125,-0.851" "0.125,-0.851" "0.125,-0.749" _closedPolyline_ ; baseplate center part
		_CIRCLE_ _origin_ 0.060 ; Inside of Ø16 U iron clamp around Ø120 suspension mast
		_CIRCLE_ _origin_ 0.076 ; Outside of clamp
	)
	(MoveUp 0.8)
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (HalfOf _proxySymbolRadius_)) ; Make space for foundation symbol
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
