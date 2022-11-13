;=========================================================================================================================
;
; Balise.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; ATC = Automatic Train Control.
; ATP = Automatic Train Protection.
; ETCS = European Train Control System, location and fixed or controlled info through balises, plus asynchronous messages through GSM-R transmission.
; NSS = National Signalling System
; Ebicab700 = Norway's legacy ATP system.
; KVB = Contrôle des Vitesses à Balise, SNCFs legacy ATP system.
; PZB = Punktformige ZugBeeinflüssung, Train speed control system using discrete balise groups, Germany's legacy ATP system.
; INDUSI = Same as PZB, last generation is INDUSI90.
; GÜ = Geschwindigkeitsüberwachung = Speed supervision, a legacy German system for controlling train speeds and for triggering braking actions.

(defun BALISE-SYSTEM ( / )
	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "ETCS-BALISE-GROUP")		(ETCS-BALISE-GROUP)
	(TraceLevel3 "ETCS-BALISE")				(ETCS-BALISE)
	(TraceLevel3 "LEGACY-BALISE-GROUP")		(LEGACY-BALISE-GROUP)
	(TraceLevel3 "LEGACY-BALISE")			(LEGACY-BALISE)
)



(defun ETCS-BALISE-GROUP ( / blockName description groupTypes variation thisBlockName thisDescription )
	(setq blockName (strcat _SIG_ "ETC-" "ETCS-BALISEGRUPPE"	))
	(setq description (strcat "ETCS BALISEGRUPPE" 				))
	(setq groupTypes '("ENKEL" "DOBBEL"							))
	(foreach variation groupTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(NOBN_DrawEtcsBaliseGroup variation thisDescription)

		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
	)
)



(defun ETCS-BALISE ( / blockName description baliseTypes variation thisBlockName thisDescription )
	(setq blockName (strcat _SIG_ "ETB-" "ETCS-BALISE"	))
	(setq description (strcat "ETCS BALISE"				))
	(setq baliseTypes '("FAST" "STYRT")					)
	(foreach variation baliseTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(NOBN_DrawEtcsBalise variation thisDescription)

		(CreateSchematicBlockFromCurrentGraphics thisBlockName)
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		(DrawMetricBaliseGraphics 
			3.0			; Nominal distance between balises inside same group
			8.0			; Nominal distance from last controlled balise to device which triggers relevant change in balise telegram
			12.0 		; Nominal distance between two adjacent balise groups (minimum clearance without balise)
			0.250		; Balise physical length in track direction [m]
			0.500		; Balise physical width across track direction [m]
			0.5  		; Metal-free tolerance in track direction (from balise edge)
			0.2  		; Metal-free tolerance across track direction (from balise edge)
		)
		(CreateMetricBlockFromCurrentGraphics thisBlockName)
	)
)



(defun LEGACY-BALISE-GROUP ( / blockName description groupTypes variation thisBlockName thisDescription )
	(setq blockName (strcat _SIG_ "ATC-" "NSS-BALISEGRUPPE"		))
	(setq description (strcat "NSS BALISEGRUPPE" 				))
	(setq groupTypes '("STANDARD")								)
	(foreach variation groupTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(NOBN_DrawLegacyBaliseGroup variation thisDescription)

		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
	)
)	



(defun LEGACY-BALISE ( / blockName description baliseTypes variation thisBlockName thisDescription )
	(setq blockName (strcat _SIG_ "ATB-" "NSS-BALISE" 						))
	(setq description (strcat "NSS BALISE" 									))
	(setq baliseTypes '("TOM-FAST" "TOM-STYRT" "FYLT-FAST" "FYLT-STYRT")	)
	(foreach variation baliseTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(NOBN_DrawLegacyBalise variation thisDescription)

		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		(DrawMetricBaliseGraphics 
			3.0			; Nominal distance between balises inside same group
			8.0			; Nominal distance from last controlled balise to device which triggers relevant change in balise telegram
			12.0 		; Nominal distance between two adjacent balise groups (minimum clearance without balise)
			0.600		; Balise physical length in track direction [m]
			0.500		; Balise physical width across track direction [m]
			0.5  		; Metal-free tolerance in track direction (from balise edge)
			0.2  		; Metal-free tolerance across track direction (from balise edge)
		)
		(CreateMetricBlockFromCurrentGraphics thisBlockName)
	)
)



;================== Common functions ==================================================================

(defun DrawMetricBaliseGraphics ( 
		dist1	; Nominal distance between balises inside same group
		dist2	; Nominal distance between two adjacent balise groups (minimum clearance without balise)
		dist3	; Nominal distance from last controlled balise to device which triggers relevant change in balise telegram (isolated joint, axle counter etc)
		bx		; Balise physical length in track direction [m]
		by		; Balise physical width across track direction [m]
		A		; Metal-free tolerance in track direction (from balise edge)
		B 		; Metal-free tolerance across track direction (from balise edge)
	/
		mx my 
	);arguments / locals

	(setq 
			mx (+ (* _half_ bx) A)	; metal-free area along track direction, from centre of balise
			my (+ (* _half_ by) B)	; metal-free area across track direction, from centre of balise
	)
	(SetLayer layDef_Balise_BaliseSeparation)
	(command 
		_POLYLINE_ (list dist1 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origin_ _setPolylineArcAngle_ 12 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
	(AddTextAtPosWithJustification layDef_Balise_BaliseSeparation _th020_ (list (- dist1 (/ dist1 10.0)) (/ dist1 -6.0)) (rtos dist1 2 2) _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_)
		
	(SetLayer layDef_Balise_ActuatorSeparation)
	(command 
		_POLYLINE_ (list dist2 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origin_ _setPolylineArcAngle_ 12 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
	(AddTextAtPosWithJustification layDef_Balise_ActuatorSeparation _th020_ (list (- dist2 (/ dist2 20.0)) (/ dist2 -6.0)) (rtos dist2 2 2) _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_)

	(SetLayer layDef_Balise_GroupSeparation)la
	(command 
		_POLYLINE_ (list dist3 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origin_ _setPolylineArcAngle_ 12 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
	(AddTextAtPosWithJustification layDef_Balise_GroupSeparation _th020_  (list (- dist3 (/ dist3 20.0) ) (/ dist3 -6.0)) (rtos dist3 2 2) _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_)

	; Metal-free area:
	(SetLayer layDef_Balise_MetalFreeArea)
	(command _RECTANGLE_ (list (- mx) (- my))   (list (+ mx) (+ my)))
)



;================== NOBN functions ==================================================================

(defun NOBN_DrawEtcsBaliseGroup ( variation thisDescription / x x2 y baliseDist a1 a2 attAbove attBelow )
	; Bane NOR symbols for the ETCS ATP system. Balise groups with Bane NOR may have one or two balises, never more.
	; Note: The schematic symbol's insertion point is the tip of the rightmost arrow.
	;
	;    a1                a1
	; +-----+        +-----------+
	; | \   |        | \     \   |
	; |   \ |        |   \     \ |
	; |  . >|        | .  >     >|		
	; |   / |        |   /     / |
	; | /   |        | /     /   |
	; +-----+        +-----------+
	;    a2               a2
	;  ENKEL            DOBBEL
	;
	; groupTypes = "ENKEL" "DOBBEL"
	;
	(setq 
		x	2.5
		x2	(* 2 x)
		y	4.0
		baliseDist	3.0
		a1	(list 0 (+ (* 1.50 y)))
		a2	(list 0 (- (* 1.50 y)))
		attAbove '("TEKSTOVER" "Tekst over" _emptyString_)
		attBelow '("TEKSTUNDER" "Tekst under" _emptyString_)
	)
	
	(cond 
		((= variation "ENKEL")
			(DrawLine layDef_Zero (PosTL x y) (PosMR x y))
			(DrawLine layDef_Zero (PosBL x y) (PosMR x y))
			(DrawBox layDef_Zero x y _noWipeout_)
		)
		((= variation "DOBBEL")
			(DrawLine layDef_Zero (PosTL x2 y) (PosMC x2 y))
			(DrawLine layDef_Zero (PosBL x2 y) (PosMC x2 y))
			(DrawLine layDef_Zero (PosTC x2 y) (PosMR x2 y))
			(DrawLine layDef_Zero (PosBC x2 y) (PosMR x2 y))
			(DrawBox layDef_Zero x2 y _noWipeout_)
		)
	)
	(AddTextAttributeAtPos layDef_Zero _th250_ a1 attAbove)
	(AddTextAttributeAtPos layDef_Zero _th250_ a2 attBelow)
	(AddDescriptionBelowOrigin thisDescription (* 0.75 y))
	(if (= variation "DOBBEL") (MoveRight (HalfOf baliseDist)))
)



(defun NOBN_DrawEtcsBalise ( variation description / )
	; Bane NOR symbols for the ETCS system. 
	; NB! Suggested symbols - none were defined by BN.
	;
	;      a1         a1
	;    +---+      +---+
	;    |   |      |   |
	;    | . |      | .*|
	;    |   |      |   |
	;    +---+      +---+
	;     a2         a2
	;    Fixed    Controlled
	;
	; baliseTypes = "FAST" "STYRT"
	;
	(setq
		x	2.0
		y	3.0
		a1	(list 0  2.500)
		a2	(list 0 -2.500)
		attAbove '("TEKSTOVER" "Tekst over" _emptyString_)
		attBelow '("TEKSTUNDER" "Tekst under" _emptyString_)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(cond 
		((= variation "FAST")
		)
		((= variation "STYRT")
			(DrawHatch _solidHatch_)
		)
	)
	(AddTextAttributeAtPos layDef_Zero _th180_ a1 attAbove)
	(AddTextAttributeAtPos layDef_Zero _th180_ a2 attBelow)
	(AddDescriptionBelowOrigin thisDescription (* 0.75 y))
)



(defun NOBN_DrawLegacyBaliseGroup ( variation description / )
	; Large dashed triangle.
	;
	;        a1
	;       
	;        9
	;       8 \
	;
	;     7     \
	;    6       \
	;        .
	;  5           \
	; 1-2 3-4 --- --+
	;
	;        a2
	;
	(setq
		p1	'(-3.000 -1.500)
		p2	'(-2.300 -1.500)
		p3	'(-1.650 -1.500)
		p4	'(-0.350 -1.500)
		p5	'(-2.190 -0.290)
		p6	'(-1.850  0.240)
		p7	'(-1.150  1.270)
		p8	'(-0.810  1.800)
		p9	'( 0.000  3.000)
		a1	'( 0.000  4.500)
		a2	'( 0.000 -3.000)
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p1 p5)
	(DrawLine layDef_Zero p6 p7)
	(DrawLine layDef_Zero p8 p9)
	(MirrorAboutYaxis _keepMirrorSource_)
)



(defun NOBN_DrawLegacyBalise ( variation thisDescription / p1 p2 p3 p4 barWidth barHeight a1 a2 attAbove attBelow )
	; Bane NOR symbols for the Ebicab 700 ATP system.
	; The first balise in group is filled then the group relays usable info to the train in this direction of reading.
	; An empty balise denotes a subsequent balise.
	; A group may consist of two filled balises, i.e. readable in both directions.
	; Line above : Controlled balise.
	; No line above: fixed telegram balise.
	;
	;      a1             a1             a1            a1
	;                  ===4===                      ===4===
	;      3              3              3             3          
	;     / \            / \            / \           / \         
	;    /   \          /   \          / * \         / * \  
	;   1--.--2        1--.--2        1--.--2       1--.--2      
	; 
	;      a2             a2             a2            a2
	;
	;    Var. 1         Var. 2         Var. 3        Var. 4    
	;  Empty/fixed   Empty/contr.  Filled/fixed   Filled/contr.
	;
	; baliseTypes = "TOM-FAST" "TOM-STYRT" "FYLT-FAST" "FYLT-STYRT"
	;
	(setq 
		p1	'(-1.500 0.000)
		p2	'( 1.500 0.000)
		p3	'( 0.000 2.500)
		p4	'( 0.000 2.700)
		barWidth	2.4
		barHeight	0.1
		a1	(list 0  4.000)
		a2	(list 0 -1.500)
		attAbove '("TEKSTOVER" "Tekst over" _emptyString_)
		attBelow '("TEKSTUNDER" "Tekst under" _emptyString_)
	)
	
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
	(cond 
		((= variation "TOM-FAST")
		)
		((= variation "TOM-STYRT")
			(DrawBoxAtPos layDef_Zero p4 barWidth barHeight _noWipeout_)
			(DrawHatch _solidHatch_)
		)
		((= variation "FYLT-FAST")
			(DrawHatch _solidHatch_)
		)
		((= variation "FYLT-STYRT")
			(DrawHatch _solidHatch_)
			(DrawBoxAtPos layDef_Zero p4 barWidth barHeight _noWipeout_)
			(DrawHatch _solidHatch_)
		)
	)
	(AddTextAttributeAtPos layDef_Zero _th180_ a1 attAbove)
	(AddTextAttributeAtPos layDef_Zero _th180_ a2 attBelow)
	(AddDescriptionBelowOrigin thisDescription (* 0.75 y))
)
