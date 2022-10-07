;=========================================================================================================================
;
; ANYADM Balise.lsp
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
; NSS = National Signaling System
; Ebicab700 = Norway's legacy ATP system.
; KVB = Contrôle des Vitesses à Balise, SNCFs legacy ATP system.
; PZB = Punktformige ZugBeeinflüssung, Train speed control system using discrete balise groups, Germany's legacy ATP system.
; INDUSI = Same as PZB, last generation is INDUSI90.
; GÜ = Geschwindigkeitsüberwachung = Speed supervision, a legacy German system for controlling train speeds and for triggering braking actions.

(defun ANYADM-BALISE-SYSTEM ( / )

	(TraceLevel3 "ETCS-BALISE-GROUP")		(ETCS-BALISE-GROUP)
	(TraceLevel3 "ETCS-BALISE")				(ETCS-BALISE)
	(TraceLevel3 "LEGACY-BALISE-GROUP")		(LEGACY-BALISE-GROUP)
	(TraceLevel3 "LEGACY-BALISE")			(LEGACY-BALISE)

	(cond 
		((= _ADM_ _XXGL_) )	
		((= _ADM_ _NOBN_) )
		((= _ADM_ _FRSR_) (TraceLevel3 "FRSR-LEGACY-BALISE-CHAMP")		(FRSR-LEGACY-BALISE-CHAMP)		)
		((= _ADM_ _DEDB_) (TraceLevel3 "DEDB-SPEED-SUPERVISION-MAGNET")	(DEDB-SPEED-SUPERVISION-MAGNET)	)
	)
)



(defun ETCS-BALISE-GROUP ( / blockName description groupTypes variation thisBlockName thisDescription )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "BGE-" "ETCS-BALISE-GROUP"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "ETC-" "ETCS-BALISEGRUPPE"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "PIE-" "POINT-INFORMATION-ETCS"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "DPE-" "DATENPUNKT-ETCS"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "ETCS BALISE GROUP" 			)))
		((= _ADM_ _NOBN_) (setq description (strcat "ETCS BALISEGRUPPE" 			)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT D'INFORMATION ETCS" 		)))
		((= _ADM_ _DEDB_) (setq description (strcat "DATENPUNKT ETCS" 				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq groupTypes '("STANDARD"														)))
		((= _ADM_ _NOBN_) (setq groupTypes '("ENKEL" "DOBBEL"												)))
		((= _ADM_ _FRSR_) (setq groupTypes '("PAIR" "IMPAIR" "BIDIRECTIONNEL"								)))
		((= _ADM_ _DEDB_) (setq groupTypes '("EINZELBALISE-UNGESTEUERT" "EINZELBALISE-GESTEUERT" 
											 "MEHRERE-BALISEN-UNGESTEUERT-1" "MEHRERE-BALISEN-GESTEUERT-1"
											 "MEHRERE-BALISEN-UNGESTEUERT-2" "MEHRERE-BALISEN-GESTEUERT-2"
											 "TRANSITION-UNGESTEUERT-1" "TRANSITION-GESTEUERT-1"
											 "TRANSITION-UNGESTEUERT-2" "TRANSITION-GESTEUERT-2"				)))
	)
	(foreach variation groupTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(cond 
			((= _ADM_ _XXGL_)	(TraceLevel3 "TODO:  XX-GL ETCS-BALISE-GROUP"))
			((= _ADM_ _NOBN_)	(NOBN_DrawEtcsBaliseGroup variation thisDescription))
			((= _ADM_ _FRSR_)	(FRSR_DrawEtcsBaliseGroup variation thisDescription))
			((= _ADM_ _DEDB_)	(DEDB_DrawEtcsBaliseGroup variation thisDescription))
		)
		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
	)
)



(defun ETCS-BALISE ( / blockName description baliseTypes variation thisBlockName thisDescription )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "BAE-" "ETCS-BALISE"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "ETB-" "ETCS-BALISE"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "BAE-" "BALISE-ETCS"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "BAE-" "BALISE-ETCS"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "ETCS BALISE"		)))
		((= _ADM_ _NOBN_) (setq description (strcat "ETCS BALISE"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "BALISE ETCS"		)))
		((= _ADM_ _DEDB_) (setq description (strcat "BALISE ETCS"		)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq baliseTypes '("STANDARD")												))
		((= _ADM_ _NOBN_) (setq baliseTypes '("FAST" "STYRT")											))
		((= _ADM_ _FRSR_) (setq baliseTypes '("FIXE" "FORTEMENT-COMMUTABLE" "FAIBLEMENT-COMMUTABLE")	))
		((= _ADM_ _DEDB_) (setq baliseTypes '("UNGESTEUERT" "GESTEUERT")								))
	)
	(foreach variation baliseTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(cond 
			((= _ADM_ _XXGL_)	(TraceLevel3 "TODO:  XX-GL ETCS-BALISE"))
			((= _ADM_ _NOBN_)	(NOBN_DrawEtcsBalise variation thisDescription))
			((= _ADM_ _FRSR_)	(FRSR_DrawEtcsBalise variation thisDescription))
			((= _ADM_ _DEDB_)	(DEDB_DrawEtcsBalise variation thisDescription))
		)
		(CreateSchematicBlockFromCurrentGraphics thisBlockName)
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		(DrawMetricBaliseGraphics 
			(if (= _ADM_ _FRSR_) 3.5 3.0)	; Nominal distance between balises inside same group
			8.0								; Nominal distance from last controlled balise to device which triggers relevant change in balise telegram
			12.0 							; Nominal distance between two adjacent balise groups (minimum clearance without balise)
			0.250							; Balise physical length in track direction [m]
			0.500							; Balise physical width across track direction [m]
			0.5  							; Metal-free tolerance in track direction (from balise edge)
			0.2  							; Metal-free tolerance across track direction (from balise edge)
		)
		(CreateMetricBlockFromCurrentGraphics thisBlockName)
	)
)



(defun LEGACY-BALISE-GROUP ( / blockName description groupTypes variation thisBlockName thisDescription )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "BGL-" "LEGACY-BALISE-GROUP"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "ATC-" "NSS-BALISEGRUPPE"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "PIK-" "POINT-INFORMATION-KVB"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "DPP-" "DATENPUNKT-PZB"				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "LEGACY BALISE GROUP" 			)))
		((= _ADM_ _NOBN_) (setq description (strcat "NSS BALISEGRUPPE" 				)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT D'INFORMATION KVB" 		)))
		((= _ADM_ _DEDB_) (setq description (strcat "DATENPUNKT PZB" 				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq groupTypes '("STANDARD")))
		((= _ADM_ _NOBN_) (setq groupTypes '("STANDARD")))
		((= _ADM_ _FRSR_) (setq groupTypes '("1" "2")))
		((= _ADM_ _DEDB_) (setq groupTypes '("STANDARD")))
	)
	(foreach variation groupTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(cond 
			((= _ADM_ _XXGL_)	(TraceLevel3 "TODO:  XX-GL LEGACY-BALISE-GROUP"))
			((= _ADM_ _NOBN_)	(NOBN_DrawLegacyBaliseGroup variation thisDescription))
			((= _ADM_ _FRSR_)	(FRSR_DrawLegacyBaliseGroup variation thisDescription))
			((= _ADM_ _DEDB_)	(DEDB_DrawLegacyBaliseGroup variation thisDescription))
		)
		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
	)
)	



(defun LEGACY-BALISE ( / blockName description baliseTypes variation thisBlockName thisDescription )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "BAL-" "LEGACY-BALISE"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "ATB-" "NSS-BALISE" 	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "BAK-" "BALISE-KVB" 	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "BAP-" "BALISE-PZB" 	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "LEGACY BALISE" 	)))
		((= _ADM_ _NOBN_) (setq description (strcat "NSS BALISE" 		)))
		((= _ADM_ _FRSR_) (setq description (strcat "BALISE KVB"	 	)))
		((= _ADM_ _DEDB_) (setq description (strcat "BALISE PZB" 		)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq baliseTypes '("STANDARD")))
		((= _ADM_ _NOBN_) (setq baliseTypes '("TOM-FAST" "TOM-STYRT" "FYLT-FAST" "FYLT-STYRT")))
		((= _ADM_ _FRSR_) (setq baliseTypes '("SERIE-FIXE" "SERIE-COMMUTABLE" "ANALOGIQUE-FIXE" "ANALOGIQUE-COMMUTABLE")))
		((= _ADM_ _DEDB_) (setq baliseTypes '("SCHALTBAR-500" "SCHALTBAR-1000" "SCHALTBAR-2000" "SCHALTBAR-1000-2000" "FEST-500" "FEST-1000" "FEST-2000" "FEST-GSE" "FEST-GSA")))
	)
	(foreach variation baliseTypes
		(setq 
			thisBlockName	(strcat	blockname   "-" variation)
			thisDescription	(strcat	description ", " variation)
		)
		(TraceLevel3 thisDescription)
		(cond 
			((= _ADM_ _XXGL_)	(TraceLevel3 "TODO:  XX-GL LEGACY-BALISE"))
			((= _ADM_ _NOBN_)	(NOBN_DrawLegacyBalise variation thisDescription))
			((= _ADM_ _FRSR_)	(FRSR_DrawLegacyBalise variation thisDescription))
			((= _ADM_ _DEDB_)	(DEDB_DrawLegacyBalise variation thisDescription))
		)
		(CreateSchematicBlockFromCurrentGraphics thisBlockName) 
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		(DrawMetricBaliseGraphics 
			(if (= _ADM_ _FRSR_) 3.5 3.0)	; Nominal distance between balises inside same group
			8.0								; Nominal distance from last controlled balise to device which triggers relevant change in balise telegram
			12.0 							; Nominal distance between two adjacent balise groups (minimum clearance without balise)
			0.600							; Balise physical length in track direction [m]
			0.500							; Balise physical width across track direction [m]
			0.5  							; Metal-free tolerance in track direction (from balise edge)
			0.2  							; Metal-free tolerance across track direction (from balise edge)
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



;================== FRSR functions ==================================================================

(defun FRSR-LEGACY-BALISE-CHAMP ( / blockName description p1 p2 p3 p4 p5 p6 a1 mv attChampKvb variation orientation thisBlockName thisDescription )
	; An arrow pointing in the group's assigned direction (up or down)
	;
	;         3 				           3                             3
	;          \			       5\       \	                 5\       \	
	;   1---.-- 2 		           |*1---.-- 2	                 | 1---.-- 2	
	;      a1  /descr              6/   a1  /descr               6/   a1  /descr
	;         4 	                       4 	                         4	
	;	
	;    DEFAUT-1                  FIXE-1                        VARIABLE-1
	;    
	;    Reversed:
	;    DEFAUT-2                  FIXE-2                        VARIABLE-2
	;
	(setq 				
		blockName	(strcat _SIG_ "CHK-" "CHAMP-KVB")
		description	(strcat "CHAMP KVB")
		p1	'( 0.350   0.000)
		p2	'( 3.200   0.000)
		p3	'( 2.500   0.600)					
		p4	'( 2.500  -0.600)
		p5	'( 0.000   0.300)
		p6	'( 0.000  -0.300)
		a1	'( 0.000  -1.200)		
		attChampKvb '("CHAMP_KVB" "Champ de la balise" _emptyString_)					
	)			 
	(foreach variation '("DEFAUT" "FIXE" "VARIABLE")								
		(foreach orientation '("1" "2")
			(setq
				thisBlockName	(strcat blockName blockName "-" variation "-" orientation)
				thisDescription	(strcat description "-" variation "-" orientation)
			)
			(cond 
				((= variation "FIXE")   
					(command _POLYLINE_ p1 p5 p6 _closedPolyline_)
					(DrawHatch _solidHatch_)							
				)
				((= variation "VARIABLE") 
					(command _POLYLINE_ p1 p5 p6 _closedPolyline_)
				)
			)					
			(DrawLine layDef_Zero p1 p2)
			(DrawLine layDef_Zero p2 p3)
			(DrawLine layDef_Zero p2 p4)
			(MoveLeft (HalfOf (xCoord p2)))
			(if (= orientation "2") (MirrorAboutYaxis _eraseMirrorSource_))

			(AddTextAttributeAtPos layDef_Zero _th100_ a1 attChampKvb)

			(AddDescriptionBelowOrigin thisDescription _two_)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		)
	)
)



(defun FRSR_DrawEtcsBaliseGroup ( variation description / p1 p2 p3 p4 a1 attNomPie )
	; An arrow pointing in the group's assigned direction (1=up, 2=down or 3=both)
	;
	;  ---->     <----     <--->
	;  Var.1     Var.2     Var.3
	;
	;         p3
	;     a1   \			
	;  p1- -.-- p2		
	;          /
	;         p4	
	;
	; groupTypes = "PAIR" "IMPAIR" "BIDIRECTIONNEL"
	(setq
		p1	'( 0.0  0.0)
		p2	'( 3.2  0.0)
		p3	'( 2.5  0.6)
		p4	'( 2.5 -0.6)
		a1	'( 0.0  1.2)
	    attNomPie '("NOM_PIE" "Nom du point d'information ETCS" _emptyString_)
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawLine layDef_Zero p2 p4)
	(MoveLeft (HalfOf (xCoord p2)))
	(if (= variation "IMPAIR") (MirrorAboutYaxis _eraseMirrorSource_))
	(if (= variation "BIDIRECTIONNEL") (MirrorAboutYaxis _keepMirrorSource_))
	(AddTextAttributeAtPos layDef_Zero _th100_ a1 attNomPie)
	(AddDescriptionBelowOrigin description (yCoord p3))
)



(defun FRSR_DrawEtcsBalise ( variation description / x y a1 a2 attAbove attBelow )
	; FRSR ETCS balise, schematic symbol
	; 
	;     a1=attAbove               a1=attAbove                           a1=attAbove      
	;     +-----+                   +-----+                               +-----+          
	;     |     |                   |     |                               |     |          
	;     |  . *| * = solid hatch   |  .  |                               |  . *| * = medium hatch
	;     |     |                   |     |                               |     |          
	; x*y +-----+               x*y +-----+                           x*y +-----+          
	;     a2=attBelow               a2=attBelow	                          a2=attBelow      
	;	                                                             	
	;   Variation 1                Variation 2                          Variation 1        
	;   BALISE_FIXE              BALISE_FORTEMENT_COMMUTABLE            BALISE_FAIBLEMENT_COMMUTABLE       
	;     
	; baliseTypes = "FIXE" "FORTEMENT-COMMUTABLE" "FAIBLEMENT-COMMUTABLE"	
	;
	(setq 
		x	1.5	
		y	2.5
		a1	(list 0 (+ y))
		a2	(list 0 (- y))
		attAbove '("TEXTE_DESSUS" "Texte au-dessus la balise" _emptyString_)
		attBelow '("TEXTE_DESSOUS" "Texte au-dessous la balise" _emptyString_)
	)
	(DrawBox layDef_Zero x y _noWipeout_)	
	(cond 
		((= variation "FIXE") (DrawHatch _solidHatch_))
		((= variation "FORTEMENT-COMMUTABLE") ) ; nothing
		((= variation "FAIBLEMENT-COMMUTABLE") (DrawHatch _mediumHatch_))
	)
	(AddTextAttributeAtPos layDef_Zero _th100_ a1 attAbove)
	(AddTextAttributeAtPos layDef_Zero _th100_ a2 attBelow)
)



(defun FRSR_DrawLegacyBaliseGroup ( variation description / p1 p2 p3 p4 a1 a2 attNomPik )
	; SNCF, Legacy ATP system KVB balise group
	; An arrow pointing in the group's assigned direction (up or down)
	;
	;		a1
	;   2---.---3		Variation 1: As shown
	;  /          \ 	Variation 2: Upsidedown
	; 1            4
	;
	; groupTypes = "1" "2"
	;
	(setq
		p1	'(-1.600  -0.600)
		p2	'(-0.900   0.000)
		p3	'( 0.900   0.000)
		p4	'( 1.600  -0.600)
		a1	'( 0.000   0.600)
		a2	'( 0.000  -0.600)
		attNomPik '("NOM_PIK" "Nom du point d'information KVB" _emptyString_)
	)			 								
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 p4 _openPolyline_)

	(cond
		((= variation "1")
			(AddTextAttributeAtPos layDef_Zero _th100_ a1 attNomPik)
			(AddDescriptionBelowOrigin thisDescription _one_)
		)
		((= variation "2")
			(MirrorAboutXaxis _eraseMirrorSource_)
			(AddTextAttributeAtPos layDef_Zero _th100_ a2 attNomPik)
			(AddDescriptionBelowOrigin thisDescription (- _one_)) ; =above
		)
	)
)



(defun FRSR_DrawLegacyBalise ( variation description / x y b p1 p2 p3 p4 p5 p6 a1 a2 attAbove attBelow )
	; FRSR schematic symbol for KVB system, 'K'ontrôle des Vitesses à Balises
	;
	;         a1=attAbove                   a1=attAbove                a1=attAbove                 a1=attAbove 
	;      +---------------2-+           +---------------2-+                 6                   +---+-------+---+-+ 
	;      |               |*|           |               | |                / \                  |  / \     / \  | |
	;      |        .      |3|           |        .      | |               / . \                 | /   \  ./   \ | |
	;      |               |*|           |               | |              /  *  \                |/     \ /  *  \| |
	;      +---------------1-+           +---------------1-+             4-------5               +-------+-------+-+
	;        a2=attBelow                  a2=attBelow	                a2=attBelow                a2=attBelow 
	;
	;       Variation 1                    Variation 2                  Variation 3                 Variation 4 
	;       SERIE-FIXE                   SERIE-COMMUTABLE             ANALOGIQUE-FIXE            ANALOGIQUE-COMMUTABLE
	;
	; baliseTypes = "SERIE-FIXE" "SERIE-COMMUTABLE" "ANALOGIQUE-FIXE" "ANALOGIQUE-COMMUTABLE"
	;
	(setq 
		x	6.000
		y	3.333		; Triangle height (EPURE symbol is 3 wide and 4 high)
		b	2.500 		; Triangle base
		p1	(list  2.000 (* -0.500 y))
		p2	(list  2.000 (*  0.500 y))
		p3  (list  2.500  0.000)
		p4	(list (* -0.500 b) 0.000)
		p5	(list (*  0.500 b) 0.000)
		p6	(list 0.000 y)
		a1	'( 0.000   5.000)
		a2  '( 0.000  -1.000)		
		attAbove '("TEXTE_DESSUS" "Texte au-dessus la balise" _emptyString_)
		attBelow '("TEXTE_DESSOUS" "Texte au-dessous la balise" _emptyString_)
	)
	(cond 
		((= variation "SERIE-FIXE")
			(DrawBox layDef_Zero x y _noWipeout_)
			(DrawLine layDef_Zero p1 p2)
			(DrawHatchAtPoint _solidHatch_ p3 _angleZero_ _offsetZero_)
		)
		((= variation "SERIE-COMMUTABLE")
			(DrawBox layDef_Zero x y _noWipeout_)
			(DrawLine layDef_Zero p1 p2)
		)
		((= variation "ANALOGIQUE-FIXE")
			(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
			(DrawHatch _solidHatch_)
			(MoveDown (HalfOf y))
		)
		((= variation "ANALOGIQUE-COMMUTABLE")
			(command _POLYLINE_ p4 p5 p6 _closedPolyline_) 
			(DrawHatch _solidHatch_)
			(MoveRight b)
			(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
			(MoveRight (HalfOf b))
			(MoveLeft (HalfOf x))
			(MoveDown (HalfOf y))
			(DrawBox layDef_Zero x y _noWipeout_)
			(DrawLine layDef_Zero p1 p2)
		)
	)
	(AddTextAttributeAtPos layDef_Zero _th100_ (list 0 (*  0.800 y)) attAbove)
	(AddTextAttributeAtPos layDef_Zero _th100_ (list 0 (* -0.800 y)) attBelow)
)



;================== DEDB functions ==================================================================
(defun DEDB-SPEED-SUPERVISION-MAGNET ( / )
	(TraceLevel3 "*** TODO ***")
	; Call to DEDB_DrawLegacySpeedSupervisionMagnet
)



(defun DEDB_DrawEtcsBaliseGroup ( variation description / x y p1 p2 p3 p4 p5 p6 p7 p8 p9 ) ; a1 attNameDpe )
	; A rectangle pr balise group, filled if controlled balise(s). An empty "arrow" triangle means multiple balises, indicating the direction of the group.
	; Var. 3 and 4 are made in left / right subvariants (1=right/2=left version)
	;
	;     +-----------+          +-----------+         +-----------2\          +------------2\          +--6-----9--2\          +--6-----9--2\   
	;  ---+     .     +----   ---+     . *   +---   ---+     .     | 1---   ---+      . *   | 1---   ---+ / \ . / \ | 1---   ---+ /*\ . /*\ | 1---
	;     +-----------+          +-----------+         +-----------3/          +------------3/          +4---5-7---83/          +4---5-7---83/   
	;
	;  Single balise/fixed    Single balise/contr'd Multiple balises/fixed  Multiple balises/contr'd Class-B trans'n/fixed   Class-B trans'n/contr'd
	;
	; groupTypes (n = 1/right or 2/left):
	; 
	; "EINZELBALISE-UNGESTEUERT" "EINZELBALISE-GESTEUERT" "MEHRERE-BALISEN-UNGESTEUERT-n" "MEHRERE-BALISEN-GESTEUERT-n" "TRANSITION-UNGESTEUERT-n" "TRANSITION-GESTEUERT-n"
	;
	(setq
		x	5.0
		y	2.0
		p1	(list (+ (HalfOf x) y) 0)
		p2	(PosTR x y)
		p3	(posBR x y)
		p4	(list (* -0.400 x) (* -0.45 y))
		p5	(list (* -0.000 x) (* -0.45 y))
		p6	(list (* -0.200 x) (*  0.45 y))
		p7	(list (*  0.000 x) (* -0.45 y))
		p8	(list (*  0.400 x) (* -0.45 y))
		p9	(list (*  0.200 x) (*  0.45 y))
;;;		a1	(list 0 (+ (HalfOf y) _th180_))
;;;	    attNameDpe '("NAME_DPE" "Name des ETCS Datenpunkts" _emptyString_)
	)
	(DrawBox layDef_Zero x y layDef_BaliseGroup_Wipeout)

	; Hatch if at least one controlled balise in group but this is not a level transition group:
	(if (and (wcmatch variation "*`-GESTEUERT*") (not (wcmatch variation "TRANSITION*")))
		(DrawHatch _denseHatch_)
	)
	
	; Add direction arrow if two or more balises in group: 
	(if (or (wcmatch variation "MEHRERE*") (wcmatch variation "TRANSITION*"))
		(progn
			(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
			(AddWipeoutToLastClosedPolyline layDef_BaliseGroup_Wipeout _keepWipeoutSource_)
		)
	)
	
	; Flip around Y-axis if leftward version:
	(if (wcmatch variation "*-2")
		(MirrorAboutYaxis _eraseMirrorSource_)
	)

	; Add two balises inside if transition-into-CLass-B 
	(if (wcmatch variation "TRANSITION*")
		(progn
			(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
			(if (wcmatch variation "*`-GESTEUERT*") (DrawHatch _denseHatch_))
			(command _POLYLINE_ p7 p8 p9 _closedPolyline_)
			(if (wcmatch variation "*`-GESTEUERT*") (DrawHatch _denseHatch_))
		)
	)

	; Epilogue:
;;;	(AddTextAttributeAtPos layDef_Zero _th180_ a1 attNameDpe)
	(AddDescriptionBelowOrigin description (* 1.2 (HalfOf y)))
)



(defun DEDB_DrawEtcsBalise ( variation description / )
	; DB ETCS balise symbols are not specified in the Richtlinien, so these are RailCOMPLETE's suggestion:
	;
	;                   
	;    +---+      +---+
	;    |   |      |   |
	;    | . |      | .*|
	;    |   |      |   |
	;    +---+      +---+
	;                
	;    Fixed    Controlled
	;
	; baliseTypes = "FAST" "STYRT"
	;
	(TraceLevel3 "*** TODO : ASK DB FOR CORRECT ETCS BALISE SYMBOL***")
	(setq
		x	0.8
		y	1.6
;;;		a1	(list 0 (+ (+ (HalfOf y) _th180_)))
;;;		a2	(list 0 (- (+ (HalfOf y) _th180_)))
;;;		attAbove '("TEXTUEBER" "Tekst over" _emptyString_)
;;;		attBelow '("TEXTUNTER" "Tekst under" _emptyString_)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(cond 
		((= variation "UNGESTEUERT")
		)
		((= variation "GESTEUERT")
			(DrawHatch _solidHatch_)
		)
	)
;;;	(AddTextAttributeAtPos layDef_Zero _th125_ a1 attAbove)
;;;	(AddTextAttributeAtPos layDef_Zero _th125_ a2 attBelow)
	(AddDescriptionBelowOrigin thisDescription (HalfOf y))
)



(defun DEDB_DrawLegacyBaliseGroup ( variation description / )
	; There are no symbols for the DB legacy balise *group* , only for single Indusi balises (Gleismagnet)
	;
	;         a1
	; +-------.-------+
	; |               |
	; +---------------+ 5x1
	;
	(setq
		x	6
		y	2
		a1	(list 0 (+ y _th180_))
	    attNameDpp '("NAME_DPP" "Name des PZB Datenpunkts" _emptyString_)
	)
	(TraceLevel3 "*** TODO ***")
;;;	TODO: Add real variations:
;;;	(cond 
;;;		((= variation "xxx")
;;;		)
;;;		((= variation "xxx")
;;;			(DrawHatch _solidHatch_)
;;;		)
;;;	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddTextAttributeAtPos layDef_Zero _th180_ a1 attNameDpp)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(MoveDown (HalfOf y))
)



(defun DEDB_DrawLegacyBalise ( variation description / )
	; PZB / Indusi-90 system balise symbols, ref. Ril.819.9002_V02 ch. 17.
	;
	; Schaltbare Gleismagnete (gesteuert):
	; 500, 1000, 2000 und 1000/2000 Hz
	; F = fahrstrassengesteuert
	;
	; To be placed to the right of the tight rail in the balise group's direction of validity:
	;
	; ---+----.----+---    ---+----.----+---    ---+----.----+---    ---+----.----+---   
	;    |         |          |         |          |         |          |    |    |      
	; 0,5+---------+ F      1 +---------+        2 +---------+       1/2+----+----+  
	;    Schaltbar 500        Schaltbar 1000       Schaltbar 2000       Schaltbar 1000/2000
	;
	;
	;
	; Ständig wirksame Gleismagnete:
	; 500, 1000 2000 Hz und GÜ- Ein- bzw. Ausschaltmagnet für die Lageplandarstellung
	;
	; ---+----.----+---    ---+----.----+---    ---+----.----+---    ---+----.----+---   
	;    ||       ||          ||       ||          ||       ||      GSE |solidFill|      
	; 0,5+=========+        1 +=========+        2 +=========+     /GSA +---------+
	;    Fest 500             Fest 1000            Fest 2000            Fest GSE/GSA
	;
	; baliseTypes = "SCHALTBAR-500" "SCHALTBAR-1000" "SCHALTBAR-2000" "SCHALTBAR-1000-2000" "FEST-500" "FEST-1000" "FEST-2000" "FEST-GSE" "FEST-GSA"
	;
	(setq
		x	5.0
		y	1.0
		a1	(list 0 (+ (+ y _th180_)))
		a2	(list 0 (- (+ y _th180_)))
		attAbove '("TEXTUEBER" "Tekst over" _emptyString_)
		attBelow '("TEXTUNTER" "Tekst under" _emptyString_)
	)
	(TraceLevel3 "*** TODO ***")
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddTextAttributeAtPos layDef_Zero _th180_ a1 attAbove)
	(AddTextAttributeAtPos layDef_Zero _th180_ a2 attBelow)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(MoveDown (HalfOf y))
)



(defun DEDB_DrawLegacySpeedSupervisionMagnet ( variation description / )
	; ???????????????? Something is fishy in the Ril symbolism for GÜ: ??????????
	; Geschwindigkeitsüberwachungseinrichtungen (GÜ) Blockschaltbilder für Übersichtspläne.
	; GÜ Einschalt-, Gleis-, Ausschaltmagnet (gesteuert)
	;
	; -----+---+=====.=====+---+---   -----+---+=====.=====+---+--
	;      | * |           | * |           | * |           | * |   
	; 1/GÜ +---+-----------+---+      2/GÜ +---+-----------+---+ 
	;           Variation 1                     Variation 2   
	;
	;
	;
	; GÜ Einschalt-, Gleis-, Ausschaltmagnet (ungesteuert, ständig wirksam)
	;
	; -----++---+----.----+----+---   -----+---+-----.-----+---+--
	;      || * |         | * ||           ||* |           | * |   
	;      |+---+---------+---+|           |+--+-----------+---+ 
	; 1/GÜ +-------------------+      2/GÜ +-------------------+ 
	;        Variation 1                Variation 2   
	;
	;
	;
	; GÜ Ausschalt-, Einschalt-, Gleismagnet
	;
	; ------+---+-+--.+-------+----   -----++---+-+--.+-------++--
	;       | * | | * |       |            || * | | * |       ||   
	;       +---+-+---+-------+            |+---+-+---+-------+| 
	; 1/GÜ                            2/GÜ +-------------------+ 
	;        Variation 1                Variation 2   
	;
	(TraceLevel3 "*** TODO ***")
	(DrawBox layDef_Zero 6 1 _noWipeout_)
	(AddDescriptionBelowOrigin description (* 1.50 y))
	(MoveUp y)
)
