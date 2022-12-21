;=========================================================================================================================
;
; 68.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Speed boards

; For debugging:
; (68A) (68B) (68C) (68D-1) (68D-2) (68D-3) (68F) (68G)

(defun 68A ( / blockName description side p1 p2 attTen attFive )
	; Warning reduced speed ahead (basic speed, applies to all train types)
	;
	; +-------+
	;  \   p2/
	;   \ p1/
	;    \ /
	;     .
	;
	(setq 
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68A-NEDSATT-HASTIGHET"
		description (strcat "SKILT SIGNAL 68A NEDSATT KJ" _uOSLASH_ "REHASTIGHET")
		side 	6.0
		p1		(list (* 0.00 side) (* 0.60 side))
		p2		(list (* 0.30 side) (* 0.70 side))
		attTen	'("HAST_10" "Vent-hastighet (10-ere):" "12")
		attFive	'("HAST_5" "Blank eller 5" _ENTER_)
	)
	(DrawTriangle side) ; pointing down
	(AddTextAttributeAtPoint layDef_Zero _th250_ p1 attTen)
	(AddTextAttributeAtPoint layDef_Zero _th100_ p2 attFive)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68B ( / blockName description side p1 p2 attTen attFive )
	; Increased speed (basic speed, applies to all train types)
	;
	;     +
	;    / \
	;   / p1\
	;  /   p2\
	; +---.---+
	;
	(setq 
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68B-OEKT-HASTIGHET"
		description (strcat "SKILT SIGNAL 68B " _uOSLASH_ "KT KJ" _uOSLASH_ "REHASTIGHET")
		side 	6.0
		p1		(list (* 0.00 side) (* 0.30 side))
		p2		(list (* 0.30 side) (* 0.15 side))
		attTen	'("HAST_10" (strcat "Kj" _OSLASH_ "-hastighet (10-ere):") "12")
		attFive	'("HAST_5" "Blank eller 5" _ENTER_)
	)
	(DrawTriangle side) ; pointing down
	(MirrorAboutXaxis _eraseMirrorSource_)
	(MoveUp (* (sqrt 0.75) side))
	(AddTextAttributeAtPoint layDef_Zero _th250_ p1 attTen)
	(AddTextAttributeAtPoint layDef_Zero _th100_ p2 attFive)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68C ( / blockName description r attTen )
	; Speed other than default speed 40 in next switch or switch group
	;      ___ 
	;     /   \    
	;    (  .  )  Speed at origin.
	;     \___/  
	;      
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68C-NEDSATT-HASTIGHET"
		description (strcat "SKILT SIGNAL 68C NEDSATT KJ" _uOSLASH_ "REHASTIGHET")
		r 		2.75
		attTen	'("HAST_10" (strcat "Kj" _OSLASH_ "-hastighet (10-ere):") "12")
	)
	(DrawCircle layDef_Zero r layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th250_ _origin_ attTen)
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68D-1 ( / blockName description side )
	; Marker for speed reduction start
	;
	; +-----+
	;  \   /
	;   \ /
	;    .
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68D-1-MARKERINGSMERKE"
		description "SKILT SIGNAL 68D-1 MARKERINGSMERKE"
		side 3.75
	)
	(DrawTriangle side)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68D-2 ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 )
	; Marker for speed reduction start, on the rear of the 'Increased speed' board
	;
	;      1
	;     /*\
	; 2 3-----4 5
	;   6     7    Clear triangle on filled background.
	;  /*\   /*\
	; 8---9 10--11
	;      .
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68D-2-MARKERINGSMERKE-MOTRETTET"
		description "SKILT SIGNAL 68D-2 MARKERINGSMERKE PÅ MOTRETTET SIGNAL 68B"
		;	6.0  ; 68B board size
		;	3.75 ; 68D board size
		p1 	'( 0.000 5.774)
		p2	'(-1.875 3.248)
		p3	'(-1.375 3.248)
		p4	'( 1.375 3.248)
		p5	'( 1.875 3.248)
		p6	'(-1.625 2.815)
		p7	'( 1.625 2.815)
		p8	'(-3.000 0.433)
		p9	'(-0.250 0.433)
		p10	'( 0.250 0.433)
		p11	'( 3.000 0.433)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p4 p5 p7 p11 p10 _origin_ p9 p8 p6 p2 p3 _closedPolyline_) ; contour
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(command _POLYLINE_ p1 p3 p4 _closedPolyline_)	(DrawHatch _solidHatch_) ; top triangle
	(command _POLYLINE_ p6 p8 p9 _closedPolyline_)	(DrawHatch _solidHatch_) ; left triangle
	(command _POLYLINE_ p7 p10 p11 _closedPolyline_)	(DrawHatch _solidHatch_) ; right triangle
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68D-3 ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 attTen )
	; Marker for speed reduction start, on the rear of the 'Increased speed after temporary restriction' board 69
	;
	;        p1 
	;      /    \
	;     p2     p7
	;     |       |
	;     p3---- p6  
	;     | \   / |
	;     |   p8  |  
	;    p4---.--p5 
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68D-3-MARKERINGSMERKE-MOTRETTET-MIDLERTIDIG"
		description "SKILT SIGNAL 68D-2 MARKERINGSMERKE PÅ MOTRETTET SIGNAL 69B"
		p1	'( 0.000 7.125)
		p2	'(-1.875 4.875)
		p3	'(-1.875 4.000)
		p4	'(-1.875 0.000)
		p5	'( 1.875 0.000)
		p6	'( 1.875 4.000)
		p7	'( 1.875 4.875)
		p8	'( 0.000 0.750)
		p9	'( 0.000 2.900)
		attTen	'("HAST_10" (strcat "Kj" _OSLASH_ "-hastighet (10-ere):") "12")
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p1 p2 p3 p4 p5 p6 p7 _closedPolyline_) ; contour
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(command _POLYLINE_ p1 p2 p3 p6 p7 _closedPolyline_)
	(DrawHatchAtPoint _solidHatch_ (AddVectors p1 _slightlyBelow_) _angleZero_ _offsetZero_) ; top 5-edge
	(command _POLYLINE_ p3 p4 p5 p6 p8 _closedPolyline_)
	(DrawHatchAtPoint _solidHatch_ (AddVectors p8 _slightlyBelow_) _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 68F ( / blockName description x y attDef )
	; Additional speed for trains with better running characteristic (bogies, air-damped coaches, low roll factor etc)
	;
	; TL--------TR
	; | addSpeed |
	; BL---.----BR
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68F-TILLEGGSHASTIGHET"
		description "SKILT SIGNAL 68F TILLEGGSHASTIGHET"
		x 7.0
		y 4.0
		attDef '("PLUSS_HAST" "Pluss-hastighet" "+5")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th250_ _origin_ attDef)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
 
 
 
(defun 68G ( / blockName description x y attDef )
	; Tilting train speed
	;
	; TL=========TR
	; ||         ||
	; ||tiltSpeed|| ; Tilting train max speed
	; ||         ||
	; BL====.====BR
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-68G-HASTIGHET-KRENGETOG"
		description "SKILT SIGNAL 68G HASTIGHET FOR KRENGETOG"
		x 9.0
		y 6.0
		x2 7.5
		y2 5.0
		attDef '("KRENGE_HAST" "Krengetoghastighet (km/h)" "160")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawBox layDef_Zero x2 y2 _noWipeout_)
	(MoveUp (HalfOf y))
	(DrawHatchAtPoint _solidHatch_ _slightlyAbove_ _angleZero_ _offsetZero_)
	(AddTextAttributeAtPoint layDef_Zero _th350_ (list 0 (HalfOf y)) attDef)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
