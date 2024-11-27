;=========================================================================================================================
;
; 75.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Mileage / Raise/Lower snow-clearing device / Mileage chain break

; For debugging:
; (75A-1) (75A-2) (75A-3) (75B) (75C-1) (75C-2) (75D-1) (75D-2) (75E-1) (75E-2)

(defun 75A-1 ( / blockName description x y p1 p2 attWholeKm attHalfKm )
	; Mileage, modern type, single-sided
	;
	; +-------+
	; |       |
	; |  p1   | 
	; |  p2   |
	; |       |
	; +---.---+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-75A-1-KILOMETER-ENKELSIDET"
		description "SKILT 75A-1 KILOMETERSKILT, ENKELSIDET"
		x 4.875
		y 6.000
		p1 (list 0 4.3)
		p2 (list 0 1.7)
		attWholeKm	'("HEL_KM" "Hel km" "462")
		attHalfKm	'("HALV_KM" "Halv km (0 eller 5)" "5")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(AddTextAttributeAtPoint layDef_Zero _th180_ p1 attWholeKm)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p2 attHalfKm)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75A-2 ( / blockName description x y attWholeKm )
	; Mileage, old type, single-sided, just Km
	;
	; +-------+
	; |  462  |
	; +---.---+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-75A-2-KILOMETER-GAMMEL"
		description "SKILT 75A-2 KILOMETERSKILT, GAMMEL TYPE"
		x 6.0
		y 3.0
		attWholeKm	'("HEL_KM_UTEN_HALV" "Hel km" "462")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAttributeAtPoint layDef_Zero _th180_ (Point11 y) attWholeKm)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75A-3 ( / blockName description x y attWholeKm attHalfKm p1 p2 p3 p4 p5 p6 pLeft pRight attWholeKm attHalfKm attWholeKm2 attHalfKm2 )
	; Mileage, modern type, double-sided
	;
	; Example: Double-sided kilometration board at Km.462,5:
	;
	; |----x----|--3--|----x----|
	;
	; +---------+     +---------+    ---
	; |   4     |     |     4   |     |
	; |   6 5   p5-.-p6   5 6   |     y
	; |   2     |     |     2   |     |
	; +---------+     +---------+    ---
	;    p1 p2          p3 p4
	;  
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-75A-3-KILOMETER-DOBBELSIDET"
		description "SKILT 75A-3 KILOMETERSKILT, DOBBELSIDET"
		x 6.0
		y 4.875
		p1 (list (+ -1.5 (* -0.75 x)) 0)
		p2 (list (+ -1.5 (* -0.25 x)) 0)
		p3 (list (+  1.5 (*  0.25 x)) 0)
		p4 (list (+  1.5 (*  0.75 x)) 0)
		p5 (list -1.5 0)
		p6 (list  1.5 0)
		pLeft (list (+ -1.5 (* -0.5 x)) 0) ; Left box center
		pRight (list (+  1.5 (*  0.5 x)) 0) ; Right box center
		attWholeKm	'("HEL_KM" "Hel km" "462")
		attHalfKm	'("HALV_KM" "Halv km (0 eller 5)" "5")
		attWholeKm2	'("HEL_KM2" "Hel km" "462")
		attHalfKm2	'("HALV_KM2" "Halv km (0 eller 5)" "5")
	)

	; left box
	(DrawBoxAtPoint layDef_Zero pLeft x y layDef_BoardOrPole_Wipeout)
	(AddAtt "HEL_KM"  "Hel km"              "462" p1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "HALV_KM" "Halv km (0 eller 5)" "5"   p2 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	; right box
	(DrawBoxAtPoint layDef_Zero pRight x y layDef_BoardOrPole_Wipeout)
	(AddAtt "HALV_KM2" "Halv km (0 eller 5)" "5"   p3 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "HEL_KM2"  "Hel km"              "462" p4 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	; connect the boxes
	(DrawLine layDef_Zero p5 p6)

	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75B ( / blockName description x y p1 p2 p3 p4a p4b p5 p6 p7a p7b p8 p9 p10 p11 p12 pLeft pRight )
	; Mileage, tunnel type, double-sided and narrow
	;
	; Example: Double-sided kilometration board at Km.462,5:
	;
	; +-------+----+     +----+-------+
	; | 4 6 2 | 5 11--.--12 5 | 2 6 4 | y
	; +-------+----+     +----+-------+
	;  p1 2 3 4 5          p6 7 8 9 10
	;      x       |<-3->|      x
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-75B-KILOMETER-TUNNEL-DOBBELSIDET"
		description "SKILT 75B KILOMETERSKILT FOR TUNNELER, DOBBELSIDET"
		x  9.0
		y  3.0
		p1  (list (+ -1.5 (* -0.875 x)) 0)
		p2  (list (+ -1.5 (* -0.625 x)) 0)
		p3  (list (+ -1.5 (* -0.375 x)) 0)
		p4a (list (+ -1.5 (* -0.250 x)) (* -0.5 y))
		p4b (list (+ -1.5 (* -0.250 x)) (*  0.5 y))
		p5  (list (+ -1.5 (* -0.125 x)) 0)
		p6  (list (+  1.5 (*  0.125 x)) 0)
		p7a (list (+  1.5 (*  0.250 x)) (* -0.5 y))
		p7b (list (+  1.5 (*  0.250 x)) (*  0.5 y))
		p8  (list (+  1.5 (*  0.375 x)) 0)
		p9  (list (+  1.5 (*  0.625 x)) 0)
		p10 (list (+  1.5 (*  0.875 x)) 0)
		p11 (list -1.5 0)
		p12 (list  1.5 0)
		pLeft  (list (+ -1.5 (* -0.5 x)) 0)
		pRight (list (+  1.5 (*  0.5 x)) 0)
	)
	; NB: Can't use AddTextAttributeAtPoint() here because it doesn't rotate attributes by 90 degrees.
	; left box
	(DrawBoxAtPoint layDef_Zero pLeft x y layDef_BoardOrPole_Wipeout)
	(AddAtt "HUNDRE_KM" "100 km" "4" p1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "TI_KM"     "10 km" "6" p2 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "EN_KM"     "1 km" "2" p3 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(DrawLine layDef_Zero p4a p4b)
	(AddAtt "HALV_KM"   "Halv km (0 eller 5)" "5" p5 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	; right box
	(DrawBoxAtPoint layDef_Zero pRight x y layDef_BoardOrPole_Wipeout)
	(AddAtt "HALV_KM2"   "Halv km (0 eller 5)" "5" p6 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(DrawLine layDef_Zero p7a p7b)
	(AddAtt "EN_KM2"     "1 km" "2" p8 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "TI_KM2"     "10 km" "6" p9 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "HUNDRE_KM2" "100 km" "4" p10 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	; connect the boxes
	(DrawLine layDef_Zero p11 p12)

	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75C-1 ( / blockName description p1 p2 p3 p4 p5 p6 )
	; Raise track cleaning equipment, right side of track
	;
	;   3
	;  / \
	; 2   4---5
	; | p6    |
	; 1-------.
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75C-HEV-HSIDE"
		description (strcat "SKILT 75C-1 HEV SPORRENSER, H" _uOSLASH_ "YREMONTERT")
		p1 (list -8.0 0.0)
		p2 (list -8.0 2.0)
		p3 (list -6.0 5.5)
		p4 (list -4.0 2.0)
		p5 (list  0.0 2.0)
		p6 (list -6.0 1.0)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 p4 p5 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(AddTextAtPoint layDef_Zero _th150_ p6 "Hev")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75C-2 ( / blockName description p1 p2 p3 p4 p5 p6 )
	; Raise track cleaning equipment, left side of track
	;
	;       3
	;      / \
	; 5---4   2
	; |    p6 |
	; .-------1
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75C-HEV-VSIDE"
		description "SKILT 75C-2 HEV SPORRENSER, VENSTREMONTERT"
		p1 (list  8.0 0.0)
		p2 (list  8.0 2.0)
		p3 (list  6.0 5.5)
		p4 (list  4.0 2.0)
		p5 (list  0.0 2.0)
		p6 (list  6.0 1.0)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 p4 p5 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(AddTextAtPoint layDef_Zero _th180_ p6 "Hev")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75D-1 ( / blockName description p1 p2 p3 p4 p5 p6 )
	; Lower track cleaning equipment, right side of track
	;
	; 2-------1
	; | p6    |
	; 3   5---.
	;  \ /
	;   4
	;
	(setq
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75D-SENK-HSIDE"
		description (strcat "SKILT 75D-1 SENK SPORRENSER, H" _uOSLASH_ "YREMONTERT")
		p1 (list  0.0  2.0)
		p2 (list -8.0  2.0)
		p3 (list -8.0  0.0)
		p4 (list -6.0 -3.5)
		p5 (list -4.0  0.0)
		p6 (list -6.0  1.0)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 p4 p5 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(AddTextAtPoint layDef_Zero _th150_ p6 "Senk")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75D-2 ( / blockName description p1 p2 p3 p4 p5 p6 )
	(setq
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75D-SENK-VSIDE"
		description "SKILT 75D-2 SENK SPORRENSER, VENSTREMONTERT"
	)
	; Lower track cleaning equipment, left side of track
	;
	; 1-------2
	; |    p6 |     
	; .---5   3
	;      \ /
	;       4
	;
	(setq
		p1 (list  0.0  2.0)
		p2 (list  8.0  2.0)
		p3 (list  8.0  0.0)
		p4 (list  6.0 -3.5)
		p5 (list  4.0  0.0)
		p6 (list  6.0  1.0)
	)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ _origin_ p1 p2 p3 p4 p5 _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(AddTextAtPoint layDef_Zero _th150_ p6 "Senk")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75E_1 ( / blockName description x y p1 p2 p3 p4 p5 p6 attFrom1 attFrom2 attTo1 attTo2 attJump t1 a1 a2 a3 a4 a5 a6 a7 ) ; Note: 75e-1 would be translated to 7.5 (75*10^-1 = 7.5)
	; Mileage chain break
	;
	; Rounded reference mileage "Km" - meant for wayside informative boards, front towards the track, informing maintenance
	; personnel (and tamping machine drivers) where they are in relation to the 'linear address position name' for the
	; overall railway line. More often than not, this means the rightmost straightest track if more tracks are present.
	;
	; Example: In = 462999,83775, Out = 465123,75678, Jump = +2123,91903
	;
	; +-------------------------+ ;                               +-------------------------+
	; |        KJEDEBRUDD       | ;     t1        fixed text      |        KJEDEBRUDD       |
	; 1------------5------------2 ;                               +------------+------------+
	; |    FRA_1   |   TIL_1    | ;  a1    a3     text attributes |     463    |    465     |
	; |    FRA_2   |   TIL_2    | ;  a2    a4     text attributes |     000    |    124     |
	; 3------------6------------4 ;                               +------------+------------+
	; | FRA_REF SPRANG  TIL_REF | ;  a6 a5 a7     text attribute  | DBVB     +2124m    DBSB |
	; +-----------.-------------+ ;                               +------------.------------+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75E-1-KJEDEBRUDD"
		description "SKILT SIGNAL 75E-1 KJEDEBRUDD"
		x 20 ; 15.75 in BN symbol library
		y 12 ; 11.25 in BN synbol library
		p1 (list (* -0.500 x) (*  0.250 y))
		p2 (list (*  0.500 x) (*  0.250 y))
		p3 (list (* -0.500 x) (* -0.250 y))
		p4 (list (*  0.500 x) (* -0.250 y))
		p5 (list (*  0.000 x) (*  0.250 y))
		p6 (list (*  0.000 x) (* -0.250 y))
		attFrom1	'("FRA_1" "Fra km:"               "463"		)
		attFrom2	'("FRA_2" "Fra meter:"            "000"		)
		attFrom3	'("FRA_REF" "Fra referanselinje:" "DBVB"	)
		attTo1		'("TIL_1" "Til km:"               "465"		)
		attTo2		'("TIL_2" "Til meter:"            "124"		)
		attTo3		'("TIL_REF" "Til referanselinje:" "DBSB"	)
		attJump		'("SPRANG" "Sprang:"              "+2124m"	)
		t1 (list (*  0.000 x) (*  0.375 y))
		a1 (list (* -0.250 x) (*  0.125 y))
		a2 (list (* -0.250 x) (* -0.125 y))
		a3 (list (*  0.250 x) (*  0.125 y))
		a4 (list (*  0.250 x) (* -0.125 y))
		a5 (list (*  0.000 x) (* -0.375 y))
		a6 (list (* -0.450 x) (* -0.375 y))
		a7 (list (*  0.450 x) (* -0.375 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(AddTextAtPoint layDef_Zero _th150_ t1 "KJEDEBRUDD")
	(SetLayer layDef_Zero)
	; Use AddAtt (instead of AddTextAttributeAtPoint) in order to dictate justification left/middle/right:
	;(AddAtt              attTag              attPrompt        attDefaultValue point textHeight _angleZero_ _rcTextStyle_ _middleCenter_)
	(AddAtt (eval (nth 0 attFrom1)) (eval (nth 1 attFrom1)) (eval (nth 2 attFrom1)) a1 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attFrom2)) (eval (nth 2 attFrom2)) (eval (nth 2 attFrom2)) a2 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attTo1  )) (eval (nth 1 attTo1  )) (eval (nth 2 attTo1  )) a3 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attTo2  )) (eval (nth 2 attTo2  )) (eval (nth 2 attTo2  )) a4 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attJump )) (eval (nth 3 attJump )) (eval (nth 2 attJump )) a5 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attFrom3)) (eval (nth 3 attFrom3)) (eval (nth 2 attFrom3)) a6 _th150_ _angleZero_ _rcTextStyle_ _middleLeft_	)
	(AddAtt (eval (nth 0 attTo3  )) (eval (nth 3 attTo3  )) (eval (nth 2 attTo3  )) a7 _th150_ _angleZero_ _rcTextStyle_ _middleRight_	)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 75E_2 ( / blockName description x y p1 p2 p3 p4 p5 p6 attFrom1 attFrom2 attTo1 attTo2 attJump t1 a1 a2 a3 a4 a5 ) ; Note: 75e-2 would be translated to 0.75 (75*10^-2 = 0.75)
	; Exact mileage chain break - meant for being glued as an engraved yellow plate, approx 7x15 cm, to the side of a rail
	; at the exact point where the chain break takes place.
	; Note: 'profile' means the distance along 'linear position name' in that particular track.
	;
	; Example: In = 462999,83775, Out = 465123,75678, Jump = +2123,91903
	;
	; +-------------------------+ ;                               +-------------------------+
	; |    EKSAKT KJEDEBRUDD    | ;     t1        fixed text      |    EKSAKT KJEDEBRUDD    |
	; 1------------5------------2 ;                               +------------5------------+
	; |      FRA_1 | TIL_1      | ;  a1    a2     text attributes |    Pr.462, | Pr.465,    |
	; |      FRA_2 | TIL_2      | ;  a3    a4     text attributes |    999838  | 123757     |
	; 3------------6------------4 ;                               +------------6------------+
	; |         SPRANG          | ;     a5        text attribute  |        +2123,919m       |
	; +-----------.-------------+ ;                               +-----------.-------------+
	; 																		          
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75E-2-EKSAKT-KJEDEBRUDD"
		description "SKILT SIGNAL 75E-2 EKSAKT KJEDEBRUDD"
		x 20
		y 12
		p1 (list (* -0.500 x) (*  0.250 y))
		p2 (list (*  0.500 x) (*  0.250 y))
		p3 (list (* -0.500 x) (* -0.250 y))
		p4 (list (*  0.500 x) (* -0.250 y))
		p5 (list (*  0.000 x) (*  0.250 y))
		p6 (list (*  0.000 x) (* -0.250 y))
		attFrom1	'("FRA_1" "Fra km:"         "Pr.462,"	)
		attFrom2	'("FRA_2" "Fra millimeter:" "999838"	)
		attTo1		'("TIL_1" "Til km:"         "Pr.465,"	)
		attTo2		'("TIL_2" "Til millimeter:" "123757"	)
		attJump		'("SPRANG" "Sprang:"        "+2123,919"	)
		t1 (list (*  0.000 x) (*  0.375 y))
		a1 (list (* -0.250 x) (*  0.125 y))
		a2 (list (* -0.250 x) (* -0.125 y))
		a3 (list (*  0.250 x) (*  0.125 y))
		a4 (list (*  0.250 x) (* -0.125 y))
		a5 (list (*  0.000 x) (* -0.375 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(AddTextAtPoint layDef_Zero _th150_ t1 "EKSAKT KJEDEBRUDD")
	(SetLayer layDef_Zero)
	; Use AddAtt (instead of AddTextAttributeAtPoint) in order to dictate justification left/middle/right:
	;(AddAtt               attTag             attPrompt        attDefaultValue point textHeight _angleZero_ _rcTextStyle_ _middleCenter_)
	(AddAtt (eval (nth 0 attFrom1)) (eval (nth 1 attFrom1)) (eval (nth 2 attFrom1)) a1 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attFrom2)) (eval (nth 2 attFrom2)) (eval (nth 2 attFrom2)) a2 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attTo1  )) (eval (nth 1 attTo1  )) (eval (nth 2 attTo1  )) a3 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attTo2  )) (eval (nth 2 attTo2  )) (eval (nth 2 attTo2  )) a4 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(AddAtt (eval (nth 0 attJump )) (eval (nth 3 attJump )) (eval (nth 2 attJump )) a5 _th150_ _angleZero_ _rcTextStyle_ _middleCenter_	)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
