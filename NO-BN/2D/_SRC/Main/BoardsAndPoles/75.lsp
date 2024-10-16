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
; (75A-1) (75A-2) (75A-3) (75B) (75C-1) (75C-2) (75D-1) (75D-2) (75E)

(defun 75A-1 ( / blockName description x y p1 p2 attWholeKm attHalfKm )
	; Mileage, modern type, single-sided
	;
	; TL-----TR
	; |       |
	; |  p1   | 
	; |  p2   |
	; |       |
	; BL--.--BR
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
	; TL-----TR
	; |  462  |
	; BL--.--BR
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
	; |----x----|--3--|----x----|
	;
	; TL-------TR     TL-------TR    ---
	; |   4     |     |     4   |     |
	; |   6 5   p5-.-p6   5 6   |     y
	; |   2     |     |     2   |     |
	; BL-------BR     BL-------BR    ---
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
	;
	; TL------+---TR     TL---+------TR
	; | 4 6 2 | 5 11--.--12 5 | 2 6 4 | y
	; BL------+---BR     BL---+------BR
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



(defun 75E ( / blockName description x y p1 p2 p3 p4 p5 p6 attFrom1 attFrom2 attTo1 attTo2 attJump p7 p8 p9 p10 p11 p12 )
	; Mileage chain break
	;
	; TL-----------TR
	; | KJEDEBRUDD  | ;   p7		fixed text
	; 1------5------2
	; |FRA_1 |TIL_1 | ; p8  p10		text attributes
	; |FRA_2 |FRA_2 | ; p9  p11		text attributes
	; 3------6------4
	; |   SPRANG    | ;   p12		text attribute
	; BL-----.-----BR
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-SIGNAL-75E-KJEDEBRUDD"
		description "SKILT SIGNAL 75E KJEDEBRUDD"
		x 15.75
		y 11.25
		p1 (list (* -0.5 x) (*  0.25 y))
		p2 (list (*  0.5 x) (*  0.25 y))
		p3 (list (* -0.5 x) (* -0.25 y))
		p4 (list (*  0.5 x) (* -0.25 y))
		p5 (list (*  0.0 x) (*  0.25 y))
		p6 (list (*  0.0 x) (* -0.25 y))
		attFrom1	'("FRA_1" "Fra km:"   "15")
		attFrom2	'("FRA_2" "Fra meter/millimeter:" "422")
		attTo1		'("TIL_1" "Til km:"   "15")
		attTo2		'("TIL_2" "Til meter/millimeter:" "450")
		attJump		'("SPRANG" "Sprang:"   "+28m")
		p7  (list (*  0.00 x) (*  0.375 y))
		p8  (list (* -0.25 x) (*  0.125 y))
		p9  (list (* -0.25 x) (* -0.125 y))
		p10 (list (*  0.25 x) (*  0.125 y))
		p11 (list (*  0.25 x) (* -0.125 y))
		p12 (list (*  0.00 x) (* -0.375 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(AddTextAtPoint layDef_Zero _th150_ p7 "KJEDEBRUDD")
	(AddTextAttributeAtPoint layDef_Zero _th180_ p8 attFrom1)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p9 attFrom2)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p10 attTo1)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p11 attTo2)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p12 attJump)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
