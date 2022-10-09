;=========================================================================================================================
;
; 64.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Poles

; For debugging:
; (63A) (63B)

(defun 64A ( / blockName description TL TR BL BR )
	; Shunting area border pole -  "Grense/Innkoblingsstolpe"
	; 
	; Nine alternating bands - Black (top) and white
	; 
	;     TL---TR
	;     p51-p52
	;     |     |
	;     p43-p44
	;     p41-p42
	;     |     |
	;     p33-p34
	;     p31-p32
	;  t1 |     | t2   Text positions
	;     p23-p24
	;     p21-p22
	;     |     |
	;     p13-p14
	;     p11-p12
	;     |     |
	;     |     |
	;     |     |
	;     BL-.-BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64A-GRENSE-INNKOBLINGSSTOLPE"
		description "SKILT SIGNAL 64A GRENSE- / INNKOBLINGSSTOLPE"
		TL	'(-0.5 10.5)	TR  (AddVectors TL  '(0.7 0))
		p51	'(-0.5  9.5)	p52 (AddVectors p51 '(0.7 0))
		p43	'(-0.5  8.5)	p44 (AddVectors p43 '(0.7 0))
		p41	'(-0.5  7.5)	p42 (AddVectors p41 '(0.7 0))
		p33	'(-0.5  6.5)	p34 (AddVectors p33 '(0.7 0))
		p31	'(-0.5  5.5)	p32 (AddVectors p31 '(0.7 0))
		p23 '(-0.5  4.5)	p24 (AddVectors p23 '(0.7 0))
		p21	'(-0.5  3.5)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  2.5)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  1.5)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  6.0)    t2  '(2.0  6.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	; No hatch
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_) ; 2
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p23 p24 p32 p31 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p31 p32 p34 p33 _closedPolyline_) ; 3
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p33 p34 p42 p41 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p41 p42 p44 p43 _closedPolyline_) ; 4
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p43 p44 p52 p51 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p51 p52 TR TL _closedPolyline_) ; 5
	(DrawHatch _blackHatch_) 
	
	(AddAtt "LeftUp" "LeftUp" "Svart/Hvit" t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" "Svart/Hvit" t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 64B ( / blockName description TL TR BL BR )
	; Catenary sectioning pole - "Seksjoneringsstolpe"
	; 
	; Four alternating bands - Red (top) and white
	; 
	;     TL---TR
	;     |     |
	;     | Red |
	;     |     |
	;     p21-p22
	;     |     |
	;     | Wh. |
	;     |     |
	;  t1 p13-p14 t2   Text positions
	;     |     |
	;     | Red | 
	;     |     |
	;     p11-p12
	;     |     |
	;     | Wh. |
	;     |     |
	;     BL-.-BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64B-SEKSJONERINGSSTOLPE"
		description "SKILT SIGNAL 64B SEKSJONERINGSSTOLPE"
		TL	'(-0.5 11.2)	TR  (AddVectors TL  '(0.7 0))
		p21	'(-0.5  8.4)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  5.6)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  2.8)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  5.0)    t2  '(2.0  5.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	; No hatch
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _redHatch_)

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p21 p22 TR TL _closedPolyline_) ; 2
	(DrawHatch _redHatch_)
	
	(AddAtt "LeftUp" "LeftUp" (strcat "R" _OSLASH_ "d/Hvit") t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" (strcat "R" _OSLASH_ "d/Hvit") t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 64C ( / blockName description TL TR BL BR )
	; Avalanche warning pole - "Rasvarslingsstolpe"
	; 
	; Nine alternating bands - Yellow (top) and white
	; 
	;     TL---TR
	;     p51-p52
	;     |     |
	;     p43-p44
	;     p41-p42
	;     |     |
	;     p33-p34
	;     p31-p32
	;  t1 |     | t2   Text positions
	;     p23-p24
	;     p21-p22
	;     |     |
	;     p13-p14
	;     p11-p12
	;     |     |
	;     |     |
	;     |     |
	;     BL-.-BR
	; 
	(setq	
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64C-RASVARSLINGSSTOLPE"
		description "SKILT SIGNAL 64C RASVARSLINGSSTOLPE"
		TL	'(-0.5 10.5)	TR  (AddVectors TL  '(0.7 0))
		p51	'(-0.5  9.5)	p52 (AddVectors p51 '(0.7 0))
		p43	'(-0.5  8.5)	p44 (AddVectors p43 '(0.7 0))
		p41	'(-0.5  7.5)	p42 (AddVectors p41 '(0.7 0))
		p33	'(-0.5  6.5)	p34 (AddVectors p33 '(0.7 0))
		p31	'(-0.5  5.5)	p32 (AddVectors p31 '(0.7 0))
		p23 '(-0.5  4.5)	p24 (AddVectors p23 '(0.7 0))
		p21	'(-0.5  3.5)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  2.5)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  1.5)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  6.0)    t2  '(2.0  6.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	; No hatch
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _yellowHatch_) 

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_) ; 2
	(DrawHatch _yellowHatch_) 

	(command _POLYLINE_ p23 p24 p32 p31 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p31 p32 p34 p33 _closedPolyline_) ; 3
	(DrawHatch _yellowHatch_) 

	(command _POLYLINE_ p33 p34 p42 p41 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p41 p42 p44 p43 _closedPolyline_) ; 4
	(DrawHatch _yellowHatch_) 

	(command _POLYLINE_ p43 p44 p52 p51 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p51 p52 TR TL _closedPolyline_) ; 5
	(DrawHatch _yellowHatch_) 
	
	(AddAtt "LeftUp" "LeftUp" "Gul/Hvit" t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" "Gul/Hvit" t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 64D ( / blockName description TL TR BL BR )
	; Brake application pole - "Bremsestolpe"
	; 
	; Four alternating bands - Yellow (top) and black
	; 
	;     TL---TR
	;     |     |
	;     | Ylw |
	;     |     |
	;     p21-p22
	;     |     |
	;     | Blk |
	;     |     |
	;  t1 p13-p14 t2   Text positions
	;     |     |
	;     | Ylw | 
	;     |     |
	;     p11-p12
	;     |     |
	;     | Blk |
	;     |     |
	;     BL-.-BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64D-BREMSESTOLPE"
		description "SKILT SIGNAL 64D BREMSESTOLPE"
		TL	'(-0.5 11.2)	TR  (AddVectors TL  '(0.7 0))
		p21	'(-0.5  8.4)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  5.6)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  2.8)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  5.0)    t2  '(2.0  5.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	(DrawHatch _blackHatch_)
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _yellowHatch_)

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	(DrawHatch _blackHatch_)
	(command _POLYLINE_ p21 p22 TR TL _closedPolyline_) ; 2
	(DrawHatch _yellowHatch_)
	
	(AddAtt "LeftUp" "LeftUp" "Gul/svart" t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" "Gul/svart" t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 64E ( / blockName description TL TR BL BR )
	; Techincal pole "Teknisk stolpe"
	; 
	; Nine alternating bands - Blue (top) and white
	; 
	;     TL---TR
	;     p51-p52
	;     |     |
	;     p43-p44
	;     p41-p42
	;     |     |
	;     p33-p34
	;     p31-p32
	;  t1 |     | t2   Text positions
	;     p23-p24
	;     p21-p22
	;     |     |
	;     p13-p14
	;     p11-p12
	;     |     |
	;     |     |
	;     |     |
	;     BL-.-BR
	; 
	(setq	
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64E-TEKNISK-STOLPE"
		description "SKILT SIGNAL 64E TEKNISK STOLPE"
		TL	'(-0.5 10.5)	TR  (AddVectors TL  '(0.7 0))
		p51	'(-0.5  9.5)	p52 (AddVectors p51 '(0.7 0))
		p43	'(-0.5  8.5)	p44 (AddVectors p43 '(0.7 0))
		p41	'(-0.5  7.5)	p42 (AddVectors p41 '(0.7 0))
		p33	'(-0.5  6.5)	p34 (AddVectors p33 '(0.7 0))
		p31	'(-0.5  5.5)	p32 (AddVectors p31 '(0.7 0))
		p23 '(-0.5  4.5)	p24 (AddVectors p23 '(0.7 0))
		p21	'(-0.5  3.5)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  2.5)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  1.5)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  6.0)    t2  '(2.0  6.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	; No hatch
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _blueHatch_) 

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_) ; 2
	(DrawHatch _blueHatch_) 

	(command _POLYLINE_ p23 p24 p32 p31 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p31 p32 p34 p33 _closedPolyline_) ; 3
	(DrawHatch _blueHatch_) 

	(command _POLYLINE_ p33 p34 p42 p41 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p41 p42 p44 p43 _closedPolyline_) ; 4
	(DrawHatch _blueHatch_) 

	(command _POLYLINE_ p43 p44 p52 p51 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p51 p52 TR TL _closedPolyline_) ; 5
	(DrawHatch _blueHatch_) 
	
	(AddAtt "LeftUp" "LeftUp" (strcat "Bl" _ARING_ "/Hvit") t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" (strcat "Bl" _ARING_ "/Hvit") t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 64F ( / blockName description TL TR BL BR )
	; Shunting signal pole  -  "Skiftestolpe" Old type, marks where the signal should have been placed
	; 
	; Six alternating bands - Black (top) and white
	; 
	;     TL---TR
	;     | Blk |
	;     p31-p32
	;     |     |
	;     p23-p24
	;  t1 | Blk | t2   Text positions
	;     p21-p22
	;     |     |
	;     p13-p14
	;     | Blk |
	;     p11-p12
	;     |     |
	;     BL-.-BR
	; 
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-64F-DVERGSIGNALSTOLPE"
		description "SKILT SIGNAL 64F DVERGSIGNALSTOLPE"
		TL	'(-0.5  6.5)	TR  (AddVectors TL  '(0.7 0))
		p31 '(-0.5  5.5)	p32 (AddVectors p31 '(0.7 0))
		p23 '(-0.5  4.5)	p24 (AddVectors p23 '(0.7 0))
		p21	'(-0.5  3.5)	p22 (AddVectors p21 '(0.7 0))
		p13	'(-0.5  2.5)	p14 (AddVectors p13 '(0.7 0))
		p11	'(-0.5  1.5)	p12 (AddVectors p11 '(0.7 0))
		BL	'(-0.5  0.0)	BR  (AddVectors BL  '(0.7 0))
		t1  '(-2.0  4.0)    t2  '(2.0  4.0)
	)
	; Wipeout under pole
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR TR TL _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _eraseWipeoutSource_)

	; Alternating bands
	(SetLayer layDef_Zero)
	(command _POLYLINE_ BL BR p12 p11 _closedPolyline_) ; Bottom shaft
	; No hatch
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_) ; 1
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	; No hatch
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_) ; 2
	(DrawHatch _blackHatch_) 

	(command _POLYLINE_ p23 p24 p32 p31 _closedPolyline_) ; 2
	; No hatch
	(command _POLYLINE_ p31 p32 TR TL _closedPolyline_) ; 3
	(DrawHatch _blackHatch_) 
	
	(AddAtt "LeftUp" "LeftUp" "Svart/Hvit" t1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(AddAtt "RightUp" "RightUp" "Svart/Hvit" t2 _th180_ _angleMinus90_ _rcTextStyle_ _middleCenter_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
