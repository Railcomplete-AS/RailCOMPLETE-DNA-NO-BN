;=========================================================================================================================
;
; E204.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Select GSM-R network Norway


(defun E204 ( /	blockName description x y m p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 
			p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26
			p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43
			p44 p45)
	;
	; TL-------TR
	; |   \\    |
	; |   \     |
	; |    \\   |
	; |  GSM-R  |
	; |  (N)    |
	; |         |
	; BL---.---BR 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-ERTMS-SELECT-GSM-R-NETWORK-NORWAY"
		description "SKILT ERTMS SELECT GSM-R NETWORK NORWAY"
		x 4.0
		y 5.72
		m 0.36
		p1  '(0.13 0.68)
		p2  '(1.77 2.01)
		p3  '(-0.11 1.04)
		p4  '(0.33 0.60)
		p5  '(0.29 0.80)
		p6  '(0.13 0.68)
		p7  '(0.33 0.60)
		p8  '(0.71 -0.10)
		p9  '(0.73 1.11)
		p10 '(0.15 0.05)
		p11 '(0.74 1.06)
		p12 '(-0.22 0.40)
		p13 '(3.09 2.92)
		p14 '(-0.55 0.88)
		p15 '(3.31 2.83)
		p16 '(-0.78 1.43)
		p17 '(0.15 1.81)
		p18 '(-0.84 1.93)
		p19 '(0.35 1.77)
		p20 '(-0.68 2.40)
		p21 '(-0.29 1.69)
		p22 '(-0.31 1.50)
		p23 '(-0.14 1.57)
		p24 '(-0.29 1.69)
		p25 '(-0.11 1.04)
		p26 '(2.09 2.29)
		p27 '(-0.31 1.50)
		p28 '(-0.20 1.75)
		p29 '(-0.59 2.44)
		p30 '(-0.46 2.52)
		p31 '(-0.40 2.38)
		p32 '(-0.31 2.49)
		p33 '(-0.02 1.99)
		p34 '(-0.14 1.92)
		p35 '(-0.03 1.84)
		p36 '(-0.20 1.75)
		p37 '(0.80 -0.05)
		p38 '(0.41 0.64)
		p39 '(0.54 0.72)
		p40 '(0.60 0.58)
		p41 '(0.69 0.69)
		p42 '(0.98 0.19)
		p43 '(0.86 0.12)
		p44 '(0.97 0.04)
		p45 '(0.80 -0.05)
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
 	(DrawBox layDef_Zero (- x m) (- y m) layDef_BoardOrPole_Wipeout)
	; DRAW PHONE
	(DrawArcByCenter layDef_Zero p2 p3  p1 )
	(DrawArcByCenter layDef_Zero p5 p6  p4 )
	(DrawLine layDef_Zero  p7 p8  )
	(DrawArcByCenter layDef_Zero p9 p10  p8 )
	(DrawArcByCenter layDef_Zero p11 p12  p10 )
	(DrawArcByCenter layDef_Zero p13 p14  p12 )
	(DrawArcByCenter layDef_Zero p15 p16  p14 )
	(DrawArcByCenter layDef_Zero p17 p18  p16 )
	(DrawArcByCenter layDef_Zero p19 p20  p28 )
	(DrawLine layDef_Zero  p20 p21  )
	(DrawArcByCenter layDef_Zero p23 p24  p22 )
	(DrawArcByCenter layDef_Zero p26 p27  p25 )

	(DrawLine layDef_Zero  p28 p29  )
	(DrawLine layDef_Zero  p29 p30  )
	(DrawArcByCenter layDef_Zero p31 p32  p30 )
	(DrawLine layDef_Zero  p32 p33  )
	(DrawArcByCenter layDef_Zero p34 p35  p33 )
	(DrawLine layDef_Zero  p35 p36  )

	(DrawLine layDef_Zero  p37 p38  )
	(DrawLine layDef_Zero  p38 p39  )
	(DrawArcByCenter layDef_Zero p40 p41  p39 )
	(DrawLine layDef_Zero  p41 p42  )
	(DrawArcByCenter layDef_Zero p43 p44  p42 )
	(DrawLine layDef_Zero  p44 p45  )

	
	(AddTextAtPoint layDef_Zero _th070_ '(0 -0.7) "GSM-R")
 	(command _ELLIPSE_ _setEllipseCenter_ '(0 -2.0) '(1 -2.0) 0.55)
	(AddTextAtPoint layDef_Zero _th070_ '(0 -2.0) "N")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)