;=========================================================================================================================
;
; 64H.lsp
;
; Copyright (c) 2015 2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022 06 02 KNHEL new sign
; 2026 01 16 CLFEY Moved contents from former E204.lsp.
;
;=========================================================================================================================

; 64H Select GSM R network Norway

;=================================================================================================================================
; Utdrag fra Operativt Regelverk (ORV) pr 2026 01 16
;                                                    
; 8.68 Orienteringsstolper og  skilt; (utdrag)
; Signal 64H «Norsk togradionettverk» er satt opp ved riksgrensen ved overgang til norsk togradionettverk på strekning med ERTMS
; Hvitt skilt med sort telefon symbol og sort tekst

; Signal							Signalnummer og signalnavn			Signalbetydning
;                               	                              		                              
; Hvitt skilt med sort telefon 		Signal 64H							Strekningen har norsk togradionettverk.
; symbol og sort tekst.				«Norsk togradionettverk»			
;=================================================================================================================================

(defun 64H ( /	blockName description x y m p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 
			p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26
			p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 )
	;
	; +---------+
	; |   \\    |
	; |   \     |	; An old fashioned telephone handle symbol
	; |    \\   |
	; |  GSM R  |
	; |  (N)    |
	; |         |
	; +----.----+ 
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-64H-NORSK-TOGRADIONETTVERK"
		description "SKILT NORSK TOGRADIONETTVERK"
		x 4.0
		y 5.72
		m 0.36

		; Main handle
		p1  '( 0.330  0.600)
		p2  '( 0.219  0.613)
		p3  '( 0.127  0.677)	; Arc p1 p2 p3
		p4  '(-0.123  1.072)
		p5  '(-0.318  1.497)	; Arc p3 p4 p5
		p6  '(-0.330  1.597)
		p7  '(-0.290  1.690)	; Arc p5 p6 p7
		p8  '(-0.680  2.400)	; Line p7 p8
		p9  '(-0.785  2.173)	
		p10 '(-0.840  1.930)	; Arc p8 p9 p10
		p11 '(-0.838  1.677)
		p12 '(-0.780  1.430)	; Arc p10 p11 p12
		p13 '(-0.549  0.888)
		p14 '(-0.230  0.392)	; Arc p12 p13 p14
		p15 '(-0.053  0.207)
		p16 '( 0.150  0.050)	; Arc p14 p15 p16
		p17 '( 0.421 -0.058)
		p18 '( 0.710 -0.098)	; Arc p16 p17 p18
								; Line p18 p1
		
		; Microphone (lower, mouth)
		p19 '( 0.970  0.040)
		p20 '( 0.800 -0.050)
		p21 '( 0.410  0.640)
		p22 '( 0.540  0.720)	; Polyline p19 p20 p21 p22
		p23 '( 0.618  0.721)
		p24 '( 0.690  0.690)	; Arc p22 p23 p24
		p25 '( 0.980  0.190)	; Line p24 p25
		p26 '( 0.996  0.114)
								; Arc p25 p26 p19
		
		; Loudspeaker (upper, ear)
		p27 '(-0.030  1.840)
		p28 '(-0.200  1.750)
		p29 '(-0.590  2.440)
		p30 '(-0.460  2.520)	; Polyline p27 p28 p28 p30
		p31 '(-0.382  2.521)
		p32 '(-0.310  2.490)	; Arc p30 p31 p32
		p33 '(-0.020  1.990)	; Line p32 p33
		p34 '(-0.004  1.914)
								; Arc p33 p34 p27

		; "GSM-R"
		p35 '( 0.000  0.700)

		; "N"
		p36 '( 0.000  2.000)
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
 	(DrawBox layDef_Zero (- x m) (- y m) layDef_BoardOrPole_Wipeout)

	; Main handle
	(DrawArc layDef_Zero p1 p2 p3)
	(DrawArc layDef_Zero p3 p4 p5)
	(DrawArc layDef_Zero p5 p6 p7)
	(DrawLine layDef_Zero  p7 p8)
	(DrawArc layDef_Zero p8 p9 p10)
	(DrawArc layDef_Zero p10 p11 p12)
	(DrawArc layDef_Zero p12 p13 p14)
	(DrawArc layDef_Zero p14 p15 p16)
	(DrawArc layDef_Zero p16 p17 p18)
	(DrawLine layDef_Zero p18 p1)

	; Microphone (lower, mouth)
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p19 p20 p21 p22 _openPolyline_)
	(DrawArc layDef_Zero p22 p23 p24)
	(DrawLine layDef_Zero p24 p25)
	(DrawArc layDef_Zero p25 p26 p19)

	; Loudspeaker (upper, ear)
	(command _POLYLINE_ p27 p28 p29 p30 _openPolyline_)
	(DrawArc layDef_Zero p30 p31 p32)
	(DrawLine layDef_Zero p32 p33)
	(DrawArc layDef_Zero p33 p34 p27)

	(MoveUp (HalfOf y))

	; "GSM-R"
	(AddTextAtPoint layDef_Zero _th070_ p35 "GSM R")
 	(command _ELLIPSE_ _setEllipseCenter_ p36 '(1  2.0) 0.55)
	
	; "N"
	(AddTextAtPoint layDef_Zero _th070_ p36 "N")

	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)