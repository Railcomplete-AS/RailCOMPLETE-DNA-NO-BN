;=========================================================================================================================
;
; Lightweight foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2017-02-15 BEHOP/CLFEY 
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Lightweight mounting (installation) devices (smaller than 100 kg / for boards and smaller items / can be installed without de-stabilizing the track)

; Ref. http://euroskilt.no/oppsetningsutstyr

(defun C:LIGHTWEIGHT-FOUNDATION ( / )

	; KU-SKF Skiltfeste
	(subSubstep "Skiltfeste_Betongfot / 500")			(Skiltfeste_Betongfot_60_225 "500")
	(subSubstep "Skiltfeste_Betongfot_60_225 / 700")	(Skiltfeste_Betongfot_60_225 "700")
	(subSubstep "Skiltfeste_Betongfot_60_200_1200)")	(Skiltfeste_Betongfot_60_200_1200)
	(subSubstep "Skiltfeste_Jordfundament_60_700")		(Skiltfeste_Jordfundament_60_700)
	(subSubstep "Skiltfeste_Jordspyd_60")				(Skiltfeste_Jordspyd_60)
	(subSubstep "Skiltfeste_Tilbakefylt_Grop")			(Skiltfeste_Tilbakefylt_Grop)
	(subSubstep "Skiltfeste_Strips_Eller_Patentbaand")	(Skiltfeste_Strips_Eller_Patentbaand)
	(subSubstep "Skiltfeste_Brakett_Mot_Vegg_60_110")	(Skiltfeste_Brakett_Mot_Vegg_60_110)
	(subSubstep "Skiltfeste_Brakett_Mot_Gulv_60_308")	(Skiltfeste_Brakett_Mot_Gulv_60_308)
	(subSubstep "Skiltfeste_Brakett_Mot_Mast")			(Skiltfeste_Brakett_Mot_Mast)

	; SA-FUN Stolpe med fot / r�rmast med fot
	(subSubstep "UNP80-STOLPE-MED-LJERN-FOT / 1")	(UNP80-STOLPE-MED-LJERN-FOT 1) ; Enkel S-l�s
	(subSubstep "UNP80-STOLPE-MED-LJERN-FOT / 2")	(UNP80-STOLPE-MED-LJERN-FOT 2) ; Dobbel S-l�s
	(subSubstep "UNP80-STOLPE-MED-LJERN-FOT / 3")	(UNP80-STOLPE-MED-LJERN-FOT 3) ; Lokalstiller
	(subSubstep "UNP80-STOLPE-MED-LJERN-FOT / 4")	(UNP80-STOLPE-MED-LJERN-FOT 4) ; Overdragstrafo
	(subSubstep "ROERMAST-MED-LJERN-FOT / 1")		(ROERMAST-MED-LJERN-FOT 1) ; �76 h�yde 2000 mm for togsporsignal og dvergsignal
	(subSubstep "ROERMAST-MED-LJERN-FOT / 2")		(ROERMAST-MED-LJERN-FOT 2) ; �76 h�yde 3000 mm for h�ye dvergsignaler og togsporsignaler
	(subSubstep "ROERMAST-MED-LJERN-FOT / 3")		(ROERMAST-MED-LJERN-FOT 3) ; �76 h�yde 1600 mm for kryssvekselsignal
	(subSubstep "ROERMAST-MED-LJERN-FOT / 4")		(ROERMAST-MED-LJERN-FOT 4) ; �127 h�yde 3200 mm for lave signaler
)



(defun Skiltfeste_Betongfot_60_225 ( variation / blockName description s1 s2 x d r )
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-SKILTFESTE-BETONGFOT-60-225-" variation)
		description	_ENTER_ ; See below
		s1 (/ 0.150 2)
		s2 (/ 0.255 2)
		x 0.150 ; four "flaps"
		d 0.12
		r 0.03
	)
	(defun localGraphics (/)
		(command 
			_LINE_ (list s2 0) (list s2 s1) (list s1 s2) (list 0 s2) _ENTER_
			_LINE_ (polar  (list x x) (D->R -45) d) (polar  (list x x) (D->R (+ _angle90_ 45)) d) _ENTER_
			_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
			_CIRCLE_ _origo_ r
		)
	)

	; Schematic symbol
	(cond
		((= variation "500") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H500")))
		((= variation "700") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H700")))
		(T (setq description "Something is fishy with SKILTFESTE BETONGFOT "))
	)
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Betongfot_60_200_1200 ( / blockName description ang1 d_ang R1 R2 rad1 rad2 )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BETONGFOT-60-200-1200"
		description	(strcat "SKILTFESTE, BETONGFOT " _uOE_ "60/H1200")
		ang1 5.0	; Add a bias angle to the symbol
		d_ang 45.0	; Angular length of "surrounding lines"
		R1 0.18	; �360 surrounding lines in symbol (4 times 45-deg pieces)
		R2 0.22	; �440 surrounding lines in symbol (4 times 45-deg pieces)
		pr 0.03	; �60 pole radius
		fr 0.10 ; �200 foundation radius
	)
	(defun localGraphics (/)
		(command
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angleZero_)) R1) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angleZero_)) R2) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angle90_)) R1) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angle90_)) R2) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angle180_)) R1) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angle180_)) R2) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angleMinus90_)) R1) _setArcAngle_ (rtos d_ang)
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (+ ang1 _angleMinus90_)) R2) _setArcAngle_ (rtos d_ang)
			_CIRCLE_  _origo_ pr
			_CIRCLE_  _origo_ fr
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Jordfundament_60_700 ( / blockName description rad1 rad2 ang )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-JORDFUNDAMENT-60-700"
		description	(strcat "SKILTFESTE, JORDFUNDAMENT " _uOE_ "60/H700")
		pr		0.030 ; �060 pole
		rad2	0.181 ; 0.151 side wings from the inner pole/tube
		ang		_angle45_
	)
	(defun localGraphics (/)
		(command 
			_CIRCLE_ _origo_ pr
			_LINE_ (list (* pr (DDcos ang)) (* pr (DDsin ang))) (list (* rad2 (DDcos ang)) (* rad2 (DDsin ang))) _ENTER_
			_ARRAY_ _lastSelection_ _ENTER_ _polarArray_ _origo_ 4 _fullCircle_ _rotateObjects_
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Jordspyd_60 ( / blockName description x y r )
	(setq 
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-JORDSPYD-60"
		description	(strcat "SKILTFESTE, JORDSPYD " _uOE_ "60/H600")
		x   0.100	; Square symbolizes the L-iron bar
		y	0.100
		r	0.030	; For �60 poles, an L-iron with a �60 hose on top
	)
	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(drawBox layDef_Zero x y _noWipeout_)
	(drawCircle layDef_Zero r _noWipeout_)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawBox layDef_MetricDetails x y _noWipeout_)
	(drawCircle layDef_MetricDetails r _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Tilbakefylt_Grop ( / blockName description r )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-TILBAKEFYLT-GROP"
		description	"SKILTFTESTE, TILBAKEFYLT GROP"
		r	0.030	; For �60 pole
	)
	(defun localgraphics (/)
		(command 
			_POLYLINE_ 
				"0.3094,0.5743" 
				_setPolylineArcMode_
				_setPolylineArcCenter_ "0.1572,0.5517" "0.0839,0.6870"
				_setPolylineArcCenter_ "-0.0163,0.5716" "-0.1528,0.6263"
				_setPolylineArcCenter_ "-0.2561,0.5066" "-0.4019,0.5570"
				_setPolylineArcCenter_ "-0.4856,0.4273" "-0.6361,0.4616"
				_setPolylineArcCenter_ "-0.4869,0.4239" "-0.4713,0.2708"
				_setPolylineArcCenter_ "-0.4470,0.1070" "-0.5927,0.0281"
				_setPolylineArcCenter_ "-0.6526,-0.1172" "-0.8096,-0.1106"
				_setPolylineArcCenter_ "-0.7679,-0.2624" "-0.8963,-0.3534"
				_setPolylineArcCenter_ "-0.7367,-0.3321" "-0.6621,-0.4748"
				_setPolylineArcCenter_ "-0.5606,-0.3520" "-0.4105,-0.4054"
				_setPolylineArcCenter_ "-0.2817,-0.3217" "-0.1590,-0.4141"
				_setPolylineArcCenter_ "-0.0258,-0.3273" "0.1013,-0.4227"
				_setPolylineArcCenter_ "0.1893,-0.29" "0.3441,-0.3274"
				_setPolylineArcCenter_ "0.4638,-0.2306" "0.5957,-0.31"
				_setPolylineArcCenter_ "0.5641,-0.1448" "0.7085,-0.0586"
				_setPolylineArcCenter_ "0.5549,-0.024" "0.5350,0.1321"
				_setPolylineArcCenter_ "0.4179,0.2440" "0.4829,0.3922"
				_setPolylineArcCenter_ "0.3324,0.4225" "0.3094,0.5743"
				_ENTER_
			_SCALE_ _selectAll_ _ENTER_ _origo_ _half_
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(drawCircle layDef_MetricDetails r _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Strips_Eller_Patentbaand ( / blockName description r )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-STRIPS-ELLER-PATENTBAAND"
		description	(strcat "SKILTFESTE, STRIPS ELLER PATENTB" _uAA_ "ND")
		r	0.030	; For �60 pole
	)
	(defun localGraphics (/)
		(command 
			_POLYLINE_
				"-0.5987,0.25"
				"-0.4487,0.25"
				_setPolylineArcMode_
				_setPolylineArcCenter_ "-0.4487,0.1513"
				_setPolylineArcAngle_ _angleMinus90_
				_setPolylineLineMode_
				"-0.35,0"
				_setPolylineArcMode_
				_setPolylineArcCenter_ _origo_ "0,-0.35"
				_ENTER_

			_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_ROTATE_ _selectAll_ _ENTER_ _origo_ _angle180_
			_SCALE_ _selectAll_ _ENTER_ _origo_ _quarter_
			_CIRCLE_ _origo_ r
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Brakett_Mot_Gulv_60_308 ( / blockName description ang1 rad1 rad2 pos side )
	;
	; TL-----TR
	; | 1   2 |
	; |  (.)  |  ; �60 pole stub welded onto a 255x255 plate with four �14 holes placed 164x164 apart.
	; | 3   4 |
	; BL-----BR
	;
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-MOT-GULV-60-308"
		description	(strcat "SKILTFESTE, BRAKETT MOT GULV " _uOE_ "60/308")
		s	0.255	; baseplate side
		b	0.164	; baseplate holes spacing
		pr 	0.030	; �60 pole radius
		br 	0.007	; �14 bolt radius
		p1  (posTL b b)
		p2  (posTR b b)
		p3  (posBL b b)
		p4  (posBR b b)
	)
	(defun localGraphics (/)
		(command
			_RECTANGLE_ (posBL s s) (posTR s s)
			_CIRCLE_ p3 br
			_CIRCLE_ p4 br
			_CIRCLE_ p5 br
			_CIRCLE_ p6 br
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Brakett_Mot_Mast ( / blockName description r )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-PAA-ANNEN-MAST"
		description	(strcat "SKILTFESTE, BRAKETT P" _uAA_ " ANNEN MAST")
		r	0.030	; For �60 pole
	)
	(defun localGraphics (/)
		(command 
			_ARC_ _setArcCenter_ _origo_ '(0.1 0.2) '(-0.1 0.2)
			_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
			_CIRCLE_ _origo_ r
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun Skiltfeste_Brakett_Mot_Vegg_60_110 ( / blockName description wingStart wingLength wingIncrY )
	(setq
		blockName	"NO-BN-2D-JBTUB-SKILTFESTE-BRAKETT-MOT-VEGG-60-110"
		description	(strcat "SKILTFESTE, VEGGFESTE " _uOE_ "60/110")
		backPlate	0.200
		wingStart	0.150
		wingLength	0.150
		wingIncrY	0.050
	)
	(defun localGraphics (/)
		(command  ; Three "eagle wings" to each side of insertion point:
			_LINE_ (list wingStart 0) (list (+ wingStart wingLength) 0) _ENTER_
			_LINE_ (list wingStart wingIncrY) (list (+ wingStart wingLength) wingIncrY) _ENTER_
			_LINE_ (list wingStart (- wingIncrY)) (list (+ wingStart wingLength) (- wingIncrY)) _ENTER_
			_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		)
		(command ; Outline of wall-mount bracket (for boards etc �60):
			_LINE_ (list (- (/ backPlate 2)) 0) (list (/ backPlate 2) 0) _ENTER_
			_RECTANGLE_ 
				(list (- (/ backPlate 4)) 0)
				(list (/ backPlate 4) (- (/ backPlate 2)))
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun UNP80-STOLPE-MED-LJERN-FOT ( variation / blockName description t1 t2 x1 x2 x3 y1 y2 y3 p1 p2 p3 xBar yBar )
	; Ref S.018812 Stolpe for festeplater for lokalstiller type Satema S-l�ser type DSI Overdragstrafo TM 14
	; Stolpen er en UNP80, 2200 h�y / 800 under bakken, 1400 over bakken. Kommer med 4 stk festeplate-varianter,
	; enkel S-l�s (bxh = 170x350), dobbel S-l�s (410x350), lokalstiller 148x96), overdragstrafo 195x240). 6mm tykke plater.
	; Platene flukter i overkant stolpe. U vender forover. Dekkplate over kabler i h�yder 700-870 og 930-1400 over UK stolpe.
	;
	;        _
	;       | |
	;       | |
	;    ___|_|___________
	;   |________4________|
	;       | +=====+
	;       |5||2,3||
	;       =====.=====    . = 1
	;       | |
	;       | |
	;       +-+ 
	;
	;
	(cond
		((= variation 1) (setq t1 "ENKEL-SAMLELAAS"  t2 (strcat "ENKEL S-L" _uAA_ "S")  x1 0.170)) ; x1 = plate width
		((= variation 2) (setq t1 "DOBBEL-SAMLELAAS" t2 (strcat "DOBBEL S-L" _uAA_ "S") x1 0.410))
		((= variation 3) (setq t1 "LOKALSTILLER"     t2 "LOKALSTILLER"                  x1 0.148))
		((= variation 4) (setq t1 "OVERDRAGSTRAFO"   t2 "OVERDRAGSTRAFO"                x1 0.195))
	)
	(setq 
		blockName	(strcat "NO-BN-2D-JBTSI-STOLPE-MED-LJERN-FOR-" t1)
		description	(strcat "STOLPE UNP80 MED L-JERN FOT FOR " t2)
		y1 0.006 	; plate thickness
		p1 '(0.000 0.003) ; center thin plate
		x2 0.080	; UNP width
		y2 0.046	; UNP depth (well, .045)
		p2 '(0.000 0.029) ; center UNP80
		x3 0.072	; Inner UNP lining
		y3 0.040	
		p3 '(0.000 0.026)
		xBar 0.500 ; L-bar foot length
		yBar 0.060 ; L-bar foot width
		p4 '(0.000 0.082) ; y=0.006 + 0.046 + 0.060/2
		p5 '(-0.070 0.029)
	)
	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(setLayer layDef_Zero)
	(drawBoxAtPos layDef_Zero p1 x1 y1 _noWipeout_)
	(drawBoxAtPos layDef_Zero p2 x2 y2 _noWipeout_)
	(drawBoxAtPos layDef_Zero p3 x3 y3 _noWipeout_)
	(drawBoxAtPos layDef_Zero p4 xBar yBar _noWipeout_)
	(drawLine layDef_Zero '(-0.250 0.058) '(0.250 0.058)) ; L-bar ==
	(drawBoxAtPos layDef_Zero p5 yBar xBar _noWipeout_)
	(drawLine layDef_Zero '(-0.046 0.250) '(-0.046 -0.250)) ; L-bar ||
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawBoxAtPos layDef_MetricDetails p1 x1 y1 _noWipeout_)
	(drawBoxAtPos layDef_MetricDetails p2 x2 y2 _noWipeout_)
	(drawBoxAtPos layDef_MetricDetails p3 x3 y3 _noWipeout_)
	(drawBoxAtPos layDef_MetricDetails p4 xBar yBar _noWipeout_)
	(drawLine layDef_MetricDetails '(-0.250 0.058) '(0.250 0.058)) ; L-bar ==
	(drawBoxAtPos layDef_MetricDetails p5 yBar xBar _noWipeout_)
	(drawLine layDef_MetricDetails '(-0.046 0.250) '(-0.046 -0.250)) ; L-bar ||
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun ROERMAST-MED-LJERN-FOT ( variation / blockName description len1 len2 r d t1 t2 r2 xBar yBar p4 p5 )
	; Ref. S-002347, for togsporsignal+dvergsignal (2000), dvergsignal+togsporsignal (3000)og kryssvekselsignal (1600)
	; �76 r�rmast lengder 2000, 3000, 1600 
	; Hull �33 for kabelboks plasseres 350 over bakkeniv� (1260 over UK r�rmast)
	; 2000 = 910 under bakken, 1090 over bakken, benyttes til normal h�yde dverg- og togsporsignaler
	; 3000 = 910 under bakken, 2090 over bakken, benyttes for h�ye dverg- og togsporsignaler
	; 1600 = 910 under bakken, 690 over bakken, benyttes for kryssvekselsignal
	;
	; Benyttes sammen med 2 stk L-jern S.005382 (for �76 og for �127 master, har hull for begge typer U-b�yler).
	; U-b�yler for �127 r�rmast: S.043753, �10 rundst�l med 53mm gjengede ender (R64-R74-�10, total h�yde 190mm)
	; Dersom r�rmasten benyttes kun for sm� skap s� p�settes svart plasthette S.040149 h�yde 35+ca10 = ca 45 mm h�y, 10mm over topp r�rmast.
	(cond
		((= variation 1) (setq r 0.038 d "76"  t1 "DS-OG-TSS-2000"         t2 "DVERGSIGNAL / TOGSPORSIGNAL, h=2000 (910+1090)"))
		((= variation 2) (setq r 0.038 d "76"  t1 "HOEY-DS-OG-TSS-3000"    t2 (strcat "H" _uOE_ "Y DVERG / H" _uOE_ "YT TOGSPORSIGNAL, h=3000 (910+2090)")))
		((= variation 3) (setq r 0.038 d "76"  t1 "KRYSSVEKSELSIGNAL-1600" t2 "KRYSSVEKSELSIGNAL, h=1600 (910+690)"))
		((= variation 4) (setq r 0.064 d "127" t1 "SIGNAL-3200"            t2 (strcat "SIGNAL P" _uAA_ " KORT MAST, h=3200")))
	)
	(setq 
		blockName	(strcat "NO-BN-2D-JBTSI-ROERMAST-" d "-MED-LJERN-FOR-" t1)
		description	(strcat _uOE_ d " R" _uOE_ "RMAST MED L-JERN FOT FOR " t2)
		; Actual measures from construction drawing:
		r2 (* 0.9 r) ; Inner diameter of mast is approx 9/10 of outer diameter
		xBar 0.500 ; L-bar foot length
		yBar 0.060 ; L-bar foot width
		p4 (cond ((= variation 4) '(0.000 0.040)) (T '(0.000 0.014))) ; r + 0.006 - 0.060/2 ==
		p5 (cond ((= variation 4) '(0.094 0.000)) (T '(0.068 0.000))) ; r + 0.060/2         ||
	)
	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "F")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(drawCircle layDef_Zero r _noWipeout_)
	(drawCircle layDef_Zero r2 _noWipeout_)
	(drawBoxAtPos layDef_Zero p4 xBar yBar _noWipeout_)
	(drawLine layDef_Zero (list -0.250 r) (list 0.250 r)) ; L-bar ==
	(drawBoxAtPos layDef_Zero p5 yBar xBar _noWipeout_)
	(drawLine layDef_Zero (list (+ r 0.006) 0.250) (list (+ r 0.006) -0.250)) ; L-bar ||
	(moveUp r)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawCircle layDef_MetricDetails r _noWipeout_)
	(drawCircle layDef_MetricDetails r2 _noWipeout_)
	(drawBoxAtPos layDef_MetricDetails p4 xBar yBar _noWipeout_)
	(drawLine layDef_MetricDetails (list -0.250 r) (list 0.250 r)) ; L-bar ==
	(drawBoxAtPos layDef_MetricDetails p5 yBar xBar _noWipeout_)
	(drawLine layDef_MetricDetails (list (+ r 0.006) 0.250) (list (+ r 0.006) -0.250)) ; L-bar ||
	(moveUp r)
	(createMetricBlockFromCurrentGraphics blockName)
)
