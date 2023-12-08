;=========================================================================================================================
;
; NO-BN Lite montasjeelement.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2017-02-15 BEHOP/CLFEY 
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Lightweight fittings (smaller than 100 kg), for boards and smaller items. Can be installed without de-stabilizing the track.

; Ref. http://euroskilt.no/oppsetningsutstyr

; For debugging:
; (NOBN-SKILTFESTE-BETONGFOT-60-225 "500") (NOBN-SKILTFESTE-BETONGFOT-60-225 "700") (NOBN-SKILTFESTE-BETONGFOT-60-200-1200) (NOBN-SKILTFESTE-JORDFUNDAMENT-60-70) (NOBN-SKILTFESTE-JORDSPYD-60)
; (NOBN-SKILTFESTE-TILBAKEFYLT-GROP) (NOBN-SKILTFESTE-STRIPS-ELLER-PATENTBAAND) (NOBN-SKILTFESTE-BRAKETT-MOT-VEGG-60-110) (NOBN-SKILTFESTE-BRAKETT-MOT-GULV-60-308) (NOBN-SKILTFESTE-BRAKETT-MOT-MAST)
; (UNP80-STOLPE-MED-LJERN-FOT 1) (UNP80-STOLPE-MED-LJERN-FOT 2) (UNP80-STOLPE-MED-LJERN-FOT 3) (UNP80-STOLPE-MED-LJERN-FOT 4) (ROERMAST-MED-LJERN-FOT 1) (ROERMAST-MED-LJERN-FOT 2) (ROERMAST-MED-LJERN-FOT 3) (ROERMAST-MED-LJERN-FOT 4)



(defun NOBN-LITE-MONTASJEELEMENT ( / )

	; KU-SKF Skiltfeste
	(TraceLevel3 "NOBN-SKILTFESTE-BETONGFOT-60-225 / 500")		(NOBN-SKILTFESTE-BETONGFOT-60-225 "500")
	(TraceLevel3 "NOBN-SKILTFESTE-BETONGFOT-60-225 / 700")		(NOBN-SKILTFESTE-BETONGFOT-60-225 "700")
	(TraceLevel3 "NOBN-SKILTFESTE-BETONGFOT-60-200-1200")		(NOBN-SKILTFESTE-BETONGFOT-60-200-1200)
	(TraceLevel3 "NOBN-SKILTFESTE-JORDFUNDAMENT-60-70")			(NOBN-SKILTFESTE-JORDFUNDAMENT-60-70)
	(TraceLevel3 "NOBN-SKILTFESTE-JORDSPYD-60")					(NOBN-SKILTFESTE-JORDSPYD-60)
	(TraceLevel3 "NOBN-SKILTFESTE-TILBAKEFYLT-GROP")			(NOBN-SKILTFESTE-TILBAKEFYLT-GROP)
	(TraceLevel3 "NOBN-SKILTFESTE-STRIPS-ELLER-PATENTBAAND")	(NOBN-SKILTFESTE-STRIPS-ELLER-PATENTBAAND)
	(TraceLevel3 "NOBN-SKILTFESTE-BRAKETT-MOT-VEGG-60-110")		(NOBN-SKILTFESTE-BRAKETT-MOT-VEGG-60-110)
	(TraceLevel3 "NOBN-SKILTFESTE-BRAKETT-MOT-GULV-60-308")		(NOBN-SKILTFESTE-BRAKETT-MOT-GULV-60-308)
	(TraceLevel3 "NOBN-SKILTFESTE-BRAKETT-MOT-MAST")			(NOBN-SKILTFESTE-BRAKETT-MOT-MAST)

	; SA-FUN Stolpe med fot / rørmast med fot
	(TraceLevel3 "UNP80-STOLPE-MED-LJERN-FOT / 1")				(UNP80-STOLPE-MED-LJERN-FOT 1) ; Enkel S-lås
	(TraceLevel3 "UNP80-STOLPE-MED-LJERN-FOT / 2")				(UNP80-STOLPE-MED-LJERN-FOT 2) ; Dobbel S-lås
	(TraceLevel3 "UNP80-STOLPE-MED-LJERN-FOT / 3")				(UNP80-STOLPE-MED-LJERN-FOT 3) ; Lokalstiller
	(TraceLevel3 "UNP80-STOLPE-MED-LJERN-FOT / 4")				(UNP80-STOLPE-MED-LJERN-FOT 4) ; Overdragstrafo
	(TraceLevel3 "ROERMAST-MED-LJERN-FOT / 1")					(ROERMAST-MED-LJERN-FOT 1) ; Ø76 høyde 2000 mm for togsporsignal og dvergsignal
	(TraceLevel3 "ROERMAST-MED-LJERN-FOT / 2")					(ROERMAST-MED-LJERN-FOT 2) ; Ø76 høyde 3000 mm for høye dvergsignaler og togsporsignaler
	(TraceLevel3 "ROERMAST-MED-LJERN-FOT / 3")					(ROERMAST-MED-LJERN-FOT 3) ; Ø76 høyde 1600 mm for kryssvekselsignal
	(TraceLevel3 "ROERMAST-MED-LJERN-FOT / 4")					(ROERMAST-MED-LJERN-FOT 4) ; Ø127 høyde 3200 mm for lave signaler
)



(defun NOBN-SKILTFESTE-BETONGFOT-60-225 ( variation / blockName description s1 s2 x d r )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-BETONGFOT-60-225-" variation)
		description	_emptyString__emptyString_ ; See below
		s1 (/ 0.150 2)
		s2 (/ 0.255 2)
		x 0.150 ; four "flaps"
		d 0.12
		r 0.03
	)
	(defun LocalGraphics (/)
		(command 
			_LINE_ (list s2 0) (list s2 s1) (list s1 s2) (list 0 s2) _ENTER_
			_LINE_ (polar  (list x x) (D->R -45) d) (polar  (list x x) (D->R (+ _angle90_ 45)) d) _ENTER_
			_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
			_MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
			_CIRCLE_ _origin_ r
		)
	)

	; Schematic symbol
	(cond
		((= variation "500") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOSLASH_ "60/H500")))
		((= variation "700") (setq description (strcat "SKILTFESTE, BETONGFOT " _uOSLASH_ "60/H700")))
		(T (setq description "Something is fishy with SKILTFESTE BETONGFOT "))
	)
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-BETONGFOT-60-200-1200 ( / blockName description angBias andArc r1 r2 pr fr )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-BETONGFOT-60-200-1200")
		description	(strcat "SKILTFESTE, BETONGFOT " _uOSLASH_ "60/H1200")
		angBias 5.0	; Add a bias angle to the symbol
		angArc 45.0	; Angular 'arc length' of "surrounding lines"
		r1 0.18	; Ø360 surrounding lines in symbol (4 times 45-deg pieces)
		r2 0.22	; Ø440 surrounding lines in symbol (4 times 45-deg pieces)
		pr 0.03	; Ø60 pole radius
		fr 0.10 ; Ø200 foundation radius
	)
	(defun LocalGraphics (/)
		(command
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angleZero_)) R1) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angleZero_)) R2) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angle90_)) R1) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angle90_)) R2) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angle180_)) R1) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angle180_)) R2) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angleMinus90_)) R1) _setArcAngle_ (rtos angArc)
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (+ angBias _angleMinus90_)) R2) _setArcAngle_ (rtos angArc)
			_CIRCLE_  _origin_ pr
			_CIRCLE_  _origin_ fr
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-JORDFUNDAMENT-60-70 ( / blockName description pr r2 ang )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-JORDFUNDAMENT-60-700")
		description	(strcat "SKILTFESTE, JORDFUNDAMENT " _uOSLASH_ "60/H700")
		pr		0.030 ; Ø060 pole
		r2	0.181 ; 0.151 side wings from the inner pole/tube
		ang		_angle45_
	)
	(defun LocalGraphics (/)
		(command 
			_CIRCLE_ _origin_ pr
			_LINE_ (list (* pr (DDcos ang)) (* pr (DDsin ang))) (list (* r2 (DDcos ang)) (* r2 (DDsin ang))) _ENTER_
			_ARRAY_ _lastSelection_ _ENTER_ _polarArray_ _origin_ 4 _fullCircle_ _rotateObjects_
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-JORDSPYD-60 ( / blockName description x y r )
	(setq 
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-JORDSPYD-60")
		description	(strcat "SKILTFESTE, JORDSPYD " _uOSLASH_ "60/H600")
		x   0.100	; Square symbolizes the L-iron bar
		y	0.100
		r	0.030	; For Ø60 poles, an L-iron with a Ø60 hose on top
	)
	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircle layDef_Zero r _noWipeout_)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawBox layDef_MetricDetails x y _noWipeout_)
	(DrawCircle layDef_MetricDetails r _noWipeout_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-TILBAKEFYLT-GROP ( / blockName description r )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-TILBAKEFYLT-GROP")
		description	"SKILTFESTE, TILBAKEFYLT GROP"
		r	0.030	; For Ø60 pole
	)
	(defun LocalGraphics (/)
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
			_SCALE_ _selectAll_ _ENTER_ _origin_ _half_
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(DrawCircle layDef_MetricDetails r _noWipeout_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-STRIPS-ELLER-PATENTBAAND ( / blockName description r )
	;
	;        __7__
	;       / --- 6
	;      | ( . ) 5
	;      |  ---  4 .
	; ----/         32--1
	;
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-STRIPS-ELLER-PATENTB" _uARING_ "ND")
		description	(strcat "SKILTFESTE, STRIPS ELLER PATENTB" _uARING_ "ND")
		p1	'(0.064 -0.032)
		p2	'(0.048 -0.032)
		p3	'(0.037 -0.027)
		p4	'(0.032 -0.016)
		p5	'(0.032  0.000)
		p6	'(0.023  0.023)
		p7	'(0.000  0.032)
		r	0.030	; Ø60 pole
	)
	(defun LocalGraphics (/)
		(DrawLine layDef_Zero p1 p2)
		(DrawArc layDef_Zero p2 p3 p4)
		(DrawLine layDef_Zero p4 p5)
		(DrawArc layDef_Zero p5 p6 p7)
		(MirrorAboutYaxis _keepMirrorSource_)
		(DrawCircle layDef_Zero r _noWipeout_)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-BRAKETT-MOT-GULV-60-308 ( / blockName description s b pr br p1 p2 p3 p4 )
	;
	; TL-----TR
	; | 1   2 |
	; |  (.)  |  ; Ø60 pole stub welded onto a 255x255 plate with four Ø14 holes placed 164x164 apart.
	; | 3   4 |
	; BL-----BR
	;
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-BRAKETT-MOT-GULV-60-308")
		description	(strcat "SKILTFESTE, BRAKETT MOT GULV " _uOSLASH_ "60/308")
		s	0.255	; baseplate side
		b	0.164	; baseplate holes spacing
		pr 	0.030	; Ø60 pole radius
		br 	0.007	; Ø14 bolt radius
		p1  (PointTL b b)
		p2  (PointTR b b)
		p3  (PointBL b b)
		p4  (PointBR b b)
	)
	(defun LocalGraphics (/)
		(command
			_RECTANGLE_ (PointBL s s) (PointTR s s)
			_CIRCLE_ p1 br
			_CIRCLE_ p2 br
			_CIRCLE_ p3 br
			_CIRCLE_ p4 br
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-BRAKETT-MOT-MAST ( / blockName description r )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-BRAKETT-P" _uARING_ "-ANNEN-MAST")
		description	(strcat "SKILTFESTE, BRAKETT P" _uARING_ " ANNEN MAST")
		r	0.030	; For Ø60 pole
	)
	(defun LocalGraphics (/)
		(command 
			_ARC_ _setArcCenter_ _origin_ '(0.1 0.2) '(-0.1 0.2)
			_MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
			_CIRCLE_ _origin_ r
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SKILTFESTE-BRAKETT-MOT-VEGG-60-110 ( / blockName description backPlate wingStart wingLength wingIncrY )
	(setq
		blockName	(strcat _SUB_ "SKF-" "SKILTFESTE-BRAKETT-MOT-VEGG-60-110")
		description	(strcat "SKILTFESTE, VEGGFESTE " _uOSLASH_ "60/110")
		backPlate	0.200
		wingStart	0.150
		wingLength	0.150
		wingIncrY	0.050
	)
	(defun LocalGraphics (/)
		(command  ; Three "eagle wings" to each side of insertion point:
			_LINE_ (list wingStart 0) (list (+ wingStart wingLength) 0) _ENTER_
			_LINE_ (list wingStart wingIncrY) (list (+ wingStart wingLength) wingIncrY) _ENTER_
			_LINE_ (list wingStart (- wingIncrY)) (list (+ wingStart wingLength) (- wingIncrY)) _ENTER_
			_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		)
		(command ; Outline of wall-mount bracket (for boards etc Ø60):
			_LINE_ (list (- (/ backPlate 2)) 0) (list (/ backPlate 2) 0) _ENTER_
			_RECTANGLE_ 
				(list (- (/ backPlate 4)) 0)
				(list (/ backPlate 4) (- (/ backPlate 2)))
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun UNP80-STOLPE-MED-LJERN-FOT ( variation / blockName description t1 t2 x1 x2 x3 y1 y2 y3 p1 p2 p3 p4 p5 xBar yBar )
	; Ref S.018812 Stolpe for festeplater for lokalstiller type Satema S-låser type DSI Overdragstrafo TM 14
	; Stolpen er en UNP80, 2200 høy / 800 under bakken, 1400 over bakken. Kommer med 4 stk festeplate-varianter,
	; enkel S-lås (bxh = 170x350), dobbel S-lås (410x350), lokalstiller 148x96), overdragstrafo 195x240). 6mm tykke plater.
	; Platene flukter i overkant stolpe. U vender forover. Dekkplate over kabler i høyder 700-870 og 930-1400 over UK stolpe.
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
		((= variation 1) (setq t1 "ENKEL-SAMLELAAS"  t2 (strcat "ENKEL S-L" _uARING_ "S")  x1 0.170)) ; x1 = plate width
		((= variation 2) (setq t1 "DOBBEL-SAMLELAAS" t2 (strcat "DOBBEL S-L" _uARING_ "S") x1 0.410))
		((= variation 3) (setq t1 "LOKALSTILLER"     t2 "LOKALSTILLER"                  x1 0.148))
		((= variation 4) (setq t1 "OVERDRAGSTRAFO"   t2 "OVERDRAGSTRAFO"                x1 0.195))
	)
	(setq 
		blockName	(strcat _SIG_ "FUN-" "STOLPE-MED-LJERN-FOR-" t1)
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
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(SetLayer layDef_Zero)
	(DrawBoxAtPoint layDef_Zero p1 x1 y1 _noWipeout_)
	(DrawBoxAtPoint layDef_Zero p2 x2 y2 _noWipeout_)
	(DrawBoxAtPoint layDef_Zero p3 x3 y3 _noWipeout_)
	(DrawBoxAtPoint layDef_Zero p4 xBar yBar _noWipeout_)
	(DrawLine layDef_Zero '(-0.250 0.058) '(0.250 0.058)) ; L-bar ==
	(DrawBoxAtPoint layDef_Zero p5 yBar xBar _noWipeout_)
	(DrawLine layDef_Zero '(-0.046 0.250) '(-0.046 -0.250)) ; L-bar ||
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawBoxAtPoint layDef_MetricDetails p1 x1 y1 _noWipeout_)
	(DrawBoxAtPoint layDef_MetricDetails p2 x2 y2 _noWipeout_)
	(DrawBoxAtPoint layDef_MetricDetails p3 x3 y3 _noWipeout_)
	(DrawBoxAtPoint layDef_MetricDetails p4 xBar yBar _noWipeout_)
	(DrawLine layDef_MetricDetails '(-0.250 0.058) '(0.250 0.058)) ; L-bar ==
	(DrawBoxAtPoint layDef_MetricDetails p5 yBar xBar _noWipeout_)
	(DrawLine layDef_MetricDetails '(-0.046 0.250) '(-0.046 -0.250)) ; L-bar ||
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun ROERMAST-MED-LJERN-FOT ( variation / r d t1 t2 blockName description ri xBar yBar p4 p5 )
	; Ref. S-002347, for togsporsignal+dvergsignal (2000), dvergsignal+togsporsignal (3000)og kryssvekselsignal (1600)
	; Ø76 rørmast lengder 2000, 3000, 1600 
	; Hull Ø33 for kabelboks plasseres 350 over bakkenivå (1260 over UK rørmast)
	; 2000 = 910 under bakken, 1090 over bakken, benyttes til normal høyde dverg- og togsporsignaler
	; 3000 = 910 under bakken, 2090 over bakken, benyttes for høye dverg- og togsporsignaler
	; 1600 = 910 under bakken, 690 over bakken, benyttes for kryssvekselsignal
	;
	; Benyttes sammen med 2 stk L-jern S.005382 (for Ø76 og for Ø127 master, har hull for begge typer U-bøyler).
	; U-bøyler for Ø127 rørmast: S.043753, Ø10 rundstål med 53mm gjengede ender (R64-R74-Ø10, total høyde 190mm)
	; Dersom rørmasten benyttes kun for små skap så påsettes svart plasthette S.040149 høyde 35+ca10 = ca 45 mm høy, 10mm over topp rørmast.
	(cond
		((= variation 1) (setq r 0.038 d "76"  t1 "DS-OG-TSS-2000"         t2 "DVERGSIGNAL / TOGSPORSIGNAL, h=2000 (910+1090)"))
		((= variation 2) (setq r 0.038 d "76"  t1 "HOEY-DS-OG-TSS-3000"    t2 (strcat "H" _uOSLASH_ "Y DVERG / H" _uOSLASH_ "YT TOGSPORSIGNAL, h=3000 (910+2090)")))
		((= variation 3) (setq r 0.038 d "76"  t1 "KRYSSVEKSELSIGNAL-1600" t2 "KRYSSVEKSELSIGNAL, h=1600 (910+690)"))
		((= variation 4) (setq r 0.064 d "127" t1 "SIGNAL-3200"            t2 (strcat "SIGNAL P" _uARING_ " KORT MAST, h=3200")))
	)
	(setq 
		blockName	(strcat _SIG_ "FUN-" "R" _uOSLASH_ "RMAST-" d "-MED-LJERN-FOR-" t1)
		description	(strcat _uOSLASH_ d " R" _uOSLASH_ "RMAST MED L-JERN FOT FOR " t2)
		; Actual measures from construction drawing:
		ri (* 0.9 r) ; Inner diameter of mast is approx 9/10 of outer diameter
		xBar 0.500 ; L-bar foot length
		yBar 0.060 ; L-bar foot width
		p4 (cond ((= variation 4) '(0.000 0.040)) (T '(0.000 0.014))) ; r + 0.006 - 0.060/2 ==
		p5 (cond ((= variation 4) '(0.094 0.000)) (T '(0.068 0.000))) ; r + 0.060/2         ||
	)
	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "F")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawCircle layDef_Zero ri _noWipeout_)
	(DrawBoxAtPoint layDef_Zero p4 xBar yBar _noWipeout_)
	(DrawLine layDef_Zero (list -0.250 r) (list 0.250 r)) ; L-bar ==
	(DrawBoxAtPoint layDef_Zero p5 yBar xBar _noWipeout_)
	(DrawLine layDef_Zero (list (+ r 0.006) 0.250) (list (+ r 0.006) -0.250)) ; L-bar ||
	(MoveUp r)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawCircle layDef_MetricDetails r _noWipeout_)
	(DrawCircle layDef_MetricDetails ri _noWipeout_)
	(DrawBoxAtPoint layDef_MetricDetails p4 xBar yBar _noWipeout_)
	(DrawLine layDef_MetricDetails (list -0.250 r) (list 0.250 r)) ; L-bar ==
	(DrawBoxAtPoint layDef_MetricDetails p5 yBar xBar _noWipeout_)
	(DrawLine layDef_MetricDetails (list (+ r 0.006) 0.250) (list (+ r 0.006) -0.250)) ; L-bar ||
	(MoveUp r)
	(CreateMetricBlockFromCurrentGraphics blockName)
)
