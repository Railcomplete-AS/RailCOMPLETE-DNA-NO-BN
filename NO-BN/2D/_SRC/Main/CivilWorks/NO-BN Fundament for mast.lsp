;=========================================================================================================================
;
; NO-BN Fundament for mast.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Foundations for masts (signals, catenary masts, level crossing boom)

; Debug helpers:
; (NOBN-FUNDAMENT-FOR-MAST)
; (NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-FRI-LINJE) 
; (NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-PLATTFORM)
; (NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-Ø680-300-SYLINDRISK)
; (NOBN-SIGNALFUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø680-210-1600)
; (NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-BORET-Ø555)
; (NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-KONISK)
; (NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-RETTVEGGET)
; (NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST)
; (NOBN-KL-FUNDAMENT-BETONG-B3-B6-LAV-MAST)
; (NOBN-KL-FUNDAMENT-BETONG-B6-HOEY-MAST)
; (NOBN-KL-FUNDAMENT-BETONG-BORET-355)
; (NOBN-KL-FUNDAMENT-BETONG-BORET-555)
; (NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-HENGEMAST)
; (NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-VEGGKONSOLL-I-FJELLTUNNEL)
; (NOBN-KL-FUNDAMENT-METALL-VEGGKONSOLL)
; (NOBN-KL-FUNDAMENT-SPESIAL)
; (DrawSmallBolt)


(defun NOBN-FUNDAMENT-FOR-MAST ( / )
	(TraceLevel3 "NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL...")
		(NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-FRI-LINJE)
		(NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-PLATTFORM)
		(NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-Ø680-300-SYLINDRISK)
		(NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-BORET-Ø555) ; Ø555 Med signalfagets boltegruppe 164x164 mm

	(TraceLevel3 "NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL...")
		(NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-KONISK)
		(NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-RETTVEGGET)

	(TraceLevel3 "NOBN-SIGNALFUNDAMENT-BETONG-VEIBOMDRIVMASKIN...")
		(NOBN-SIGNALFUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø680-210-1600) ; OK, kanskje ikke en mast... men samme slags fundament, nesten, som gammelt signalfundament

	; Ny standard, med boltegruppe 374x191xM36 - arg=sålebredde (1600-2200) og 'såleoffset'.
	; Offset 000=søyle bakerst på sålen (lengst fra sporet innsatt med dir="both")
	; Offset 100=søyle 100 mm fra bakerst på sålen, osv
	(TraceLevel3 "NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1600" "000") 
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1700" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1800" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1800" "100")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1900" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "1900" "100")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2000" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2000" "100")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2000" "200")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2100" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2100" "100")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2100" "200")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2200" "000")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2200" "100")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2200" "200")
		(NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST "2200" "300")

	(TraceLevel3 "NOBN-KL-FUNDAMENT-BETONG-B3/B6/H3")
		(NOBN-KL-FUNDAMENT-BETONG-B3-B6-LAV-MAST) ; Lav B3 og B6 med 240x800 boltegruppe
		(NOBN-KL-FUNDAMENT-BETONG-B6-HOEY-MAST)	 ; Høy B6 med 260x940 boltegruppe
		(NOBN-KL-FUNDAMENT-BETONG-H3 "6000-9500") ; Beregnet for mastehøyder fra-til [mm]
		(NOBN-KL-FUNDAMENT-BETONG-H3 "10000-12000")
		(NOBN-KL-FUNDAMENT-BETONG-H3 "12500-13000")

	(TraceLevel3 "NOBN-KL-FUNDAMENT-BETONG-BORET-355/555")
		(NOBN-KL-FUNDAMENT-BETONG-BORET-355) ; Ø355 Med KL-fagets boltegruppe
		(NOBN-KL-FUNDAMENT-BETONG-BORET-555) ; Ø555 Med KL-fagets boltegruppe

	(TraceLevel3 (strcat
			"NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-HENGEMAST "
			"/ NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-VEGGKONSOLL-I-FJELLTUNNEL "
			"/ NOBN-KL-FUNDAMENT-METALL-VEGGKONSOLL "
			"/ NOBN-KL-FUNDAMENT-SPESIAL"))
		(NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-HENGEMAST)
		(NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-VEGGKONSOLL-I-FJELLTUNNEL)
		(NOBN-KL-FUNDAMENT-METALL-VEGGKONSOLL)
		(NOBN-KL-FUNDAMENT-SPESIAL)
)



(defun NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-FRI-LINJE ( / blockName description baseX baseY shaftX shaftY x y boltRadius cutoutX cutoutY )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-HS-PAA-FRI-LINJE-300x300xM24")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR HS PAA FRI LINJE 2000x2000x300-650x650x1700 300x300xM24")
		baseX 2.000 ; Fotplate
		baseY 2.000 
		shaftX 0.600 ; Hals
		shaftY 0.600
		x (/ 0.300 2) ; Bolteplassering (gjengejern)
		y (/ 0.300 2)
		boltRadius (/ 0.024 2) ; M24
		cutoutX 0.150 ; For cables / flexible tubes, front of shaft
		cutoutY 0.050
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(command
			_POLYLINE_ ; 
				(list (- (/ baseX 2)) (/ baseY 2))
				(list (+ (/ baseX 2)) (/ baseY 2))
				(list (+ (/ shaftX 2)) (/ shaftY 2))
				(list (- (/ shaftX 2)) (/ shaftY 2))
				_closedPolyline_
			_CIRCLE_ (list x y) boltRadius
		)
		(MirrorAboutXaxis _keepMirrorSource_)
		(MirrorAboutYaxis _keepMirrorSource_)
		(MirrorAboutDiagonal _keepMirrorSource_)
		(command _RECTANGLE_ (list (- (/ cutoutX 2)) (- (/ shaftY 2))) (list (+ (/ cutoutX 2)) (- cutoutY (/ shaftY 2))))
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-HOVEDSIGNAL-PAA-PLATTFORM ( / blockName description s1 s2 dim1 dim2 rThreadedIron rFlexibleTube yFlexibleTube )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-HS-PAA-PLATTFORM-300x300xM24")
		description	"SIGNALFUNDAMENT BETONG, FOR HS PAA PLATTFORM 1750x1750x300-650x650x1000 300x300xM24"
		s1 2.0
		s2 0.6
		dim1 0.15
		dim2 0.05
		rThreadedIron 0.0120
		rFlexibleTube 0.0420
		yFlexibleTube (+ (/ s2 2) rFlexibleTube) 
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(command
			_RECTANGLE_	(list (- (/ s1 2)) (- (/ s1 2)))	(list (/ s1 2) (/ s1 2))			;stort kvadrat, ytre
			_RECTANGLE_	(list (- (/ s2 2)) (- (/ s2 2)))	(list (/ s2 2) (/ s2 2))			;lite kvadrat, indre
			; skrålinjer:
			_LINE_		(list (- (/ s1 2)) (- (/ s1 2)))	(list (- (/ s2 2)) (- (/ s2 2))) _ENTER_
			_LINE_		(list (/ s1 2) (/ s1 2))			(list (/ s2 2) (/ s2 2)) _ENTER_
			_LINE_		(list (/ s1 -2) (/ s1 2))			(list (/ s2 -2) (/ s2 2)) _ENTER_
			_LINE_		(list (/ s1 2) (/ s1 -2))			(list (/ s2 2) (/ s2 -2)) _ENTER_
			_CIRCLE_		(list dim1 dim1) rThreadedIron
			_CIRCLE_		(list (- dim1) dim1) rThreadedIron
			_CIRCLE_		(list dim1 (- dim1)) rThreadedIron
			_CIRCLE_		(list (- dim1) (- dim1)) rThreadedIron
			_CIRCLE_		_origin_ rThreadedIron
			_CIRCLE_		(list (- rFlexibleTube) (- yFlexibleTube)) rFlexibleTube
			_CIRCLE_		(list rFlexibleTube (- yFlexibleTube)) rFlexibleTube
			_CIRCLE_		(list 0 yFlexibleTube) rFlexibleTube
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-Ø680-300-SYLINDRISK ( / blockName description ri ro d rThreadedIron )
	; Ref. S.xxxxxx
	(setq 
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-HS-Ø300-Ø680-300x300xM24-H1800")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR HS, " _uOSLASH_ "300/" _uOSLASH_ "680, 300x300xM24 H=1800")
		ri (/ 0.680 2) ; Ytre radius, stående betongrør
		ro (/ 0.300 2) ; Indre radius, stående betongrør
		d (/ 0.300 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		rThreadedIron (/ 0.024 2) ; M24 gjengejern (stikker 100 opp)
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(command
			_CIRCLE_ _origin_ ri
			_CIRCLE_ _origin_ ro
			_CIRCLE_ (list d d) rThreadedIron
			_CIRCLE_ (list d (- d)) rThreadedIron
			_CIRCLE_ (list (- d) d) rThreadedIron
			_CIRCLE_ (list (- d) (- d)) rThreadedIron
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø680-210-1600 ( / blockName description ri ro d rThreadedIron )
	; Ref. S.xxxxxx
	(setq 
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø210-Ø680-210x210xM20-H1600")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR VEIBOMDRIVMASKIN, " _uOSLASH_ "210/" _uOSLASH_ "680 210x210xM20 H=1600")
		ri (/ 0.680 2) ; Ytre radius, stående betongrør
		ro (/ 0.210 2) ; Indre radius, stående betongrør
		d (/ 0.300 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		rThreadedIron (/ 0.020 2) ; M20 gjengejern (stikker 100 opp)
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(command ; Add metric graphics
			_CIRCLE_ _origin_ ri
			_CIRCLE_ _origin_ ro
			_CIRCLE_ (list d d) rThreadedIron
			_CIRCLE_ (list d (- d)) rThreadedIron
			_CIRCLE_ (list (- d) d) rThreadedIron
			_CIRCLE_ (list (- d) (- d)) rThreadedIron
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-BETONG-HOVEDSIGNAL-BORET-Ø555 ( / blockName description r R1 R2 rConcretePpole x ang angArcLen )
	; Ref. S.xxxxxx
	; 2D symbol: The Ø555 concrete pole with two layers of dashed arcs around
	(setq
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-BORET-" _uOSLASH_ "555-164x164xM32")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR HS, BORET " _uOSLASH_ "555 164x164xM32")
		rThreadedIron (/ 0.032 2) ; M32 gjengejern for de indre Ø45 hullene i signalets mastefot-plate
		R1 0.15
		R2 0.4
		rConcretePpole (/ 0.555 2) ; Ø555
		x (/ 0.164 2) ; Bolteplassering 164x164 mm for signalers mastefot (de indre hullene)
		ang 5
		angArcLen 45
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(command
			_CIRCLE_ (list x x) rThreadedIron
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R ang) R2) _setArcAngle_ angArcLen
			_ARC_ _setArcCenter_ _origin_ (polar _origin_ (D->R (/ angArcLen -2.0)) R1) _setArcAngle_ angArcLen
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origin_ 4 _fullCircle_ _rotateObjects_
			_CIRCLE_ _origin_ rConcretePpole
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-KONISK ( / blockName description s1 s2 dim1 dim2 )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-DS-KONISK-" _uOSLASH_ "76")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR DS, KONISK 600(400)x600(400)x500 " _uOSLASH_ "76")
		s1 (/ 0.600 2)
		s2 (/ 0.400 2)
		dim1 0.12
		dim2 0.10
	)
	(defun LocalGraphics ( layDef /)
		(DrawSmallBolt layDef)
		; make three of them:
		(command	
			_MIRROR_ _selectAll_ _ENTER_ (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) _eraseMirrorSource_
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origin_ 3 _fullCircle_ _rotateObjects_
			_CIRCLE_ _origin_ 0.0413
			_CIRCLE_ _origin_ 0.0445
		)
		; Tapered foundation for Ds shunting signal:
		(command
			_LINE_ 
				(list (/ dim1 -2) (- s2))
				(strcat "@0,-" (rtos dim2))
				(strcat "@-" (rtos (- s1 (/ dim1 2))) ",0")
				(strcat "@0," (rtos (* 2 s1)))
				(strcat "@" (rtos (* 2 s1)) ",0")
				(strcat "@0," (rtos (* -2 s1)))
				(strcat "@-" (rtos (- s1 (/ dim1 2))) ",0")
				(strcat "@0," (rtos dim2))
				(strcat "@" (rtos (- s2 (/ dim1 2))) ",0")
				(strcat "@0," (rtos (* 2 s2)))
				(strcat "@" (rtos (* -2 s2)) ",0")
				(strcat "@0," (rtos (* -2 s2)))
				(strcat "@" (rtos (- s2 (/ dim1 2))) ",0")
				_ENTER_
			"LINE"
				(list (- s1) (- s1))
				(list (- s2) (- s2))
				_ENTER_
			_ARRAY_ 
				_lastSelection_ _ENTER_ _polarArray_ _origin_ 4 360 _rotateObjects_
			_RECTANGLE_
				(list (/ dim1 -2) -0.24)
				(list (/ dim1 2) (+ -0.24 dim2))
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-SIGNALFUNDAMENT-BETONG-DVERGSIGNAL-RETTVEGGET ( / blockName description s1 s2 dim1 dim2 )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat _SUB_ "FUN-" "SIGNALFUNDAMENT-BETONG-DS-RETTVEGGET-" _uOSLASH_ "76")
		description	(strcat "SIGNALFUNDAMENT BETONG, FOR DS, RETTVEGGET 600(400)x600(400)x500 " _uOSLASH_ "76")
        ;miderste rør med skruer:
		s1 (/ 0.600 2)
		s2 (/ 0.400 2)
		dim1 0.12
		dim2 0.10
	)
	(defun LocalGraphics ( layDef /)
		(DrawSmallBolt layDef)
		(command
			_MIRROR_ _selectAll_ _ENTER_ (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) _eraseMirrorSource_
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origin_ 3 _fullCircle_ _rotateObjects_
			_CIRCLE_  _origin_ 0.0413
			_CIRCLE_ _origin_ 0.0445
		)
		; Straight-sided foundation for Ds shunting signal:
		(command
			_RECTANGLE_
				(list (- s1) (- s1)) (list s1 s1)
			_LINE_
				(list (/ dim1 2) (- s2))
				(list s2 (- s2))
					(strcat "@0," (rtos (* 2 s2)))
					(strcat "@" (rtos (* -2 s2)) ",0")
					(strcat "@0," (rtos (* -2 s2)))
					(strcat "@" (rtos (- s2 (/ dim1 2))) ",0")
					(strcat "@0," (rtos (/ dim1 2)))
					(strcat "@" (rtos dim1) ",0")
					(strcat "@0," (rtos (/ dim1 -2.0)))
				_ENTER_
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-BJELKEMAST ( baseYmm baseOffsetYmm / blockName description baseX baseY shaftX shaftY baseOffsetY ductRadius )
	; Ref EH-800151. NB! Det er angitt "1500" som mål i snitt C-C, men korrekt mål er 1600. 
	; Variable base width is 1600..2200 in steps of 100. base depth is always 1600 (seen along the track)
	; The shaft is always 700x700, but it may be offset in steps of 100 as long as there is still at least 450 on each side of the shaft to the base edge.
	; Insert with direction "both" or "none". "none" shall be used if the foundation is non-symmetrical and pushed awayfrom the track.
	; Insertion point is center of bolt group.
	; Base is 500 high (not shown in 2D).
	; Shaft varies in height (not showm in 2D).
	; baseYmm is 1600 to 2200 transversal to track (ASCII string), in steps of 100
	; baseOffsetYmm is 000 to 300 (ASCII string), in steps of 100 - Såle offset fra ytterste posisjon
	;
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-BJELKEMAST-1600x" baseYmm "-" baseOffsetYmm "-191x374xM36")
		description	(strcat "KL-FUNDAMENT BETONG, FOR BJELKEMAST, 1600x" baseYmm "-" baseOffsetYmm " 700x700 191x374xM36")
		baseX 1.600 ; Alongside the track, constant
		baseY (/ (atof baseYmm) 1000.0) ; Transversal to track
		; baseZ 1.000 ; Sålehøyde - irrelevant for 2D
		shaftX 0.700 ; x = along track
		shaftY 0.700 ; y = transversal to track
		; shaftZ - Varierer i steg på 100 til max 2700 (total fundamenthøyde blir da 1000 (såle) + 2700 (hals) hvorav minst 200 og max 700 skal rage opp over terreng.
		baseOffsetY (/ (atof baseOffsetYmm) 1000.0) ; Convert offset from millimeter to meter, varies from 0.000 to 0.300
		ductRadius (* _half_ 0.050)
	)
	(defun LocalGraphics ( layDef / )
		(SetLayer layDef)
		(DrawBoltGroup191x374xM36 layDef)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origin_ ductRadius
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-B3-B6-LAV-MAST ( / blockName description baseX baseY baseOffsetY shaftX shaftY ductRadius bx by bd )
	; Ref EH.707387-001 på Elkraftportalen, plasstøpt fundament for B3 og B6 stålmast på flatt terreng (-002 er fjællskjæring).
	; Høyde 500+1500+700 (sålehøyde / søyle fra såle til formasjonsplan / søyle oppraging over formasjonsplanet)
	; Overkant oppraging = SOK
	; Oppstikk bolter over SOK: 180 mm
	; Sålebredde 1000+800=1800 på tvers av sporretningen (1000 mot sporet, 800 vekk fra sporet). I skrått terreng: 2200.
	; Såledybde  800+800=1600 i sporretningen (symmetrisk om mastesenter)
	;
	; B3 og B6 fund for mastehøyder 6,0-9,5 m har boltegruppe a x b = 600 x 1200 
	; (B6 fund for mastehøyder 10,0-13,0 m har boltegruppe a x b = 260 x 940)
	;
	; For å sette inn mastefundament med motsatt kraftretning, snu om dir fra both til none.
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-B3-B6-LAV-1600x1800-000-240x800xM36")
		description	(strcat "KL-FUNDAMENT BETONG, FOR B3 OG B6 LAV MAST, 1600x1800-000 600x1200 240x800xM36")
		baseX 1.600 ; Langs med sporet
		baseY 1.800 ; På tvers av sporet
		; baseZ 0.500 ; Sålehøyde - irrelevant for 2D
		shaftX 0.600 ; Hals i spor-retning
		shaftY 1.200 ; Hals på tvers av spor-retning
		baseOffsetY 0.000 ; Såle offset fra ytterste posisjon
		; shaftZ - Varierer - ikke relevant for 2D symbolet
		ductRadius (* _half_ 0.050)
		bx	0.240	; Bolt distance along track
		by	0.800	; Bolt distance transversl to track
		bd	0.036	; Bolt diameter
	)
	(defun LocalGraphics ( layDef / )
		(DrawBoltGroup layDef bx by bd)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origin_ ductRadius
		)
	)

	; Schematic symbol
	(LocalGraphics layDef_Zero)
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-B6-HOEY-MAST ( / blockName description baseX baseY baseOffsetY shaftX shaftY ductRadius bx by bd )
	; Ref EH.707387-001 på Elkraftportalen, plasstøpt fundament for B3 og B6 stålmast på flatt terreng (-002 er fjællskjæring).
	; Høyde 500+1500+700 (sålehøyde / søyle fra såle til formasjonsplan / søyle oppraging over formasjonsplanet)
	; Overkant oppraging = SOK
	; Oppstikk bolter over SOK: 180 mm
	; Sålebredde 1000+800=1800 på tvers av sporretningen (1000 mot sporet, 800 vekk fra sporet). I skrått terreng: 2200.
	; Såledybde  800+800=1600 i sporretningen (symmetrisk om mastesenter)
	;
	; (B3 og B6 fund for mastehøyder 6,0-9,5 m har boltegruppe a x b = 600 x 1200)
	; B6 fund for mastehøyder 10,0-13,0 m har boltegruppe a x b = 260 x 940
	;
	; For å sette inn mastefundament med motsatt kraftretning, snu om dir fra both til none.
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-B6-HOEY-1600x1800-000-260x940xM36")
		description	(strcat "KL-FUNDAMENT BETONG, FOR HOEY B6-MAST, 1600x1800-000 600x1200 260x940xM36")
		baseX 1.600 ; Langs med sporet
		baseY 1.800 ; På tvers av sporet
		baseOffsetY 0.000 ; Såle offset fra ytterste posisjon
		; baseZ 0.500 ; Sålehøyde - irrelevant for 2D
		shaftX 0.600 ; Hals i spor-retning
		shaftY 1.200 ; Hals på tvers av spor-retning
		; shaftZ - Varierer - ikke relevant for 2D symbolet
		ductRadius (* _half_ 0.050)
		bx	0.260	; Bolt distance along track
		by	0.940	; Bolt distance transversal to track
		bd	0.036	; Bolt diameter
	)
	(defun LocalGraphics ( layDef / )
		(DrawBoltGroup layDef bx by bd)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origin_ ductRadius
		)
	)
	
	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-H3 ( mastHeight / blockName description baseX baseY shaftX shaftY baseOffsetY ductRadius bx by bd )
	; Ref EH.707388-001 på Elkraftportalen,  plasstøpt fundament for  for H3 på flatt terreng (-002 er fjællskjæring).
	; NB: Vi har her ikke tatt med H5 og stålmast for utliggeråk, som har annen bolteplassering.
	; Lage ekte H3-fundamenter, det finnes VELDIG mange i regelverket.
	; Inntil videre lages det kun et "standard" H3 fundament:
	; Høyde 500+1500+700 (sålehøyde / søyle fra såle til formasjonsplan / søyle oppraging over formasjonsplanet)
	; Overkant oppraging = SOK
	; Oppstikk bolter over SOK: 180 mm
	; Sålebredde 1000+800=1800 på tvers av sporretningen (1000 mot sporet, 800 vekk fra sporet)
	; Såledybde  800+800=1600 i sporretningen (symmetrisk om mastesenter)
	; Søylebredde a=1000 på tvers av sporretningen x søyledybde b=1000 i sporretningen
	;
	; De fleste H3 fund har a=b=1000 (mastehøyder 6,0-9,5 og H5 utliggeråk 7,0-8,5)
	; Øvrige H3 fund har a=b=1100 (mastehøyder 10,0-13,0 og H5 utliggeråk 9,0-13,0)
	; For å sette inn mastefundament med motsatt kraftretning, snu om dir fra both til none.
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-H3-1600x1800-000")
		description	(strcat "KL-FUNDAMENT BETONG, FOR H3-MAST, 1600x1800-000 ")
		baseX 1.600
		baseY 1.800 
		baseOffsetY 0.000 ; Såle offset fra ytterste posisjon
		; shaftZ - Varierer - ikke relevant for 2D symbolet
		; baseZ 0.500 ; fra underkant fundament til overkant såle (dette er irrelevant for 2D-symbolet)
		ductRadius (* _half_ 0.050)
	)
	(cond 
		; Boltegruppe i retning langs sporet ("bredde") (y=halvbredden) 700x700 for Hr 6,0..9,5m, 790x790 for 10,0..12,0m, 830x830 for 12,5-13,0m
		; Boltegruppe i retning på tvers av sporet ("dybde") (x=halvdybden)
		((= mastHeight "6000-9500")
			(setq 
				blockName (strcat blockName "-700x700xM36-6000-9500")
				description (strcat description "1000x1000 700x700xM36 FOR H3 MAST 6,0m-9,5m")
				shaftX 1.000	; Finnes i 1.000 x 1.000 (H3 mast 6,0 til 9,5m høyde) og 1.100 x 1.100 (H3 mast 10,0 til 12,0m høyde)
				shaftY 1.000
				bx 0.700
				by 0.700
				bd 0.036
			)
		)
		((= mastHeight "10000-12000")
			(setq
				blockName	(strcat blockName "-790x790xM36-10000-12000")
				description (strcat description "1100x1100 790x790xM36 FOR H3 MAST 10.0m-12.0m")
				shaftX 1.100
				shaftY 1.100
				bx 0.790
				by 0.790
				bd 0.036
			)
		)
		((= mastHeight "12500-13000")
			(setq 
				blockName	(strcat blockName "-830x830xM36-12500-13000")
				description (strcat description "1100x1100 830x830xM36 FOR H3 MAST 12.5m-13.0m")
				shaftX 1.100
				shaftY 1.100
				bx 0.830
				by 0.830
				bd 0.036
			)
		)
	)
	(defun LocalGraphics ( layDef / )
		(SetLayer layDef)
		(DrawBoltGroup layDef bx by bd)
		(command ; Draw shaft and duct
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origin_ ductRadius
		)
	)	
	
	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-BORET-355 ( / blockName description x y x1 y1 x2 y2 pd pr bx by bd br )
	; 2020-04-08 Bane NOR 1:250 skala 2D layer/symbol: JBTEH_Komponenter / (nonexistent, created by CLFEY)
	; Ref EH(EK).707523-000_001 "Prefabrikert ; Betongsøylefundament; Ø355 for; kontaktledningsmast"
	; Symbol: A circle with eight hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-BORET-" _uOSLASH_ "355-" _uOSLASH_ "232xDD27xM36")
		description	(strcat "KL-FUNDAMENT BETONG, BORET, " _uOSLASH_ "355 " _uOSLASH_ "232x27DDxM36")
		x	(* 0.090 (DDcos 27)) ; Line from origin
		y	(* 0.090 (DDsin 27))
		x1	(* 0.160 (DDcos 27)) ; Start hatch over bolt, from centre pole
		y1	(* 0.160 (DDsin 27))
		x2	(* 0.300 (DDcos 27)) ; End hatch over bolt, from centre pole
		y2	(* 0.300 (DDsin 27))
		pd  0.355 ; pole diameter Ø355 mm
		pr	(* _half_ pd) ; pole radius
		bx	(* 0.232 (DDcos 27)) ; Bolt distance, from centre pole (8 total)
		by	(* 0.232 (DDsin 27))
		bd	0.036 ; Bolt diameter
		br  (* _half_ bd)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(DrawLine layDef_Zero _origin_ (PointTR x y))					; ./ line from origin
	(DrawCircleAtPoint layDef_Zero (PointTR bx by) br _noWipeout_)	; ( ) bolt
	(DrawLine layDef_Zero (PointTR x1 y1) (PointTR x2 y2))			; (/) hatch over bolt
	(MirrorAboutXaxis _keepMirrorSource_)
	(MirrorAboutYaxis _keepMirrorSource_)
	(MirrorAboutDiagonal _keepMirrorSource_) ; Now we have 8 bolts
	(DrawCircle layDef_Zero pr _noWipeout_) ; Foundation concrete pole
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(DrawLine layDef_MetricDetails _origin_ (PointTR x y))					
	(DrawCircleAtPoint layDef_MetricDetails (PointTR bx by) br _noWipeout_)	
	(DrawLine layDef_MetricDetails (PointTR x1 y1) (PointTR x2 y2))			
	(MirrorAboutXaxis _keepMirrorSource_)
	(MirrorAboutYaxis _keepMirrorSource_)
	(MirrorAboutDiagonal _keepMirrorSource_)
	(DrawCircle layDef_MetricDetails pr _noWipeout_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BETONG-BORET-555 ( / blockName description bx by pd pr )
	; JBTEH_Komponenter / EH_Fund1 (which is 5x real size)
	; Ref EH.800091-000_001 "Høyspenning, kontaktledningsanlegg; Prefabrikkert betongsøylefundament; Ø555 for kontaktledningsmast"
	; Symbol: A circle with four hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-BETONG-BORET-" _uOSLASH_ "555-191x374xM36")
		description	(strcat "KL-FUNDAMENT BETONG, BORET, " _uOSLASH_ "555 191x374xM36")
		bx 0.191 ; Bolt group
		by 0.374
		bd 0.036 ; Bolt diameter
		pd  0.555 ; pole diameter Ø555 mm
		pr	(* _half_ pd) ; pole radius
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(DrawBoltGroup191x374xM36 layDef_Zero)
	(DrawCircle layDef_Zero pr _noWipeout_)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(DrawBoltGroup191x374xM36 layDef_MetricDetails)
	(DrawCircle layDef_MetricDetails pr _noWipeout_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-HENGEMAST ( / blockName description x y bx by bd )
	; Symbol: Just four hatched smaller circles, and a cross in the middle.
	; Plays the role of BN foundation EH_Fund_Spesial
	;
	(setq 
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-FJELLBOLTER-FOR-BRU-OG-TUNNEL-350x350xM24")
		description	(strcat "KL-FUNDAMENT BOLTEGRUPPE, FOR KL- HENGEMAST FOR BRU OG TUNNEL, 350x350xM24")
		; Schematic = 5x metric
		x	0.230  ; 'X' in the middle of four circles
		y	0.230
		bx	0.350 ; Bolt group
		by	0.350
		bd 	0.024 ; Bolt diameter
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBoltGroup layDef_Zero bx by bd)
	(DrawStAndrewCross layDef_Zero x y)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawBoltGroup layDef_MetricDetails bx by bd)
	(DrawStAndrewCross layDef_MetricDetails x y)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-BOLTEGRUPPE-FOR-VEGGKONSOLL-I-FJELLTUNNEL ( / blockName description bx bl bd )
;
;      +-+       +-+    ... bl
;      | |       | |
;      | |       | |   
;      | |       | |
;      +-+   .   +-+    ... zero
;      bd         bd
;      <--- bx  --->
;
	(setq 
		; 4 x Horizontal threaded iron 'gjengestag'
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-FJELLBOLTER-FOR-VEGGKONSOLL-600x700xM24")
		description	(strcat "KL-FUNDAMENT FJELLBOLTER, FOR VEGGKONSOLL, 600x700xM24")
		bx 0.600 ; x-distance between bolts
		bl 3.000 ; Bolt length (typical real length is 3 meters)
		bd 0.036 ; Bolt diameter
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBoxAtPoint layDef_Zero (list (* -0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Left bolts, viewed from above
	(DrawBoxAtPoint layDef_Zero (list (*  0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Right bolts, viewed from above
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawBoxAtPoint layDef_MetricDetails (list (* -0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Left bolts, viewed from above
	(DrawBoxAtPoint layDef_MetricDetails (list (*  0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Right bolts, viewed from above
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-METALL-VEGGKONSOLL ( / blockName description wx wy cx cy sx sy bx by bd )
	; Scale is mandated by Bane NOR symbol catalogue, "EH-KL_Strekningsplan.dwg" found as download at the TRV web site.
	; Bane NOR 2D layer/symbol: JBTEH_Komponenter / EH_Fund_Konsoll
	; Ref EH.xxxxx / Multiconsult drawing IAT-20-R-00006, ref email 2020-09-29 MB-CF (Marita Bjorøy) - RE Watch - SV Fjellbrakett railscomplete
	; Foundation for OCS masts with standard 191x374 bolt group.
	; Symbol: A wall mount bracket with a bolt group, and an "X" in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center side against mounting surface (concrete wall or other).
	;
	; -wx..................+wx
	;    -cx............+cx
	;
	; 01-02-------.------03-04    ...   0
	; 05-06-07--------08-09-10    ... -wy
	;    |  |          |  |
	;    |  |  (\) (/) |  |
	;    |  |     X    |  |       ... -(wy+by/2)
	;    |  |  (/) (\) |  |
	;    |  |          |  |
	;    11-12--------13-14       ... -(wy+sy) = -(wy+by)
	;    <sx>          <sx>
	;
	;
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-VEGGKONSOLL-191x374xM36")
		description	(strcat "KL-FUNDAMENT METALL, VEGGKONSOLL, 191x374xM36")
		; TRV symbol wallmount cradle, wall-mount plate + 2xsidewalls + baseplate
		wx 0.80  ; wall plate width againts conrete support 01-04
		wy 0.039 ; thickness								01-05
		cx 0.50  ; base plate width against wall plate		06-09
		cy 0.55  ; base plate extends from wall plate		06-11
		sx 0.025 ; side plates' thickness					06-07
		sy 0.55  ; side plates extends from wall plate		07-12
		bx 0.191 ; Bolt group
		by 0.374
		bd 0.036 ; Bolt diameter
	)
	(defun LocalGraphics ( layDef /)
		(SetLayer layDef)
		(DrawBoltGroup191x374xM36 layDef)
		(MoveDown (+ wy (* _half_ cy)))
		(command 
			; Wallmount plate:
			_RECTANGLE_ (list (/ wx -2) 0) (list (/ wx 2) (- wy))
			; Base plate:
			_RECTANGLE_ (list (/ cx -2) (- wy)) (list (/ cx 2) (- (+ wy cy)))
			; Side plates:
			_RECTANGLE_ (list (- (/ cx -2) sx) (- wy)) (list (+ (/ cx 2) sx) (- (+ wy sy)))
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(LocalGraphics layDef_Zero)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(LocalGraphics layDef_MetricDetails)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun NOBN-KL-FUNDAMENT-SPESIAL ( / blockName description x y bx by bd )
	; Ref TRV / EH symboler
	; Ref JBTEH_Komponenter / EH_Fund_Spesial (which is 5x real size)
	; Foundation for OCS masts with standard 191x374 bolt group, no further definition of the foundation's construction or layout.
	; Insertion direction is "both" in a track.
	;
	; S symbol: A 2x2 bolt group 191x374 (unhatched), and an "X" in the middle.
	;
	; ()     () ; Schematic
	;   
	;    \ /
	;     . 
	;    / \    ; 1.4 diagonal, 63 degrees (=5x metric)
	;   
	; ()     ()
	;
	(setq
		blockName	(strcat _OCS_ "FUN-" "KL-FUNDAMENT-SPESIAL-191x374xM36")
		description	(strcat "KL-FUNDAMENT, SPESIAL, 191x374xM36")
		x	(* 0.280 (DDcos 63)) ; Schematic symbol is 5x metric
		y	(* 0.280 (DDsin 63))
		bx	(* 0.420 (DDcos 63)) ; center of circles at four corners of a virtual box
		by	(* 0.420 (DDsin 63))
		bd	0.036
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "KL")
	(AddDescriptionBelowOrigin description (+ 0.50 _proxySymbolRadius_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBoltGroup191x374xM36 layDef_Zero)
	(DrawStAndrewCross layDef_Zero x y)
	(ScaleAll _four_)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(DrawBoltGroup191x374xM36 layDef_MetricDetails)
	(DrawStAndrewCross layDef_MetricDetails x y)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



;==========================
; Private Draw...X...() functions
;==========================
(defun DrawSmallBolt ( layDef / )
	(SetLayer layDef)
	(command
		_LINE_ _origin_ "0.00519615,0" _ENTER_
		_LINE_ "0.00347980,0.005" "@0,-0.00407180" _ENTER_
		_POLYLINE_ 
			_origin_ 
			_setPolylineArcMode_
			_setPolylineArcCenter_ "0,0.00692818" "0.00347980,0.00092820"
			_setPolylineArcCenter_ "0.00521209,0.00208057" "0.00694390,0.00092820"
			_setPolylineLineMode_
			"@0,0.00407180" 
			_ENTER_
		_LINE_
			"0,0.005"
			"@0.00741570,0"
			"@0,0.0012"
			"@-0.00741570,0"
			_ENTER_
		_LINE_ "0.00401569,0.008" "@-0.004,0" _ENTER_
		_LINE_ "0.00401569,0.005" "@0,0.0395" _ENTER_
		_LINE_ "0.00314964,0.045" "@0,-0.037" _ENTER_
		_LINE_ 
			"0,0.04500000"
			"0.00314964,0.04500000"
			"0.00401567,0.04450039"
			"0,0.04450039"
			_ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
)



(defun DrawBoltGroup191x374xM36 ( layDef / x y x1 y1 x2 y2 bx by bd br )
	(setq
		; TRV symbol bolt group:
		x	(* 0.200 (* (DDcos 63))) ; diagonal distance - X in the middle
		y	(* 0.200 (* (DDsin 63)))
		x1	(* 0.360 (* (DDcos 63))) ; diagonal distance, start 'ears'
		y1	(* 0.360 (* (DDsin 63)))
		x2	(* 0.480 (* (DDcos 63))) ; diagonal distance, end 'ears'
		y2	(* 0.480 (* (DDsin 63)))
		bx	(* 0.420 (* (DDcos 63))) ; center of circles at four corners of a virtual box
		by	(* 0.420 (* (DDsin 63)))
		bd	0.036 ; Bolt diameter
		br 	(* _half_ bd)
	)
	(DrawStAndrewCross layDef x y) ; inner cross
	(DrawLine layDef (PointTL x1 y1) (PointTL x2 y2))
	(DrawLine layDef (PointTR x1 y1) (PointTR x2 y2))
	(DrawLine layDef (PointBL x1 y1) (PointBL x2 y2))
	(DrawLine layDef (PointBR x1 y1) (PointBR x2 y2))
	(DrawCircleAtPoint layDef (PointTL bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointTR bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointBL bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointBR bx by) br _noWipeout_)
)



(defun DrawBoltGroup ( layDef bx by bd / br )
	(setq
		br 	(* _half_ bd) ; Bolt radius
	)
	(DrawCircleAtPoint layDef (PointTL bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointTR bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointBL bx by) br _noWipeout_)
	(DrawCircleAtPoint layDef (PointBR bx by) br _noWipeout_)
)



; DEPRECATED - bolt group real size:
;(defun DrawNut ( / )
	;	nutRadius 0.0277 ; M24
	;	boltRadius 0.012 ; M24
	;	x (/ 0.350 2) ; 350x350 bolteplassering
	;	y (/ 0.350 2)
	;(command 
	;	"._POLYGON" 6 (list x y) "_INSCRIBED" nutRadius
	;	_CIRCLE_ (list x y) boltRadius
	;	_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	;	_MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
	;)
;)
