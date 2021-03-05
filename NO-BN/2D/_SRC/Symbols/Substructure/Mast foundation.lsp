;=========================================================================================================================
;
; Mast foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Foundations for masts (signals, catenary masts, level crossing boom)

; Debug helpers:
; (C:MAST-FOUNDATION)
; (BETONG-HS-FUNDAMENT-FRI-LINJE)
; (BETONG-HS-FUNDAMENT-PLATTFORM)
; (BETONG-HS-FUNDAMENT-Ø680-300)
; (BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600)
; (BETONG-HS-FUNDAMENT-BORET)
; (BETONG-DS-FUNDAMENT-KONISK)
; (BETONG-DS-FUNDAMENT-RETTSIDET)
; (BETONG-KL-FUNDAMENT-BJELKEMAST)
; (BETONG-KL-FUNDAMENT-B3-B6-LAV-MAST)
; (BETONG-KL-FUNDAMENT-B6-HOEY-MAST)
; (BETONG-KL-FUNDAMENT-BORET-355)
; (BETONG-KL-FUNDAMENT-BORET-555)
; (FJELLBOLTER-FOR-HENGEMAST)
; (FJELLBOLTER-FOR-KONSOLL-PAA-VEGG)
; (METALL-KL-KONSOLL-VEGGFESTE)
; (SPESIAL-KL-FUNDAMENT)
; (drawSmallBolt)


(defun C:MAST-FOUNDATION ( / )
	(subSubStep "BETONG-HS-FUNDAMENT")
		(BETONG-HS-FUNDAMENT-FRI-LINJE)
		(BETONG-HS-FUNDAMENT-PLATTFORM)
		(BETONG-HS-FUNDAMENT-Ø680-300)
		(BETONG-HS-FUNDAMENT-Ø680-300)
		(BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600) ; OK, kanskje ikke en mast... men samme slags fundament, nesten, som gammelt signalfundament
		(BETONG-HS-FUNDAMENT-BORET) ; Ø555 Med signalfagets boltegruppe 164x164 mm

	(subSubStep "BETONG-DS-FUNDAMENT")
		(BETONG-DS-FUNDAMENT-KONISK)
		(BETONG-DS-FUNDAMENT-RETTSIDET)

	; Ny standard, med boltegruppe 374x191xM36 - arg=sålebredde (1600-2200) og 'såleoffset'.
	; Offset 000=søyle bakerst på sålen (lengst fra sporet innsatt med dir="both")
	; Offset 100=søyle 100 mm fra bakerst på sålen, osv
	(subSubStep "BETONG-KL-FUNDAMENT-BJELKEMAST")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1600" "000") 
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1700" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1800" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1800" "100")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1900" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "1900" "100")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2000" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2000" "100")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2000" "200")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2100" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2100" "100")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2100" "200")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2200" "000")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2200" "100")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2200" "200")
		(BETONG-KL-FUNDAMENT-BJELKEMAST "2200" "300")

	(subSubStep "BETONG-KL-FUNDAMENT-B3/B6/H3")
		(BETONG-KL-FUNDAMENT-B3-B6-LAV-MAST) ; B3 og B6 med 240x800 boltegruppe
		(BETONG-KL-FUNDAMENT-B6-HOEY-MAST)	 ; B6 med 260x940 boltegruppe
		(BETONG-KL-FUNDAMENT-H3 "6000-9500") ; Beregnet for mastehøyder fra-til [mm]
		(BETONG-KL-FUNDAMENT-H3 "10000-12000")
		(BETONG-KL-FUNDAMENT-H3 "12500-13000")

	(subSubStep "BETONG-KL-FUNDAMENT-BORET-355/555")
		(BETONG-KL-FUNDAMENT-BORET-355) ; Ø355 Med KL-fagets boltegruppe
		(BETONG-KL-FUNDAMENT-BORET-555) ; Ø555 Med KL-fagets boltegruppe

	(subSubStep "FJELLBOLTER / METALL-KL-KONSOLL-VEGGFESTE / FJELLBOLTER-FOR-KONSOLL-PAA-VEGG / SPESIAL-KL-FUNDAMENT")
		(FJELLBOLTER-FOR-HENGEMAST)
		(FJELLBOLTER-FOR-KONSOLL-PAA-VEGG)
		(METALL-KL-KONSOLL-VEGGFESTE)
		(SPESIAL-KL-FUNDAMENT)
)



(defun BETONG-HS-FUNDAMENT-FRI-LINJE ( / blockName description baseX baseY shaftX shaftY x y boltRadius cutoutX cutoutY paperScale )
	; Ref. S.xxxxxx
	(setq
		blockName	"NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-FRI-LINJE-300x300xM24"
		description	"BETONG HOVEDSIGNAL- FUNDAMENT FRI LINJE 2000x2000x300-650x650x1700 300x300xM24"
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
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(command
			_POLYLINE_ ; 
				(list (- (/ baseX 2)) (/ baseY 2))
				(list (+ (/ baseX 2)) (/ baseY 2))
				(list (+ (/ shaftX 2)) (/ shaftY 2))
				(list (- (/ shaftX 2)) (/ shaftY 2))
				_closedPolyline_
			_CIRCLE_ (list x y) boltRadius
		)
		(mirrorAboutXaxis _keepMirrorSource_)
		(mirrorAboutYaxis _keepMirrorSource_)
		(mirrorAboutDiagonal _keepMirrorSource_)
		(command _RECTANGLE_ (list (- (/ cutoutX 2)) (- (/ shaftY 2))) (list (+ (/ cutoutX 2)) (- cutoutY (/ shaftY 2))))
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-HS-FUNDAMENT-PLATTFORM ( / blockName description s1 s2 dim1 dim2 radius_gjengejern radius_roer roer_y paperScale )
	; Ref. S.xxxxxx
	(setq
		blockName	"NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-PLATTFORM-300x300xM24"
		description	"BETONG HOVEDSIGNAL- FUNDAMENT PLATTFORM 1750x1750x300-650x650x1000 300x300xM24"
		s1 2.0
		s2 0.6
		dim1 0.15
		dim2 0.05
		radius_gjengejern 0.0120
		radius_roer 0.0420
		roer_y (+ (/ s2 2) radius_roer) 
	)
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(command
			_RECTANGLE_	(list (- (/ s1 2)) (- (/ s1 2)))	(list (/ s1 2) (/ s1 2))			;stort kvadrat, ytre
			_RECTANGLE_	(list (- (/ s2 2)) (- (/ s2 2)))	(list (/ s2 2) (/ s2 2))			;lite kvadrat, indre
			; skrålinjer:
			_LINE_		(list (- (/ s1 2)) (- (/ s1 2)))	(list (- (/ s2 2)) (- (/ s2 2))) _ENTER_
			_LINE_		(list (/ s1 2) (/ s1 2))			(list (/ s2 2) (/ s2 2)) _ENTER_
			_LINE_		(list (/ s1 -2) (/ s1 2))			(list (/ s2 -2) (/ s2 2)) _ENTER_
			_LINE_		(list (/ s1 2) (/ s1 -2))			(list (/ s2 2) (/ s2 -2)) _ENTER_
			_CIRCLE_		(list dim1 dim1) radius_gjengejern
			_CIRCLE_		(list (- dim1) dim1) radius_gjengejern
			_CIRCLE_		(list dim1 (- dim1)) radius_gjengejern
			_CIRCLE_		(list (- dim1) (- dim1)) radius_gjengejern
			_CIRCLE_		_origo_ radius_gjengejern
			_CIRCLE_		(list (- radius_roer) (- roer_y)) radius_roer
			_CIRCLE_		(list radius_roer (- roer_y)) radius_roer
			_CIRCLE_		(list 0 roer_y) radius_roer
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-HS-FUNDAMENT-Ø680-300 ( / blockName description r1 r2 d radius_gjengejern paperScale )
	; Ref. S.xxxxxx
	(setq 
		blockName 	"NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-Ø680-300x300xM24"
		description	(strcat "BETONG HOVEDSIGNAL- FUNDAMENT " _uOE_ "300/" _uOE_ "680 300x300xM24")
		r1 (/ 0.680 2) ; Ytre radius, stående betongrør
		r2 (/ 0.300 2) ; Indre radius, stående betongrør
		d (/ 0.300 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		radius_gjengejern (/ 0.024 2) ; M24 gjengejern (stikker 100 opp)
	)
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(command
			_CIRCLE_ _origo_ r1
			_CIRCLE_ _origo_ r2
			_CIRCLE_ (list d d) radius_gjengejern
			_CIRCLE_ (list d (- d)) radius_gjengejern
			_CIRCLE_ (list (- d) d) radius_gjengejern
			_CIRCLE_ (list (- d) (- d)) radius_gjengejern
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600 ( / blockName description r1 r2 d radius_gjengejern paperScale )
	; Ref. S.xxxxxx
	(setq 
		blockName	"NO-BN-2D-JBTUB-FUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø680-210x210xM20-1600"
		description	(strcat "BETONG VEIBOMDRIVMASKIN- FUNDAMENT " _uOE_ "210/" _uOE_ "680 210x210xM20 h=1600")
		r1 (/ 0.680 2) ; Ytre radius, stående betongrør
		r2 (/ 0.210 2) ; Indre radius, stående betongrør
		d (/ 0.300 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		radius_gjengejern (/ 0.020 2) ; M20 gjengejern (stikker 100 opp)
	)
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(command ; Add metric graphics
			_CIRCLE_ _origo_ r1
			_CIRCLE_ _origo_ r2
			_CIRCLE_ (list d d) radius_gjengejern
			_CIRCLE_ (list d (- d)) radius_gjengejern
			_CIRCLE_ (list (- d) d) radius_gjengejern
			_CIRCLE_ (list (- d) (- d)) radius_gjengejern
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-HS-FUNDAMENT-BORET ( / blockName description r R1 R2 R_circ x ang d_ang )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-BORET-" _uOE_ "555-164x164xM32")
		description	(strcat "BETONG HOVEDSIGNAL- FUNDAMENT BORET " _uOE_ "555 164x164xM32")
		r (/ 0.032 2) ; M32 gjengejern for de indre Ø45 hullene i signalets mastefot-plate
		R1 0.15
		R2 0.4
		R_pole (/ 0.555 2) ; Ø555
		x (/ 0.164 2) ; Bolteplassering 164x164 mm for signalers mastefot (de indre hullene)
		ang 5
		d_ang 45
	)
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(command
			_CIRCLE_ (list x x) r
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R ang) R2) _setArcAngle_ d_ang
			_ARC_ _setArcCenter_ _origo_ (polar _origo_ (D->R (/ d_ang -2)) R1) _setArcAngle_ d_ang
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origo_ 4 _fullCircle_ _rotateObjects_
			_CIRCLE_ _origo_ R_pole
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-DS-FUNDAMENT-KONISK ( / blockName description s1 s2 dim1 dim2 paperScale )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-DS-" _uOE_ "76-KONISK")
		description	(strcat "BETONG DVERGSIGNAL- FUNDAMENT KONISK 600(400)x600(400)x500 " _uOE_ "76")
		s1 (/ 0.600 2)
		s2 (/ 0.400 2)
		dim1 0.12
		dim2 0.10
	)
	(defun localGraphics ( layDef /)
		(drawSmallBolt layDef)
		; make three of them:
		(command	
			_MIRROR_ _selectAll_ _ENTER_ (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) _eraseMirrorSource_
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origo_ 3 _fullCircle_ _rotateObjects_
			_CIRCLE_ _origo_ 0.0413
			_CIRCLE_ _origo_ 0.0445
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
				_lastSelection_ _ENTER_ _polarArray_ _origo_ 4 360 _rotateObjects_
			_RECTANGLE_
				(list (/ dim1 -2) -0.24)
				(list (/ dim1 2) (+ -0.24 dim2))
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-DS-FUNDAMENT-RETTSIDET ( / blockName description s1 s2 dim1 dim2 paperScale )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-DS-" _uOE_ "76-RETTSIDET")
		description	(strcat "BETONG DVERGSIGNAL- FUNDAMENT RETTSIDET 600(400)x600(400)x500 " _uOE_ "76")
        ;miderste rør med skruer:
		s1 (/ 0.600 2)
		s2 (/ 0.400 2)
		dim1 0.12
		dim2 0.10
	)
	(defun localGraphics ( layDef /)
		(drawSmallBolt layDef)
		(command
			_MIRROR_ _selectAll_ _ENTER_ (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) _eraseMirrorSource_
			_ARRAY_ _selectAll_ _ENTER_ _polarArray_ _origo_ 3 _fullCircle_ _rotateObjects_
			_CIRCLE_  _origo_ 0.0413
			_CIRCLE_ _origo_ 0.0445
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
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-BJELKEMAST ( baseYmm baseOffsetYmm / blockName description baseX baseY shaftX shaftY baseOffsetY ductRadius )
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
		blockName	(strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-BJELKEMAST-1600x" baseYmm "-" baseOffsetYmm "-191x374xM36")
		description	(strcat "BETONG KL- FUNDAMENT BJELKEMAST 1600x" baseYmm "-" baseOffsetYmm " 700x700 191x374xM36")
		baseX 1.600 ; Alongside the track, constant
		baseY (/ (atof baseYmm) 1000.0) ; Transversal to track
		; baseZ 1.000 ; Sålehøyde - irrelevant for 2D
		shaftX 0.700 ; x = along track
		shaftY 0.700 ; y = transversal to track
		; shaftZ - Varierer i steg på 100 til max 2700 (total fundamenthøyde blir da 1000 (såle) + 2700 (hals) hvorav minst 200 og max 700 skal rage opp over terreng.
		baseOffsetY (/ (atof baseOffsetYmm) 1000.0) ; Convert offset from millimeter to meter, varies from 0.000 to 0.300
		ductRadius (* _half_ 0.050)
	)
	(defun localGraphics ( layDef / )
		(setLayer layDef)
		(drawBoltGroup191x374xM36 layDef)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origo_ ductRadius
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-B3-B6-LAV-MAST ( / blockName description baseX baseY baseOffsetY shaftX shaftY ductRadius bx by bd )
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
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-BETONG-B3-B6-LAV-1600x1800-000-240x800xM36"
		description	"BETONG KL- FUNDAMENT B3 OG B6 LAV MAST 1600x1800-000 600x1200 240x800xM36"
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
	(defun localGraphics ( layDef / )
		(drawBoltGroup layDef bx by bd)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origo_ ductRadius
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-B6-HOEY-MAST ( / blockName description baseX baseY baseOffsetY shaftX shaftY ductRadius bx by bd )
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
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-BETONG-B6-HOEY-1600x1800-000-260x940xM36"
		description	"BETONG KL- FUNDAMENT HOEY B6-MAST 1600x1800-000 600x1200 260x940xM36"
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
	(defun localGraphics ( layDef / )
		(drawBoltGroup layDef bx by bd)
		(command
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_ ; shaft
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origo_ ductRadius
		)
	)
	
	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-H3 ( mastHeight / blockName description baseX baseY shaftX shaftY baseOffsetY ductRadius bx by bd )
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
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-BETONG-H3-1600x1800-000"
		description	"BETONG KL- FUNDAMENT H3-MAST 1600x1800-000 " ;...
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
				blockName (strcat blockName "-790x790xM36-10000-12000")
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
				blockName (strcat blockName "-830x830xM36-12500-13000")
				description (strcat description "1100x1100 830x830xM36 FOR H3 MAST 12.5m-13.0m")
				shaftX 1.100
				shaftY 1.100
				bx 0.830
				by 0.830
				bd 0.036
			)
		)
	)
	(defun localGraphics ( layDef / )
		(setLayer layDef)
		(drawBoltGroup layDef bx by bd)
		(command ; Draw shaft and duct
			_RECTANGLE_ ; base
				(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
				(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
			_RECTANGLE_
				(list (- (/ shaftX 2)) (- (/ shaftY 2)))
				(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
			_CIRCLE_ _origo_ ductRadius
		)
	)	
	
	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-BORET-355 ( / blockName description x y x1 y1 x2 y2 pd pr bx by bd br )
	; 2020-04-08 Bane NOR 1:250 skala 2D layer/symbol: JBTEH_Komponenter / (nonexistent, created by CLFEY)
	; Ref EH(EK).707523-000_001 "Prefabrikert ; Betongsøylefundament; Ø355 for; kontaktledningsmast"
	; Symbol: A circle with eight hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-BORET-" _uOE_ "355-" _uOE_ "232xDD27xM36")
		description	(strcat "BETONG KL- FUNDAMENT BORET " _uOE_ "355 " _uOE_ "232x27DDxM36")
		x	(* 0.090 (DDcos 27)) ; Line from origo
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
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(drawLine layDef_Zero _origo_ (posTR x y))					; ./ line from origo
	(drawCircleAtPos layDef_Zero (posTR bx by) br _noWipeout_)	; ( ) bolt
	(drawLine layDef_Zero (posTR x1 y1) (posTR x2 y2))			; (/) hatch over bolt
	(mirrorAboutXaxis _keepMirrorSource_)
	(mirrorAboutYaxis _keepMirrorSource_)
	(mirrorAboutDiagonal _keepMirrorSource_) ; Now we have 8 bolts
	(drawCircle layDef_Zero pr _noWipeout_) ; Foundation concrete pole
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(drawLine layDef_MetricDetails _origo_ (posTR x y))					
	(drawCircleAtPos layDef_MetricDetails (posTR bx by) br _noWipeout_)	
	(drawLine layDef_MetricDetails (posTR x1 y1) (posTR x2 y2))			
	(mirrorAboutXaxis _keepMirrorSource_)
	(mirrorAboutYaxis _keepMirrorSource_)
	(mirrorAboutDiagonal _keepMirrorSource_)
	(drawCircle layDef_MetricDetails pr _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun BETONG-KL-FUNDAMENT-BORET-555 ( / blockName description bx by pd pr )
	; JBTEH_Komponenter / EH_Fund1 (which is 5x real size)
	; Ref EH.800091-000_001 "Høyspenning, kontaktledningsanlegg; Prefabrikkert betongsøylefundament; Ø555 for kontaktledningsmast"
	; Symbol: A circle with four hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-BORET-" _uOE_ "555-191x374xM36")
		description	(strcat "BETONG KL- FUNDAMENT BORET " _uOE_ "555 191x374xM36")
		bx 0.191 ; Bolt group
		by 0.374
		bd 0.036 ; Bolt diameter
		pd  0.555 ; pole diameter Ø555 mm
		pr	(* _half_ pd) ; pole radius
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative block
	(drawBoltGroup191x374xM36 layDef_Zero)
	(drawCircle layDef_Zero pr _noWipeout_)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric block
	(drawBoltGroup191x374xM36 layDef_MetricDetails)
	(drawCircle layDef_MetricDetails pr _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun FJELLBOLTER-FOR-HENGEMAST ( / blockName description x y bx by bd )
	; Symbol: Just four hatched smaller circles, and a cross in the middle.
	; Plays the role of BN foundation EH_Fund_Spesial
	;
	(setq 
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-FJELLBOLTER-FOR-BRU-OG-TUNNEL-350x350xM24"
		description	"BOLTEGRUPPE FOR KL- HENGEMAST FOR BRU OG TUNNEL 350x350xM24"
		; Schematic = 5x metric
		x	0.230  ; 'X' in the middle of four circles
		y	0.230
		bx	0.350 ; Bolt group
		by	0.350
		bd 	0.024 ; Bolt diameter
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(drawBoltGroup layDef_Zero bx by bd)
	(drawStAndrewCross layDef_Zero x y)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawBoltGroup layDef_MetricDetails bx by bd)
	(drawStAndrewCross layDef_MetricDetails x y)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun FJELLBOLTER-FOR-KONSOLL-PAA-VEGG ( / blockName description bx bl bd )
;
;      +-+       +-+    ... bl
;      + +       + +
;      + +       + +   
;      + +       + +
;      +-+   .   +-+    ... zero
;      bd         bd
;      <--- bx  --->
;
	(setq 
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-FJELLBOLTER-FOR-VEGGKONSOLL-600x700xM24" ; 4 x Horizontal threaded iron 'gjengestag'
		description	"BOLTEGRUPPE FOR KL- VEGGKONSOLL 600x700xM24"
		bx 0.600 ; x-distance between bolts
		bl 3.000 ; Bolt length (typical real length is 3 meters)
		bd 0.036 ; Bolt diameter
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(drawBoxAtPos layDef_Zero (list (* -0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Left bolts, viewed from above
	(drawBoxAtPos layDef_Zero (list (*  0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Right bolts, viewed from above
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawBoxAtPos layDef_MetricDetails (list (* -0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Left bolts, viewed from above
	(drawBoxAtPos layDef_MetricDetails (list (*  0.5 bx) (* 0.5 bl)) bd bl _noWipeout_) ; Right bolts, viewed from above
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun METALL-KL-KONSOLL-VEGGFESTE ( / blockName description wx wy cx cy sx sy bx by bd )
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
	;    <sx >        < sx>
	;
	;
	(setq
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-KONSOLL-VEGGFESTE-191x374xM36"
		description	"METALL KL- KONSOLL VEGGFESTE 191x374xM36"
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
	(defun localGraphics ( layDef /)
		(setLayer layDef)
		(drawBoltGroup191x374xM36 layDef)
		(moveDown (+ wy (* _half_ cy)))
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
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(localGraphics layDef_Zero)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(localGraphics layDef_MetricDetails)
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun SPESIAL-KL-FUNDAMENT ( / blockName description x y bx by bd )
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
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-SPESIAL-191x374xM36"
		description	"SPESIAL KL- FUNDAMENT 191x374xM36"
		x	(* 0.280 (DDcos 63)) ; Schematic symbol is 5x metric
		y	(* 0.280 (DDsin 63))
		bx	(* 0.420 (DDcos 63)) ; center of circles at four corners of a virtual box
		by	(* 0.420 (DDsin 63))
		bd	0.036
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(drawBoltGroup191x374xM36 layDef_Zero)
	(drawStAndrewCross layDef_Zero x y)
	(scaleAll _four_)
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
	
	; Metric symbol
	(drawBoltGroup191x374xM36 layDef_MetricDetails)
	(drawStAndrewCross layDef_MetricDetails x y)
	(createMetricBlockFromCurrentGraphics blockName)
)



;==========================
; Private draw...X...() functions
;==========================
(defun drawSmallBolt ( layDef / )
	(setLayer layDef)
	(command
		_LINE_ _origo_ "0.00519615,0" _ENTER_
		_LINE_ "0.00347980,0.005" "@0,-0.00407180" _ENTER_
		_POLYLINE_ 
			_origo_ 
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
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
)



(defun drawBoltGroup191x374xM36 ( layDef / x y x1 y1 x2 y2 bx by bd br )
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
	(drawStAndrewCross layDef x y) ; inner cross
	(drawLine layDef (posTL x1 y1) (posTL x2 y2))
	(drawLine layDef (posTR x1 y1) (posTR x2 y2))
	(drawLine layDef (posBL x1 y1) (posBL x2 y2))
	(drawLine layDef (posBR x1 y1) (posBR x2 y2))
	(drawCircleAtPos layDef (posTL bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posTR bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posBL bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posBR bx by) br _noWipeout_)
)



(defun drawBoltGroup ( layDef bx by bd / br )
	(setq
		br 	(* _half_ bd) ; Bolt radius
	)
	(drawCircleAtPos layDef (posTL bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posTR bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posBL bx by) br _noWipeout_)
	(drawCircleAtPos layDef (posBR bx by) br _noWipeout_)
)



; DEPRECATED - bolt group real size:
;(defun drawNut ( / )
	;	nutRadius 0.0277 ; M24
	;	boltRadius 0.012 ; M24
	;	x (/ 0.350 2) ; 350x350 bolteplassering
	;	y (/ 0.350 2)
	;(command 
	;	"._POLYGON" 6 (list x y) "_INSCRIBED" nutRadius
	;	_CIRCLE_ (list x y) boltRadius
	;	_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	;	_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
	;)
;)
