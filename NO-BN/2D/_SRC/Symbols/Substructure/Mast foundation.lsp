;=========================================================================================================================
;
; Mast foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
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
; (BETONG-KL-FUNDAMENT-B3)
; (BETONG-KL-FUNDAMENT-H3)
; (BETONG-KL-FUNDAMENT-BORET-355)
; (BETONG-KL-FUNDAMENT-BORET-555)
; (FJELLBOLTER-FOR-HENGEMAST)
; (METALL-KL-KONSOLL-VEGGFESTE)
; (SPESIAL-KL-FUNDAMENT)
; (DRAWBOLT)


(defun C:MAST-FOUNDATION ( / )
	(BETONG-HS-FUNDAMENT-FRI-LINJE)
	(BETONG-HS-FUNDAMENT-PLATTFORM)
	(BETONG-HS-FUNDAMENT-Ø680-300)
	(BETONG-HS-FUNDAMENT-Ø680-300)
	(BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600) ; OK, kanskje ikke en mast... men samme slags fundament, nesten, som gammelt signalfundament
	(BETONG-HS-FUNDAMENT-BORET) ; Ø555 Med signalfagets boltegruppe 164x164 mm
	(BETONG-DS-FUNDAMENT-KONISK)
	(BETONG-DS-FUNDAMENT-RETTSIDET)

	; Ny standard, med boltegruppe 374x191xM36 - arg=sålebredde (1600-2200) og 'såleoffset'.
	; Offset 000=søyle bakerst på sålen (lengst fra sporet innsatt med dir="both")
	; Offset 100=søyle 100 mm fra bakerst på sålen, osv
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

	(BETONG-KL-FUNDAMENT-B3)
	(BETONG-KL-FUNDAMENT-H3 "6000-9500") ; Beregnet for mastehøyder fra-til [mm]
	(BETONG-KL-FUNDAMENT-H3 "10000-12000")
	(BETONG-KL-FUNDAMENT-H3 "12500-13000")

	(BETONG-KL-FUNDAMENT-BORET-355) ; Ø355 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET-555) ; Ø555 Med KL-fagets boltegruppe

	(FJELLBOLTER-FOR-HENGEMAST)
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayer layer_Zero)
	(command
		"._PLINE" ; 
			(list (- (/ baseX 2)) (/ baseY 2))
			(list (+ (/ baseX 2)) (/ baseY 2))
			(list (+ (/ shaftX 2)) (/ shaftY 2))
			(list (- (/ shaftX 2)) (/ shaftY 2))
			"_CLOSE"
		"._CIRCLE" (list x y) boltRadius
	)
	(mirrorAboutXaxis _keep_)
	(mirrorAboutYaxis _keep_)
	(mirrorAboutDiagonal _keep_)
	(command "._RECTANGLE" (list (- (/ cutoutX 2)) (- (/ shaftY 2))) (list (+ (/ cutoutX 2)) (- cutoutY (/ shaftY 2))))
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayer layer_Zero)
	(command
		"._RECTANGLE"	(list (- (/ s1 2)) (- (/ s1 2)))	(list (/ s1 2) (/ s1 2))			;stort kvadrat, ytre
		"._RECTANGLE"	(list (- (/ s2 2)) (- (/ s2 2)))	(list (/ s2 2) (/ s2 2))			;lite kvadrat, indre
		; skrålinjer:
		"._LINE"		(list (- (/ s1 2)) (- (/ s1 2)))	(list (- (/ s2 2)) (- (/ s2 2))) ""
		"._LINE"		(list (/ s1 2) (/ s1 2))			(list (/ s2 2) (/ s2 2)) ""
		"._LINE"		(list (/ s1 -2) (/ s1 2))			(list (/ s2 -2) (/ s2 2)) ""
		"._LINE"		(list (/ s1 2) (/ s1 -2))			(list (/ s2 2) (/ s2 -2)) ""
		"._CIRCLE"		(list dim1 dim1) radius_gjengejern
		"._CIRCLE"		(list (- dim1) dim1) radius_gjengejern
		"._CIRCLE"		(list dim1 (- dim1)) radius_gjengejern
		"._CIRCLE"		(list (- dim1) (- dim1)) radius_gjengejern
		"._CIRCLE"		(list 0 0) radius_gjengejern
		"._CIRCLE"		(list (- radius_roer) (- roer_y)) radius_roer
		"._CIRCLE"		(list radius_roer (- roer_y)) radius_roer
		"._CIRCLE"		(list 0 roer_y) radius_roer
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	(setLayer layer_Zero)
	(command
		"._CIRCLE" (list 0 0) r1
		"._CIRCLE" (list 0 0) r2
		"._CIRCLE" (list d d) radius_gjengejern
		"._CIRCLE" (list d (- d)) radius_gjengejern
		"._CIRCLE" (list (- d) d) radius_gjengejern
		"._CIRCLE" (list (- d) (- d)) radius_gjengejern
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayer layer_Zero)
	(command ; Add metric graphics
		"._CIRCLE" (list 0 0) r1
		"._CIRCLE" (list 0 0) r2
		"._CIRCLE" (list d d) radius_gjengejern
		"._CIRCLE" (list d (- d)) radius_gjengejern
		"._CIRCLE" (list (- d) d) radius_gjengejern
		"._CIRCLE" (list (- d) (- d)) radius_gjengejern
	)
	(command "._SCALE" "_ALL" "" "0,0" scale)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONG-HS-FUNDAMENT-BORET ( / blockName description rad R1 R2 R_circ x ang d_ang )
	; Ref. S.xxxxxx
	(setq
		blockName	(strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-BORET-" _uOE_ "555-164x164xM32")
		description	(strcat "BETONG HOVEDSIGNAL- FUNDAMENT BORET " _uOE_ "555 164x164xM32")
		rad (/ 0.032 2) ; M32 gjengejern for de indre Ø45 hullene i signalets mastefot-plate
		R1 0.15
		R2 0.4
		R_pole (/ 0.555 2) ; Ø555
		x (/ 0.164 2) ; Bolteplassering 164x164 mm for signalers mastefot (de indre hullene)
		ang 5
		d_ang 45
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayer layer_Zero)
	(command
		"._CIRCLE" (list x x) rad
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R ang) R2) "Angle" d_ang
		"._ARC" "C" "0,0" (polar (list 0 0) (D->R (/ d_ang -2)) R1) "Angle" d_ang
		"._ARRAY" "_ALL" "" "_POLAR" "0,0" 4 360 "_YES" "._CIRCLE" "0,0" R_pole
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayer layer_Zero)
	(drawBolt)
	; make three of them:
	(command	
		"._MIRROR" "_ALL" "" (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) "_YES"
		"._ARRAY" "_ALL" "" "_POLAR" "0,0" 3 360 "_YES"
		"._CIRCLE" "0,0" 0.0413
		"._CIRCLE" "0,0" 0.0445
	)
	; Tapered foundation for Ds shunting signal:
	(command
		"._LINE" 
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
			""
		"LINE"
			(list (- s1) (- s1))
			(list (- s2) (- s2))
			""
		"._ARRAY" "_LAST" ""
			"_POLAR" "0,0" 4 360 "_YES"
		"._RECTANGLE"
			(list (/ dim1 -2) -0.24)
			(list (/ dim1 2) (+ -0.24 dim2))
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
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
	; Schematic symbol
	(setLayer layer_Zero)
	(drawBolt)
	(command
		"._MIRROR" "_ALL" "" (list -1 (/ (+ 0.045 0.0380) 2)) (list 1 (/ (+ 0.045 0.0380) 2)) "_YES"
		"._ARRAY" "_ALL" ""  "_POLAR" "0,0" 3 360  "_YES"
		"._CIRCLE"  "0,0" 0.0413
		"._CIRCLE" "0,0" 0.0445
	)
	; Straight-sided foundation for Ds shunting signal:
	(command
		"._RECTANGLE"
			(list (- s1) (- s1)) (list s1 s1)
		"._LINE"
			(list (/ dim1 2) (- s2))
			(list s2 (- s2))
				(strcat "@0," (rtos (* 2 s2)))
				(strcat "@" (rtos (* -2 s2)) ",0")
				(strcat "@0," (rtos (* -2 s2)))
				(strcat "@" (rtos (- s2 (/ dim1 2))) ",0")
				(strcat "@0," (rtos (/ dim1 2)))
				(strcat "@" (rtos dim1) ",0")
				(strcat "@0," (rtos (/ dim1 -2.0)))
			""
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONG-KL-FUNDAMENT-BJELKEMAST ( baseYmm baseOffsetYmm / blockName description baseX baseY shaftX shaftY baseOffsetY boltRadius ductRadius x y paperScale )
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
		baseY (/ (atof baseYmm) 1000.0)
		; baseZ 1.000 ; Sålehøyde - irrelevant for 2D
		shaftX 0.700 ; x = along track
		shaftY 0.700 ; y = transversal to track
		; shaftZ - Varierer i steg på 100 til max 2700 (total fundamenthøyde blir da 1000 (såle) + 2700 (hals) hvorav minst 200 og max 700 skal rage opp over terreng.
		baseOffsetY (/ (atof baseOffsetYmm) 1000.0) ; Convert offset from millimeter to meter, varies from 0.000 to 0.300
		boltRadius (/ 0.036 2) ; M36 threaded irons ("bolt")
		ductRadius (/ 0.050 2) ; For earthing conductor etc.
		x (/ 0.19068 2) ; Bolt group 190.68 * 374.22
		y (/ 0.37422 2)
	)
	(setLayer layer_Zero)
	(command
		"._RECTANGLE" ; base
			(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
			(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
		"._RECTANGLE" ; shaft
			(list (- (/ shaftX 2)) (- (/ shaftY 2)))
			(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
		"._CIRCLE" "0,0" ductRadius
		"._CIRCLE" (list (+ x) (+ y)) boltRadius
		"._CIRCLE" (list (- x) (+ y)) boltRadius
		"._CIRCLE" (list (+ x) (- y)) boltRadius
		"._CIRCLE" (list (- x) (- y)) boltRadius
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONG-KL-FUNDAMENT-B3 ( / blockName description baseX baseY shaftX shaftY baseOffsetY boltRadius ductRadius x y paperScale )
	; Ref EH.707387-001 på Elkraftportalen, plasstøpt fundament for B3 og B6 stålmast på flatt terreng (-002 er fjællskjæring).
	; Høyde 500+1500+700 (sålehøyde / søyle fra såle til formasjonsplan / søyle oppraging over formasjonsplanet)
	; Overkant oppraging = SOK
	; Oppstikk bolter over SOK: 180 mm
	; Sålebredde 1000+800=1800 på tvers av sporretningen (1000 mot sporet, 800 vekk fra sporet). I skrått terreng: 2200.
	; Såledybde  800+800=1600 i sporretningen (symmetrisk om mastesenter)
	; Søylebredde a=1000 på tvers av sporretningen x søyledybde b=1000 i sporretningen
	;
	; De fleste H3 fund har a=b=1000 (mastehøyder 6,0-9,5 og H5 utliggeråk 7,0-8,5)
	; Øvrige H3 fund har a=b=1100 (mastehøyder 10,0-13,0 og H5 utliggeråk 9,0-13,0)
	; For å sette inn mastefundament med motsatt kraftretning, snu om dir fra both til none.
	(setq
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-BETONG-B3-1600x1800-000-240x800xM36"
		description	"BETONG KL- FUNDAMENT B3-MAST 1600x1800-000 600x1200 240x800xM36"
		baseX 1.600 ; Langs med sporet
		baseY 1.800 ; På tvers av sporet
		; baseZ 0.500 ; Sålehøyde - irrelevant for 2D
		shaftX 0.600 ; Hals
		shaftY 1.200 ; Hals B3
		baseOffsetY 0.000 ; Såle offset fra ytterste posisjon
		; shaftZ - Varierer - ikke relevant for 2D symbolet
		boltRadius (/ 0.036 2) ; M36 gjengestag
		ductRadius (/ 0.050 2)
		x (/ 0.240 2) ; Boltegruppe 240x800
		y (/ 0.800 2)
	)
	(setLayer layer_Zero)
	(command
		"._RECTANGLE" ; base
			(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
			(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
		"._RECTANGLE" ; shaft
			(list (- (/ shaftX 2)) (- (/ shaftY 2)))
			(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
		"._CIRCLE" "0,0" ductRadius
		"._CIRCLE" (list x y) boltRadius
		"._CIRCLE" (list (- x) y) boltRadius
		"._CIRCLE" (list x (- y)) boltRadius
		"._CIRCLE" (list (- x) (- y)) boltRadius
	)
	(scaleAll _four_) ; Schematic = 4x real size
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONG-KL-FUNDAMENT-H3 ( mastHeight / blockName description baseX baseY shaftX shaftY baseOffsetY x y boltRadius ductRadius paperScale )
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
		boltRadius (/ 0.036 2) ; M36 gjengestag, stikker 180 mm opp over SOK=overkant søyle 
		ductRadius (/ 0.050 2)
	)
	(cond 
		((= mastHeight "6000-9500")
			(setq 
				blockName (strcat blockName "-700x700xM36-6000-9500")
				description (strcat description "1000x1000 700x700xM36 FOR H3 MAST 6,0m-9,5m")
				shaftX 1.000	; Finnes i 1.000 x 1.000 (H3 mast 6,0 til 9,5m høyde) og 1.100 x 1.100 (H3 mast 10,0 til 12,0m høyde)
				shaftY 1.000
				x (/ 0.700 2) ; Boltegruppe i retning langs sporet ("bredde") (y=halvbredden) 700x700 for Hr 6,0..9,5m, 790x790 for 10,0..12,0m, 830x830 for 12,5-13,0m
				y (/ 0.700 2) ; Boltegruppe i retning på tvers av sporet ("dybde") (x=halvdybden)
			)
		)
		((= mastHeight "10000-12000")
			(setq
				blockName (strcat blockName "-790x790xM36-10000-12000")
				description (strcat description "1100x1100 790x790xM36 FOR H3 MAST 10.0m-12.0m")
				shaftX 1.100
				shaftY 1.100
				x (/ 0.790 2) ; Boltegruppe i retning langs sporet ("bredde") (y=halvbredden) 700x700 for Hr 6,0..9,5m, 790x790 for 10,0..12,0m, 830x830 for 12,5-13,0m
				y (/ 0.790 2) ; Boltegruppe i retning på tvers av sporet ("dybde") (x=halvdybden)
			)
		)
		((= mastHeight "12500-13000")
			(setq 
				blockName (strcat blockName "-830x830xM36-12500-13000")
				description (strcat description "1100x1100 830x830xM36 FOR H3 MAST 12.5m-13.0m")
				shaftX 1.100
				shaftY 1.100
				x (/ 0.830 2)
				y (/ 0.830 2)
			)
		)
	)
	(setLayer layer_Zero)
	(command
		"._RECTANGLE" ; base
			(list (- (/ baseX 2)) (+ 0.800 baseOffsetY (- baseY)))
			(list (+ (/ baseX 2)) (+ 0.800 baseOffsetY))
		"._RECTANGLE"
			(list (- (/ shaftX 2)) (- (/ shaftY 2)))
			(list (+ (/ shaftX 2)) (+ (/ shaftY 2)))
		"._CIRCLE" "0,0" ductRadius
		"._CIRCLE" (list (+ x) (+ y)) boltRadius
		"._CIRCLE" (list (- x) (+ y)) boltRadius
		"._CIRCLE" (list (+ x) (- y)) boltRadius
		"._CIRCLE" (list (- x) (- y)) boltRadius
	)
	(scaleAll _four_) ; 1:250 is real size
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONG-KL-FUNDAMENT-BORET-355 ( / blockName description x y x1 y1 x2 y2 cx cy r rf )
	; 2020-04-08 Bane NOR 1:250 skala 2D layer/symbol: JBTEH_Komponenter / (nonexistent, created by CLFEY)
	; Ref EH(EK).707523-000_001 "Prefabrikert ; Betongsøylefundament; Ø355 for; kontaktledningsmast"
	; Symbol: A circle with eight hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-BORET-" _uOE_ "355-" _uOE_ "232xDD27xM36")
		description	(strcat "BETONG KL- FUNDAMENT BORET " _uOE_ "355 " _uOE_ "232x27DDxM36")
		x	(* 0.55 (DDcos 27)) ; Line from origo
		y	(* 0.55 (DDsin 27))
		x1	(* 1.00 (DDcos 27)) ; Start hatch over bolt
		y1	(* 1.00 (DDsin 27))
		x2	(* 1.50 (DDcos 27)) ; End hatch over bolt
		y2	(* 1.50 (DDsin 27))
		cx	(* 1.23 (DDcos 27)) ; Bolt (8 total)
		cy	(* 1.23 (DDsin 27))
		r	0.1
		rf	(/ 3.55 4) ; 1/2 size of 10xØ355mm. Pole foundation with pole group, to be lowered into a drilled hole in the ground
	)
	; Schematic
	(drawLine layer_Zero _origo_ (posTR x y))			; ./ line from origo
	(drawCircleAtPos layer_Zero r (posTR cx cy) _noWipeout_)	; ( ) bolt
	(drawLine layer_Zero (posTR x1 y1) (posTR x2 y2))	; (/) hatch over bolt
	(mirrorAboutXaxis _keep_)
	(mirrorAboutYaxis _keep_)
	(mirrorAboutDiagonal _keep_) ; Now we have 8 bolts
	(drawCircle layer_Zero rf _noWipeout_) ; no wipeout - Th Æ355 symbol in 10x scale for 1:250 drawings
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.80 blockName) ; S = 1:1000 is 80% of schematic symbol => 4x real size
)



(defun BETONG-KL-FUNDAMENT-BORET-555 ( / blockName description x y x1 y1 x2 y2 cx cy r rf )
	; 2020-04-08 Bane NOR 1:250 skala 2D layer/symbol: JBTEH_Komponenter / EH_Fund1 (which is 5x real size)
	; Ref EH.800091-000_001 "Høyspenning, kontaktledningsanlegg; Prefabrikkert betongsøylefundament; Ø555 for kontaktledningsmast"
	; Symbol: A circle with four hatched smaller circles, and a cross in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center symbol.
	; Foundation for OCS masts.
	(setq
		blockName	(strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-BORET-" _uOE_ "555-191x374xM36")
		description	(strcat "BETONG KL- FUNDAMENT BORET " _uOE_ "555 191x374xM36")
		;r1 (/ 0.555 2) ; Ytre diameters radius
		;r3 (/ 0.036 2) ; M36 gjengejern
		;x (/ 0.191 2) ; Boltegruppe 190.68 * 374.22
		;y (/ 0.374 2)
		;f1 0.4762 ; end cross
		;f2 0.8570 ; start of line over bolt
		;f3 1.1587 ; end of line over bolt
		x	(* 1.0 (DDcos 63)) ; Line from origo
		y	(* 1.0 (DDsin 63))
		x1	(* 1.8 (DDcos 63)) ; Start hatch over bolt
		y1	(* 1.8 (DDsin 63))
		x2	(* 2.4 (DDcos 63)) ; End hatch over bolt
		y2	(* 2.4 (DDsin 63))
		cx	(* 2.1 (DDcos 63)) ; center of circles at four corners of a virtual box
		cy	(* 2.1 (DDsin 63))
		r	0.1
		rf	(/ 5.55 4) ; 1/2 size of 10xØ355mm. Pole foundation with pole group, to be lowered into a drilled hole in the ground
	)
	(drawCircleAtPos layer_Zero r (posTR cx cy) _noWipeout_)	; ( )
	(drawLine layer_Zero (posTR x1 y1) (posTR x2 y2))	; (/)
	(mirrorAboutXaxis _keep_)
	(mirrorAboutYaxis _keep_)
	(drawStAndrewCross layer_Zero x y)					; X
	(drawCircle layer_Zero rf _noWipeout_) ; no wipeout
	
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	(createGeoBlockInAllPaperScalesFromBlock blockName 0.80 blockName) ; S = 1:1000 is 80% of schematic symbol => 4x real size
)



(defun FJELLBOLTER-FOR-HENGEMAST ( / blockName description x y cx cy r )
	;Symbol: Just four hatched smaller circles, and a cross in the middle. 
	; Plays the role of BN foundation EH_Fund_Spesial
	;
	; DEPRECATED - bolt group real size:
	;	nutRadius 0.0277 ; M24
	;	boltRadius 0.012 ; M24
	;	x (/ 0.350 2) ; 350x350 bolteplassering
	;	y (/ 0.350 2)
	;(command 
	;	"._POLYGON" 6 (list x y) "_INSCRIBED" nutRadius
	;	"._CIRCLE" (list x y) boltRadius
	;	"._MIRROR" "_ALL" "" (list 0 0) (list 0 1) "_NO"
	;	"._MIRROR" "_ALL" "" (list 0 0) (list 1 0) "_NO"
	;)
	(setq
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-FJELLBOLTER-FOR-HENGEMAST-350x350xM24"
		description	"BOLTEGRUPPE FOR KL- HENGEMAST FOR BRU OG TUNNEL 350x350xM24"
		x	(* 1.4 (* (DDcos 63))) ; 1.4 is one diagonal
		y	(* 1.4 (* (DDsin 63)))
		cx	(* 2.1 (* (DDcos 63))) ; center of circles at four corners of a virtual box
		cy	(* 2.1 (* (DDsin 63)))
		r	0.1
	)
	; Schematic symbol
	(drawCircleAtPos layer_Zero r (posTR cx cy) _noWipeout_)
	(mirrorAboutXaxis _keep_)
	(mirrorAboutYaxis _keep_)
	(drawStAndrewCross layer_Zero x y)
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.80 blockName) ; S = 1:1000 is 80% of schematic symbol => 4x real size
)



(defun METALL-KL-KONSOLL-VEGGFESTE ( / blockName x y x1 y1 x2 y2 cx cy r wx wy bx by sx sy )
	; 2020-04-08 Bane NOR 2D layer/symbol: JBTEH_Komponenter / EH_Fund_Konsoll
	; Ref EH.xxxxx 
	; Foundation for OCS masts with standard 191x374 bolt group.
	; Symbol: A wall mount bracket with a bolt group, and an "X" in the middle.
	; Insertion direction is "both" in a track.
	; Insertion point is center side against mounting surface (concrete wall or other).
	;
	; 01-02-------.------03-04  
	; 05-06-07--------08-09-10
	;    |  |          |  |
	;    |  |  (\) (/) |  |
	;    |  |     X    |  |
	;    |  |  (/) (\) |  |
	;    |  |          |  |
	;    11-12--------13-14
	;
	(setq
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-KONSOLL-VEGGFESTE-191x374xM36"
		description	"METALL KL- KONSOLL VEGGFESTE 191x374xM36"
		; TRV symbol bolt group:
		x	(* 1.0 (* (DDcos 63))) ; diagonal distance
		y	(* 1.0 (* (DDsin 63)))
		x1	(* 1.8 (* (DDcos 63))) ; diagonal distance
		y1	(* 1.8 (* (DDsin 63)))
		x2	(* 2.4 (* (DDcos 63))) ; diagonal distance
		y2	(* 2.4 (* (DDsin 63)))
		cx	(* 2.1 (* (DDcos 63))) ; center of circles at four corners of a virtual box
		cy	(* 2.1 (* (DDsin 63)))
		r	0.1
		; TRV symbol wallmount cradle, wall-mount plate + 2xsidewalls + baseplate
		wx 0.80  ; wall plate width againts conrete support 01-04
		wy 0.039 ; thickness								01-05
		bx 0.50  ; base plate width against wall plate		06-09
		by 0.55  ; base place extends from wall plate		06-11
		sx 0.025 ; side plates' thickness					06-07
		sy 0.55  ; side plates extends from wall plate		07-12
	)
	; Schematic symbol
	; Wallmount console graphics:
	(setLayer layer_Zero)
	(command 
		; wall plate:
		"._RECTANGLE" (list (/ wx -2) 0) (list (/ wx 2) (- wy))
		; base plate:
		"._RECTANGLE" (list (/ bx -2) (- wy)) (list (/ bx 2) (- (+ wy by)))
		; side plates:
		"._RECTANGLE" (list (- (/ bx -2) sx) (- wy)) (list (+ (/ bx 2) sx) (- (+ wy sy)))
	)
	(createSchematicBlockFromCurrentGraphics "TMP") ; stash graphics

	; Bolt group:
	(drawCircleAtPos layer_Zero r (posTL cx cy) _noWipeout_)
	(drawCircleAtPos layer_Zero r (posTR cx cy) _noWipeout_)
	(drawCircleAtPos layer_Zero r (posBL cx cy) _noWipeout_)
	(drawCircleAtPos layer_Zero r (posBR cx cy) _noWipeout_)
	(drawLine layer_Zero (posTL x1 y1) (posTL x2 y2))
	(drawLine layer_Zero (posTR x1 y1) (posTR x2 y2))
	(drawLine layer_Zero (posBL x1 y1) (posBL x2 y2))
	(drawLine layer_Zero (posBR x1 y1) (posBR x2 y2))
	(drawStAndrewCross layer_Zero x y) ; inner cross
	(moveDown (* _five_ (* _half_(+ wy by))))

	; Add back the wallmount bracket, in 5x size
	(addScaledGraphicsFromBlock "TMP" _five_)

	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)

	(createSchematicBlockFromCurrentGraphics blockName)
	(eraseBlock "TMP")
	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.80 blockName) ; S = 1:1000 is 80% of schematic symbol => 1:250 is real size
)



(defun SPESIAL-KL-FUNDAMENT ( / blockName description x y r cx cy )
	; 2020-04-08 Bane NOR 2D layer/symbol: JBTEH_Komponenter / EH_Fund_Spesial (which is 5x real size)
	; Ref TRV / EH symboler
	; Foundation for OCS masts with standard 191x374 bolt group, no further definition of the foundation's construction or layout.
	; Insertion direction is "both" in a track.
	;
	; S symbol: A 2x2 bolt group 191x374 (unhatched), and an "X" in the middle.
	;
	; ()     () ; Schematic
	;   
	;    \ /
	;     . 
	;    / \    ; 1.4 diagonal, 63 degrees
	;   
	; ()     ()
	;
	(setq
		blockName	"NO-BN-2D-JBTKL-FUNDAMENT-SPESIAL-191x374xM36"
		description	"SPESIAL KL- FUNDAMENT 191x374xM36"
		x	(* 1.4 (DDcos 63)) ; 1.4 is one diagonal
		y	(* 1.4 (DDsin 63))
		r	0.1
		cx	(* 2.1 (DDcos 63)) ; center of circles at four corners of a virtual box
		cy	(* 2.1 (DDsin 63))
	)
	; Schematic symbol
	(drawCircleAtPos layer_Zero r (posTR cx cy) _noWipeout_)
	(mirrorAboutXaxis _keep_)
	(mirrorAboutYaxis _keep_)
	(drawStAndrewCross layer_Zero x y)
	(drawProxySymbol layer_FoundationLocator "KL")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.80 blockName) ; S = 1:1000 is 80% of schematic symbol => 4x real size
)



;==========================
; Private draw...X...() functions
;==========================
(defun drawBolt ( / )  
	(command
		"._LINE" "0,0" "0.00519615,0" ""
		"._LINE" "0.00347980,0.005" "@0,-0.00407180" ""
		"._PLINE" 
			"0,0" 
			"_ARC"
			"_CE" "0,0.00692818" "0.00347980,0.00092820"
			"_CE" "0.00521209,0.00208057" "0.00694390,0.00092820"
			"_LINE"
			"@0,0.00407180" 
			""
		"._LINE"
			"0,0.005"
			"@0.00741570,0"
			"@0,0.0012"
			"@-0.00741570,0"
			""
		"._LINE" "0.00401569,0.008" "@-0.004,0" ""
		"._LINE" "0.00401569,0.005" "@0,0.0395" ""
		"._LINE" "0.00314964,0.045" "@0,-0.037" ""
		"._LINE" 
			"0,0.04500000"
			"0.00314964,0.04500000"
			"0.00401567,0.04450039"
			"0,0.04450039"
			""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
	)
)
