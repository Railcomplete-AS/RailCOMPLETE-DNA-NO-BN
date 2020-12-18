;
; Mastefundament.lsp
;
(defun C:MASTEFUNDAMENT ()
	(newLayer "JBTUB__FE_FUN_TYPENAVN" 62 "Fundamenttype")
	(newLayer "JBTUB__FE_FUN_LOKALISERING" 62 "Fundamentlokalisering")
	(BETONG-HS-FUNDAMENT-FRI-LINJE)
	(BETONG-HS-FUNDAMENT-PLATTFORM)
	(BETONG-HS-FUNDAMENT-Ø680-300 "1600")
	(BETONG-HS-FUNDAMENT-Ø680-300 "1800")
	(BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600) ; OK, kanskje ikke en mast... men samme slags fundament, nesten, som gammelt signalfundament
	(BETONG-HS-FUNDAMENT-BORET "4500") ; Ø555 Med signalfagets boltegruppe 164x164 mm
	(BETONG-DS-FUNDAMENT-KONISK)
	(BETONG-DS-FUNDAMENT-RETTSIDET)
	(BETONG-KL-FUNDAMENT-BJELKEMAST "SENTRERT") ; Ny standard, med boltegruppe 374x191xM36 - Strekk - Sentrert - Trykk (ledning drar mast mot spor osv)
	(BETONG-KL-FUNDAMENT-B3)
	(BETONG-KL-FUNDAMENT-H3 "6000-9500") ; Mastehøyde fra-til i mmm
	(BETONG-KL-FUNDAMENT-H3 "10000-12000")
	(BETONG-KL-FUNDAMENT-H3 "12500-13000")
	(BETONG-KL-FUNDAMENT-BORET "2500") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "3000") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "3500") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "4000") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "4500") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "5000") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "5500") ; Ø555 Med KL-fagets boltegruppe
	(BETONG-KL-FUNDAMENT-BORET "6000") ; Ø555 Med KL-fagets boltegruppe
	(FJELLBOLTER-FOR-HENGEMAST)
)



(defun BETONG-HS-FUNDAMENT-FRI-LINJE (/ blockName s1 s2 dim1 dim2 radius_gjengejern)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-FRI-LINJE-300-M24"
		s1 2.0
		s2 0.6
		dim1 0.15
		dim2 0.05
		radius_gjengejern 0.0120
	)
	(command
		"._RECTANG"
			(list (- (/ s1 2)) (- (/ s1 2)))
			(list (/ s1 2) (/ s1 2))
		"._RECTANG"
			(list (- (/ s2 2)) (- (/ s2 2)))
			(list (/ s2 2) (/ s2 2))
		"._RECTANG"
			(list (/ dim1 -2) (/ s2 -2))
			(list (/ dim1 2) (+ (/ s2 -2) dim2))
		"._LINE"
			(list (- (/ s1 2)) (- (/ s1 2)))
			(list (- (/ s2 2)) (- (/ s2 2)))
			"" 
		"._LINE"
			(list (/ s1 2) (/ s1 2))
			(list (/ s2 2) (/ s2 2))
			""
		"._LINE"
			(list (/ s1 -2) (/ s1 2))
			(list (/ s2 -2) (/ s2 2))
			""
		"._LINE"
			(list (/ s1 2) (/ s1 -2))
			(list (/ s2 2) (/ s2 -2))
			""
		"._CIRCLE"
			(list dim1 dim1) radius_gjengejern
		"._CIRCLE"
			(list (- dim1) dim1) radius_gjengejern
		"._CIRCLE"
			(list dim1 (- dim1)) radius_gjengejern
		"._CIRCLE"
			(list (- dim1) (- dim1)) radius_gjengejern
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG HOVEDSIGNAL- FUNDAMENT FRI LINJE 300x300xM24 / 2000(650)x2000(650)x2000" "0,-1.1275" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-HS-FUNDAMENT-PLATTFORM (/ blockName s1 s2 dim1 dim2 radius_gjengejern radius_roer roer_y)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-PLATTFORM-300-M24"
		s1 2.0
		s2 0.6
		dim1 0.15
		dim2 0.05
		radius_gjengejern 0.0120
		radius_roer 0.0420
		roer_y (+ (/ s2 2) radius_roer) 
	)
	(command
		"._RECTANG"
			(list (- (/ s1 2)) (- (/ s1 2)))
			(list (/ s1 2) (/ s1 2)) ;stort kvadrat, ytre
		"._RECTANG"
			(list (- (/ s2 2)) (- (/ s2 2)))
			(list (/ s2 2) (/ s2 2)) ;lite kvadrat, indre
		"._LINE"			;skrålinjer
			(list (- (/ s1 2)) (- (/ s1 2)))
			(list (- (/ s2 2)) (- (/ s2 2)))
			"" 
		"._LINE"
			(list (/ s1 2) (/ s1 2))
			(list (/ s2 2) (/ s2 2))
			""
		"._LINE"
			(list (/ s1 -2) (/ s1 2))
			(list (/ s2 -2) (/ s2 2))
			""
		"._LINE"
			(list (/ s1 2) (/ s1 -2))
			(list (/ s2 2) (/ s2 -2))
			""
		"._CIRCLE"
			(list dim1 dim1) radius_gjengejern
		"._CIRCLE"
			(list (- dim1) dim1) radius_gjengejern
		"._CIRCLE"
			(list dim1 (- dim1)) radius_gjengejern
		"._CIRCLE"
			(list (- dim1) (- dim1)) radius_gjengejern
		"._CIRCLE"
			(list 0 0) radius_gjengejern
		"._CIRCLE"
			(list (- radius_roer) (- roer_y)) radius_roer
		"._CIRCLE"
			(list radius_roer (- roer_y)) radius_roer
		"._CIRCLE"
			(list 0 roer_y) radius_roer
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG HOVEDSIGNAL- FUNDAMENT PLATTFORM 300x300xM24 / 2000(650)x2000(650)x1300" "0,-1.1275" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-HS-FUNDAMENT-Ø680-300 (height / blockName r1 r2 d radius_gjengejern)
	(setq 
		blockName (strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-Ø680-300-M24-" height)
		r1 (/ 0.680 2) ; Ytre radius, stående betongrør
		r2 (/ 0.300 2) ; Indre radius, stående betongrør
		d (/ 0.300 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		radius_gjengejern (/ 0.024 2) ; M24 gjengejern (stikker 100 opp)
	)
	(command
		"._CIRCLE" (list 0 0) r1
		"._CIRCLE" (list 0 0) r2
		"._CIRCLE" (list d d) radius_gjengejern
		"._CIRCLE" (list d (- d)) radius_gjengejern
		"._CIRCLE" (list (- d) d) radius_gjengejern
		"._CIRCLE" (list (- d) (- d)) radius_gjengejern
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText (strcat "BETONG HOVEDSIGNAL- FUNDAMENT Ø680/300x300xM24 h=" height)
		"0,-1.1275" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-VEIBOMDRIVMASKIN-FUNDAMENT-Ø680-210-1600 (/ blockName r1 r2 d radius_gjengejern)
	(setq 
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-VEIBOMDRIVMASKIN-Ø680-210-M20-1600"
		r1 (/ 0.680 2) ; Ytre radius, stående betongrør
		r2 (/ 0.210 2) ; Indre radius, stående betongrør
		d (/ 0.210 2) ; 300x300 bolteplassering for de ytre hullene i S.023166 signalmastefot
		radius_gjengejern (/ 0.020 2) ; M20 gjengejern (stikker 100 opp)
	)
	(command
		"._CIRCLE" (list 0 0) r1
		"._CIRCLE" (list 0 0) r2
		"._CIRCLE" (list d d) radius_gjengejern
		"._CIRCLE" (list d (- d)) radius_gjengejern
		"._CIRCLE" (list (- d) d) radius_gjengejern
		"._CIRCLE" (list (- d) (- d)) radius_gjengejern
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG VEIBOMDRIVMASKIN- FUNDAMENT Ø680/210x210xM20 h=1600" "0,-1.1275" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-HS-FUNDAMENT-BORET ( height / blockName rad R1 R2 R_circ x ang d_ang
	)
	(setq
		blockName (strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-HS-BORET-Ø555-164x164-M32-" height)
		rad (/ 0.032 2) ; M32 gjengejern for de indre Ø45 hullene i signalets mastefot-plate
		R1 0.15
		R2 0.4
		R_circ 0.2
		x (/ 0.164 2) ; Bolteplassering 164x164 mm for signalers mastefot (de indre hullene)
		ang 5
		d_ang 45
	)
	(command
		"._CIRCLE" (list x x) rad
		"._ARC" "C" "0,0"
			(polar (list 0 0) (D->R ang) R2)
			"Angle" d_ang
		"._ARC" "C" "0,0"
			(polar (list 0 0) (D->R (/ d_ang -2)) R1)
			"Angle" d_ang
		"._ARRAY" "All" ""
			"Polar" "0,0" 4 360 "Y"
		"._CIRCLE" "0,0" R_circ
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText (strcat "BETONG HOVEDSIGNAL- FUNDAMENT BORET Ø555/164x164xM32 h=" height)
		"0,-0.5500" 0.18 2 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-DS-FUNDAMENT-KONISK (/ blockName s1 s2 dim1 dim2)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-DS-KONISK"
	)
	(drawSkrue)
	(command	
		"._MIRROR" "ALL" ""
			(list -1 (/ (+ 0.045 0.0380) 2))
			(list 1 (/ (+ 0.045 0.0380) 2))
			"Y"
		"._ARRAY" "All" ""
			"Polar" "0,0" 3 360 
			"Y"
		"._CIRCLE" "0,0" 0.0413
		"._CIRCLE" "0,0" 0.0445
	)
	(setq 
		s1 (/ 0.6 2)
		s2 (/ 0.4 2)
		dim1 0.12
		dim2 0.10
	)
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
		"._ARRAY" "L" ""
			"Polar" "0,0" 4 360 "Y"
		"._RECTANG"
			(list (/ dim1 -2) -0.24)
			(list (/ dim1 2) (+ -0.24 dim2))
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG DVERGSIGNAL- FUNDAMENT KONISK 600(400)x600(400)x500" "0,-0.5982" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-DS-FUNDAMENT-RETTSIDET (/ blockName s1 s2 dim1 dim2)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-DS-RETTSIDET"
	)
	(drawSkrue)
	(command
		"._MIRROR" "ALL" ""
			(list -1 (/ (+ 0.045 0.0380) 2))
			(list 1 (/ (+ 0.045 0.0380) 2))
			"Y"
		"._ARRAY" "All" ""
			"Polar" "0,0" 3 360 
			"Y"
		"._CIRCLE" 
			"0,0" 0.0413
		"._CIRCLE"
			"0,0" 0.0445
	);miderste rør med skruer:
	(setq
		s1 (/ 0.6 2)
		s2 (/ 0.4 2)
		dim1 0.12
		dim2 0.10
	)
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
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG DVERGSIGNAL- FUNDAMENT RETTSIDET 600(400)x600(400)x500"
		"0,-0.5982" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "S" 2.5)
	(newBlock blockName)
)



(defun BETONG-KL-FUNDAMENT-BJELKEMAST (plassering / blockName saale_bredde soeyle_dybde soeyle_bredde radius_gjengestag radius_kabelhulrom x y)
	(setq
		; TODO 'plassering' skal benyttes til å spesifisere at senter i betongklossen er nærmest spor, sentrert på masten eller lenger fra spor
		blockName (strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-KL-BJELKEMAST-" plassering)
		saale_bredde 1.600 ; Kvadratisk bunnplate
		soeyle_bredde 0.700
		soeyle_dybde 0.700
		radius_gjengestag (/ 0.036 2) ; M36 gjengestag
		radius_kabelhulrom (/ 0.050 2)
		x (/ 0.19068 2) ; Boltegruppe 190.68 * 374.22
		y (/ 0.37422 2)
	)
	(command
		"._RECTANGLE"
			(list (/ saale_bredde -2) (/ saale_bredde -2))
			(list (/ saale_bredde 2) (/ saale_bredde 2))
		"._RECTANGLE"
			(list (/ soeyle_bredde -2) (/ soeyle_dybde -2))
			(list (/ soeyle_bredde 2) (/ soeyle_dybde 2))
		"._CIRCLE" "0,0" radius_kabelhulrom
		"._CIRCLE" (list x y) radius_gjengestag
		"._CIRCLE" (list (- x) y) radius_gjengestag
		"._CIRCLE" (list x (- y)) radius_gjengestag
		"._CIRCLE" (list (- x) (- y)) radius_gjengestag
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText (strcat "BETONG KL- FUNDAMENT BJELKEMAST 191x374xM36" plassering)
		(list 0 (- (+ (/ saale_bredde 2) x))) 0.18 2.0 0 "iso" "TC")
	(drawFoundationLocator "KL" 1.8)
	(newBlock blockName)
)



(defun BETONG-KL-FUNDAMENT-B3 (/ blockName saale_bredde soeyle_bredde soeyle_dybde radius_gjengestag radius_kabelhulrom x y)
	(setq
		blockName "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-KL-B3"
		saale_bredde 2.300 ; Kvadratisk bunnplate
		soeyle_bredde 1.100 ; På tvers av sporet
		soeyle_dybde 0.500 ; Langs med sporet
		radius_gjengestag (/ 0.036 2) ; M36 gjengestag
		radius_kabelhulrom (/ 0.050 2)
		x (/ 0.240 2) ; Boltegruppe 240x800
		y (/ 0.800 2)
	)
	(command
		"._RECTANGLE"
			(list (/ saale_bredde -2) (/ saale_bredde -2))
			(list (/ saale_bredde 2) (/ saale_bredde 2))
			
		"._RECTANGLE"
			(list (/ soeyle_dybde -2) (/ soeyle_bredde -2))
			(list (/ soeyle_dybde 2) (/ soeyle_bredde 2))
		"._CIRCLE" "0,0" radius_kabelhulrom
		"._CIRCLE" (list x y) radius_gjengestag
		"._CIRCLE" (list (- x) y) radius_gjengestag
		"._CIRCLE" (list x (- y)) radius_gjengestag
		"._CIRCLE" (list (- x) (- y)) radius_gjengestag
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG KL- FUNDAMENT B3-MAST 240x800xM36"
		(list 0 (- (+ (/ saale_bredde 2) x))) 0.18 2.0 0 "iso" "TC")
	(drawFoundationLocator "KL" 1.8)
	(newBlock blockName)  
)



; Se tegning EH.707388 på Elkraftportalen, H3 fundamenter.
; Lage ekte H3-fundamenter, det finnes VELDIG mange i regelverket.
; Inntil lages det kun en "standard" H3:
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
(defun BETONG-KL-FUNDAMENT-H3 ( height
	/	blockName 
		saale_y_near saale_y_far saale_x 
		soeyle_x soyle_y 
		gjengestag_x gjengestag_y
		radius_gjengestag 
		radius_kabelhulrom
	)
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-KL-H3-" height)
		saale_y_near 1.000 ; på tvers av sporet i retning mot sporet ("bredde")
		saale_y_far 0.800 ; på tvers av sporet i retning mot sporet ("bredde")
		saale_x 1.600 ; langsmed sporet ("dybde")
		;saale_height 0.500 ; fra underkant fundament til overkant såle, er irrelevant for 2D-symbolet
		radius_gjengestag (/ 0.036 2) ; M36 gjengestag, stikker 180 mm opp over SOK=overkant søyle 
		radius_kabelhulrom (/ 0.050 2)
	)

	(cond 
		((= height "6000-9500")
			(setq 
				soeyle_x 1.000	; Finnes i 1.000 x 1.000 (H3 mast 6,0 til 9,5m høyde) og 1.100 x 1.100 (H3 mast 10,0 til 12,0m høyde)
				soeyle_y 1.000
				;soeyle_height 2.200 - denne varierer mye, er irrelevant for 2D-symbolet
				gjengestag_x (/ 0.700 2) ; Boltegruppe i retning langs sporet ("bredde") (y=halvbredden) 700x700 for Hr 6,0..9,5m, 790x790 for 10,0..12,0m, 830x830 for 12,5-13,0m
				gjengestag_y (/ 0.700 2) ; Boltegruppe i retning på tvers av sporet ("dybde") (x=halvdybden)
			)
		)
		((= height "10000-12000")
			(setq 
				soeyle_x 1.100
				soeyle_y 1.100
				;soeyle_height 2.200 - denne varierer mye, er irrelevant for 2D-symbolet
				gjengestag_x (/ 0.790 2) ; Boltegruppe i retning langs sporet ("bredde") (y=halvbredden) 700x700 for Hr 6,0..9,5m, 790x790 for 10,0..12,0m, 830x830 for 12,5-13,0m
				gjengestag_y (/ 0.790 2) ; Boltegruppe i retning på tvers av sporet ("dybde") (x=halvdybden)
			)
		)
		((= height "12500-13000")
			(setq 
				soeyle_x 1.100
				soeyle_y 1.100
				;soeyle_height 2.200
				gjengestag_x (/ 0.830 2)
				gjengestag_y (/ 0.830 2)
			)
		)
	)
	(command
		"._RECTANGLE"
			(list (/ saale_x -2) (- saale_y_near))
			(list (/ saale_x 2) saale_y_far)
		"._RECTANGLE"
			(list (/ soeyle_x -2) (/ soeyle_y -2))
			(list (/ soeyle_x 2) (/ soeyle_y 2))
		"._CIRCLE" "0,0" radius_kabelhulrom
		"._CIRCLE" (list gjengestag_x gjengestag_y) radius_gjengestag
		"._CIRCLE" (list (- gjengestag_x) gjengestag_y) radius_gjengestag
		"._CIRCLE" (list gjengestag_x (- gjengestag_y)) radius_gjengestag
		"._CIRCLE" (list (- gjengestag_x) (- gjengestag_y)) radius_gjengestag
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(cond 
		((= height "6000-9500")
			(addMText "BETONG KL- FUNDAMENT H3-MAST 1800x1600-1000x1000-700x700xM36"
				(list 0 (- (+ (/ (+ saale_y_near saale_y_far) 2) gjengestag_x))) 0.18 2.0 0 "iso" "TC")
		)
		((= height "10000-12000")
			(addMText "BETONG KL- FUNDAMENT H3-MAST 1800x1600-1100x1100-790x790xM36"
				(list 0 (- (+ (/ (+ saale_y_near saale_y_far) 2) gjengestag_x))) 0.18 2.0 0 "iso" "TC")
		)
		((= height "12500-13000")
			(addMText "BETONG KL- FUNDAMENT H3-MAST 1800x1600-1100x1100-830x830xM36"
				(list 0 (- (+ (/ (+ saale_y_near saale_y_far) 2) gjengestag_x))) 0.18 2.0 0 "iso" "TC")
		)
	)
	(drawFoundationLocator "KL" 1.8)
	(newBlock blockName)
)



(defun BETONG-KL-FUNDAMENT-BORET (height / blockName r1 r2 r3 x y)
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-FUNDAMENT-BETONG-KL-BORET-Ø555-" height)
		r1 (/ 0.555 2) ; Ytre diameter
		r2 (/ 0.42 2)
		r3 (/ 0.036 2) ; M36 gjengejern
		x (/ 0.19068 2) ; Boltegruppe 190.68 * 374.22
		y (/ 0.37422 2)
	)
	(command
		"._CIRCLE" "0,0" r1 
		"._CIRCLE" "0,0" r2 
		"._CIRCLE" (list x y) r3
		"._CIRCLE" (list (- x) y) r3
		"._CIRCLE" (list x (- y)) r3
		"._CIRCLE" (list (- x) (- y)) r3
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText (strcat "BETONG KL- FUNDAMENT BORET Ø555/191x374xM36 h=" height)
	    (list 0 (- (+ r1 r2))) 0.18 2.0 0 "iso" "TC")
	(drawFoundationLocator "KL" 1.8)
	(newBlock blockName)  
)



(defun FJELLBOLTER-FOR-HENGEMAST (/ blockName nutRadius boltRadius arrayDist)
	(setq
		blockName "NO-BN-2D-JBTKL-FUNDAMENT-FJELLBOLTER-FOR-HENGEMAST-350x350xM24"
		nutRadius (/ 0.0554 2)
		boltRadius 0.012 ; M24 gjengejern
		arrayDist 0.35 ; 350x350 bolteplassering
	)
	(command
		"._POLYGON" 6 (list 0 0) "Inscribed" (list 0 nutRadius)
		"._CIRCLE" (list 0 0) boltRadius
		"._ARRAY" "ALL" "" "R" "2" "2" arrayDist arrayDist
		"._MOVE" "ALL" "" (list 0 0) (list (- (/ arrayDist 2)) (- (/ arrayDist 2)))
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BOLTEGRUPPE FOR KL- HENGEMAST I TUNNELHVELV 350x350xM24" "0,-1.1275" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "KL" 1.8)
	(newBlock blockName)
	blockName
)



(defun drawSkrue ()  
	(command
		"._LINE" "0,0" "0.00519615,0" ""
		"._LINE" "0.00347980,0.005" "@0,-0.00407180" ""
		"._PLINE" "0,0" "Arc"
			"CE" "0,0.00692818" "0.00347980,0.00092820"
			"CE" "0.00521209,0.00208057" "0.00694390,0.00092820"
			"L" "@0,0.00407180" 
			""
		"._LINE"  "0,0.005"
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
		"._MIRROR" "All" "" "0,0" "0,1" "N"
	)
)
