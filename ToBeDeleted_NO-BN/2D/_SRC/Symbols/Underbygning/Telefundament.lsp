;
; Telefundament.lsp
;
(defun C:TELEFUNDAMENT ()
	(newLayer "JBTUB__FE_FUN_LOKALISERING" 62 "Fundamentlokalisering")
	(BETONG-TELE-OG-FORDELINGSSKAP)
	(BETONG-ANVISERSOEYLE)
	(BETONG-MONITOR)
	(BETONG-VALIDATOR)
)

(defun BETONG-TELE-OG-FORDELINGSSKAP (/ blockName s1 s2 h1 h2 h3 len)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-TELE-OG-FORDELINGSSKAP"
		s1 (/ 0.9 2)
		s2 (/ 0.8 2)
		h1 (/ 0.4 2)
		h2 (/ 0.3 2)
		h3 (- h1 (- h1 (* h1 (/ 3.0 4.0))))
		len (/ (- s2 h2) 5)
	)
	(command
		"._RECTANGLE" (list (- s1) (- s2)) (list s1 s2)
		"._RECTANGLE" (list (- h1) (- h2)) (list h1 h2)
		"._LINE"
			(list h3 (- h2)) (strcat "@0," (rtos (- len)))
			""
		"._LINE"
			(list h3 (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len)))
			""
		"._LINE"
			(list h3 (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len)))
			""
		"._LINE"
			(list (- h3) (- h2)) (strcat "@0," (rtos (- len)))
			""
		"._LINE"
			(list (- h3) (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len)))
			""
		"._LINE"
			(list (- h3) (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len)))
			""
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG TELE OG FORDELINGSSKAP- FUNDAMENT" "0,-0.55" 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "T" 2.5)
	(newBlock blockName)
	blockName
)



(defun BETONG-ANVISERSOEYLE (/ blockName s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-ANVISERSOEYLE"
		s1 (/ 1.5 2)
		radius1 0.3
		radius_skrue 0.0120
		radius_roer 0.025
		dist1 (/ radius1 30)
		skrue_xy (/ s1 5)
		roer_y (- (- radius1) (/ radius1 3))
	)
	(command
		"._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
		"._CIRCLE" "0,0" radius1
		"._CIRCLE" (list skrue_xy skrue_xy) radius_skrue
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._MIRROR" "L" "" "0,0" "1,0" "NO"
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._CIRCLE" "0,0" radius_roer
		"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
		"._MIRROR" "L" "" "0,0" "0,1" "NO" ;neste fire LINE er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
		"._LINE"
			(list (- radius_roer) roer_y) 
			(list radius_roer roer_y) 
			(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1))))))  ;y=R*sin(arccos(x/R))
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE"
			(list (+ radius_roer dist1) roer_y) 
			(list (+ (* 3 radius_roer) dist1) roer_y) 
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE"
			(list (+ radius_roer dist1) roer_y) 
			(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE"
			(list (+ (* 3 radius_roer) dist1) roer_y)
			(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1)))))) 
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG ANVISERSØYLE- FUNDAMENT 1500x1500x1000-Ø600"
		(list 0 (- (- s1) radius1)) 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "T" 2.5)
	(newBlock blockName)
	blockName
)



(defun BETONG-MONITOR (/ blockName s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-MONITOR"
		s1 (/ 1.0 2)
		radius1 0.3
		radius_skrue 0.0120
		radius_roer 0.025
		dist1 (/ radius1 30)
		skrue_xy (/ s1 5)
		roer_y (- (- radius1) (/ radius1 3))
	)
	(command 
		"._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
		"._CIRCLE" "0,0" radius1
		"._CIRCLE" (list skrue_xy skrue_xy) radius_skrue
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._MIRROR" "L" "" "0,0" "1,0" "NO"
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._CIRCLE" "0,0" radius_roer
		"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
		"._MIRROR" "L" "" "0,0" "0,1" "NO" ;neste fire LINES er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
		"._LINE"
			(list (- radius_roer) roer_y)
			(list radius_roer roer_y)
			(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1)))))) ;y=Rsin(arccos(x/R))
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE" (list (+ radius_roer dist1) roer_y) (list (+ (* 3 radius_roer) dist1) roer_y) ""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE"
			(list (+ radius_roer dist1) roer_y)
			(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE"
			(list (+ (* 3 radius_roer) dist1) roer_y)
			(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1))))))
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG TELEMONITOR- FUNDAMENT 1000x1000x1000-Ø600"
		(list 0 (- (- s1) radius1)) 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "T" 2.5)
	(newBlock blockName)
	blockName
)



(defun BETONG-VALIDATOR (/ blockName s1 radius_skrue radius_roer lengde_roer dist1 skrue_x skrue_y)
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-VALIDATOR"
		s1 (/ 0.36 2.0)
		radius_skrue 0.008
		radius_roer 0.025
		lengde_roer 0.125
		dist1 (/ s1 (* s1 100.0))
		skrue_x (+ (* 3.0 radius_roer) dist1 (/ (* s1 2.0) 30.0)) 
		skrue_y (* s1 (/ 0.0854 s1))
	)
	(command "._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
		"._CIRCLE" (list skrue_x skrue_y) radius_skrue
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._MIRROR" "L" "" "0,0" "1,0" "NO"
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._CIRCLE" "0,0" radius_roer
		"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
		"._MIRROR" "L" "" "0,0" "0,1" "NO"  
		"._RECTANGLE" (list radius_roer (- s1)) (list (- radius_roer) (- (- s1) lengde_roer))
		"._RECTANGLE" (list (+ radius_roer dist1) (- s1)) (list (+ (* 3 radius_roer) dist1) (- (- s1) lengde_roer))
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "BETONG VALIDATOR- FUNDAMENT 300x360x500"
		(list 0 (* 2 (- s1))) 0.18 3 0 "iso" "TC")
	(drawFoundationLocator "T" 2.5)
	(newBlock blockName)
	blockName
)
