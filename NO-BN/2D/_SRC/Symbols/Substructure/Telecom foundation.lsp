;=========================================================================================================================
;
; Telecom foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Telecom equipment foundations

(defun C:TELECOM-FOUNDATION ( / )
	(BETONG-TELE-OG-FORDELINGSSKAP)
	(BETONG-ANVISERSOEYLE)
	(BETONG-MONITOR)
	(BETONG-VALIDATOR)
)



(defun BETONG-TELE-OG-FORDELINGSSKAP ( / blockName s1 s2 h1 h2 h3 len )
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-TELE-OG-FORDELINGSSKAP"
		s1 (/ 0.9 2)
		s2 (/ 0.8 2)
		h1 (/ 0.4 2)
		h2 (/ 0.3 2)
		h3 (- h1 (- h1 (* h1 (/ 3.0 4.0))))
		len (/ (- s2 h2) 5)
	)
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "BETONG TELE OG FORDELINGSSKAP- FUNDAMENT" "0,-0.55" _descriptionTextHeight_ 3 0 "iso" "_TC")
	(drawProxySymbol layer_FoundationLocator "T")
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(foreach paperScale paperScaleList
		(addScaledGraphicsFromBlock blockName (/ (* (atof paperScale)) 1000.0)) 	; Retrieve, explode and scale S symbol
		; Add metric graphics
		(command
			"._RECTANGLE" (list (- s1) (- s2)) (list s1 s2)
			"._RECTANGLE" (list (- h1) (- h2)) (list h1 h2)
			"._LINE" (list h3 (- h2)) (strcat "@0," (rtos (- len))) ""
			"._LINE" (list h3 (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) ""
			"._LINE" (list h3 (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) ""
			"._LINE" (list (- h3) (- h2)) (strcat "@0," (rtos (- len))) ""
			"._LINE" (list (- h3) (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) ""
			"._LINE" (list (- h3) (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) ""
		)
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



(defun BETONG-ANVISERSOEYLE ( / blockName s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y )
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "BETONG ANVISERSØYLE- FUNDAMENT 1500x1500x1000-Ø600" (list 0 (- (- s1) radius1)) _descriptionTextHeight_ 3 0 "iso" "_TC")
	(drawProxySymbol layer_FoundationLocator "T")
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(foreach paperScale paperScaleList
		(addScaledGraphicsFromBlock blockName (/ (* (atof paperScale)) 1000.0)) 	; Retrieve, explode and scale S symbol
		; Add metric graphics
		(command
			"._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
			"._CIRCLE" "0,0" radius1
			"._CIRCLE" (list skrue_xy skrue_xy) radius_skrue
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "1,0" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._CIRCLE" "0,0" radius_roer
			"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO" ;neste fire LINE er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
			"._LINE"
				(list (- radius_roer) roer_y) 
				(list radius_roer roer_y) 
				(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1))))))  ;y=R*sin(arccos(x/R))
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE"
				(list (+ radius_roer dist1) roer_y) 
				(list (+ (* 3 radius_roer) dist1) roer_y) 
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE"
				(list (+ radius_roer dist1) roer_y) 
				(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE"
				(list (+ (* 3 radius_roer) dist1) roer_y)
				(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1)))))) 
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
		)
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



(defun BETONG-MONITOR ( / blockName s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y )
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "BETONG TELEMONITOR- FUNDAMENT 1000x1000x1000-Ø600" (list 0 (- (- s1) radius1)) _descriptionTextHeight_ 3 0 "iso" "_TC")
	(drawProxySymbol layer_FoundationLocator "T")
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(foreach paperScale paperScaleList
		(addScaledGraphicsFromBlock blockName (/ (* (atof paperScale)) 1000.0)) 	; Retrieve, explode and scale S symbol
		; Add metric graphics
		(command 
			"._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
			"._CIRCLE" "0,0" radius1
			"._CIRCLE" (list skrue_xy skrue_xy) radius_skrue
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "1,0" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._CIRCLE" "0,0" radius_roer
			"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO" ;neste fire LINES er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
			"._LINE"
				(list (- radius_roer) roer_y)
				(list radius_roer roer_y)
				(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1)))))) ;y=Rsin(arccos(x/R))
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE" (list (+ radius_roer dist1) roer_y) (list (+ (* 3 radius_roer) dist1) roer_y) ""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE"
				(list (+ radius_roer dist1) roer_y)
				(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._LINE"
				(list (+ (* 3 radius_roer) dist1) roer_y)
				(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1))))))
				""
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
		)
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



(defun BETONG-VALIDATOR ( / blockName s1 radius_skrue radius_roer lengde_roer dist1 skrue_x skrue_y )
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
	; Schematic symbol (just a circle with proxy symbol text)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "BETONG VALIDATOR- FUNDAMENT 300x360x500" (list 0 (* 2 (- s1))) _descriptionTextHeight_ 3 0 "iso" "_TC")
	(drawProxySymbol layer_FoundationLocator "T")
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(foreach paperScale paperScaleList
		(addScaledGraphicsFromBlock blockName (/ (* (atof paperScale)) 1000.0)) 	; Retrieve, explode and scale S symbol
		; Add metric graphics
		(command "._RECTANGLE" (list (- s1) (- s1)) (list s1 s1)
			"._CIRCLE" (list skrue_x skrue_y) radius_skrue
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "1,0" "_NO"
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
			"._CIRCLE" "0,0" radius_roer
			"._CIRCLE" (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"  
			"._RECTANGLE" (list radius_roer (- s1)) (list (- radius_roer) (- (- s1) lengde_roer))
			"._RECTANGLE" (list (+ radius_roer dist1) (- s1)) (list (+ (* 3 radius_roer) dist1) (- (- s1) lengde_roer))
			"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
		)
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)
