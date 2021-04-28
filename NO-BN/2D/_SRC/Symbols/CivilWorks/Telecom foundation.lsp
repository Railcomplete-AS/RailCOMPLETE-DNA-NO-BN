;=========================================================================================================================
;
; Telecom foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Telecom equipment foundations

(defun C:TELECOM-FOUNDATION ( / )
	(BETONG-TELE-OG-FORDELINGSSKAP)
	(BETONG-ANVISERSOEYLE)
	(BETONG-MONITOR)
	(BETONG-VALIDATOR)
)



(defun BETONG-TELE-OG-FORDELINGSSKAP ( / blockName description s1 s2 h1 h2 h3 len )
;            s1 wide
;  +-------------------+
;  |                   |
;  |                   |
;  |         h1 wide   |
;  | +---------------+ |
;  | |               | | 
;  | |       .       | | s2, h2 high
;  | | -h3       +h3 | | 
;  | +---------------+ |
;  |   |           |   |
;  |   |           |   |  ; Three dashed lines between the two rectangles: Opening for cables
;  |   |           |   |
;  +-------------------+
;
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-TELE-OG-FORDELINGSSKAP"
		description (strcat "BETONG TELE OG FORDELINGSSKAP- FUNDAMENT")
		s1 (* _half_ 0.9)
		s2 (* _half_ 0.8)
		h1 (* _half_ 0.4)
		h2 (* _half_ 0.3)
		h3 (* _half_ _threeQuarters_ h1) 
		len (* 0.200 (- s2 h2)) ; Dash lengths
	)
	(defun LocalGraphics (/)
		(command
			_RECTANGLE_ (list (- s1) (- s2)) (list s1 s2)
			_RECTANGLE_ (list (- h1) (- h2)) (list h1 h2)
			_LINE_ (list h3 (- h2)) (strcat "@0," (rtos (- len))) _ENTER_
			_LINE_ (list h3 (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) _ENTER_
			_LINE_ (list h3 (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) _ENTER_
			_LINE_ (list (- h3) (- h2)) (strcat "@0," (rtos (- len))) _ENTER_
			_LINE_ (list (- h3) (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) _ENTER_
			_LINE_ (list (- h3) (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) _ENTER_
		)
	)	

	; Schematic symbol
	(AddDescriptionBelowOrigo description s1)
	(DrawProxySymbol layDef_FoundationLocator "T")
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



(defun BETONG-ANVISERSOEYLE ( / blockName description s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y )
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-ANVISERSOEYLE"
		description (strcat "BETONG ANVISERS" _uOSLASH_ "YLE- FUNDAMENT 1500x1500x1000-" _uOSLASH_ "600")
		s1 (/ 1.5 2)
		radius1 0.3
		radius_skrue 0.0120
		radius_roer 0.025
		dist1 (/ radius1 30)
		skrue_xy (/ s1 5)
		roer_y (- (- radius1) (/ radius1 3))
	)
	(defun LocalGraphics (/)
		(command
			_RECTANGLE_ (list (- s1) (- s1)) (list s1 s1)
			_CIRCLE_ _origo_ radius1
			_CIRCLE_ (list skrue_xy skrue_xy) radius_skrue
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_CIRCLE_ _origo_ radius_roer
			_CIRCLE_ (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_ ;neste fire LINE er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
			_LINE_
				(list (- radius_roer) roer_y) 
				(list radius_roer roer_y) 
				(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1))))))  ;y=R*sin(arccos(x/R))
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_
				(list (+ radius_roer dist1) roer_y) 
				(list (+ (* 3 radius_roer) dist1) roer_y) 
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_
				(list (+ radius_roer dist1) roer_y) 
				(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_
				(list (+ (* 3 radius_roer) dist1) roer_y)
				(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1)))))) 
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		)
	)
	
	; Schematic symbol
	(AddDescriptionBelowOrigo description (- (+ s1 radius1)))
	(DrawProxySymbol layDef_FoundationLocator "T")
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



(defun BETONG-MONITOR ( / blockName description s1 radius1 radius_skrue radius_roer dist1 skrue_xy roer_y )
	(setq
		blockName "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-MONITOR"
		description (strcat "BETONG TELEMONITOR- FUNDAMENT 1000x1000x1000-" _uOSLASH_ "600")
		s1 (/ 1.0 2)
		radius1 0.3
		radius_skrue 0.0120
		radius_roer 0.025
		dist1 (/ radius1 30)
		skrue_xy (/ s1 5)
		roer_y (- (- radius1) (/ radius1 3))
	)
	(defun LocalGraphics (/)
		(command 
			_RECTANGLE_ (list (- s1) (- s1)) (list s1 s1)
			_CIRCLE_ _origo_ radius1
			_CIRCLE_ (list skrue_xy skrue_xy) radius_skrue
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_CIRCLE_ _origo_ radius_roer
			_CIRCLE_ (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_ ;neste fire LINES er de tre rørene som stikker ut nederst på den store sirkelen: y koordinat for brytning med sirkel er gitt av 
			_LINE_
				(list (- radius_roer) roer_y)
				(list radius_roer roer_y)
				(list radius_roer (- (* radius1 (sin (acos (/ radius_roer radius1)))))) ;y=Rsin(arccos(x/R))
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_ (list (+ radius_roer dist1) roer_y) (list (+ (* 3 radius_roer) dist1) roer_y) _ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_
				(list (+ radius_roer dist1) roer_y)
				(list (+ radius_roer dist1) (- (* radius1 (sin (acos (/ (+ radius_roer dist1) radius1)))))) 
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_LINE_
				(list (+ (* 3 radius_roer) dist1) roer_y)
				(list (+ (* 3 radius_roer) dist1) (- (* radius1 (sin (acos (/ (+ (* 3 radius_roer) dist1) radius1))))))
				_ENTER_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		)
	)
	
	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "T")
	(AddDescriptionBelowOrigo description (- (+ s1 radius1)))
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



(defun BETONG-VALIDATOR ( / blockName description s1 radius_skrue radius_roer lengde_roer dist1 skrue_x skrue_y )
	(setq
		blockName	"NO-BN-2D-JBTUB-FUNDAMENT-BETONG-VALIDATOR"
		description	"BETONG VALIDATOR- FUNDAMENT 300x360x500"
		s1 (* _half_ 0.360)
		radius_skrue 0.008
		radius_roer 0.025
		lengde_roer 0.125
		dist1 (/ s1 (* s1 100.0))
		skrue_x (+ (* 3.0 radius_roer) dist1 (/ (* s1 2.0) 30.0)) 
		skrue_y (* s1 (/ 0.0854 s1))
	)
	(defun LocalGraphics (/)
		(command
			_RECTANGLE_ (list (- s1) (- s1)) (list s1 s1)
			_CIRCLE_ (list skrue_x skrue_y) radius_skrue
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
			_CIRCLE_ _origo_ radius_roer
			_CIRCLE_ (list (+ (* 2 radius_roer) dist1) 0) radius_roer
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_  
			_RECTANGLE_ (list radius_roer (- s1)) (list (- radius_roer) (- (- s1) lengde_roer))
			_RECTANGLE_ (list (+ radius_roer dist1) (- s1)) (list (+ (* 3 radius_roer) dist1) (- (- s1) lengde_roer))
			_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		)
	)

	; Schematic symbol
	(DrawProxySymbol layDef_FoundationLocator "T")
	(AddDescriptionBelowOrigo description s1)
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
