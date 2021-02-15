;=========================================================================================================================
;
; Cabinet foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Signaling cabinet foundation

(defun C:CABINET-FOUNDATION ( / )
	;
	; Schematic symbols feature only a circle with the discipline's letter combination - a 'proxy symbol'.
	; Annotative symbol are drawn to metric scale in 1:250 drawings.
	;
	(BETONG-APPARATSKAP-FUNDAMENT "LANGSIDE")
	(BETONG-APPARATSKAP-FUNDAMENT "KORTSIDE")
)



(defun BETONG-APPARATSKAP-FUNDAMENT ( variation / blockName description s1 s2 h1 h2 len1 len2 )
	(setq
		blockName (strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-APPARATSKAP-" variation)
		description _ENTER_ ; see below
		s1 (/ 1.4 2)	; 1.4 wide x 0.82 deep 
		s2 (/ 0.82 2)
		h1 (/ 0.76 2)	; Inner hole 0.76 x 0.38
		h2 (/ 0.38 2)
		len1 (/ (- s2 h2) 5) ; 5 parts dash-space-dash-space-dash makes a dashed line for cable throughput
		len2 (/ (- s1 h1) 5) ; 5 parts dash-space-dash-space-dash makes a dashed line for cable throughput
	)
	(cond 
		((= variation "LANGSIDE")
			(setq 
				description "FUNDAMENT BETONG, APPARATSKAP LANGSIDE 1400x820x540"
			)
			(defun localGraphics (/)
				(command
					; Outer and inner 'box'
					_RECTANGLE_ (list (- s1) (- s2)) (list s1 s2)
					_RECTANGLE_ (list (- h1) (- h2)) (list h1 h2)
					; Left dashed line
					_LINE_ (list (- h1) (- h2)) (strcat "@0," (rtos (- len1))) _ENTER_	
					_LINE_ (list (- h1) (- (- h2) (* 2 len1))) (strcat "@0," (rtos (- len1))) _ENTER_
					_LINE_ (list (- h1) (- (- h2) (* 4 len1))) (strcat "@0," (rtos (- len1))) _ENTER_
					; Right dashed line
					_LINE_ (list h1 (- h2)) (strcat "@0," (rtos (- len1))) _ENTER_
					_LINE_ (list h1 (- (- h2) (* 2 len1))) (strcat "@0," (rtos (- len1))) _ENTER_
					_LINE_ (list h1 (- (- h2) (* 4 len1))) (strcat "@0," (rtos (- len1))) _ENTER_
				)
			)
		)
		((= variation "KORTSIDE")
			(setq 
				description	"FUNDAMENT BETONG, APPARATSKAP KORTSIDE 1400x820x540"
			)
			(defun localGraphics (/)
				(command
					; Outer and inner 'box'
					_RECTANGLE_ (list (- s2) (- s1)) (list s2 s1)
					_RECTANGLE_ (list (- h2) (- h1)) (list h2 h1)
					; Left dashed line
					_LINE_ (list (- h2) (- h1)) (strcat "@0," (rtos (- len2))) _ENTER_	
					_LINE_ (list (- h2) (- (- h1) (* 2 len2))) (strcat "@0," (rtos (- len2))) _ENTER_
					_LINE_ (list (- h2) (- (- h1) (* 4 len2))) (strcat "@0," (rtos (- len2))) _ENTER_
					; Right dashed line
					_LINE_ (list h2 (- h1)) (strcat "@0," (rtos (- len2))) _ENTER_
					_LINE_ (list h2 (- (- h1) (* 2 len2))) (strcat "@0," (rtos (- len2))) _ENTER_
					_LINE_ (list h2 (- (- h1) (* 4 len2))) (strcat "@0," (rtos (- len2))) _ENTER_
				)
			)
		)
	)

	; Schematic symbol
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(setLayer layDef_Zero)
	(localGraphics)
	(scaleAll _four_)
	(drawProxySymbol layDef_FoundationLocator "S")
	(addDescriptionBelowOrigo description _proxySymbolRadius_)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(setLayer layDef_MetricDetails)
	(localGraphics)
	(createMetricBlockFromCurrentGraphics blockName)
)
