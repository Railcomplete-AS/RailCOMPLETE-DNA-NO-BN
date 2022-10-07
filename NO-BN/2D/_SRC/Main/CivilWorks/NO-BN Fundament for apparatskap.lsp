;=========================================================================================================================
;
; NO-BN Fundament for apparatskap.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Signaling cabinet foundation

; For debugging:
; 


(defun NOBN-FUNDAMENT-FOR-SKAP ( / )
	;
	; Schematic symbols feature only a circle with the discipline's letter combination - a 'proxy symbol'.
	; Annotative symbol are drawn to metric scale in 1:250 drawings.
	;
	(NOBN-BETONG-APPARATSKAP-FUNDAMENT "LANGSIDE")
	(NOBN-BETONG-APPARATSKAP-FUNDAMENT "KORTSIDE")
)



(defun NOBN-BETONG-APPARATSKAP-FUNDAMENT ( variation / blockName description s1 s2 h1 h2 len1 len2 )
	(setq
		blockName (strcat "NO-BN-2D-JBTKU_FUN-SKAPFUNDAMENT-BETONG-APPARATSKAP-" variation)
		description _emptyString_ ; see below
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
				description "SKAPFUNDAMENT BETONG, APPARATSKAP LANGSIDE 1400x820x540"
			)
			(defun LocalGraphics (/)
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
				description	"SKAPFUNDAMENT BETONG, APPARATSKAP KORTSIDE 1400x820x540"
			)
			(defun LocalGraphics (/)
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
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(ScaleAll _four_)
	(DrawProxySymbol layDef_FoundationLocator "S")
	(AddDescriptionBelowOrigin description _proxySymbolRadius_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)
