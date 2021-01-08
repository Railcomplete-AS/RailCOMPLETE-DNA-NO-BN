;=========================================================================================================================
;
; Cabinet foundation.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Signaling cabinet foundation

(defun C:CABINET-FOUNDATION ( / )
	;
	; Schematic symbols feature only a circle with the discipline's letter combination - a 'proxy symbol'.
	; Geo symbols are drawn to metric scale in 1:250 drawings.
	;
	(BETONG-APPARATSKAP-FUNDAMENT "LANGSIDE")
	(BETONG-APPARATSKAP-FUNDAMENT "KORTSIDE")
)



(defun BETONG-APPARATSKAP-FUNDAMENT ( variation / blockName description s1 s2 h1 h2 len )
	(setq
		blockName (strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-APPARATSKAP-" variation)
		description "" ; see below
		s1 (/ 1.4 2)	; 1.4 wide x 0.82 deep 
		s2 (/ 0.82 2)
		h1 (/ 0.76 2)	; Inner hole 0.76 x 0.38
		h2 (/ 0.38 2)
		len (/ (- s2 h2) 5)
	)
	(cond 
		((= variation "LANGSIDE")
			(setq description "BETONG APPARATSKAP - FUNDAMENT LANGSIDE 1400x820x540")
			; Schematic symbol (just a circle with proxy symbol text)
			(drawProxySymbol layer_FoundationLocator "S")
			(addDescriptionBelowOrigo description _proxySymbolRadius_)
			(createSchematicBlockFromCurrentGraphics blockName)

			; Geo symbols:
			(setLayer layer_Zero)
			(command
				; Outer and inner 'box'
				"._RECTANGLE" (list (- s1) (- s2)) (list s1 s2)
				"._RECTANGLE" (list (- h1) (- h2)) (list h1 h2)
				; Left dashed line
				"._LINE" (list (- h1) (- h2)) (strcat "@0," (rtos (- len))) ""	
				"._LINE" (list (- h1) (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) ""
				"._LINE" (list (- h1) (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) ""
				; Right dashed line
				"._LINE" (list h1 (- h2)) (strcat "@0," (rtos (- len))) ""
				"._LINE" (list h1 (- (- h2) (* 2 len))) (strcat "@0," (rtos (- len))) ""
				"._LINE" (list h1 (- (- h2) (* 4 len))) (strcat "@0," (rtos (- len))) ""
			)
			(scaleAll _four_) ; 1:250 is real size
			(drawProxySymbol layer_FoundationLocator "S")
			(addDescriptionBelowOrigo description _proxySymbolRadius_)
			(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName) ; S = 1:1000
		)
		((= variation "KORTSIDE")
			(setq description	"BETONG APPARATSKAP - FUNDAMENT KORTSIDE 1400x820x540")
			; Schematic symbol (just a circle with proxy symbol text)
			(drawProxySymbol layer_FoundationLocator "S")
			(addDescriptionBelowOrigo description _proxySymbolRadius_)
			(createSchematicBlockFromCurrentGraphics blockName)

			; Geo symbols:
			(setLayer layer_Zero)
			(command
				; Outer and inner 'box'
				"._RECTANGLE" (list (- s1) (- s2)) (list s1 s2)
				"._RECTANGLE" (list (- h1) (- h2)) (list h1 h2)
				; Lower dashed line
				"._LINE" (list h1 (- h2)) (strcat "@" (rtos len) ",0") ""
				"._LINE" (list (+ h1 (* 2 len)) (- h2)) (strcat "@" (rtos len) ",0") ""
				"._LINE" (list (+ h1 (* 4 len)) (- h2)) (strcat "@" (rtos len) ",0") ""
				; Upper dashed line
				"._LINE" (list h1 h2) (strcat "@" (rtos len) ",0") ""
				"._LINE" (list (+ h1 (* 2 len)) h2) (strcat "@" (rtos len) ",0") ""
				"._LINE" (list (+ h1 (* 4 len)) h2) (strcat "@" (rtos len) ",0") ""
			)
			(rotateLeft 90)
			(scaleAll _four_) ; 1:250 is real size
			(drawProxySymbol layer_FoundationLocator "S")
			(addDescriptionBelowOrigo description _proxySymbolRadius_)
			(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName) ; S = 1:1000
		)
	)
)
