;=========================================================================================================================
;
; Yoke.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Yoke

(defun C:YOKE ( / )
	(setq counter 10)
	(repeat 34
		(subSubStep (strcat "YOKE: " (rtos counter _decimal_ 0) " m"))
		(AAK counter) ; 10 to 43 inclusive
		(setq counter (+ counter 1))
	)
  	(UTLIGGERAAK)
)



(defun AAK ( len / blockName )
	(setq 
		blockName (strcat "NO-BN-2D-JBTKL-AAK-" (rtos (* 1000 len) _decimal_ 0))
		description (strcat "KL-" _uAA_ "K " (rtos len _decimal_ 0) " METER")
	)

	; Schematic symbol
	; Adapt to schematic plan with 21 units between center tracks (instead of standard ~4.7 in shunting yards
	(drawLine layer_Zero _origo_ (list 0 (atoi (rtos (* (/ _schematicTrackSpacing_ _geographicTrackSpacing_) (- (+ 1 len))) _decimal_ 0)))) ; scaled, floored to int
	(addDescriptionBelowOrigo description -1.0) ; A little *above* yoke
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	; Just annotative text, no graphics
	(addDescriptionBelowOrigo description -1.0) ; A little *above* yoke
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawLine layer_Zero _origo_ (list 0 (- len)))
	(createMetricBlockFromCurrentGraphics blockName)
)



(defun UTLIGGERAAK ( / blockName len )
	(setq 
		len 6.000 ; Fixed length [m]
		blockName (strcat "NO-BN-2D-JBTKL-AAK-UTLIGGERAAK-" (rtos (* 1000 len) _decimal_ 0))
		description (strcat "KL UTLIGGER-" _uAA_ "K " (rtos len _decimal_ 0) " METER")
	)

	; Schematic symbol
	; Adapt to schematic plan with _schematicTrackSpacing_ units between center tracks (instead of standard ~4.7 in shunting yards
	(drawLine layer_Zero _origo_ (list 0 (atoi (rtos (* (/ _schematicTrackSpacing_ _geographicTrackSpacing_) (- (+ 1 len))) _decimal_ 0)))) ; scaled, floored to int
	(addDescriptionBelowOrigo description -1.0) ; A little *above* yoke
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	; Just annotative text, no graphics
	(addDescriptionBelowOrigo description -1.0) ; A little *above* yoke
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(drawLine layer_Zero _origo_ (list 0 (- len)))
	(createMetricBlockFromCurrentGraphics blockName)
)
