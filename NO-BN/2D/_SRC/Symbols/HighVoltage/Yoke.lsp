;=========================================================================================================================
;
; Yoke.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Yoke

(defun C:YOKE ( / )
	(setq counter 10)
	(repeat 34
		(AAK counter) ; 10 to 43 inclusive
		(setq counter (+ counter 1))
	)
  	(UTLIGGERAAK)
)



(defun AAK ( len / blockName )
	(setq 
		blockName (strcat "NO-BN-2D-JBTKL-AAK-" (rtos (* len 1000) 2 0))
	)

	; Schematic symbol - Adapt to schematic plan with 21 units between center tracks (instead of standard ~4.7 in shunting yards
	(drawLine layer_Zero (list 0 0) (list 0 (atoi (rtos (* (/ 21.0 4.7) (- len)) _decimal_ 0)))) ; scaled, floored to int
	(addTextAtPos layer_Description _descriptionTextHeight_ (list 0 _descriptionTextHeight_) (strcat "KL-aak " (rtos len _decimal_ 0) " meter")) ; text above
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols - metric in all scales
	(foreach paperScale paperScaleList
		(drawLine layer_Zero (list 0 0) (list 0 (- len))) ; metric
		(addTextAtPos layer_Description _descriptionTextHeight_ (list 0 _descriptionTextHeight_) (strcat "KL-aak " (rtos len _decimal_ 0)  " meter")) ; text above
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



(defun UTLIGGERAAK ( / blockName len )
	(setq 
		blockName "NO-BN-2D-JBTKL-AAK-UTLIGGERAAK-6000"
		len 6.0 ; Fixed length
	)

	; Schematic symbol - Adapt to schematic plan with 21 units between center tracks (instead of standard ~4.7 in shunting yards
	(drawLine layer_Zero (list 0 0) (list 0 (atoi (rtos (* (/ 21.0 4.7) (- len)) _decimal_ 0)))) ; scaled, floored to int
	(addTextAtPos layer_Description _descriptionTextHeight_ (list 0 _descriptionTextHeight_) (strcat "KL utligger-aak " (rtos len _decimal_ 0)  " meter")) ; text above
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols - metric in all scales
	(foreach paperScale paperScaleList
		(drawLine layer_Zero (list 0 0) (list 0 (- len))) ; metric
		(addTextAtPos layer_Description _descriptionTextHeight_ (list 0 _descriptionTextHeight_) (strcat "KL utligger-aak " (rtos len _decimal_ 0)  " meter")) ; text above
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)
