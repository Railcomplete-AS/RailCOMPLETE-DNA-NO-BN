;=========================================================================================================================
;
; Axle counter system.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Axle counter sensor and axle counter tuning unit

(defun C:AXLE-COUNTER-SYSTEM ( / )
	(AKSELTELLER)
	(AKSELTELLER-SOPP)
)



(defun AKSELTELLER ( / blockName description gx gy gp paperScale )
	;
	;  -(*)--.--(*)-   ; Two hatched circles on a line
	;        |         ; Vertical snapline added on own layer (in Geo versions)
	;
	(setq
		blockName "NO-BN-2D-JBTSI-TOGDETEKSJON-TELLEPUNKT-SENSOR"
		description	"AKSELTELLER SENSOR"
		gx 4.0 ; reserved area
		gy 0.5
		gp (list 0 3.0) ; center area
	)

	; Schematic symbol:
	(drawLine layer_Zero _origo_ (list 0.875 0.0))					; .--
	(drawCircleAtPos layer_Zero 1.0 (list 1.875 0.0) _noWipeout_)	; .--( )
	(drawHatch _mediumHatch_)										; .--(*)
	(drawLine layer_Zero (list 2.875 0) (list 3.375 0))				; .--(*)-
	(mirrorAboutYaxis _keep_)										; -(*)--.--(*)-
	(addDescriptionBelowOrigo description (* -3 gy))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(foreach paperScale paperScaleList
		(addScaledGraphicsFromBlock blockName (/ (atof paperScale) 1000.0)) ; Pre-scale symbolic elements...

		; Add metric graphics for reserved area for tuning unit
		(drawBoxAtPos layer_AxleCounter_ReservedAreaForTuningUnit gx gy gp nil) ; no wipeout
		(drawHatch _filledHatch_)
		(drawLine layer_AxleCounter_SnaplineForPositioning _origo_ (list 0 -0.75)) ; Add snap line to geo symbols (half track gauge)
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



(defun AKSELTELLER-SOPP ( / blockName description )
	; TODO: 2020-08-04 CLFEY - Maybe not really needed in schematic drawings?
	(setq
		blockName	"NO-BN-2D-JBTSI-TOGDETEKSJON-TELLEPUNKT-SOPP"
		description	"AKSELTELLER TUNINGENHET"
	)
	; Schematic symbol
	(setLayer layer_Zero)
	(command
		"._PLINE" (list 0.0 (- 0.06)) (list 1.0 (- 0.06)) (list 1.0 0.06) (list 0.0 0.06) ""
		"._PLINE" (list 0.06 0.06) (list 0.06 1.0) (list 0.0 1.0) ""
		"._PLINE" (list 0.06 1.0) (list 0.85 1.0) (list 0.85 1.1) (list 0 1.1) ""
		"._PLINE" (list 0.75 1.1) (list 0.7 2.0) (list 0.6 2.15) (list 0 2.15) ""
		"._MIRROR" "_ALL" "" (list 0.0 0.0) (list 0.0 1.0) "_NO"
	)
	(addTextAtPos layer_Zero 0.50 (list 0 1.625) "Az")
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
