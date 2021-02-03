;=========================================================================================================================
;
; Axle counter system.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Axle counter sensor and axle counter tuning unit

(defun C:AXLE-COUNTER-SYSTEM ( / )
	(subSubStep "AKSELTELLER")		(AKSELTELLER)
	(subSubStep "AKSELTELLER-SOPP")	(AKSELTELLER-SOPP)
)



(defun AKSELTELLER ( / blockName description rax ray rap disatAlong loLilmit hiLimit gx gy gp )
	;
	;  -(*)--.--(*)-   ; Two hatched circles on a line
	;        |         ; Vertical snapline added on own layer (in Geo versions)
	;
	(setq
		blockName "NO-BN-2D-JBTSI-TOGDETEKSJON-TELLEPUNKT-SENSOR"
		description	"AKSELTELLER SENSOR"
		rax 4.0 ; reserved area, assume along the track, outside a possible 60 cm wide cable duct
		ray 0.5
		rap (list 0 3.0) ; center reserved area
		distAlong _sleeperSpacing_	; Width of the area indicating minimum rail separation (enough to be well visible during design, and to allow for mounting tolerances)
		loLimit 0.30 	; Minimum rail separation at sensor for speeds up to 120 km/h inclusive
		hiLimit 1.00 	; Minimum rail separation at sensor for speeds above 120 km/h
	)
		gx 4.0 ; reserved area
		gy 0.5
		gp (list 0 3.0) ; center area

	; Schematic symbol
	(drawLine layer_Zero _origo_ (list 0.875 0.0))					; .--
	(drawCircleAtPos layer_Zero 1.0 (list 1.875 0.0) _noWipeout_)	; .--( )
	(drawHatch _mediumHatch_)										; .--(*)
	(drawLine layer_Zero (list 2.875 0) (list 3.375 0))				; .--(*)-
	(mirrorAboutYaxis _keepMirrorSource_)										; -(*)--.--(*)-
	(addDescriptionBelowOrigo description (* -1.5 _cantReferenceGauge_))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(addGraphicsFromScaledSchematicBlock blockName _one_)
	(drawLine layer_Zero _origo_ (list 0 (* -0.5 _cantReferenceGauge_))) ; -(*)--|--(*)- Add vertical snap line to annotative part of geo symbol (half track gauge, from centre track to annotative symbol)
	(createAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	; Reserved area for tuning unit
	(drawBoxAtPos layer_AxleCounter_ReservedAreaForTuningUnit rax ray rap _noWipeout_)
	(drawHatch _solidHatch_)
	; Metal-free area:
	; Ref. Bane NOR, ERTMS Engineering Guidelines ERP-30-S-00097_16E ch. 4.2 Points: Requirements ENI-SS-ENG-1513 / 1514 / 1515,
	; stating that the distance required from centre of a sensor's rail to the nearest other rail shall be at 
	; least +/- 0.30m for speeds up to 120 km/h, and +/- 1.0m for speeds above 120 km/h.
	(drawBoxAtPos layer_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh distAlong loLimit _origo_ _noWipeout_)
	(drawHatch _solidHatch_) ; Hatching the smaller of the two areas
	(drawBoxAtPos layer_AxleCounter_MinimumRailSeparationAtSpeedsAbove_120_kmh distAlong hiLimit _origo_ _noWipeout_)
	(createMetricBlockFromCurrentGraphics blockName)
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
		_POLYLINE_ (list 0.0 (- 0.06)) (list 1.0 (- 0.06)) (list 1.0 0.06) (list 0.0 0.06) _ENTER_
		_POLYLINE_ (list 0.06 0.06) (list 0.06 1.0) (list 0.0 1.0) _ENTER_
		_POLYLINE_ (list 0.06 1.0) (list 0.85 1.0) (list 0.85 1.1) (list 0 1.1) _ENTER_
		_POLYLINE_ (list 0.75 1.1) (list 0.7 2.0) (list 0.6 2.15) (list 0 2.15) _ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	(addTextAtPos layer_Zero 0.50 (list 0 1.625) "Az")
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
