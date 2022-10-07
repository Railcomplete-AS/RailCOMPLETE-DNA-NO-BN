;=========================================================================================================================
;
; ANYADM Axle counter system.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Axle counter sensor and axle counter tuning unit

(defun ANYADM-AXLE-COUNTER-SYSTEM ( / )
	(TraceLevel3 "AXLE-COUNTER-SENSOR")	(AXLE-COUNTER-SENSOR)
	(TraceLevel3 "AXLE-COUNTER-TUNER")	(AXLE-COUNTER-TUNER)
)



(defun AXLE-COUNTER-SENSOR ( / blockName description rax ray rap distAlong loLilmit hiLimit gx gy gp )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "ACS-" "AXLE-COUNTER-SENSOR"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "TEL-" "AKSELTELLER-SENSOR"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "CEC-" "COMPTEUR-ESSIEUX-CAPTEUR"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "AZR-" "ACHSZAEHLER-RADSENSOR"		)))
	)   
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat "AXLE COUNTER SENSOR"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "AKSELTELLER SENSOR"			)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT DE COMPTAGE, CAPTEUR"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "ACHSZAEHLER-RADSENSOR"			)))
	)
	(cond 
		((or (= _ADM_ _XXGL_) (= _ADM_ _NOBN_))
			;
			;  --(*)---0--1(2)3-4	; Two hatched circles on a line
			;          |         	; Vertical snapline added on own layer (in Geo versions)
			;          .
			;          |
			;          5 (corresponding to centreline of the track)
			;
			(setq
				rax 4.0	; [m] Reserved area for tuning unit, along the track, outside a possible 60 cm wide cable duct
				ray 0.5	; [m] Reserved area for tuning unit, width of area perpendicularly to the track
				rap (list 0 3.0) ; center reserved area
				distAlong _sleeperSpacing_	; Width of the area indicating minimum rail separation (enough to be well visible during design, and to allow for mounting tolerances)
				loLimit 0.30 	; Minimum rail separation at sensor for speeds up to 120 km/h inclusive
				hiLimit 1.00 	; Minimum rail separation at sensor for speeds above 120 km/h
				rhd2	(halfOf _railHeadDistance_)
				p0	(list 0.000 rhd2)
				p1	(list 0.875 rhd2)
				p2	(list 1.875 rhd2)
				p3	(list 2.875 rhd2)
				p4	(list 3.375 rhd2)
				p5	(list 0.000 (- rhd2))
				radius	(* 0.5 (- (xCoord p3) (xCoord p1)))
			)

			; Schematic symbol
			(DrawLine layDef_Zero p0 p1)								;        --
			(DrawCircleAtPos layDef_Zero p2 radius _noWipeout_)			;        --( )
			(DrawHatch _denseHatch_)									;        --(*)
			(DrawLine layDef_Zero p3 p4)								;        --(*)-
			(MirrorAboutYaxis _keepMirrorSource_)						; -(*)-----(*)-
			(AddDescriptionBelowOrigin description (* -1.5 _railHeadDistance_)) ; above symbol
			(moveUp (halfOf _railHeadDistance_))						
			(CreateSchematicBlockFromCurrentGraphics blockName)
		
			; Annotative symbol
			(AddGraphicsFromScaledSchematicBlock blockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics blockName)

		
			; Annotative symbol
			(AddGraphicsFromScaledSchematicBlock blockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics blockName)
		

			; Metric symbol:
			; Vertical snap line from center track to center rail:
			(DrawLine layDef_AxleCounter_SnaplineForPositioning _origin_ p5)
		
			; Reserved area for tuning unit:
			(DrawBoxAtPos layDef_AxleCounter_ReservedAreaForTuningUnit rap rax ray _noWipeout_)
			(DrawHatch _solidHatch_)

			; Metal-free area:
			; Ref. Bane NOR, ERTMS Engineering Guidelines ERP-30-S-00097_16E ch. 4.2 Points: Requirements ENI-SS-ENG-1513 / 1514 / 1515,
			; stating that the distance required from centre of a sensor's rail to the nearest other rail shall be at 
			; least +/- 0.30m for speeds up to 120 km/h, and +/- 1.0m for speeds above 120 km/h.
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh distAlong (* 2 loLimit) _noWipeout_)
			(DrawHatch _solidHatch_) ; Hatching the smaller of the two areas
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsAboveOrEqualTo_120_kmh distAlong (* 2 hiLimit) _noWipeout_)
			(CreateMetricBlockFromCurrentGraphics blockName)
		)

		((= _ADM_ _FRSR_)
			;
			; +-----+
			; |\ * /|  ; * = dense hatch
			; | \./ |  ; Note: Insertion point is placed at RailHeadSeparation/2 from center track
			; |  X  |
			; | / \ |
			; |/ * \|  ; * = dense hatch
			; +-----+
			;
			(setq
				x	4.0
				y	7.0
				rax 4.0	; [m] Reserved area for tuning unit, along the track, outside a possible 60 cm wide cable duct
				ray 0.5	; [m] Reserved area for tuning unit, width of area perpendicularly to the track
				rap (list 0 3.0) ; center reserved area
				distAlong _sleeperSpacing_	; Width of the area indicating minimum rail separation (enough to be well visible during design, and to allow for mounting tolerances)
				loLimit 0.30 	; Minimum rail separation at sensor for speeds up to 120 km/h inclusive
				hiLimit 1.00 	; Minimum rail separation at sensor for speeds above 120 km/h
			)
		
			; Schematic symbol
			(DrawBox layDef_Zero x y layDef_AxleCounter_Wipeout)
			; (DrawBox layDef_Zero x y _noWipeout_)
			(DrawStAndrewCross layDef_Zero x y)
			(DrawHatchAtPoint _denseHatch_ _slightlyAbove_ _angleZero_ 0)
			(DrawHatchAtPoint _denseHatch_ _slightlyBelow_ _angleZero_ 0)
			(AddDescriptionBelowOrigin description (HalfOf y))
			(MoveDown (HalfOf _railHeadDistance_))
			(CreateSchematicBlockFromCurrentGraphics blockName)
		
			; Annotative symbol
			(AddGraphicsFromScaledSchematicBlock blockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics blockName)
		
			; Metric symbol
			
			; Reserved area for tuning unit
			(DrawBoxAtPos layDef_AxleCounter_ReservedAreaForTuningUnit rap rax ray _noWipeout_)
			(DrawHatch _solidHatch_)
			
			; Metal-free area: TODO 2021-04-06 check size with SNCF
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh distAlong loLimit _noWipeout_)
			(DrawHatch _solidHatch_) ; Hatching the smaller of the two areas
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsAboveOrEqualTo_120_kmh distAlong hiLimit _noWipeout_)
			(CreateMetricBlockFromCurrentGraphics blockName)
		);FRSR
		
		((= _ADM_ _DEDB_)
			;
			;  --(*)---0--1(2)3-4	; Two hatched circles on a line
			;          |         	; Vertical snapline added on own layer (in Geo versions)
			;          .
			;          |
			;          5 (corresponding to centreline of the track)
			;
			(setq
				rax 4.0	; [m] Reserved area for tuning unit, along the track, outside a possible 60 cm wide cable duct
				ray 0.5	; [m] Reserved area for tuning unit, width of area perpendicularly to the track
				rap (list 0 3.0) ; center reserved area
				distAlong _sleeperSpacing_	; Width of the area indicating minimum rail separation (enough to be well visible during design, and to allow for mounting tolerances)
				loLimit 0.30 	; Minimum rail separation at sensor for speeds up to 120 km/h inclusive : TODO: CHECK WITH DB
				hiLimit 1.00 	; Minimum rail separation at sensor for speeds above 120 km/h : TODO: CHECK WITH DB
				rhd2	(halfOf _railHeadDistance_)
				p0	(list 0.000 rhd2)
				p1	(list 0.125 rhd2)
				p2	(list 0.775 rhd2)
				p3	(list 1.425 rhd2)
				p4	(list 2.000 rhd2)
				p5	(list 0.000 (- rhd2))
				radius	(* 0.5 (- (xCoord p3) (xCoord p1)))
			)
		
			; Schematic symbol
			(DrawLine layDef_Zero p0 p1)								;        --
			(DrawCircleAtPos layDef_Zero p2 radius _noWipeout_)			;        --( )
			(DrawHatch _denseHatch_)									;        --(*)
			(DrawLine layDef_Zero p3 p4)								;        --(*)-
			(MirrorAboutYaxis _keepMirrorSource_)						; -(*)-----(*)-
			(AddDescriptionBelowOrigin description (* -1.5 _railHeadDistance_)) ; above symbol
			(CreateSchematicBlockFromCurrentGraphics blockName)
		
			; Annotative symbol
			(AddGraphicsFromScaledSchematicBlock blockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics blockName)
		

			; Metric symbol:
			; Vertical snap line from center track to center rail:
			(DrawLine layDef_AxleCounter_SnaplineForPositioning _origin_ p5)
		
			; Reserved area for tuning unit:
			(DrawBoxAtPos layDef_AxleCounter_ReservedAreaForTuningUnit rap rax ray _noWipeout_)
			(DrawHatch _solidHatch_)

			; Metal-free area:
			; TODO: CHECK WITH DB
			; The distance required from centre of a sensor's rail to the nearest other rail shall be at 
			; least +/- 0.30m for speeds up to 120 km/h, and +/- 1.0m for speeds above 120 km/h.
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh distAlong (* 2 loLimit) _noWipeout_)
			(DrawHatch _solidHatch_) ; Hatching the smaller of the two areas
			(DrawBox layDef_AxleCounter_MinimumRailSeparationAtSpeedsAboveOrEqualTo_120_kmh distAlong (* 2 hiLimit) _noWipeout_)

			(CreateMetricBlockFromCurrentGraphics blockName)
		);DEDB
	);cond
)



(defun AXLE-COUNTER-TUNER ( / blockName description axleCounterCaption )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "ACT-" "AXLE-COUNTER-TUNING-UNIT"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "TET-" "AKSELTELLER-TUNING-ENHET"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "CER-" "COMPTEUR-ESSIEUX-REGLAGE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "AZK-" "ACHSZAEHLER-KABELVERTEILER"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "AXLE COUNTER TUNING UNIT"		)))
		((= _ADM_ _NOBN_) (setq description (strcat "AKSELTELLER TUNING-ENHET"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT DE COMPTAGE, REGLAGE"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "ACHSZAEHLER KABELVERTEILER"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq axleCounterCaption "AC"	)) ; Axle Counter
		((= _ADM_ _NOBN_) (setq axleCounterCaption "Az"	)) ; Akselteller (tysk forkortelse for å ikke forveksle med AutoTrafo AT)
		((= _ADM_ _FRSR_) (setq axleCounterCaption "PC"	)) ; Point comptage
		((= _ADM_ _DEDB_) (setq axleCounterCaption "Az"	)) ; Achszähler
	)
	; Schematic symbol
	(SetLayer layDef_Zero)
	(command
		_POLYLINE_ (list 0.0 (- 0.06)) (list 1.0 (- 0.06)) (list 1.0 0.06) (list 0.0 0.06) _ENTER_
		_POLYLINE_ (list 0.06 0.06) (list 0.06 1.0) (list 0.0 1.0) _ENTER_
		_POLYLINE_ (list 0.06 1.0) (list 0.85 1.0) (list 0.85 1.1) (list 0 1.1) _ENTER_
		_POLYLINE_ (list 0.75 1.1) (list 0.7 2.0) (list 0.6 2.15) (list 0 2.15) _ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
	(AddTextAtPos layDef_Zero 0.50 (list 0 1.625) axleCounterCaption)

	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
