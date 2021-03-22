;=========================================================================================================================
;
; Level crossing road signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Road signal at level crossing

; Veisignalene er ikke reglet i TRV for jernbane (SJT TFF), men i Veitrafikkens regelverk.

(defun C:LEVEL-CROSSING-ROAD-SIGNAL ( / variation yokeMounted )
	(foreach variation '("VS1" "VS2")			; 'VS1'= single mast, 'VS2' = forked mast (double signal)
		(foreach yokeMounted '(nil T)			; 'nil' = upright mast, T = yoke mounting
			; Single or double road signal (for level crossing), free-standing mast or yoke mounted:
			(TraceLevel3 (strcat "VEISIGNAL-" variation (if yokeMounted (strcat "-AAK" ) "" ))) (VEISIGNAL variation yokeMounted )
		)
	)
)



(defun VEISIGNAL ( variation yokeMounted / )
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-" variation (if (= yokeMounted nil) _ENTER_ "-AAK"))
		description "VEISIGNAL (SIGNAL MOT VEITRAFIKK)"
	)
	
	(ThawAllLayers)

	; Schematic symbol
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(SetLayer layDef_View_SchematicPlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(SetLayer layDef_View_CablePlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "WIDE")
	
	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	(SetLayer layDef_View_TrackIsolationPlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Create schematic symbol
	(ThawAllLayers)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	; Geo 1-line signaling / schematic plan ('skjematisk plan') view
	(SetLayer layDef_View_SchematicPlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	; Geo 1-line signaling / cable plan ('plan og kabelplan') view
	(SetLayer layDef_View_CablePlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "NARROW")
	
	; Geo 2-line signaling / track insulation plan OR high voltage / return current plan view
	(SetLayer layDef_View_TrackIsolationPlan)
	(DrawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	(ThawAllLayers)
	(ScaleAll _twoThirds_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawLevelCrossingSignalTowardsRoad ( variation yokeMounted size width / lowerPole upperPole topOfPole lat r x y )
	; Drawn upright
	; Argument 'width' is ignored for 'VS1' signal
	(setq
		lowerPole	4.0
		upperPole	(cond ((= size "SMALL") 1.5) ((= size "LARGE") 2.0))
		topOfPole	(+ lowerPole upperPole)
		yokePole	2.0
		lat			(cond ; lateral displacement of arms from centre
						((= size "LARGE") (if (= width "WIDE") 3.5 2.5))
						((= size "SMALL") (if (= width "WIDE") 2.5 1.85))
					)
		r 			(cond ((= size "LARGE") (GetLargeLanternRadius)) ((= size "SMALL") (GetSmallLanternRadius))) ; lantern size
		x 			(cond ((= width "WIDE") (* 2.0 r)) ((= width "NARROW") (* 0.75 2.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
		y  			(cond ((= width "WIDE") (* 1.0 r)) ((= width "NARROW") (* 0.75 1.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
	)
	(FreezeAllLayersExceptCurrentLayer)
	(cond
		((= variation "VS1")
			(DrawLantern (list 0 (- r)) r)	; lower lantern, below origo
			(DrawLantern (list 0 (+ r)) r)	; upper lantern, above origo
			(command
				; St Andrew cross at origo
				_LINE_ (list (- x) (- y))   (list x (+ y)) _ENTER_
				_LINE_ (list (- x) (+ y))   (list x (- y)) _ENTER_
			)
			(if yokeMounted
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (* -2 r)))	; shift down by one lantern
					(DrawVerticalPole yokePole)								; add yoke pole
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (- yokePole)))	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (* 2 r) topOfPole))) ; shift up by one lantern + pole
					(DrawVerticalPole topOfPole)
					(DrawHorizontalHsBase)
				)
			)
		)
		((= variation "VS2")
			; Draw forked part, based at x=0
			(command _LINE_ (list (- lat) 0) (list (+ lat) 0) _ENTER_) ; horizontal line between arms
			(command _LINE_ (list (- lat) 0) (list (- lat) upperPole) _ENTER_) ; left upright arm
			(command _LINE_ (list (+ lat) 0) (list (+ lat) upperPole) _ENTER_)	; right upright arm
			(DrawLantern (list (- lat) (+ upperPole      r )) r)
			(DrawLantern (list (- lat) (+ upperPole (* 3 r))) r)
			(DrawLantern (list (+ lat) (+ upperPole      r)) r)
			(DrawLantern (list (+ lat) (+ upperPole (* 3 r))) r)
			(command 
				; Left arm's St Andrew cross
				_LINE_ (list (+ (- lat) (- x)) (+ (+ upperPole (* 2 r)) (- y)))   (list (+ (- lat) (+ x)) (+ (+ upperPole (* 2 r)) (+ y))) _ENTER_ ; Left '/'
				_LINE_ (list (+ (- lat) (- x)) (+ (+ upperPole (* 2 r)) (+ y)))   (list (+ (- lat) (+ x)) (+ (+ upperPole (* 2 r)) (- y))) _ENTER_ ; Left '\'
				; Right arm's St Andrew cross
				_LINE_ (list (+ (+ lat) (- x)) (+ (+ upperPole (* 2 r)) (- y)))   (list (+ (+ lat) (+ x)) (+ (+ upperPole (* 2 r)) (+ y))) _ENTER_ ; Right '/'
				_LINE_ (list (+ (+ lat) (- x)) (+ (+ upperPole (* 2 r)) (+ y)))   (list (+ (+ lat) (+ x)) (+ (+ upperPole (* 2 r)) (- y))) _ENTER_ ; Right '\'
			)
			(if yokeMounted
				(progn
					(command _MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_)		; flip down around x=0
					(DrawVerticalPole yokePole)								; add yoke pole
					(command _MOVE_ _selectAll_ _ENTER_ (list 0 yokePole) _origo_)	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 lowerPole)) ; shift up by one lantern + lower pole
					(DrawVerticalPole lowerPole)
					(DrawHorizontalHsBase)
				)
			)
		)
	)
	(SetLayer layDef_Zero)
)
