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
			(VEISIGNAL variation yokeMounted )	; Single or double road signal (for level crossing), free-standing mast or yoke mpunted
		)
	)
)



(defun VEISIGNAL ( variation yokeMounted / )
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-" variation (if (= yokeMounted nil) _ENTER_ "-AAK"))
		description "VEISIGNAL (SIGNAL MOT VEITRAFIKK)"
	)
	
	(thawAllLayers)

	; Schematic symbol
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layDef_View_SchematicPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layDef_View_CablePlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "WIDE")
	
	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	(setLayer layDef_View_TrackIsolationPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Create schematic symbol
	(thawAllLayers)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	; Geo 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layDef_View_SchematicPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	; Geo 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layDef_View_CablePlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "NARROW")
	
	; Geo 2-line signaling / track insulation plan OR high voltage / return current plan view
	(setLayer layDef_View_TrackIsolationPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	(thawAllLayers)
	(scaleAll _twoThirds_)
	(createAnnotativeBlockFromCurrentGraphics blockName)
)



;==========================
; draw...X...() functions
;==========================
(defun drawLevelCrossingSignalTowardsRoad ( variation yokeMounted size width / lowerPole upperPole topOfPole lat r x y )
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
		r 			(cond ((= size "LARGE") (getLargeLanternRadius)) ((= size "SMALL") (getSmallLanternRadius))) ; lantern size
		x 			(cond ((= width "WIDE") (* 2.0 r)) ((= width "NARROW") (* 0.75 2.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
		y  			(cond ((= width "WIDE") (* 1.0 r)) ((= width "NARROW") (* 0.75 1.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
	)
	(freezeAllLayersExceptCurrentLayer)
	(cond
		((= variation "VS1")
			(drawLantern (list 0 (- r)) r)	; lower lantern, below origo
			(drawLantern (list 0 (+ r)) r)	; upper lantern, above origo
			(command
				; St Andrew cross at origo
				_LINE_ (list (- x) (- y))   (list x (+ y)) _ENTER_
				_LINE_ (list (- x) (+ y))   (list x (- y)) _ENTER_
			)
			(if yokeMounted
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (* -2 r)))	; shift down by one lantern
					(drawVerticalPole yokePole)								; add yoke pole
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (- yokePole)))	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (* 2 r) topOfPole))) ; shift up by one lantern + pole
					(drawVerticalPole topOfPole)
					(drawHorizontalHsBase)
				)
			)
		)
		((= variation "VS2")
			; Draw forked part, based at x=0
			(command _LINE_ (list (- lat) 0) (list (+ lat) 0) _ENTER_) ; horizontal line between arms
			(command _LINE_ (list (- lat) 0) (list (- lat) upperPole) _ENTER_) ; left upright arm
			(command _LINE_ (list (+ lat) 0) (list (+ lat) upperPole) _ENTER_)	; right upright arm
			(drawLantern (list (- lat) (+ upperPole      r )) r)
			(drawLantern (list (- lat) (+ upperPole (* 3 r))) r)
			(drawLantern (list (+ lat) (+ upperPole      r)) r)
			(drawLantern (list (+ lat) (+ upperPole (* 3 r))) r)
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
					(drawVerticalPole yokePole)								; add yoke pole
					(command _MOVE_ _selectAll_ _ENTER_ (list 0 yokePole) _origo_)	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 lowerPole)) ; shift up by one lantern + lower pole
					(drawVerticalPole lowerPole)
					(drawHorizontalHsBase)
				)
			)
		)
	)
	(setLayer layDef_Zero)
)
